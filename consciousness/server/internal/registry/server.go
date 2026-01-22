package registry
import (
"cmp"
"context"
"encoding/json"
"errors"
"fmt"
"io"
"log/slog"
"net/http"
"slices"
"strings"
"sync"
"time"
"github.com/EchoCog/echollama/server/internal/cache/blob"
"github.com/EchoCog/echollama/server/internal/client/ollama"
"github.com/EchoCog/echollama/server/internal/internal/backoff"
)
type Local struct {
Client *ollama.Registry
Logger *slog.Logger
Fallback http.Handler
Prune func() error
}
type serverError struct {
Status int `json:"-"`
Code string `json:"code"`
Message string `json:"error"`
}
func (e serverError) Error() string {
return e.Message
}
var (
errMethodNotAllowed = &serverError{405, "method_not_allowed", "method not allowed"}
errNotFound         = &serverError{404, "not_found", "not found"}
errModelNotFound    = &serverError{404, "not_found", "model not found"}
errInternalError    = &serverError{500, "internal_error", "internal server error"}
)
type statusCodeRecorder struct {
_status int
http.ResponseWriter
}
func (r *statusCodeRecorder) WriteHeader(status int) {
if r._status == 0 {
r._status = status
r.ResponseWriter.WriteHeader(status)
}
}
func (r *statusCodeRecorder) Write(b []byte) (int, error) {
r._status = r.status()
return r.ResponseWriter.Write(b)
}
var (
_ http.ResponseWriter = (*statusCodeRecorder)(nil)
_ http.CloseNotifier  = (*statusCodeRecorder)(nil)
_ http.Flusher        = (*statusCodeRecorder)(nil)
)
func (r *statusCodeRecorder) CloseNotify() <-chan bool {
return r.ResponseWriter.(http.CloseNotifier).CloseNotify()
}
func (r *statusCodeRecorder) Flush() {
r.ResponseWriter.(http.Flusher).Flush()
}
func (r *statusCodeRecorder) status() int {
return cmp.Or(r._status, 200)
}
func (s *Local) ServeHTTP(w http.ResponseWriter, r *http.Request) {
rec := &statusCodeRecorder{ResponseWriter: w}
s.serveHTTP(rec, r)
}
func (s *Local) serveHTTP(rec *statusCodeRecorder, r *http.Request) {
var errattr slog.Attr
proxied, err := func() (bool, error) {
switch r.URL.Path {
case "/api/delete":
return false, s.handleDelete(rec, r)
case "/api/pull":
return false, s.handlePull(rec, r)
default:
if s.Fallback != nil {
s.Fallback.ServeHTTP(rec, r)
return true, nil
}
return false, errNotFound
}
}()
if err != nil {
errattr = slog.String("error", err.Error())
var e *serverError
switch {
case errors.As(err, &e):
case errors.Is(err, ollama.ErrNameInvalid):
e = &serverError{400, "bad_request", err.Error()}
default:
e = errInternalError
}
data, err := json.Marshal(e)
if err != nil {
panic(err)
}
rec.Header().Set("Content-Type", "application/json")
rec.WriteHeader(e.Status)
rec.Write(data)
}
if !proxied {
var level slog.Level
if rec.status() >= 500 {
level = slog.LevelError
} else if rec.status() >= 400 {
level = slog.LevelWarn
}
s.Logger.LogAttrs(r.Context(), level, "http",
errattr,
slog.Int("status", rec.status()),
slog.String("method", r.Method),
slog.String("path", r.URL.Path),
slog.Int64("content-length", r.ContentLength),
slog.String("remote", r.RemoteAddr),
slog.String("proto", r.Proto),
slog.String("query", r.URL.RawQuery),
)
}
}
type params struct {
DeprecatedName string `json:"name"`
Model string `json:"model"`
AllowNonTLS bool `json:"insecure"`
Stream *bool `json:"stream"`
}
func (p params) model() string {
return cmp.Or(p.Model, p.DeprecatedName)
}
func (p params) stream() bool {
if p.Stream == nil {
return true
}
return *p.Stream
}
func (s *Local) handleDelete(_ http.ResponseWriter, r *http.Request) error {
if r.Method != "DELETE" {
return errMethodNotAllowed
}
p, err := decodeUserJSON[*params](r.Body)
if err != nil {
return err
}
ok, err := s.Client.Unlink(p.model())
if err != nil {
return err
}
if !ok {
return errModelNotFound
}
if s.Prune != nil {
return s.Prune()
}
return nil
}
type progressUpdateJSON struct {
Error     string      `json:"error,omitempty,omitzero"`
Status    string      `json:"status,omitempty,omitzero"`
Digest    blob.Digest `json:"digest,omitempty,omitzero"`
Total     int64       `json:"total,omitempty,omitzero"`
Completed int64       `json:"completed,omitempty,omitzero"`
}
func (s *Local) handlePull(w http.ResponseWriter, r *http.Request) error {
if r.Method != "POST" {
return errMethodNotAllowed
}
p, err := decodeUserJSON[*params](r.Body)
if err != nil {
return err
}
enc := json.NewEncoder(w)
if !p.stream() {
if err := s.Client.Pull(r.Context(), p.model()); err != nil {
if errors.Is(err, ollama.ErrModelNotFound) {
return errModelNotFound
}
return err
}
enc.Encode(progressUpdateJSON{Status: "success"})
return nil
}
var mu sync.Mutex
var progress []progressUpdateJSON
flushProgress := func() {
mu.Lock()
progress := slices.Clone(progress)
mu.Unlock()
for _, p := range progress {
enc.Encode(p)
}
fl, _ := w.(http.Flusher)
if fl != nil {
fl.Flush()
}
}
t := time.NewTicker(1<<63 - 1)
start := sync.OnceFunc(func() {
flushProgress()
t.Reset(100 * time.Millisecond)
})
ctx := ollama.WithTrace(r.Context(), &ollama.Trace{
Update: func(l *ollama.Layer, n int64, err error) {
if err != nil && !errors.Is(err, ollama.ErrCached) {
s.Logger.Error("pulling", "model", p.model(), "error", err)
return
}
func() {
mu.Lock()
defer mu.Unlock()
for i, p := range progress {
if p.Digest == l.Digest {
progress[i].Completed = n
return
}
}
progress = append(progress, progressUpdateJSON{
Digest: l.Digest,
Total:  l.Size,
})
}()
start()
},
})
done := make(chan error, 1)
go func() (err error) {
defer func() { done <- err }()
for _, err := range backoff.Loop(ctx, 3*time.Second) {
if err != nil {
return err
}
err := s.Client.Pull(ctx, p.model())
if canRetry(err) {
continue
}
return err
}
return nil
}()
enc.Encode(progressUpdateJSON{Status: "pulling manifest"})
for {
select {
case <-t.C:
flushProgress()
case err := <-done:
flushProgress()
if err != nil {
if errors.Is(err, ollama.ErrModelNotFound) {
return &serverError{
Status:  404,
Code:    "not_found",
Message: fmt.Sprintf("model %q not found", p.model()),
}
} else {
return err
}
}
enc.Encode(progressUpdateJSON{Status: "verifying sha256 digest"})
enc.Encode(progressUpdateJSON{Status: "writing manifest"})
enc.Encode(progressUpdateJSON{Status: "success"})
return nil
}
}
}
func decodeUserJSON[T any](r io.Reader) (T, error) {
var v T
err := json.NewDecoder(r).Decode(&v)
if err == nil {
return v, nil
}
var zero T
var a *json.UnmarshalTypeError
var b *json.SyntaxError
if errors.As(err, &a) || errors.As(err, &b) {
err = &serverError{Status: 400, Message: err.Error(), Code: "bad_request"}
}
if errors.Is(err, io.EOF) {
err = &serverError{Status: 400, Message: "empty request body", Code: "bad_request"}
}
return zero, err
}
func canRetry(err error) bool {
if err == nil {
return false
}
var oe *ollama.Error
if errors.As(err, &oe) {
return oe.Temporary()
}
s := err.Error()
return cmp.Or(
errors.Is(err, context.DeadlineExceeded),
strings.Contains(s, "unreachable"),
strings.Contains(s, "no route to host"),
strings.Contains(s, "connection reset by peer"),
)
}