package testutil
import (
"bytes"
"io"
"log/slog"
"os"
"path/filepath"
"testing"
"time"
)
func LogWriter(t *testing.T) io.Writer {
return testWriter{t}
}
type testWriter struct{ t *testing.T }
func (w testWriter) Write(b []byte) (int, error) {
w.t.Logf("%s", b)
return len(b), nil
}
func Slogger(t *testing.T) *slog.Logger {
return slog.New(slog.NewTextHandler(LogWriter(t), nil))
}
func SlogBuffer() (lg *slog.Logger, out *bytes.Buffer) {
var buf bytes.Buffer
lg = slog.New(slog.NewTextHandler(&buf, nil))
return lg, &buf
}
func Check(t *testing.T, err error) {
if err != nil {
t.Helper()
t.Fatal(err)
}
}
type CheckFunc func(err error)
func Checker(t *testing.T) (check func(err error)) {
return func(err error) {
if err != nil {
t.Helper()
t.Fatal(err)
}
}
}
func StopPanic(f func()) {
defer func() { recover() }()
f()
}
func CheckTime(t *testing.T, got, want time.Time) {
t.Helper()
if !got.Equal(want) {
t.Fatalf("got %v, want %v (%v)", got.UTC(), want.UTC(), want.Sub(got))
}
}
func WriteFile[S []byte | string](t testing.TB, name string, data S) {
t.Helper()
if filepath.IsAbs(name) {
t.Fatalf("WriteFile: name must be a relative path, got %q", name)
}
name = filepath.Clean(name)
dir := filepath.Dir(name)
if err := os.MkdirAll(dir, 0o755); err != nil {
t.Fatal(err)
}
if err := os.WriteFile(name, []byte(data), 0o644); err != nil {
t.Fatal(err)
}
}