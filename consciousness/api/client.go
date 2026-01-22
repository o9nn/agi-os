package api
import (
"bufio"
"bytes"
"context"
"encoding/json"
"errors"
"fmt"
"io"
"net/http"
"net/url"
"runtime"
"strconv"
"time"
"github.com/EchoCog/echollama/auth"
"github.com/EchoCog/echollama/envconfig"
"github.com/EchoCog/echollama/format"
"github.com/EchoCog/echollama/version"
)
type Client struct {
Base *url.URL
http *http.Client
}
func checkError(resp *http.Response, body []byte) error {
if resp.StatusCode < http.StatusBadRequest {
return nil
}
apiError := StatusError{StatusCode: resp.StatusCode}
err := json.Unmarshal(body, &apiError)
if err != nil {
apiError.ErrorMessage = string(body)
}
return apiError
}
func ClientFromEnvironment() (*Client, error) {
return &Client{
Base: envconfig.Host(),
http: http.DefaultClient,
}, nil
}
func NewClient(base *url.URL, http *http.Client) *Client {
return &Client{
Base: base,
http: http,
}
}
func getAuthorizationToken(ctx context.Context, challenge string) (string, error) {
token, err := auth.Sign(ctx, []byte(challenge))
if err != nil {
return "", err
}
return token, nil
}
func (c *Client) do(ctx context.Context, method, path string, reqData, respData any) error {
var reqBody io.Reader
var data []byte
var err error
switch reqData := reqData.(type) {
case io.Reader:
reqBody = reqData
case nil:
default:
data, err = json.Marshal(reqData)
if err != nil {
return err
}
reqBody = bytes.NewReader(data)
}
requestURL := c.Base.JoinPath(path)
var token string
if envconfig.UseAuth() || c.Base.Hostname() == "ollama.com" {
now := strconv.FormatInt(time.Now().Unix(), 10)
chal := fmt.Sprintf("%s,%s?ts=%s", method, path, now)
token, err = getAuthorizationToken(ctx, chal)
if err != nil {
return err
}
q := requestURL.Query()
q.Set("ts", now)
requestURL.RawQuery = q.Encode()
}
request, err := http.NewRequestWithContext(ctx, method, requestURL.String(), reqBody)
if err != nil {
return err
}
request.Header.Set("Content-Type", "application/json")
request.Header.Set("Accept", "application/json")
request.Header.Set("User-Agent", fmt.Sprintf("ollama/%s (%s %s) Go/%s", version.Version, runtime.GOARCH, runtime.GOOS, runtime.Version()))
if token != "" {
request.Header.Set("Authorization", token)
}
respObj, err := c.http.Do(request)
if err != nil {
return err
}
defer respObj.Body.Close()
respBody, err := io.ReadAll(respObj.Body)
if err != nil {
return err
}
if err := checkError(respObj, respBody); err != nil {
return err
}
if len(respBody) > 0 && respData != nil {
if err := json.Unmarshal(respBody, respData); err != nil {
return err
}
}
return nil
}
const maxBufferSize = 512 * format.KiloByte
func (c *Client) stream(ctx context.Context, method, path string, data any, fn func([]byte) error) error {
var buf io.Reader
if data != nil {
bts, err := json.Marshal(data)
if err != nil {
return err
}
buf = bytes.NewBuffer(bts)
}
requestURL := c.Base.JoinPath(path)
var token string
if envconfig.UseAuth() || c.Base.Hostname() == "ollama.com" {
var err error
now := strconv.FormatInt(time.Now().Unix(), 10)
chal := fmt.Sprintf("%s,%s?ts=%s", method, path, now)
token, err = getAuthorizationToken(ctx, chal)
if err != nil {
return err
}
q := requestURL.Query()
q.Set("ts", now)
requestURL.RawQuery = q.Encode()
}
request, err := http.NewRequestWithContext(ctx, method, requestURL.String(), buf)
if err != nil {
return err
}
request.Header.Set("Content-Type", "application/json")
request.Header.Set("Accept", "application/x-ndjson")
request.Header.Set("User-Agent", fmt.Sprintf("ollama/%s (%s %s) Go/%s", version.Version, runtime.GOARCH, runtime.GOOS, runtime.Version()))
if token != "" {
request.Header.Set("Authorization", token)
}
response, err := c.http.Do(request)
if err != nil {
return err
}
defer response.Body.Close()
scanner := bufio.NewScanner(response.Body)
scanBuf := make([]byte, 0, maxBufferSize)
scanner.Buffer(scanBuf, maxBufferSize)
for scanner.Scan() {
var errorResponse struct {
Error string `json:"error,omitempty"`
}
bts := scanner.Bytes()
if err := json.Unmarshal(bts, &errorResponse); err != nil {
return fmt.Errorf("unmarshal: %w", err)
}
if response.StatusCode >= http.StatusBadRequest {
return StatusError{
StatusCode:   response.StatusCode,
Status:       response.Status,
ErrorMessage: errorResponse.Error,
}
}
if errorResponse.Error != "" {
return errors.New(errorResponse.Error)
}
if err := fn(bts); err != nil {
return err
}
}
return nil
}
type GenerateResponseFunc func(GenerateResponse) error
func (c *Client) Generate(ctx context.Context, req *GenerateRequest, fn GenerateResponseFunc) error {
return c.stream(ctx, http.MethodPost, "/api/generate", req, func(bts []byte) error {
var resp GenerateResponse
if err := json.Unmarshal(bts, &resp); err != nil {
return err
}
return fn(resp)
})
}
type ChatResponseFunc func(ChatResponse) error
func (c *Client) Chat(ctx context.Context, req *ChatRequest, fn ChatResponseFunc) error {
return c.stream(ctx, http.MethodPost, "/api/chat", req, func(bts []byte) error {
var resp ChatResponse
if err := json.Unmarshal(bts, &resp); err != nil {
return err
}
return fn(resp)
})
}
type PullProgressFunc func(ProgressResponse) error
func (c *Client) Pull(ctx context.Context, req *PullRequest, fn PullProgressFunc) error {
return c.stream(ctx, http.MethodPost, "/api/pull", req, func(bts []byte) error {
var resp ProgressResponse
if err := json.Unmarshal(bts, &resp); err != nil {
return err
}
return fn(resp)
})
}
type PushProgressFunc func(ProgressResponse) error
func (c *Client) Push(ctx context.Context, req *PushRequest, fn PushProgressFunc) error {
return c.stream(ctx, http.MethodPost, "/api/push", req, func(bts []byte) error {
var resp ProgressResponse
if err := json.Unmarshal(bts, &resp); err != nil {
return err
}
return fn(resp)
})
}
type CreateProgressFunc func(ProgressResponse) error
func (c *Client) Create(ctx context.Context, req *CreateRequest, fn CreateProgressFunc) error {
return c.stream(ctx, http.MethodPost, "/api/create", req, func(bts []byte) error {
var resp ProgressResponse
if err := json.Unmarshal(bts, &resp); err != nil {
return err
}
return fn(resp)
})
}
func (c *Client) List(ctx context.Context) (*ListResponse, error) {
var lr ListResponse
if err := c.do(ctx, http.MethodGet, "/api/tags", nil, &lr); err != nil {
return nil, err
}
return &lr, nil
}
func (c *Client) ListRunning(ctx context.Context) (*ProcessResponse, error) {
var lr ProcessResponse
if err := c.do(ctx, http.MethodGet, "/api/ps", nil, &lr); err != nil {
return nil, err
}
return &lr, nil
}
func (c *Client) Copy(ctx context.Context, req *CopyRequest) error {
if err := c.do(ctx, http.MethodPost, "/api/copy", req, nil); err != nil {
return err
}
return nil
}
func (c *Client) Delete(ctx context.Context, req *DeleteRequest) error {
if err := c.do(ctx, http.MethodDelete, "/api/delete", req, nil); err != nil {
return err
}
return nil
}
func (c *Client) Show(ctx context.Context, req *ShowRequest) (*ShowResponse, error) {
var resp ShowResponse
if err := c.do(ctx, http.MethodPost, "/api/show", req, &resp); err != nil {
return nil, err
}
return &resp, nil
}
func (c *Client) Heartbeat(ctx context.Context) error {
if err := c.do(ctx, http.MethodHead, "/", nil, nil); err != nil {
return err
}
return nil
}
func (c *Client) Embed(ctx context.Context, req *EmbedRequest) (*EmbedResponse, error) {
var resp EmbedResponse
if err := c.do(ctx, http.MethodPost, "/api/embed", req, &resp); err != nil {
return nil, err
}
return &resp, nil
}
func (c *Client) Embeddings(ctx context.Context, req *EmbeddingRequest) (*EmbeddingResponse, error) {
var resp EmbeddingResponse
if err := c.do(ctx, http.MethodPost, "/api/embeddings", req, &resp); err != nil {
return nil, err
}
return &resp, nil
}
func (c *Client) CreateBlob(ctx context.Context, digest string, r io.Reader) error {
return c.do(ctx, http.MethodPost, fmt.Sprintf("/api/blobs/%s", digest), r, nil)
}
func (c *Client) Version(ctx context.Context) (string, error) {
var version struct {
Version string `json:"version"`
}
if err := c.do(ctx, http.MethodGet, "/api/version", nil, &version); err != nil {
return "", err
}
return version.Version, nil
}
func (c *Client) BaseURL() *url.URL {
return c.Base
}
func (c *Client) Do(req *http.Request) (*http.Response, error) {
return c.http.Do(req)
}