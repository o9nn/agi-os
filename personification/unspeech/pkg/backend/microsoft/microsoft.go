package microsoft
import (
"crypto/tls"
"log/slog"
"net/http"
"strings"
"github.com/moeru-ai/unspeech/pkg/apierrors"
"github.com/moeru-ai/unspeech/pkg/utils"
"github.com/samber/mo"
)
var (
httpClient = &http.Client{
Transport: &http.Transport{
TLSNextProto: make(map[string]func(authority string, c *tls.Conn) http.RoundTripper),
},
}
)
func handleResponseError(res *http.Response) mo.Result[any] {
if res.Header.Get("Content-Length") == "" || res.Header.Get("Content-Length") == "0" {
return mo.Err[any](apierrors.NewUpstreamError(res.StatusCode).WithDetail(res.Status))
}
switch {
case strings.HasPrefix(res.Header.Get("Content-Type"), "application/json"):
return mo.Err[any](apierrors.
NewUpstreamError(res.StatusCode).
WithDetail(utils.NewJSONResponseError(res.StatusCode, res.Body).OrEmpty().Error()))
case strings.HasPrefix(res.Header.Get("Content-Type"), "text/"):
return mo.Err[any](apierrors.
NewUpstreamError(res.StatusCode).
WithDetail(utils.NewTextResponseError(res.StatusCode, res.Body).OrEmpty().Error()))
default:
slog.Warn("unknown upstream error with unknown Content-Type",
slog.Int("status", res.StatusCode),
slog.String("content_type", res.Header.Get("Content-Type")),
slog.String("content_length", res.Header.Get("Content-Length")),
)
return mo.Err[any](apierrors.NewUpstreamError(res.StatusCode).WithDetail(res.Status))
}
}