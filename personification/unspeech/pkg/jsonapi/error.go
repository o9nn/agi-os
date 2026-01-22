package jsonapi
import (
"github.com/moeru-ai/unspeech/pkg/logs"
"github.com/samber/mo"
)
type Links struct {
Href string `json:"href,omitempty"`
Rel mo.Option[string] `json:"rel,omitempty"`
Describedby mo.Option[string] `json:"describedby,omitempty"`
Title mo.Option[string] `json:"title,omitempty"`
Type mo.Option[string] `json:"type,omitempty"`
Hreflang mo.Option[string] `json:"hreflang,omitempty"`
Meta mo.Option[map[string]any] `json:"meta,omitempty"`
}
type ErrorObjectSource struct {
Pointer string `json:"pointer,omitempty"`
Parameter string `json:"parameter,omitempty"`
Header string `json:"header,omitempty"`
}
type ErrorObject struct {
ID string `json:"id,omitempty"`
Links mo.Option[*Links] `json:"links,omitempty"`
Status int `json:"status,omitempty"`
Code string `json:"code,omitempty"`
Title string `json:"title,omitempty"`
Detail string `json:"detail,omitempty"`
Source mo.Option[ErrorObjectSource] `json:"source,omitempty"`
Meta mo.Option[map[string]any] `json:"meta,omitempty"`
}
var _ logs.CallerLike = (*ErrorCaller)(nil)
type ErrorCaller struct {
File     string `json:"file,omitempty"`
Line     int64  `json:"line,omitempty"`
Function string `json:"function,omitempty"`
}
func (e *ErrorCaller) GetFile() string {
return e.File
}
func (e *ErrorCaller) GetLine() int64 {
return e.Line
}
func (e *ErrorCaller) GetFunction() string {
return e.Function
}