package galateav1connect
import (
context "context"
errors "errors"
connect_go "github.com/bufbuild/connect-go"
http "net/http"
v1 "shared/go/pb/galatea/v1"
strings "strings"
)
const _ = connect_go.IsAtLeastVersion0_1_0
const (
ImageUploadServiceName = "galatea.v1.ImageUploadService"
)
const (
ImageUploadServiceImageUploadProcedure = "/galatea.v1.ImageUploadService/ImageUpload"
)
type ImageUploadServiceClient interface {
ImageUpload(context.Context, *connect_go.Request[v1.ImageUploadRequest]) (*connect_go.Response[v1.ImageUploadResponse], error)
}
func NewImageUploadServiceClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) ImageUploadServiceClient {
baseURL = strings.TrimRight(baseURL, "/")
return &imageUploadServiceClient{
imageUpload: connect_go.NewClient[v1.ImageUploadRequest, v1.ImageUploadResponse](
httpClient,
baseURL+ImageUploadServiceImageUploadProcedure,
opts...,
),
}
}
type imageUploadServiceClient struct {
imageUpload *connect_go.Client[v1.ImageUploadRequest, v1.ImageUploadResponse]
}
func (c *imageUploadServiceClient) ImageUpload(ctx context.Context, req *connect_go.Request[v1.ImageUploadRequest]) (*connect_go.Response[v1.ImageUploadResponse], error) {
return c.imageUpload.CallUnary(ctx, req)
}
type ImageUploadServiceHandler interface {
ImageUpload(context.Context, *connect_go.Request[v1.ImageUploadRequest]) (*connect_go.Response[v1.ImageUploadResponse], error)
}
func NewImageUploadServiceHandler(svc ImageUploadServiceHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
mux := http.NewServeMux()
mux.Handle(ImageUploadServiceImageUploadProcedure, connect_go.NewUnaryHandler(
ImageUploadServiceImageUploadProcedure,
svc.ImageUpload,
opts...,
))
return "/galatea.v1.ImageUploadService/", mux
}
type UnimplementedImageUploadServiceHandler struct{}
func (UnimplementedImageUploadServiceHandler) ImageUpload(context.Context, *connect_go.Request[v1.ImageUploadRequest]) (*connect_go.Response[v1.ImageUploadResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ImageUploadService.ImageUpload is not implemented"))
}