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
BotServiceName = "galatea.v1.BotService"
)
const (
BotServiceBotAllProcedure = "/galatea.v1.BotService/BotAll"
BotServiceBotCreateProcedure = "/galatea.v1.BotService/BotCreate"
BotServiceBotUpdateProcedure = "/galatea.v1.BotService/BotUpdate"
BotServiceBotDeleteProcedure = "/galatea.v1.BotService/BotDelete"
)
type BotServiceClient interface {
BotAll(context.Context, *connect_go.Request[v1.BotAllRequest]) (*connect_go.Response[v1.BotAllResponse], error)
BotCreate(context.Context, *connect_go.Request[v1.BotCreateRequest]) (*connect_go.Response[v1.BotCreateResponse], error)
BotUpdate(context.Context, *connect_go.Request[v1.BotUpdateRequest]) (*connect_go.Response[v1.BotUpdateResponse], error)
BotDelete(context.Context, *connect_go.Request[v1.BotDeleteRequest]) (*connect_go.Response[v1.BotDeleteResponse], error)
}
func NewBotServiceClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) BotServiceClient {
baseURL = strings.TrimRight(baseURL, "/")
return &botServiceClient{
botAll: connect_go.NewClient[v1.BotAllRequest, v1.BotAllResponse](
httpClient,
baseURL+BotServiceBotAllProcedure,
opts...,
),
botCreate: connect_go.NewClient[v1.BotCreateRequest, v1.BotCreateResponse](
httpClient,
baseURL+BotServiceBotCreateProcedure,
opts...,
),
botUpdate: connect_go.NewClient[v1.BotUpdateRequest, v1.BotUpdateResponse](
httpClient,
baseURL+BotServiceBotUpdateProcedure,
opts...,
),
botDelete: connect_go.NewClient[v1.BotDeleteRequest, v1.BotDeleteResponse](
httpClient,
baseURL+BotServiceBotDeleteProcedure,
opts...,
),
}
}
type botServiceClient struct {
botAll    *connect_go.Client[v1.BotAllRequest, v1.BotAllResponse]
botCreate *connect_go.Client[v1.BotCreateRequest, v1.BotCreateResponse]
botUpdate *connect_go.Client[v1.BotUpdateRequest, v1.BotUpdateResponse]
botDelete *connect_go.Client[v1.BotDeleteRequest, v1.BotDeleteResponse]
}
func (c *botServiceClient) BotAll(ctx context.Context, req *connect_go.Request[v1.BotAllRequest]) (*connect_go.Response[v1.BotAllResponse], error) {
return c.botAll.CallUnary(ctx, req)
}
func (c *botServiceClient) BotCreate(ctx context.Context, req *connect_go.Request[v1.BotCreateRequest]) (*connect_go.Response[v1.BotCreateResponse], error) {
return c.botCreate.CallUnary(ctx, req)
}
func (c *botServiceClient) BotUpdate(ctx context.Context, req *connect_go.Request[v1.BotUpdateRequest]) (*connect_go.Response[v1.BotUpdateResponse], error) {
return c.botUpdate.CallUnary(ctx, req)
}
func (c *botServiceClient) BotDelete(ctx context.Context, req *connect_go.Request[v1.BotDeleteRequest]) (*connect_go.Response[v1.BotDeleteResponse], error) {
return c.botDelete.CallUnary(ctx, req)
}
type BotServiceHandler interface {
BotAll(context.Context, *connect_go.Request[v1.BotAllRequest]) (*connect_go.Response[v1.BotAllResponse], error)
BotCreate(context.Context, *connect_go.Request[v1.BotCreateRequest]) (*connect_go.Response[v1.BotCreateResponse], error)
BotUpdate(context.Context, *connect_go.Request[v1.BotUpdateRequest]) (*connect_go.Response[v1.BotUpdateResponse], error)
BotDelete(context.Context, *connect_go.Request[v1.BotDeleteRequest]) (*connect_go.Response[v1.BotDeleteResponse], error)
}
func NewBotServiceHandler(svc BotServiceHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
mux := http.NewServeMux()
mux.Handle(BotServiceBotAllProcedure, connect_go.NewUnaryHandler(
BotServiceBotAllProcedure,
svc.BotAll,
opts...,
))
mux.Handle(BotServiceBotCreateProcedure, connect_go.NewUnaryHandler(
BotServiceBotCreateProcedure,
svc.BotCreate,
opts...,
))
mux.Handle(BotServiceBotUpdateProcedure, connect_go.NewUnaryHandler(
BotServiceBotUpdateProcedure,
svc.BotUpdate,
opts...,
))
mux.Handle(BotServiceBotDeleteProcedure, connect_go.NewUnaryHandler(
BotServiceBotDeleteProcedure,
svc.BotDelete,
opts...,
))
return "/galatea.v1.BotService/", mux
}
type UnimplementedBotServiceHandler struct{}
func (UnimplementedBotServiceHandler) BotAll(context.Context, *connect_go.Request[v1.BotAllRequest]) (*connect_go.Response[v1.BotAllResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.BotService.BotAll is not implemented"))
}
func (UnimplementedBotServiceHandler) BotCreate(context.Context, *connect_go.Request[v1.BotCreateRequest]) (*connect_go.Response[v1.BotCreateResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.BotService.BotCreate is not implemented"))
}
func (UnimplementedBotServiceHandler) BotUpdate(context.Context, *connect_go.Request[v1.BotUpdateRequest]) (*connect_go.Response[v1.BotUpdateResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.BotService.BotUpdate is not implemented"))
}
func (UnimplementedBotServiceHandler) BotDelete(context.Context, *connect_go.Request[v1.BotDeleteRequest]) (*connect_go.Response[v1.BotDeleteResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.BotService.BotDelete is not implemented"))
}