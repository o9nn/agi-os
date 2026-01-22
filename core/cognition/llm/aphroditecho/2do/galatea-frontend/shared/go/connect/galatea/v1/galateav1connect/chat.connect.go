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
ChatServiceName = "galatea.v1.ChatService"
)
const (
ChatServiceChatAllProcedure = "/galatea.v1.ChatService/ChatAll"
ChatServiceChatGetProcedure = "/galatea.v1.ChatService/ChatGet"
ChatServiceChatCreateProcedure = "/galatea.v1.ChatService/ChatCreate"
ChatServiceChatDeleteProcedure = "/galatea.v1.ChatService/ChatDelete"
ChatServiceChatSendMessageProcedure = "/galatea.v1.ChatService/ChatSendMessage"
ChatServiceChatSubscribeProcedure = "/galatea.v1.ChatService/ChatSubscribe"
)
type ChatServiceClient interface {
ChatAll(context.Context, *connect_go.Request[v1.ChatAllRequest]) (*connect_go.Response[v1.ChatAllResponse], error)
ChatGet(context.Context, *connect_go.Request[v1.ChatGetRequest]) (*connect_go.Response[v1.ChatGetResponse], error)
ChatCreate(context.Context, *connect_go.Request[v1.ChatCreateRequest]) (*connect_go.Response[v1.ChatCreateResponse], error)
ChatDelete(context.Context, *connect_go.Request[v1.ChatDeleteRequest]) (*connect_go.Response[v1.ChatDeleteResponse], error)
ChatSendMessage(context.Context, *connect_go.Request[v1.ChatSendMessageRequest]) (*connect_go.Response[v1.ChatSendMessageResponse], error)
ChatSubscribe(context.Context, *connect_go.Request[v1.ChatSubscribeRequest]) (*connect_go.ServerStreamForClient[v1.ChatSubscribeResponse], error)
}
func NewChatServiceClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) ChatServiceClient {
baseURL = strings.TrimRight(baseURL, "/")
return &chatServiceClient{
chatAll: connect_go.NewClient[v1.ChatAllRequest, v1.ChatAllResponse](
httpClient,
baseURL+ChatServiceChatAllProcedure,
opts...,
),
chatGet: connect_go.NewClient[v1.ChatGetRequest, v1.ChatGetResponse](
httpClient,
baseURL+ChatServiceChatGetProcedure,
opts...,
),
chatCreate: connect_go.NewClient[v1.ChatCreateRequest, v1.ChatCreateResponse](
httpClient,
baseURL+ChatServiceChatCreateProcedure,
opts...,
),
chatDelete: connect_go.NewClient[v1.ChatDeleteRequest, v1.ChatDeleteResponse](
httpClient,
baseURL+ChatServiceChatDeleteProcedure,
opts...,
),
chatSendMessage: connect_go.NewClient[v1.ChatSendMessageRequest, v1.ChatSendMessageResponse](
httpClient,
baseURL+ChatServiceChatSendMessageProcedure,
opts...,
),
chatSubscribe: connect_go.NewClient[v1.ChatSubscribeRequest, v1.ChatSubscribeResponse](
httpClient,
baseURL+ChatServiceChatSubscribeProcedure,
opts...,
),
}
}
type chatServiceClient struct {
chatAll         *connect_go.Client[v1.ChatAllRequest, v1.ChatAllResponse]
chatGet         *connect_go.Client[v1.ChatGetRequest, v1.ChatGetResponse]
chatCreate      *connect_go.Client[v1.ChatCreateRequest, v1.ChatCreateResponse]
chatDelete      *connect_go.Client[v1.ChatDeleteRequest, v1.ChatDeleteResponse]
chatSendMessage *connect_go.Client[v1.ChatSendMessageRequest, v1.ChatSendMessageResponse]
chatSubscribe   *connect_go.Client[v1.ChatSubscribeRequest, v1.ChatSubscribeResponse]
}
func (c *chatServiceClient) ChatAll(ctx context.Context, req *connect_go.Request[v1.ChatAllRequest]) (*connect_go.Response[v1.ChatAllResponse], error) {
return c.chatAll.CallUnary(ctx, req)
}
func (c *chatServiceClient) ChatGet(ctx context.Context, req *connect_go.Request[v1.ChatGetRequest]) (*connect_go.Response[v1.ChatGetResponse], error) {
return c.chatGet.CallUnary(ctx, req)
}
func (c *chatServiceClient) ChatCreate(ctx context.Context, req *connect_go.Request[v1.ChatCreateRequest]) (*connect_go.Response[v1.ChatCreateResponse], error) {
return c.chatCreate.CallUnary(ctx, req)
}
func (c *chatServiceClient) ChatDelete(ctx context.Context, req *connect_go.Request[v1.ChatDeleteRequest]) (*connect_go.Response[v1.ChatDeleteResponse], error) {
return c.chatDelete.CallUnary(ctx, req)
}
func (c *chatServiceClient) ChatSendMessage(ctx context.Context, req *connect_go.Request[v1.ChatSendMessageRequest]) (*connect_go.Response[v1.ChatSendMessageResponse], error) {
return c.chatSendMessage.CallUnary(ctx, req)
}
func (c *chatServiceClient) ChatSubscribe(ctx context.Context, req *connect_go.Request[v1.ChatSubscribeRequest]) (*connect_go.ServerStreamForClient[v1.ChatSubscribeResponse], error) {
return c.chatSubscribe.CallServerStream(ctx, req)
}
type ChatServiceHandler interface {
ChatAll(context.Context, *connect_go.Request[v1.ChatAllRequest]) (*connect_go.Response[v1.ChatAllResponse], error)
ChatGet(context.Context, *connect_go.Request[v1.ChatGetRequest]) (*connect_go.Response[v1.ChatGetResponse], error)
ChatCreate(context.Context, *connect_go.Request[v1.ChatCreateRequest]) (*connect_go.Response[v1.ChatCreateResponse], error)
ChatDelete(context.Context, *connect_go.Request[v1.ChatDeleteRequest]) (*connect_go.Response[v1.ChatDeleteResponse], error)
ChatSendMessage(context.Context, *connect_go.Request[v1.ChatSendMessageRequest]) (*connect_go.Response[v1.ChatSendMessageResponse], error)
ChatSubscribe(context.Context, *connect_go.Request[v1.ChatSubscribeRequest], *connect_go.ServerStream[v1.ChatSubscribeResponse]) error
}
func NewChatServiceHandler(svc ChatServiceHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
mux := http.NewServeMux()
mux.Handle(ChatServiceChatAllProcedure, connect_go.NewUnaryHandler(
ChatServiceChatAllProcedure,
svc.ChatAll,
opts...,
))
mux.Handle(ChatServiceChatGetProcedure, connect_go.NewUnaryHandler(
ChatServiceChatGetProcedure,
svc.ChatGet,
opts...,
))
mux.Handle(ChatServiceChatCreateProcedure, connect_go.NewUnaryHandler(
ChatServiceChatCreateProcedure,
svc.ChatCreate,
opts...,
))
mux.Handle(ChatServiceChatDeleteProcedure, connect_go.NewUnaryHandler(
ChatServiceChatDeleteProcedure,
svc.ChatDelete,
opts...,
))
mux.Handle(ChatServiceChatSendMessageProcedure, connect_go.NewUnaryHandler(
ChatServiceChatSendMessageProcedure,
svc.ChatSendMessage,
opts...,
))
mux.Handle(ChatServiceChatSubscribeProcedure, connect_go.NewServerStreamHandler(
ChatServiceChatSubscribeProcedure,
svc.ChatSubscribe,
opts...,
))
return "/galatea.v1.ChatService/", mux
}
type UnimplementedChatServiceHandler struct{}
func (UnimplementedChatServiceHandler) ChatAll(context.Context, *connect_go.Request[v1.ChatAllRequest]) (*connect_go.Response[v1.ChatAllResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatAll is not implemented"))
}
func (UnimplementedChatServiceHandler) ChatGet(context.Context, *connect_go.Request[v1.ChatGetRequest]) (*connect_go.Response[v1.ChatGetResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatGet is not implemented"))
}
func (UnimplementedChatServiceHandler) ChatCreate(context.Context, *connect_go.Request[v1.ChatCreateRequest]) (*connect_go.Response[v1.ChatCreateResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatCreate is not implemented"))
}
func (UnimplementedChatServiceHandler) ChatDelete(context.Context, *connect_go.Request[v1.ChatDeleteRequest]) (*connect_go.Response[v1.ChatDeleteResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatDelete is not implemented"))
}
func (UnimplementedChatServiceHandler) ChatSendMessage(context.Context, *connect_go.Request[v1.ChatSendMessageRequest]) (*connect_go.Response[v1.ChatSendMessageResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatSendMessage is not implemented"))
}
func (UnimplementedChatServiceHandler) ChatSubscribe(context.Context, *connect_go.Request[v1.ChatSubscribeRequest], *connect_go.ServerStream[v1.ChatSubscribeResponse]) error {
return connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.ChatService.ChatSubscribe is not implemented"))
}