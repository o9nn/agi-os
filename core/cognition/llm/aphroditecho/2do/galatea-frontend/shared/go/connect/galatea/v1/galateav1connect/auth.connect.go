package galateav1connect
import (
context "context"
errors "errors"
connect_go "github.com/bufbuild/connect-go"
http "net/http"
v1 "shared/go/pb/galatea/v1"
strings "strings"
)
const _ = connect_go.IsAtLeastVersion1_7_0
const (
AuthServiceName = "galatea.v1.AuthService"
)
const (
AuthServiceSignInProcedure = "/galatea.v1.AuthService/SignIn"
AuthServiceSignUpProcedure = "/galatea.v1.AuthService/SignUp"
AuthServiceVerifyProcedure = "/galatea.v1.AuthService/Verify"
AuthServiceCheckProcedure = "/galatea.v1.AuthService/Check"
)
type AuthServiceClient interface {
SignIn(context.Context, *connect_go.Request[v1.SignInRequest]) (*connect_go.Response[v1.SignInResponse], error)
SignUp(context.Context, *connect_go.Request[v1.SignUpRequest]) (*connect_go.Response[v1.SignUpResponse], error)
Verify(context.Context, *connect_go.Request[v1.VerifyRequest]) (*connect_go.Response[v1.VerifyResponse], error)
Check(context.Context, *connect_go.Request[v1.CheckRequest]) (*connect_go.Response[v1.CheckResponse], error)
}
func NewAuthServiceClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) AuthServiceClient {
baseURL = strings.TrimRight(baseURL, "/")
return &authServiceClient{
signIn: connect_go.NewClient[v1.SignInRequest, v1.SignInResponse](
httpClient,
baseURL+AuthServiceSignInProcedure,
opts...,
),
signUp: connect_go.NewClient[v1.SignUpRequest, v1.SignUpResponse](
httpClient,
baseURL+AuthServiceSignUpProcedure,
opts...,
),
verify: connect_go.NewClient[v1.VerifyRequest, v1.VerifyResponse](
httpClient,
baseURL+AuthServiceVerifyProcedure,
opts...,
),
check: connect_go.NewClient[v1.CheckRequest, v1.CheckResponse](
httpClient,
baseURL+AuthServiceCheckProcedure,
connect_go.WithIdempotency(connect_go.IdempotencyNoSideEffects),
connect_go.WithClientOptions(opts...),
),
}
}
type authServiceClient struct {
signIn *connect_go.Client[v1.SignInRequest, v1.SignInResponse]
signUp *connect_go.Client[v1.SignUpRequest, v1.SignUpResponse]
verify *connect_go.Client[v1.VerifyRequest, v1.VerifyResponse]
check  *connect_go.Client[v1.CheckRequest, v1.CheckResponse]
}
func (c *authServiceClient) SignIn(ctx context.Context, req *connect_go.Request[v1.SignInRequest]) (*connect_go.Response[v1.SignInResponse], error) {
return c.signIn.CallUnary(ctx, req)
}
func (c *authServiceClient) SignUp(ctx context.Context, req *connect_go.Request[v1.SignUpRequest]) (*connect_go.Response[v1.SignUpResponse], error) {
return c.signUp.CallUnary(ctx, req)
}
func (c *authServiceClient) Verify(ctx context.Context, req *connect_go.Request[v1.VerifyRequest]) (*connect_go.Response[v1.VerifyResponse], error) {
return c.verify.CallUnary(ctx, req)
}
func (c *authServiceClient) Check(ctx context.Context, req *connect_go.Request[v1.CheckRequest]) (*connect_go.Response[v1.CheckResponse], error) {
return c.check.CallUnary(ctx, req)
}
type AuthServiceHandler interface {
SignIn(context.Context, *connect_go.Request[v1.SignInRequest]) (*connect_go.Response[v1.SignInResponse], error)
SignUp(context.Context, *connect_go.Request[v1.SignUpRequest]) (*connect_go.Response[v1.SignUpResponse], error)
Verify(context.Context, *connect_go.Request[v1.VerifyRequest]) (*connect_go.Response[v1.VerifyResponse], error)
Check(context.Context, *connect_go.Request[v1.CheckRequest]) (*connect_go.Response[v1.CheckResponse], error)
}
func NewAuthServiceHandler(svc AuthServiceHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
mux := http.NewServeMux()
mux.Handle(AuthServiceSignInProcedure, connect_go.NewUnaryHandler(
AuthServiceSignInProcedure,
svc.SignIn,
opts...,
))
mux.Handle(AuthServiceSignUpProcedure, connect_go.NewUnaryHandler(
AuthServiceSignUpProcedure,
svc.SignUp,
opts...,
))
mux.Handle(AuthServiceVerifyProcedure, connect_go.NewUnaryHandler(
AuthServiceVerifyProcedure,
svc.Verify,
opts...,
))
mux.Handle(AuthServiceCheckProcedure, connect_go.NewUnaryHandler(
AuthServiceCheckProcedure,
svc.Check,
connect_go.WithIdempotency(connect_go.IdempotencyNoSideEffects),
connect_go.WithHandlerOptions(opts...),
))
return "/galatea.v1.AuthService/", mux
}
type UnimplementedAuthServiceHandler struct{}
func (UnimplementedAuthServiceHandler) SignIn(context.Context, *connect_go.Request[v1.SignInRequest]) (*connect_go.Response[v1.SignInResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.AuthService.SignIn is not implemented"))
}
func (UnimplementedAuthServiceHandler) SignUp(context.Context, *connect_go.Request[v1.SignUpRequest]) (*connect_go.Response[v1.SignUpResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.AuthService.SignUp is not implemented"))
}
func (UnimplementedAuthServiceHandler) Verify(context.Context, *connect_go.Request[v1.VerifyRequest]) (*connect_go.Response[v1.VerifyResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.AuthService.Verify is not implemented"))
}
func (UnimplementedAuthServiceHandler) Check(context.Context, *connect_go.Request[v1.CheckRequest]) (*connect_go.Response[v1.CheckResponse], error) {
return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("galatea.v1.AuthService.Check is not implemented"))
}