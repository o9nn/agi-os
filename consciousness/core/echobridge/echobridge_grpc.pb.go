package echobridge
import (
context "context"
grpc "google.golang.org/grpc"
codes "google.golang.org/grpc/codes"
status "google.golang.org/grpc/status"
)
const _ = grpc.SupportPackageIsVersion9
const (
EchoBridge_ScheduleEvent_FullMethodName      = "/echobridge.EchoBridge/ScheduleEvent"
EchoBridge_GetState_FullMethodName           = "/echobridge.EchoBridge/GetState"
EchoBridge_UpdateState_FullMethodName        = "/echobridge.EchoBridge/UpdateState"
EchoBridge_StreamThoughts_FullMethodName     = "/echobridge.EchoBridge/StreamThoughts"
EchoBridge_StreamEvents_FullMethodName       = "/echobridge.EchoBridge/StreamEvents"
EchoBridge_RegisterGoal_FullMethodName       = "/echobridge.EchoBridge/RegisterGoal"
EchoBridge_UpdateGoalProgress_FullMethodName = "/echobridge.EchoBridge/UpdateGoalProgress"
EchoBridge_GetActiveGoals_FullMethodName     = "/echobridge.EchoBridge/GetActiveGoals"
)
type EchoBridgeClient interface {
ScheduleEvent(ctx context.Context, in *CognitiveEvent, opts ...grpc.CallOption) (*EventResponse, error)
GetState(ctx context.Context, in *StateRequest, opts ...grpc.CallOption) (*CognitiveState, error)
UpdateState(ctx context.Context, in *CognitiveState, opts ...grpc.CallOption) (*StateResponse, error)
StreamThoughts(ctx context.Context, opts ...grpc.CallOption) (grpc.BidiStreamingClient[Thought, ThoughtResponse], error)
StreamEvents(ctx context.Context, in *EventStreamRequest, opts ...grpc.CallOption) (grpc.ServerStreamingClient[CognitiveEvent], error)
RegisterGoal(ctx context.Context, in *Goal, opts ...grpc.CallOption) (*GoalResponse, error)
UpdateGoalProgress(ctx context.Context, in *GoalProgress, opts ...grpc.CallOption) (*GoalResponse, error)
GetActiveGoals(ctx context.Context, in *GoalRequest, opts ...grpc.CallOption) (*GoalList, error)
}
type echoBridgeClient struct {
cc grpc.ClientConnInterface
}
func NewEchoBridgeClient(cc grpc.ClientConnInterface) EchoBridgeClient {
return &echoBridgeClient{cc}
}
func (c *echoBridgeClient) ScheduleEvent(ctx context.Context, in *CognitiveEvent, opts ...grpc.CallOption) (*EventResponse, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(EventResponse)
err := c.cc.Invoke(ctx, EchoBridge_ScheduleEvent_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
func (c *echoBridgeClient) GetState(ctx context.Context, in *StateRequest, opts ...grpc.CallOption) (*CognitiveState, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(CognitiveState)
err := c.cc.Invoke(ctx, EchoBridge_GetState_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
func (c *echoBridgeClient) UpdateState(ctx context.Context, in *CognitiveState, opts ...grpc.CallOption) (*StateResponse, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(StateResponse)
err := c.cc.Invoke(ctx, EchoBridge_UpdateState_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
func (c *echoBridgeClient) StreamThoughts(ctx context.Context, opts ...grpc.CallOption) (grpc.BidiStreamingClient[Thought, ThoughtResponse], error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
stream, err := c.cc.NewStream(ctx, &EchoBridge_ServiceDesc.Streams[0], EchoBridge_StreamThoughts_FullMethodName, cOpts...)
if err != nil {
return nil, err
}
x := &grpc.GenericClientStream[Thought, ThoughtResponse]{ClientStream: stream}
return x, nil
}
type EchoBridge_StreamThoughtsClient = grpc.BidiStreamingClient[Thought, ThoughtResponse]
func (c *echoBridgeClient) StreamEvents(ctx context.Context, in *EventStreamRequest, opts ...grpc.CallOption) (grpc.ServerStreamingClient[CognitiveEvent], error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
stream, err := c.cc.NewStream(ctx, &EchoBridge_ServiceDesc.Streams[1], EchoBridge_StreamEvents_FullMethodName, cOpts...)
if err != nil {
return nil, err
}
x := &grpc.GenericClientStream[EventStreamRequest, CognitiveEvent]{ClientStream: stream}
if err := x.ClientStream.SendMsg(in); err != nil {
return nil, err
}
if err := x.ClientStream.CloseSend(); err != nil {
return nil, err
}
return x, nil
}
type EchoBridge_StreamEventsClient = grpc.ServerStreamingClient[CognitiveEvent]
func (c *echoBridgeClient) RegisterGoal(ctx context.Context, in *Goal, opts ...grpc.CallOption) (*GoalResponse, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(GoalResponse)
err := c.cc.Invoke(ctx, EchoBridge_RegisterGoal_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
func (c *echoBridgeClient) UpdateGoalProgress(ctx context.Context, in *GoalProgress, opts ...grpc.CallOption) (*GoalResponse, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(GoalResponse)
err := c.cc.Invoke(ctx, EchoBridge_UpdateGoalProgress_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
func (c *echoBridgeClient) GetActiveGoals(ctx context.Context, in *GoalRequest, opts ...grpc.CallOption) (*GoalList, error) {
cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
out := new(GoalList)
err := c.cc.Invoke(ctx, EchoBridge_GetActiveGoals_FullMethodName, in, out, cOpts...)
if err != nil {
return nil, err
}
return out, nil
}
type EchoBridgeServer interface {
ScheduleEvent(context.Context, *CognitiveEvent) (*EventResponse, error)
GetState(context.Context, *StateRequest) (*CognitiveState, error)
UpdateState(context.Context, *CognitiveState) (*StateResponse, error)
StreamThoughts(grpc.BidiStreamingServer[Thought, ThoughtResponse]) error
StreamEvents(*EventStreamRequest, grpc.ServerStreamingServer[CognitiveEvent]) error
RegisterGoal(context.Context, *Goal) (*GoalResponse, error)
UpdateGoalProgress(context.Context, *GoalProgress) (*GoalResponse, error)
GetActiveGoals(context.Context, *GoalRequest) (*GoalList, error)
mustEmbedUnimplementedEchoBridgeServer()
}
type UnimplementedEchoBridgeServer struct{}
func (UnimplementedEchoBridgeServer) ScheduleEvent(context.Context, *CognitiveEvent) (*EventResponse, error) {
return nil, status.Error(codes.Unimplemented, "method ScheduleEvent not implemented")
}
func (UnimplementedEchoBridgeServer) GetState(context.Context, *StateRequest) (*CognitiveState, error) {
return nil, status.Error(codes.Unimplemented, "method GetState not implemented")
}
func (UnimplementedEchoBridgeServer) UpdateState(context.Context, *CognitiveState) (*StateResponse, error) {
return nil, status.Error(codes.Unimplemented, "method UpdateState not implemented")
}
func (UnimplementedEchoBridgeServer) StreamThoughts(grpc.BidiStreamingServer[Thought, ThoughtResponse]) error {
return status.Error(codes.Unimplemented, "method StreamThoughts not implemented")
}
func (UnimplementedEchoBridgeServer) StreamEvents(*EventStreamRequest, grpc.ServerStreamingServer[CognitiveEvent]) error {
return status.Error(codes.Unimplemented, "method StreamEvents not implemented")
}
func (UnimplementedEchoBridgeServer) RegisterGoal(context.Context, *Goal) (*GoalResponse, error) {
return nil, status.Error(codes.Unimplemented, "method RegisterGoal not implemented")
}
func (UnimplementedEchoBridgeServer) UpdateGoalProgress(context.Context, *GoalProgress) (*GoalResponse, error) {
return nil, status.Error(codes.Unimplemented, "method UpdateGoalProgress not implemented")
}
func (UnimplementedEchoBridgeServer) GetActiveGoals(context.Context, *GoalRequest) (*GoalList, error) {
return nil, status.Error(codes.Unimplemented, "method GetActiveGoals not implemented")
}
func (UnimplementedEchoBridgeServer) mustEmbedUnimplementedEchoBridgeServer() {}
func (UnimplementedEchoBridgeServer) testEmbeddedByValue()                    {}
type UnsafeEchoBridgeServer interface {
mustEmbedUnimplementedEchoBridgeServer()
}
func RegisterEchoBridgeServer(s grpc.ServiceRegistrar, srv EchoBridgeServer) {
if t, ok := srv.(interface{ testEmbeddedByValue() }); ok {
t.testEmbeddedByValue()
}
s.RegisterService(&EchoBridge_ServiceDesc, srv)
}
func _EchoBridge_ScheduleEvent_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(CognitiveEvent)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).ScheduleEvent(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_ScheduleEvent_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).ScheduleEvent(ctx, req.(*CognitiveEvent))
}
return interceptor(ctx, in, info, handler)
}
func _EchoBridge_GetState_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(StateRequest)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).GetState(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_GetState_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).GetState(ctx, req.(*StateRequest))
}
return interceptor(ctx, in, info, handler)
}
func _EchoBridge_UpdateState_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(CognitiveState)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).UpdateState(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_UpdateState_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).UpdateState(ctx, req.(*CognitiveState))
}
return interceptor(ctx, in, info, handler)
}
func _EchoBridge_StreamThoughts_Handler(srv interface{}, stream grpc.ServerStream) error {
return srv.(EchoBridgeServer).StreamThoughts(&grpc.GenericServerStream[Thought, ThoughtResponse]{ServerStream: stream})
}
type EchoBridge_StreamThoughtsServer = grpc.BidiStreamingServer[Thought, ThoughtResponse]
func _EchoBridge_StreamEvents_Handler(srv interface{}, stream grpc.ServerStream) error {
m := new(EventStreamRequest)
if err := stream.RecvMsg(m); err != nil {
return err
}
return srv.(EchoBridgeServer).StreamEvents(m, &grpc.GenericServerStream[EventStreamRequest, CognitiveEvent]{ServerStream: stream})
}
type EchoBridge_StreamEventsServer = grpc.ServerStreamingServer[CognitiveEvent]
func _EchoBridge_RegisterGoal_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(Goal)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).RegisterGoal(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_RegisterGoal_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).RegisterGoal(ctx, req.(*Goal))
}
return interceptor(ctx, in, info, handler)
}
func _EchoBridge_UpdateGoalProgress_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(GoalProgress)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).UpdateGoalProgress(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_UpdateGoalProgress_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).UpdateGoalProgress(ctx, req.(*GoalProgress))
}
return interceptor(ctx, in, info, handler)
}
func _EchoBridge_GetActiveGoals_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
in := new(GoalRequest)
if err := dec(in); err != nil {
return nil, err
}
if interceptor == nil {
return srv.(EchoBridgeServer).GetActiveGoals(ctx, in)
}
info := &grpc.UnaryServerInfo{
Server:     srv,
FullMethod: EchoBridge_GetActiveGoals_FullMethodName,
}
handler := func(ctx context.Context, req interface{}) (interface{}, error) {
return srv.(EchoBridgeServer).GetActiveGoals(ctx, req.(*GoalRequest))
}
return interceptor(ctx, in, info, handler)
}
var EchoBridge_ServiceDesc = grpc.ServiceDesc{
ServiceName: "echobridge.EchoBridge",
HandlerType: (*EchoBridgeServer)(nil),
Methods: []grpc.MethodDesc{
{
MethodName: "ScheduleEvent",
Handler:    _EchoBridge_ScheduleEvent_Handler,
},
{
MethodName: "GetState",
Handler:    _EchoBridge_GetState_Handler,
},
{
MethodName: "UpdateState",
Handler:    _EchoBridge_UpdateState_Handler,
},
{
MethodName: "RegisterGoal",
Handler:    _EchoBridge_RegisterGoal_Handler,
},
{
MethodName: "UpdateGoalProgress",
Handler:    _EchoBridge_UpdateGoalProgress_Handler,
},
{
MethodName: "GetActiveGoals",
Handler:    _EchoBridge_GetActiveGoals_Handler,
},
},
Streams: []grpc.StreamDesc{
{
StreamName:    "StreamThoughts",
Handler:       _EchoBridge_StreamThoughts_Handler,
ServerStreams: true,
ClientStreams: true,
},
{
StreamName:    "StreamEvents",
Handler:       _EchoBridge_StreamEvents_Handler,
ServerStreams: true,
},
},
Metadata: "echobridge.proto",
}