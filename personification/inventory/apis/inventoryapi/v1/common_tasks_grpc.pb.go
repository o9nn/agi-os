package v1
import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
)
const _ = grpc.SupportPackageIsVersion7
const (
	CommonTasksService_GetModels_FullMethodName = "/apis.inventoryapi.v1.CommonTasksService/GetModels"
)
type CommonTasksServiceClient interface {
	GetModels(ctx context.Context, in *GetModelsRequest, opts ...grpc.CallOption) (*GetModelsResponse, error)
}
type commonTasksServiceClient struct {
	cc grpc.ClientConnInterface
}
func NewCommonTasksServiceClient(cc grpc.ClientConnInterface) CommonTasksServiceClient {
	return &commonTasksServiceClient{cc}
}
func (c *commonTasksServiceClient) GetModels(ctx context.Context, in *GetModelsRequest, opts ...grpc.CallOption) (*GetModelsResponse, error) {
	out := new(GetModelsResponse)
	err := c.cc.Invoke(ctx, CommonTasksService_GetModels_FullMethodName, in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}
type CommonTasksServiceServer interface {
	GetModels(context.Context, *GetModelsRequest) (*GetModelsResponse, error)
	mustEmbedUnimplementedCommonTasksServiceServer()
}
type UnimplementedCommonTasksServiceServer struct {
}
func (UnimplementedCommonTasksServiceServer) GetModels(context.Context, *GetModelsRequest) (*GetModelsResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetModels not implemented")
}
func (UnimplementedCommonTasksServiceServer) mustEmbedUnimplementedCommonTasksServiceServer() {}
type UnsafeCommonTasksServiceServer interface {
	mustEmbedUnimplementedCommonTasksServiceServer()
}
func RegisterCommonTasksServiceServer(s grpc.ServiceRegistrar, srv CommonTasksServiceServer) {
	s.RegisterService(&CommonTasksService_ServiceDesc, srv)
}
func _CommonTasksService_GetModels_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(GetModelsRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(CommonTasksServiceServer).GetModels(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: CommonTasksService_GetModels_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(CommonTasksServiceServer).GetModels(ctx, req.(*GetModelsRequest))
	}
	return interceptor(ctx, in, info, handler)
}
var CommonTasksService_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "apis.inventoryapi.v1.CommonTasksService",
	HandlerType: (*CommonTasksServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "GetModels",
			Handler:    _CommonTasksService_GetModels_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "apis/inventoryapi/v1/common_tasks.proto",
}