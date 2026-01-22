package jsonapi
type Response struct {
Data []any `protobuf:"bytes,1,rep,name=data,proto3" json:"data,omitempty"`
Errors []*ErrorObject `protobuf:"bytes,2,rep,name=errors,proto3" json:"errors,omitempty"`
}