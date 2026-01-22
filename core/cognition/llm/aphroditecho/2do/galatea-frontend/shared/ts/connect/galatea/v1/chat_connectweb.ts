import { ChatAllRequest, ChatAllResponse, ChatCreateRequest, ChatCreateResponse, ChatDeleteRequest, ChatDeleteResponse, ChatGetRequest, ChatGetResponse, ChatSendMessageRequest, ChatSendMessageResponse, ChatSubscribeRequest, ChatSubscribeResponse } from "./chat_pb.js";
import { MethodKind } from "@bufbuild/protobuf";
export const ChatService = {
typeName: "galatea.v1.ChatService",
methods: {
chatAll: {
name: "ChatAll",
I: ChatAllRequest,
O: ChatAllResponse,
kind: MethodKind.Unary,
},
chatGet: {
name: "ChatGet",
I: ChatGetRequest,
O: ChatGetResponse,
kind: MethodKind.Unary,
},
chatCreate: {
name: "ChatCreate",
I: ChatCreateRequest,
O: ChatCreateResponse,
kind: MethodKind.Unary,
},
chatDelete: {
name: "ChatDelete",
I: ChatDeleteRequest,
O: ChatDeleteResponse,
kind: MethodKind.Unary,
},
chatSendMessage: {
name: "ChatSendMessage",
I: ChatSendMessageRequest,
O: ChatSendMessageResponse,
kind: MethodKind.Unary,
},
chatSubscribe: {
name: "ChatSubscribe",
I: ChatSubscribeRequest,
O: ChatSubscribeResponse,
kind: MethodKind.ServerStreaming,
},
}
} as const;