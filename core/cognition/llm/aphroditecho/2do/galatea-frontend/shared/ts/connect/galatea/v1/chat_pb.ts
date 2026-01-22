import type { BinaryReadOptions, FieldList, JsonReadOptions, JsonValue, PartialMessage, PlainMessage } from "@bufbuild/protobuf";
import { Message, proto3, protoInt64 } from "@bufbuild/protobuf";
export enum SenderType {
UNSPECIFIED = 0,
USER = 1,
BOT = 2,
}
proto3.util.setEnumType(SenderType, "galatea.v1.SenderType", [
{ no: 0, name: "SENDER_TYPE_UNSPECIFIED" },
{ no: 1, name: "SENDER_TYPE_USER" },
{ no: 2, name: "SENDER_TYPE_BOT" },
]);
export class ChatMessage extends Message<ChatMessage> {
id = "";
sender = SenderType.UNSPECIFIED;
message = "";
createdAt = protoInt64.zero;
constructor(data?: PartialMessage<ChatMessage>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatMessage";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "sender", kind: "enum", T: proto3.getEnumType(SenderType) },
{ no: 3, name: "message", kind: "scalar", T: 9  },
{ no: 4, name: "created_at", kind: "scalar", T: 3  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatMessage {
return new ChatMessage().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatMessage {
return new ChatMessage().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatMessage {
return new ChatMessage().fromJsonString(jsonString, options);
}
static equals(a: ChatMessage | PlainMessage<ChatMessage> | undefined, b: ChatMessage | PlainMessage<ChatMessage> | undefined): boolean {
return proto3.util.equals(ChatMessage, a, b);
}
}
export class Chat extends Message<Chat> {
id = "";
userId = "";
botId = "";
messages: ChatMessage[] = [];
constructor(data?: PartialMessage<Chat>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.Chat";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "user_id", kind: "scalar", T: 9  },
{ no: 3, name: "bot_id", kind: "scalar", T: 9  },
{ no: 4, name: "messages", kind: "message", T: ChatMessage, repeated: true },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): Chat {
return new Chat().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): Chat {
return new Chat().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): Chat {
return new Chat().fromJsonString(jsonString, options);
}
static equals(a: Chat | PlainMessage<Chat> | undefined, b: Chat | PlainMessage<Chat> | undefined): boolean {
return proto3.util.equals(Chat, a, b);
}
}
export class ChatAllRequest extends Message<ChatAllRequest> {
constructor(data?: PartialMessage<ChatAllRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatAllRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatAllRequest {
return new ChatAllRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatAllRequest {
return new ChatAllRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatAllRequest {
return new ChatAllRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatAllRequest | PlainMessage<ChatAllRequest> | undefined, b: ChatAllRequest | PlainMessage<ChatAllRequest> | undefined): boolean {
return proto3.util.equals(ChatAllRequest, a, b);
}
}
export class ChatAllResponse extends Message<ChatAllResponse> {
chats: Chat[] = [];
constructor(data?: PartialMessage<ChatAllResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatAllResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "chats", kind: "message", T: Chat, repeated: true },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatAllResponse {
return new ChatAllResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatAllResponse {
return new ChatAllResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatAllResponse {
return new ChatAllResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatAllResponse | PlainMessage<ChatAllResponse> | undefined, b: ChatAllResponse | PlainMessage<ChatAllResponse> | undefined): boolean {
return proto3.util.equals(ChatAllResponse, a, b);
}
}
export class ChatGetRequest extends Message<ChatGetRequest> {
id = "";
constructor(data?: PartialMessage<ChatGetRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatGetRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatGetRequest {
return new ChatGetRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatGetRequest {
return new ChatGetRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatGetRequest {
return new ChatGetRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatGetRequest | PlainMessage<ChatGetRequest> | undefined, b: ChatGetRequest | PlainMessage<ChatGetRequest> | undefined): boolean {
return proto3.util.equals(ChatGetRequest, a, b);
}
}
export class ChatGetResponse extends Message<ChatGetResponse> {
chat?: Chat;
constructor(data?: PartialMessage<ChatGetResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatGetResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "chat", kind: "message", T: Chat },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatGetResponse {
return new ChatGetResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatGetResponse {
return new ChatGetResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatGetResponse {
return new ChatGetResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatGetResponse | PlainMessage<ChatGetResponse> | undefined, b: ChatGetResponse | PlainMessage<ChatGetResponse> | undefined): boolean {
return proto3.util.equals(ChatGetResponse, a, b);
}
}
export class ChatCreateRequest extends Message<ChatCreateRequest> {
botId = "";
constructor(data?: PartialMessage<ChatCreateRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatCreateRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "bot_id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatCreateRequest {
return new ChatCreateRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatCreateRequest {
return new ChatCreateRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatCreateRequest {
return new ChatCreateRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatCreateRequest | PlainMessage<ChatCreateRequest> | undefined, b: ChatCreateRequest | PlainMessage<ChatCreateRequest> | undefined): boolean {
return proto3.util.equals(ChatCreateRequest, a, b);
}
}
export class ChatCreateResponse extends Message<ChatCreateResponse> {
chat?: Chat;
constructor(data?: PartialMessage<ChatCreateResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatCreateResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "chat", kind: "message", T: Chat },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatCreateResponse {
return new ChatCreateResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatCreateResponse {
return new ChatCreateResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatCreateResponse {
return new ChatCreateResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatCreateResponse | PlainMessage<ChatCreateResponse> | undefined, b: ChatCreateResponse | PlainMessage<ChatCreateResponse> | undefined): boolean {
return proto3.util.equals(ChatCreateResponse, a, b);
}
}
export class ChatDeleteRequest extends Message<ChatDeleteRequest> {
id = "";
constructor(data?: PartialMessage<ChatDeleteRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatDeleteRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatDeleteRequest {
return new ChatDeleteRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatDeleteRequest {
return new ChatDeleteRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatDeleteRequest {
return new ChatDeleteRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatDeleteRequest | PlainMessage<ChatDeleteRequest> | undefined, b: ChatDeleteRequest | PlainMessage<ChatDeleteRequest> | undefined): boolean {
return proto3.util.equals(ChatDeleteRequest, a, b);
}
}
export class ChatDeleteResponse extends Message<ChatDeleteResponse> {
chat?: Chat;
constructor(data?: PartialMessage<ChatDeleteResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatDeleteResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "chat", kind: "message", T: Chat },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatDeleteResponse {
return new ChatDeleteResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatDeleteResponse {
return new ChatDeleteResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatDeleteResponse {
return new ChatDeleteResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatDeleteResponse | PlainMessage<ChatDeleteResponse> | undefined, b: ChatDeleteResponse | PlainMessage<ChatDeleteResponse> | undefined): boolean {
return proto3.util.equals(ChatDeleteResponse, a, b);
}
}
export class ChatSubscribeRequest extends Message<ChatSubscribeRequest> {
id = "";
constructor(data?: PartialMessage<ChatSubscribeRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatSubscribeRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatSubscribeRequest {
return new ChatSubscribeRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatSubscribeRequest {
return new ChatSubscribeRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatSubscribeRequest {
return new ChatSubscribeRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatSubscribeRequest | PlainMessage<ChatSubscribeRequest> | undefined, b: ChatSubscribeRequest | PlainMessage<ChatSubscribeRequest> | undefined): boolean {
return proto3.util.equals(ChatSubscribeRequest, a, b);
}
}
export class ChatSubscribeResponse extends Message<ChatSubscribeResponse> {
id = "";
sender = SenderType.UNSPECIFIED;
message = "";
constructor(data?: PartialMessage<ChatSubscribeResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatSubscribeResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "sender", kind: "enum", T: proto3.getEnumType(SenderType) },
{ no: 3, name: "message", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatSubscribeResponse {
return new ChatSubscribeResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatSubscribeResponse {
return new ChatSubscribeResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatSubscribeResponse {
return new ChatSubscribeResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatSubscribeResponse | PlainMessage<ChatSubscribeResponse> | undefined, b: ChatSubscribeResponse | PlainMessage<ChatSubscribeResponse> | undefined): boolean {
return proto3.util.equals(ChatSubscribeResponse, a, b);
}
}
export class ChatSendMessageRequest extends Message<ChatSendMessageRequest> {
chatId = "";
message = "";
constructor(data?: PartialMessage<ChatSendMessageRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatSendMessageRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "chat_id", kind: "scalar", T: 9  },
{ no: 2, name: "message", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatSendMessageRequest {
return new ChatSendMessageRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatSendMessageRequest {
return new ChatSendMessageRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatSendMessageRequest {
return new ChatSendMessageRequest().fromJsonString(jsonString, options);
}
static equals(a: ChatSendMessageRequest | PlainMessage<ChatSendMessageRequest> | undefined, b: ChatSendMessageRequest | PlainMessage<ChatSendMessageRequest> | undefined): boolean {
return proto3.util.equals(ChatSendMessageRequest, a, b);
}
}
export class ChatSendMessageResponse extends Message<ChatSendMessageResponse> {
message?: ChatMessage;
constructor(data?: PartialMessage<ChatSendMessageResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ChatSendMessageResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "message", kind: "message", T: ChatMessage },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ChatSendMessageResponse {
return new ChatSendMessageResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ChatSendMessageResponse {
return new ChatSendMessageResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ChatSendMessageResponse {
return new ChatSendMessageResponse().fromJsonString(jsonString, options);
}
static equals(a: ChatSendMessageResponse | PlainMessage<ChatSendMessageResponse> | undefined, b: ChatSendMessageResponse | PlainMessage<ChatSendMessageResponse> | undefined): boolean {
return proto3.util.equals(ChatSendMessageResponse, a, b);
}
}