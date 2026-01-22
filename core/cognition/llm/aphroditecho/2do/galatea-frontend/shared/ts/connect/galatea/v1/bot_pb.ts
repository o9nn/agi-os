import type { BinaryReadOptions, FieldList, JsonReadOptions, JsonValue, PartialMessage, PlainMessage } from "@bufbuild/protobuf";
import { Message, proto3 } from "@bufbuild/protobuf";
export class Bot extends Message<Bot> {
id = "";
name = "";
description = "";
avatarUrl = "";
constructor(data?: PartialMessage<Bot>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.Bot";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "name", kind: "scalar", T: 9  },
{ no: 3, name: "description", kind: "scalar", T: 9  },
{ no: 4, name: "avatar_url", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): Bot {
return new Bot().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): Bot {
return new Bot().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): Bot {
return new Bot().fromJsonString(jsonString, options);
}
static equals(a: Bot | PlainMessage<Bot> | undefined, b: Bot | PlainMessage<Bot> | undefined): boolean {
return proto3.util.equals(Bot, a, b);
}
}
export class BotAllRequest extends Message<BotAllRequest> {
constructor(data?: PartialMessage<BotAllRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotAllRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotAllRequest {
return new BotAllRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotAllRequest {
return new BotAllRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotAllRequest {
return new BotAllRequest().fromJsonString(jsonString, options);
}
static equals(a: BotAllRequest | PlainMessage<BotAllRequest> | undefined, b: BotAllRequest | PlainMessage<BotAllRequest> | undefined): boolean {
return proto3.util.equals(BotAllRequest, a, b);
}
}
export class BotAllResponse extends Message<BotAllResponse> {
bots: Bot[] = [];
constructor(data?: PartialMessage<BotAllResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotAllResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "bots", kind: "message", T: Bot, repeated: true },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotAllResponse {
return new BotAllResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotAllResponse {
return new BotAllResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotAllResponse {
return new BotAllResponse().fromJsonString(jsonString, options);
}
static equals(a: BotAllResponse | PlainMessage<BotAllResponse> | undefined, b: BotAllResponse | PlainMessage<BotAllResponse> | undefined): boolean {
return proto3.util.equals(BotAllResponse, a, b);
}
}
export class BotCreateRequest extends Message<BotCreateRequest> {
id = "";
bot?: Bot;
constructor(data?: PartialMessage<BotCreateRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotCreateRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "bot", kind: "message", T: Bot },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotCreateRequest {
return new BotCreateRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotCreateRequest {
return new BotCreateRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotCreateRequest {
return new BotCreateRequest().fromJsonString(jsonString, options);
}
static equals(a: BotCreateRequest | PlainMessage<BotCreateRequest> | undefined, b: BotCreateRequest | PlainMessage<BotCreateRequest> | undefined): boolean {
return proto3.util.equals(BotCreateRequest, a, b);
}
}
export class BotCreateResponse extends Message<BotCreateResponse> {
bot?: Bot;
constructor(data?: PartialMessage<BotCreateResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotCreateResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "bot", kind: "message", T: Bot },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotCreateResponse {
return new BotCreateResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotCreateResponse {
return new BotCreateResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotCreateResponse {
return new BotCreateResponse().fromJsonString(jsonString, options);
}
static equals(a: BotCreateResponse | PlainMessage<BotCreateResponse> | undefined, b: BotCreateResponse | PlainMessage<BotCreateResponse> | undefined): boolean {
return proto3.util.equals(BotCreateResponse, a, b);
}
}
export class BotUpdateRequest extends Message<BotUpdateRequest> {
id = "";
bot?: Bot;
constructor(data?: PartialMessage<BotUpdateRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotUpdateRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "bot", kind: "message", T: Bot },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotUpdateRequest {
return new BotUpdateRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotUpdateRequest {
return new BotUpdateRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotUpdateRequest {
return new BotUpdateRequest().fromJsonString(jsonString, options);
}
static equals(a: BotUpdateRequest | PlainMessage<BotUpdateRequest> | undefined, b: BotUpdateRequest | PlainMessage<BotUpdateRequest> | undefined): boolean {
return proto3.util.equals(BotUpdateRequest, a, b);
}
}
export class BotUpdateResponse extends Message<BotUpdateResponse> {
bot?: Bot;
constructor(data?: PartialMessage<BotUpdateResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotUpdateResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "bot", kind: "message", T: Bot },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotUpdateResponse {
return new BotUpdateResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotUpdateResponse {
return new BotUpdateResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotUpdateResponse {
return new BotUpdateResponse().fromJsonString(jsonString, options);
}
static equals(a: BotUpdateResponse | PlainMessage<BotUpdateResponse> | undefined, b: BotUpdateResponse | PlainMessage<BotUpdateResponse> | undefined): boolean {
return proto3.util.equals(BotUpdateResponse, a, b);
}
}
export class BotDeleteRequest extends Message<BotDeleteRequest> {
id = "";
constructor(data?: PartialMessage<BotDeleteRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotDeleteRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotDeleteRequest {
return new BotDeleteRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotDeleteRequest {
return new BotDeleteRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotDeleteRequest {
return new BotDeleteRequest().fromJsonString(jsonString, options);
}
static equals(a: BotDeleteRequest | PlainMessage<BotDeleteRequest> | undefined, b: BotDeleteRequest | PlainMessage<BotDeleteRequest> | undefined): boolean {
return proto3.util.equals(BotDeleteRequest, a, b);
}
}
export class BotDeleteResponse extends Message<BotDeleteResponse> {
bot?: Bot;
constructor(data?: PartialMessage<BotDeleteResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.BotDeleteResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 2, name: "bot", kind: "message", T: Bot },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): BotDeleteResponse {
return new BotDeleteResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): BotDeleteResponse {
return new BotDeleteResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): BotDeleteResponse {
return new BotDeleteResponse().fromJsonString(jsonString, options);
}
static equals(a: BotDeleteResponse | PlainMessage<BotDeleteResponse> | undefined, b: BotDeleteResponse | PlainMessage<BotDeleteResponse> | undefined): boolean {
return proto3.util.equals(BotDeleteResponse, a, b);
}
}