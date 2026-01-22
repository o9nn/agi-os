import type { BinaryReadOptions, FieldList, JsonReadOptions, JsonValue, PartialMessage, PlainMessage } from "@bufbuild/protobuf";
import { Message, proto3 } from "@bufbuild/protobuf";
export class SignInRequest extends Message<SignInRequest> {
email = "";
password = "";
constructor(data?: PartialMessage<SignInRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.SignInRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "email", kind: "scalar", T: 9  },
{ no: 2, name: "password", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): SignInRequest {
return new SignInRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): SignInRequest {
return new SignInRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): SignInRequest {
return new SignInRequest().fromJsonString(jsonString, options);
}
static equals(a: SignInRequest | PlainMessage<SignInRequest> | undefined, b: SignInRequest | PlainMessage<SignInRequest> | undefined): boolean {
return proto3.util.equals(SignInRequest, a, b);
}
}
export class SignInResponse extends Message<SignInResponse> {
accessToken = "";
tokenType = "";
expiresIn = 0;
expiresAt = 0;
refreshToken = "";
constructor(data?: PartialMessage<SignInResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.SignInResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "access_token", kind: "scalar", T: 9  },
{ no: 2, name: "token_type", kind: "scalar", T: 9  },
{ no: 3, name: "expires_in", kind: "scalar", T: 5  },
{ no: 4, name: "expires_at", kind: "scalar", T: 5  },
{ no: 5, name: "refresh_token", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): SignInResponse {
return new SignInResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): SignInResponse {
return new SignInResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): SignInResponse {
return new SignInResponse().fromJsonString(jsonString, options);
}
static equals(a: SignInResponse | PlainMessage<SignInResponse> | undefined, b: SignInResponse | PlainMessage<SignInResponse> | undefined): boolean {
return proto3.util.equals(SignInResponse, a, b);
}
}
export class SignUpRequest extends Message<SignUpRequest> {
email = "";
password = "";
constructor(data?: PartialMessage<SignUpRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.SignUpRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "email", kind: "scalar", T: 9  },
{ no: 2, name: "password", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): SignUpRequest {
return new SignUpRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): SignUpRequest {
return new SignUpRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): SignUpRequest {
return new SignUpRequest().fromJsonString(jsonString, options);
}
static equals(a: SignUpRequest | PlainMessage<SignUpRequest> | undefined, b: SignUpRequest | PlainMessage<SignUpRequest> | undefined): boolean {
return proto3.util.equals(SignUpRequest, a, b);
}
}
export class SignUpResponse extends Message<SignUpResponse> {
id = "";
email = "";
confirmationSentAt = "";
constructor(data?: PartialMessage<SignUpResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.SignUpResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "id", kind: "scalar", T: 9  },
{ no: 2, name: "email", kind: "scalar", T: 9  },
{ no: 3, name: "confirmation_sent_at", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): SignUpResponse {
return new SignUpResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): SignUpResponse {
return new SignUpResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): SignUpResponse {
return new SignUpResponse().fromJsonString(jsonString, options);
}
static equals(a: SignUpResponse | PlainMessage<SignUpResponse> | undefined, b: SignUpResponse | PlainMessage<SignUpResponse> | undefined): boolean {
return proto3.util.equals(SignUpResponse, a, b);
}
}
export class VerifyRequest extends Message<VerifyRequest> {
type = "";
email = "";
token = "";
constructor(data?: PartialMessage<VerifyRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.VerifyRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "type", kind: "scalar", T: 9  },
{ no: 2, name: "email", kind: "scalar", T: 9  },
{ no: 3, name: "token", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): VerifyRequest {
return new VerifyRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): VerifyRequest {
return new VerifyRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): VerifyRequest {
return new VerifyRequest().fromJsonString(jsonString, options);
}
static equals(a: VerifyRequest | PlainMessage<VerifyRequest> | undefined, b: VerifyRequest | PlainMessage<VerifyRequest> | undefined): boolean {
return proto3.util.equals(VerifyRequest, a, b);
}
}
export class VerifyResponse extends Message<VerifyResponse> {
accessToken = "";
tokenType = "";
expiresIn = 0;
expiresAt = 0;
refreshToken = "";
constructor(data?: PartialMessage<VerifyResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.VerifyResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "access_token", kind: "scalar", T: 9  },
{ no: 2, name: "token_type", kind: "scalar", T: 9  },
{ no: 3, name: "expires_in", kind: "scalar", T: 5  },
{ no: 4, name: "expires_at", kind: "scalar", T: 5  },
{ no: 5, name: "refresh_token", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): VerifyResponse {
return new VerifyResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): VerifyResponse {
return new VerifyResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): VerifyResponse {
return new VerifyResponse().fromJsonString(jsonString, options);
}
static equals(a: VerifyResponse | PlainMessage<VerifyResponse> | undefined, b: VerifyResponse | PlainMessage<VerifyResponse> | undefined): boolean {
return proto3.util.equals(VerifyResponse, a, b);
}
}
export class CheckRequest extends Message<CheckRequest> {
constructor(data?: PartialMessage<CheckRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.CheckRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): CheckRequest {
return new CheckRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): CheckRequest {
return new CheckRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): CheckRequest {
return new CheckRequest().fromJsonString(jsonString, options);
}
static equals(a: CheckRequest | PlainMessage<CheckRequest> | undefined, b: CheckRequest | PlainMessage<CheckRequest> | undefined): boolean {
return proto3.util.equals(CheckRequest, a, b);
}
}
export class CheckResponse extends Message<CheckResponse> {
userId = "";
email = "";
sessionId = "";
constructor(data?: PartialMessage<CheckResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.CheckResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "user_id", kind: "scalar", T: 9  },
{ no: 2, name: "email", kind: "scalar", T: 9  },
{ no: 3, name: "session_id", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): CheckResponse {
return new CheckResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): CheckResponse {
return new CheckResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): CheckResponse {
return new CheckResponse().fromJsonString(jsonString, options);
}
static equals(a: CheckResponse | PlainMessage<CheckResponse> | undefined, b: CheckResponse | PlainMessage<CheckResponse> | undefined): boolean {
return proto3.util.equals(CheckResponse, a, b);
}
}