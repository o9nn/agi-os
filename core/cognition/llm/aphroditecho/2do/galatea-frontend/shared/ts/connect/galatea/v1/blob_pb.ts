import type { BinaryReadOptions, FieldList, JsonReadOptions, JsonValue, PartialMessage, PlainMessage } from "@bufbuild/protobuf";
import { Message, proto3 } from "@bufbuild/protobuf";
export class ImageUploadRequest extends Message<ImageUploadRequest> {
image = new Uint8Array(0);
constructor(data?: PartialMessage<ImageUploadRequest>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ImageUploadRequest";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "image", kind: "scalar", T: 12  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ImageUploadRequest {
return new ImageUploadRequest().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ImageUploadRequest {
return new ImageUploadRequest().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ImageUploadRequest {
return new ImageUploadRequest().fromJsonString(jsonString, options);
}
static equals(a: ImageUploadRequest | PlainMessage<ImageUploadRequest> | undefined, b: ImageUploadRequest | PlainMessage<ImageUploadRequest> | undefined): boolean {
return proto3.util.equals(ImageUploadRequest, a, b);
}
}
export class ImageUploadResponse extends Message<ImageUploadResponse> {
url = "";
constructor(data?: PartialMessage<ImageUploadResponse>) {
super();
proto3.util.initPartial(data, this);
}
static readonly runtime: typeof proto3 = proto3;
static readonly typeName = "galatea.v1.ImageUploadResponse";
static readonly fields: FieldList = proto3.util.newFieldList(() => [
{ no: 1, name: "url", kind: "scalar", T: 9  },
]);
static fromBinary(bytes: Uint8Array, options?: Partial<BinaryReadOptions>): ImageUploadResponse {
return new ImageUploadResponse().fromBinary(bytes, options);
}
static fromJson(jsonValue: JsonValue, options?: Partial<JsonReadOptions>): ImageUploadResponse {
return new ImageUploadResponse().fromJson(jsonValue, options);
}
static fromJsonString(jsonString: string, options?: Partial<JsonReadOptions>): ImageUploadResponse {
return new ImageUploadResponse().fromJsonString(jsonString, options);
}
static equals(a: ImageUploadResponse | PlainMessage<ImageUploadResponse> | undefined, b: ImageUploadResponse | PlainMessage<ImageUploadResponse> | undefined): boolean {
return proto3.util.equals(ImageUploadResponse, a, b);
}
}