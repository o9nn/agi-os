import { ImageUploadRequest, ImageUploadResponse } from "./blob_pb.js";
import { MethodKind } from "@bufbuild/protobuf";
export const ImageUploadService = {
  typeName: "galatea.v1.ImageUploadService",
  methods: {
    imageUpload: {
      name: "ImageUpload",
      I: ImageUploadRequest,
      O: ImageUploadResponse,
      kind: MethodKind.Unary,
    },
  }
} as const;