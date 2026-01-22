import { BotAllRequest, BotAllResponse, BotCreateRequest, BotCreateResponse, BotDeleteRequest, BotDeleteResponse, BotUpdateRequest, BotUpdateResponse } from "./bot_pb.js";
import { MethodKind } from "@bufbuild/protobuf";
export const BotService = {
typeName: "galatea.v1.BotService",
methods: {
botAll: {
name: "BotAll",
I: BotAllRequest,
O: BotAllResponse,
kind: MethodKind.Unary,
},
botCreate: {
name: "BotCreate",
I: BotCreateRequest,
O: BotCreateResponse,
kind: MethodKind.Unary,
},
botUpdate: {
name: "BotUpdate",
I: BotUpdateRequest,
O: BotUpdateResponse,
kind: MethodKind.Unary,
},
botDelete: {
name: "BotDelete",
I: BotDeleteRequest,
O: BotDeleteResponse,
kind: MethodKind.Unary,
},
}
} as const;