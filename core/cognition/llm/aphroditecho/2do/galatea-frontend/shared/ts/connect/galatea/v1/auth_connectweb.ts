import { CheckRequest, CheckResponse, SignInRequest, SignInResponse, SignUpRequest, SignUpResponse, VerifyRequest, VerifyResponse } from "./auth_pb.js";
import { MethodIdempotency, MethodKind } from "@bufbuild/protobuf";
export const AuthService = {
typeName: "galatea.v1.AuthService",
methods: {
signIn: {
name: "SignIn",
I: SignInRequest,
O: SignInResponse,
kind: MethodKind.Unary,
},
signUp: {
name: "SignUp",
I: SignUpRequest,
O: SignUpResponse,
kind: MethodKind.Unary,
},
verify: {
name: "Verify",
I: VerifyRequest,
O: VerifyResponse,
kind: MethodKind.Unary,
},
check: {
name: "Check",
I: CheckRequest,
O: CheckResponse,
kind: MethodKind.Unary,
idempotency: MethodIdempotency.NoSideEffects,
},
}
} as const;