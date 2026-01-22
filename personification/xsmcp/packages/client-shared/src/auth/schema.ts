import type { InferInput } from 'valibot'
import { any, array, check, looseObject, number, object, optional, pipe, string } from 'valibot'
export const OAuthMetadataSchema = looseObject({
authorization_endpoint: string(),
code_challenge_methods_supported: optional(array(string())),
grant_types_supported: optional(array(string())),
introspection_endpoint: optional(string()),
introspection_endpoint_auth_methods_supported: optional(array(string())),
introspection_endpoint_auth_signing_alg_values_supported: optional(array(string())),
issuer: string(),
registration_endpoint: optional(string()),
response_modes_supported: optional(array(string())),
response_types_supported: array(string()),
revocation_endpoint: optional(string()),
revocation_endpoint_auth_methods_supported: optional(array(string())),
revocation_endpoint_auth_signing_alg_values_supported: optional(array(string())),
scopes_supported: optional(array(string())),
service_documentation: optional(string()),
token_endpoint: string(),
token_endpoint_auth_methods_supported: optional(array(string())),
token_endpoint_auth_signing_alg_values_supported: optional(array(string())),
})
export const OAuthTokensSchema = object({
access_token: string(),
expires_in: optional(number()),
refresh_token: optional(string()),
scope: optional(string()),
token_type: string(),
})
export const OAuthErrorResponseSchema = object({
error: string(),
error_description: optional(string()),
error_uri: optional(string()),
})
export const OAuthClientMetadataSchema = object({
client_name: optional(string()),
client_uri: optional(string()),
contacts: optional(array(string())),
grant_types: optional(array(string())),
jwks: optional(any()),
jwks_uri: optional(string()),
logo_uri: optional(string()),
policy_uri: optional(string()),
redirect_uris: pipe(
array(string()),
check(uris => uris.every(uri => URL.canParse(uri)), 'redirect_uris must contain valid URLs'),
),
response_types: optional(array(string())),
scope: optional(string()),
software_id: optional(string()),
software_version: optional(string()),
token_endpoint_auth_method: optional(string()),
tos_uri: optional(string()),
})
export const OAuthClientInformationSchema = object({
client_id: string(),
client_id_issued_at: optional(number()),
client_secret: optional(string()),
client_secret_expires_at: optional(number()),
})
export const OAuthClientInformationFullSchema = object({
...OAuthClientMetadataSchema.entries,
...OAuthClientInformationSchema.entries,
})
export const OAuthClientRegistrationErrorSchema = object({
error: string(),
error_description: optional(string()),
})
export const OAuthTokenRevocationRequestSchema = object({
token: string(),
token_type_hint: optional(string()),
})
export type OAuthClientInformation = InferInput<typeof OAuthClientInformationSchema>
export type OAuthClientInformationFull = InferInput<typeof OAuthClientInformationFullSchema>
export type OAuthClientMetadata = InferInput<typeof OAuthClientMetadataSchema>
export type OAuthClientRegistrationError = InferInput<typeof OAuthClientRegistrationErrorSchema>
export type OAuthErrorResponse = InferInput<typeof OAuthErrorResponseSchema>
export type OAuthMetadata = InferInput<typeof OAuthMetadataSchema>
export type OAuthTokenRevocationRequest = InferInput<typeof OAuthTokenRevocationRequestSchema>
export type OAuthTokens = InferInput<typeof OAuthTokensSchema>