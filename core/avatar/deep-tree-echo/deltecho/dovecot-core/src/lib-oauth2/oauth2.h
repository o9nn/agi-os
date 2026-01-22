#ifndef OAUTH2_H
#define OAUTH2_H
#include "net.h"
struct dict;
struct oauth2_request;
struct oauth2_validation_key_cache;
struct oauth2_field {
const char *name;
const char *value;
};
ARRAY_DEFINE_TYPE(oauth2_field, struct oauth2_field);
struct oauth2_settings {
struct http_client *client;
const char *tokeninfo_url;
const char *grant_url;
const char *introspection_url;
const char *refresh_url;
const char *client_id;
const char *client_secret;
const char *scope;
struct dict *key_dict;
struct oauth2_validation_key_cache *key_cache;
const char *const *issuers;
enum {
INTROSPECTION_MODE_GET_AUTH,
INTROSPECTION_MODE_GET,
INTROSPECTION_MODE_POST,
INTROSPECTION_MODE_LOCAL,
} introspection_mode;
unsigned int timeout_msecs;
bool send_auth_headers;
bool use_grant_password;
};
struct oauth2_request_result {
ARRAY_TYPE(oauth2_field) *fields;
const char *error;
time_t expires_at;
bool valid:1;
};
struct oauth2_request_input {
const char *token;
const char *service;
struct ip_addr local_ip, real_local_ip, remote_ip, real_remote_ip;
in_port_t local_port, real_local_port, remote_port, real_remote_port;
};
typedef void
oauth2_request_callback_t(struct oauth2_request_result*, void*);
bool oauth2_valid_token(const char *token);
struct oauth2_request*
oauth2_passwd_grant_start(const struct oauth2_settings *set,
const struct oauth2_request_input *input,
const char *username,
const char *password,
oauth2_request_callback_t *callback,
void *context);
#define oauth2_passwd_grant_start(set, input, username, password, callback, \
context) \
oauth2_passwd_grant_start( \
set, input - CALLBACK_TYPECHECK( \
callback, void(*)(struct oauth2_request_result*, \
typeof(context))), \
username, password, \
(oauth2_request_callback_t*)callback, (void*)context);
struct oauth2_request*
oauth2_token_validation_start(const struct oauth2_settings *set,
const struct oauth2_request_input *input,
oauth2_request_callback_t *callback,
void *context);
#define oauth2_token_validation_start(set, input, callback, context) \
oauth2_token_validation_start( \
set, input - CALLBACK_TYPECHECK( \
callback, void(*)(struct oauth2_request_result*, \
typeof(context))), \
(oauth2_request_callback_t*)callback, (void*)context);
struct oauth2_request*
oauth2_introspection_start(const struct oauth2_settings *set,
const struct oauth2_request_input *input,
oauth2_request_callback_t *callback,
void *context);
#define oauth2_introspection_start(set, input, callback, context) \
oauth2_introspection_start( \
set, input - CALLBACK_TYPECHECK( \
callback, void(*)(struct oauth2_request_result*, \
typeof(context))), \
(oauth2_request_callback_t*)callback, (void*)context);
struct oauth2_request *
oauth2_refresh_start(const struct oauth2_settings *set,
const struct oauth2_request_input *input,
oauth2_request_callback_t *callback,
void *context);
#define oauth2_refresh_start(set, input, callback, context) \
oauth2_refresh_start( \
set, input - CALLBACK_TYPECHECK( \
callback, void(*)(struct oauth2_request_result*, \
typeof(context))), \
(oauth2_request_callback_t*)callback, (void*)context);
void oauth2_request_abort(struct oauth2_request **);
int oauth2_try_parse_jwt(const struct oauth2_settings *set,
const char *token, ARRAY_TYPE(oauth2_field) *fields,
bool *is_jwt_r, const char **error_r);
struct oauth2_validation_key_cache *oauth2_validation_key_cache_init(void);
int oauth2_validation_key_cache_evict(struct oauth2_validation_key_cache *cache,
const char *key_id);
void oauth2_validation_key_cache_deinit(
struct oauth2_validation_key_cache **_cache);
#endif