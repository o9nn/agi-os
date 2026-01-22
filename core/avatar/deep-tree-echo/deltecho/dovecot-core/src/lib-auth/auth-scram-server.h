#ifndef AUTH_SCRAM_SERVER_H
#define AUTH_SCRAM_SERVER_H
#include "auth-scram.h"
struct auth_scram_server;
enum auth_scram_server_error {
AUTH_SCRAM_SERVER_ERROR_NONE,
AUTH_SCRAM_SERVER_ERROR_PROTOCOL_VIOLATION,
AUTH_SCRAM_SERVER_ERROR_BAD_USERNAME,
AUTH_SCRAM_SERVER_ERROR_BAD_LOGIN_USERNAME,
AUTH_SCRAM_SERVER_ERROR_LOOKUP_FAILED,
AUTH_SCRAM_SERVER_ERROR_VERIFICATION_FAILED,
};
enum auth_scram_server_state {
AUTH_SCRAM_SERVER_STATE_INIT = 0,
AUTH_SCRAM_SERVER_STATE_CLIENT_FIRST,
AUTH_SCRAM_SERVER_STATE_CREDENTIALS_LOOKUP,
AUTH_SCRAM_SERVER_STATE_SERVER_FIRST,
AUTH_SCRAM_SERVER_STATE_CLIENT_FINAL,
AUTH_SCRAM_SERVER_STATE_SERVER_FINAL,
AUTH_SCRAM_SERVER_STATE_CLIENT_FINISH,
AUTH_SCRAM_SERVER_STATE_END,
AUTH_SCRAM_SERVER_STATE_ERROR,
};
struct auth_scram_server_backend {
bool (*set_username)(struct auth_scram_server *server,
const char *username, const char **error_r);
bool (*set_login_username)(struct auth_scram_server *server,
const char *username, const char **error_r);
int (*credentials_lookup)(struct auth_scram_server *server,
struct auth_scram_key_data *key_data);
};
struct auth_scram_server {
pool_t pool;
const struct hash_method *hash_method;
const struct auth_scram_server_backend *backend;
void *context;
enum auth_scram_server_state state;
const char *server_first_message;
const char *snonce;
const char *gs2_header;
const char *cnonce;
const char *client_first_message_bare;
const char *client_final_message_without_proof;
buffer_t *proof;
struct auth_scram_key_data key_data;
};
void auth_scram_server_init(struct auth_scram_server *server_r, pool_t pool,
const struct hash_method *hmethod,
const struct auth_scram_server_backend *backend);
void auth_scram_server_deinit(struct auth_scram_server *server);
bool auth_scram_server_acces_granted(struct auth_scram_server *server);
int auth_scram_server_input(struct auth_scram_server *server,
const unsigned char *input, size_t input_len,
enum auth_scram_server_error *error_code_r,
const char **error_r);
bool auth_scram_server_output(struct auth_scram_server *server,
const unsigned char **output_r,
size_t *output_len_r);
#endif