#ifndef AUTH_CLIENT_H
#define AUTH_CLIENT_H
#include "net.h"
#include "auth-client-interface.h"
struct auth_client;
struct auth_client_request;
enum auth_request_flags {
AUTH_REQUEST_FLAG_CONN_SECURED = 0x01,
AUTH_REQUEST_FLAG_VALID_CLIENT_CERT = 0x02,
AUTH_REQUEST_FLAG_NO_PENALTY = 0x04,
AUTH_REQUEST_FLAG_SUPPORT_FINAL_RESP = 0x08,
AUTH_REQUEST_FLAG_DEBUG = 0x10,
AUTH_REQUEST_FLAG_CONN_SECURED_TLS = 0x20,
};
enum auth_request_status {
AUTH_REQUEST_STATUS_ABORT = -3,
AUTH_REQUEST_STATUS_INTERNAL_FAIL = -2,
AUTH_REQUEST_STATUS_FAIL = -1,
AUTH_REQUEST_STATUS_CONTINUE,
AUTH_REQUEST_STATUS_OK
};
struct auth_mech_desc {
char *name;
enum mech_security_flags flags;
};
struct auth_connect_id {
unsigned int server_pid;
unsigned int connect_uid;
};
struct auth_request_info {
const char *mech;
const char *service;
const char *session_id;
const char *cert_username;
const char *local_name;
const char *client_id;
const char *const *forward_fields;
ARRAY_TYPE(const_string) extra_fields;
unsigned int ssl_cipher_bits;
const char *ssl_cipher;
const char *ssl_pfs;
const char *ssl_protocol;
const char *ssl_ja3_hash;
enum auth_request_flags flags;
struct ip_addr local_ip, remote_ip, real_local_ip, real_remote_ip;
in_port_t local_port, remote_port, real_local_port, real_remote_port;
const char *initial_resp_base64;
};
typedef void auth_request_callback_t(struct auth_client_request *request,
enum auth_request_status status,
const char *data_base64,
const char *const *args, void *context);
typedef void auth_connect_notify_callback_t(struct auth_client *client,
bool connected, void *context);
struct auth_client *
auth_client_init(const char *auth_socket_path, unsigned int client_pid,
bool debug);
void auth_client_deinit(struct auth_client **client);
void auth_client_connect(struct auth_client *client);
void auth_client_disconnect(struct auth_client *client, const char *reason);
bool auth_client_is_connected(struct auth_client *client);
bool auth_client_is_disconnected(struct auth_client *client);
void auth_client_set_connect_timeout(struct auth_client *client,
unsigned int msecs);
void auth_client_set_connect_notify(struct auth_client *client,
auth_connect_notify_callback_t *callback,
void *context) ATTR_NULL(2, 3);
const struct auth_mech_desc *
auth_client_get_available_mechs(struct auth_client *client,
unsigned int *mech_count);
const struct auth_mech_desc *
auth_client_find_mech(struct auth_client *client, const char *name);
void auth_client_get_connect_id(struct auth_client *client,
unsigned int *server_pid_r,
unsigned int *connect_uid_r);
struct auth_client_request *
auth_client_request_new(struct auth_client *client,
const struct auth_request_info *request_info,
auth_request_callback_t *callback, void *context)
ATTR_NULL(4);
void auth_client_request_continue(struct auth_client_request *request,
const char *data_base64);
void auth_client_request_abort(struct auth_client_request **request,
const char *reason) ATTR_NULL(2);
unsigned int auth_client_request_get_id(struct auth_client_request *request);
unsigned int
auth_client_request_get_server_pid(struct auth_client_request *request);
const char *auth_client_request_get_cookie(struct auth_client_request *request);
void auth_client_send_cancel(struct auth_client *client, unsigned int id);
#endif