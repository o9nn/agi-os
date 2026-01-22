#ifndef IMAPC_CLIENT_H
#define IMAPC_CLIENT_H
#include "net.h"
#include "iostream-ssl.h"
#define IMAPC_DEFAULT_MAX_IDLE_TIME (60*29)
enum imapc_command_state {
IMAPC_COMMAND_STATE_OK = 0,
IMAPC_COMMAND_STATE_NO,
IMAPC_COMMAND_STATE_BAD,
IMAPC_COMMAND_STATE_AUTH_FAILED,
IMAPC_COMMAND_STATE_DISCONNECTED
};
extern const char *imapc_command_state_names[];
enum imapc_capability {
IMAPC_CAPABILITY_SASL_IR = 0x01,
IMAPC_CAPABILITY_LITERALPLUS = 0x02,
IMAPC_CAPABILITY_QRESYNC = 0x04,
IMAPC_CAPABILITY_IDLE = 0x08,
IMAPC_CAPABILITY_UIDPLUS = 0x10,
IMAPC_CAPABILITY_AUTH_PLAIN = 0x20,
IMAPC_CAPABILITY_STARTTLS = 0x40,
IMAPC_CAPABILITY_X_GM_EXT_1 = 0x80,
IMAPC_CAPABILITY_CONDSTORE = 0x100,
IMAPC_CAPABILITY_NAMESPACE = 0x200,
IMAPC_CAPABILITY_UNSELECT = 0x400,
IMAPC_CAPABILITY_ESEARCH = 0x800,
IMAPC_CAPABILITY_WITHIN = 0x1000,
IMAPC_CAPABILITY_QUOTA = 0x2000,
IMAPC_CAPABILITY_ID = 0x4000,
IMAPC_CAPABILITY_SAVEDATE = 0x8000,
IMAPC_CAPABILITY_METADATA = 0x10000,
IMAPC_CAPABILITY_IMAP4REV1 = 0x40000000
};
struct imapc_capability_name {
const char *name;
enum imapc_capability capability;
};
extern const struct imapc_capability_name imapc_capability_names[];
enum imapc_command_flags {
IMAPC_COMMAND_FLAG_SELECT = 0x01,
IMAPC_COMMAND_FLAG_PRELOGIN = 0x02,
IMAPC_COMMAND_FLAG_RETRIABLE = 0x04,
IMAPC_COMMAND_FLAG_LOGOUT = 0x08,
IMAPC_COMMAND_FLAG_RECONNECTED = 0x10
};
enum imapc_client_ssl_mode {
IMAPC_CLIENT_SSL_MODE_NONE,
IMAPC_CLIENT_SSL_MODE_IMMEDIATE,
IMAPC_CLIENT_SSL_MODE_STARTTLS
};
#define IMAPC_DEFAULT_CONNECT_TIMEOUT_MSECS (1000*30)
#define IMAPC_DEFAULT_COMMAND_TIMEOUT_MSECS (1000*60*5)
#define IMAPC_DEFAULT_MAX_LINE_LENGTH (SIZE_MAX)
struct imapc_throttling_settings {
unsigned int init_msecs;
unsigned int max_msecs;
unsigned int shrink_min_msecs;
};
struct imapc_client_settings {
const char *host;
in_port_t port;
const char *master_user;
const char *username;
const char *password;
const char *sasl_mechanisms;
bool use_proxyauth;
unsigned int max_idle_time;
const char *session_id_prefix;
const char *dns_client_socket_path;
const char *temp_path_prefix;
struct ssl_iostream_settings ssl_set;
enum imapc_client_ssl_mode ssl_mode;
const char *rawlog_dir;
bool debug;
unsigned int connect_timeout_msecs;
unsigned int connect_retry_count;
unsigned int connect_retry_interval_msecs;
unsigned int cmd_timeout_msecs;
size_t max_line_length;
struct imapc_throttling_settings throttle_set;
};
struct imapc_command_reply {
enum imapc_command_state state;
const char *resp_text_key, *resp_text_value;
const char *text_full;
const char *text_without_resp;
};
struct imapc_arg_file {
int fd;
const struct imap_arg *parent_arg;
unsigned int list_idx;
};
struct imapc_untagged_reply {
const char *name;
uint32_t num;
const struct imap_arg *args;
const struct imapc_arg_file *file_args;
unsigned int file_args_count;
const char *resp_text_key, *resp_text_value;
void *untagged_box_context;
};
enum imapc_state_change_event {
IMAPC_STATE_CHANGE_AUTH_OK,
IMAPC_STATE_CHANGE_AUTH_FAILED,
};
typedef void imapc_command_callback_t(const struct imapc_command_reply *reply,
void *context);
typedef void imapc_untagged_callback_t(const struct imapc_untagged_reply *reply,
void *context);
typedef void imapc_state_change_callback_t(void *context,
enum imapc_state_change_event event,
const char *error);
struct imapc_client *
imapc_client_init(const struct imapc_client_settings *set,
struct event *event_parent);
void imapc_client_disconnect(struct imapc_client *client);
void imapc_client_deinit(struct imapc_client **client);
void
imapc_client_set_login_callback(struct imapc_client *client,
imapc_command_callback_t *callback, void *context);
void imapc_client_login(struct imapc_client *client);
void imapc_client_logout(struct imapc_client *client);
struct imapc_command *
imapc_client_cmd(struct imapc_client *client,
imapc_command_callback_t *callback, void *context);
void imapc_command_set_flags(struct imapc_command *cmd,
enum imapc_command_flags flags);
bool imapc_command_connection_is_selected(struct imapc_command *cmd);
void imapc_command_send(struct imapc_command *cmd, const char *cmd_str);
void imapc_command_sendf(struct imapc_command *cmd, const char *cmd_fmt, ...)
ATTR_FORMAT(2, 3);
void imapc_command_sendvf(struct imapc_command *cmd,
const char *cmd_fmt, va_list args) ATTR_FORMAT(2, 0);
const char *imapc_command_get_tag(struct imapc_command *cmd);
void imapc_command_abort(struct imapc_command **cmd);
struct timeval imapc_command_get_start_time(struct imapc_command *cmd);
struct imapc_command *
imapc_client_find_command_by_tag(struct imapc_client *client, const char *tag);
void imapc_client_register_untagged(struct imapc_client *client,
imapc_untagged_callback_t *callback,
void *context);
void imapc_client_run(struct imapc_client *client);
void imapc_client_stop(struct imapc_client *client);
bool imapc_client_is_running(struct imapc_client *client);
struct imapc_client_mailbox *
imapc_client_mailbox_open(struct imapc_client *client,
void *untagged_box_context);
void imapc_client_mailbox_set_reopen_cb(struct imapc_client_mailbox *box,
void (*callback)(void *context),
void *context);
void imapc_client_mailbox_close(struct imapc_client_mailbox **box);
bool imapc_client_mailbox_can_reconnect(struct imapc_client_mailbox *box);
void imapc_client_mailbox_reconnect(struct imapc_client_mailbox *box,
const char *errmsg);
struct imapc_command *
imapc_client_mailbox_cmd(struct imapc_client_mailbox *box,
imapc_command_callback_t *callback, void *context);
struct imapc_msgmap *
imapc_client_mailbox_get_msgmap(struct imapc_client_mailbox *box);
void imapc_client_mailbox_idle(struct imapc_client_mailbox *box);
bool imapc_client_mailbox_is_opened(struct imapc_client_mailbox *box);
int imapc_client_get_capabilities(struct imapc_client *client,
enum imapc_capability *capabilities_r);
int imapc_client_create_temp_fd(struct imapc_client *client,
const char **path_r);
void imapc_client_register_state_change_callback(struct imapc_client *client,
imapc_state_change_callback_t *cb,
void *context);
#endif