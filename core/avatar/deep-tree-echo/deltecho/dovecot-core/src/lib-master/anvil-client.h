#ifndef ANVIL_CLIENT_H
#define ANVIL_CLIENT_H
#define ANVIL_DEFAULT_LOOKUP_TIMEOUT_MSECS (5*1000)
#define ANVIL_DEFAULT_KICK_TIMEOUT_MSECS (25*1000)
enum anvil_client_flags {
ANVIL_CLIENT_FLAG_HIDE_ENOENT	= 0x01
};
struct anvil_client_callbacks {
bool (*reconnect)(void);
bool (*command)(const char *cmd, const char *const *args);
};
typedef void anvil_callback_t(const char *reply, void *context);
struct anvil_client *
anvil_client_init(const char *path,
const struct anvil_client_callbacks *callbacks,
enum anvil_client_flags flags) ATTR_NULL(2);
void anvil_client_deinit(struct anvil_client **client);
int anvil_client_connect(struct anvil_client *client, bool retry);
struct anvil_query *
anvil_client_query(struct anvil_client *client, const char *query,
unsigned int timeout_msecs,
anvil_callback_t *callback, void *context);
#define anvil_client_query(client, query, timeout_msecs, callback, context) \
anvil_client_query(client, query, timeout_msecs, \
(anvil_callback_t *)(callback), 1 ? (context) : \
CALLBACK_TYPECHECK(callback, \
void (*)(const char *, typeof(context))))
void anvil_client_query_abort(struct anvil_client *client,
struct anvil_query **query);
void anvil_client_cmd(struct anvil_client *client, const char *cmd);
void anvil_client_send_reply(struct anvil_client *client, const char *reply);
bool anvil_client_is_connected(struct anvil_client *client);
#endif