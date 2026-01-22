#ifndef IMAP_URLAUTH_CONNECTION_H
#define IMAP_URLAUTH_CONNECTION_H
#define IMAP_URLAUTH_PROTOCOL_MAJOR_VERSION 2
#define IMAP_URLAUTH_PROTOCOL_MINOR_VERSION 0
struct imap_urlauth_request;
struct imap_urlauth_fetch_reply;
typedef int
imap_urlauth_request_callback_t(struct imap_urlauth_fetch_reply *reply,
void *context);
struct imap_urlauth_connection *
imap_urlauth_connection_init(const char *path, const char *service,
struct mail_user *user, const char *session_id,
unsigned int idle_timeout_msecs);
void imap_urlauth_connection_deinit(struct imap_urlauth_connection **conn);
int imap_urlauth_connection_connect(struct imap_urlauth_connection *conn);
void imap_urlauth_connection_continue(struct imap_urlauth_connection *conn);
struct imap_urlauth_request *
imap_urlauth_request_new(struct imap_urlauth_connection *conn,
const char *target_user, const char *url,
enum imap_urlauth_fetch_flags flags,
imap_urlauth_request_callback_t *callback,
void *context);
void imap_urlauth_request_abort(struct imap_urlauth_connection *conn,
struct imap_urlauth_request *urlreq);
void imap_urlauth_request_abort_by_context(struct imap_urlauth_connection *conn,
void *context);
bool imap_urlauth_connection_is_connected(struct imap_urlauth_connection *conn);
#endif