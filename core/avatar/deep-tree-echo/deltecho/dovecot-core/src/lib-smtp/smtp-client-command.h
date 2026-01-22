#ifndef SMTP_CLIENT_COMMAND
#define SMTP_CLIENT_COMMAND
struct smtp_reply;
struct smtp_params_mail;
struct smtp_params_rcpt;
struct smtp_client_command;
struct smtp_client_connection;
enum smtp_client_command_state {
SMTP_CLIENT_COMMAND_STATE_NEW = 0,
SMTP_CLIENT_COMMAND_STATE_SUBMITTED,
SMTP_CLIENT_COMMAND_STATE_SENDING,
SMTP_CLIENT_COMMAND_STATE_WAITING,
SMTP_CLIENT_COMMAND_STATE_FINISHED,
SMTP_CLIENT_COMMAND_STATE_ABORTED
};
enum smtp_client_command_flags {
SMTP_CLIENT_COMMAND_FLAG_PRELOGIN = 0x01,
SMTP_CLIENT_COMMAND_FLAG_PIPELINE = 0x02,
SMTP_CLIENT_COMMAND_FLAG_PRIORITY = 0x04
};
typedef void smtp_client_command_callback_t(const struct smtp_reply *reply,
void *context);
struct smtp_client_command *
smtp_client_command_new(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_new(conn, flags, callback, context) \
smtp_client_command_new(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_plug(struct smtp_client_connection *conn,
struct smtp_client_command *after);
void smtp_client_command_ref(struct smtp_client_command *cmd);
bool smtp_client_command_unref(struct smtp_client_command **_cmd)
ATTR_NOWARN_UNUSED_RESULT;
bool smtp_client_command_name_equals(struct smtp_client_command *cmd,
const char *name);
void smtp_client_command_lock(struct smtp_client_command *cmd);
void smtp_client_command_unlock(struct smtp_client_command *cmd);
void smtp_client_command_set_flags(struct smtp_client_command *cmd,
enum smtp_client_command_flags flags);
void smtp_client_command_set_stream(struct smtp_client_command *cmd,
struct istream *input, bool dot);
void smtp_client_command_write(struct smtp_client_command *cmd,
const char *cmd_str);
void smtp_client_command_printf(struct smtp_client_command *cmd,
const char *cmd_fmt, ...) ATTR_FORMAT(2, 3);
void smtp_client_command_vprintf(struct smtp_client_command *cmd,
const char *cmd_fmt, va_list args)
ATTR_FORMAT(2, 0);
void smtp_client_command_submit_after(struct smtp_client_command *cmd,
struct smtp_client_command *after);
void smtp_client_command_submit(struct smtp_client_command *cmd);
void smtp_client_command_abort(struct smtp_client_command **_cmd);
void smtp_client_command_set_abort_callback(struct smtp_client_command *cmd,
void (*callback)(void *context),
void *context);
void smtp_client_command_set_sent_callback(struct smtp_client_command *cmd,
void (*callback)(void *context),
void *context);
void smtp_client_command_set_replies(struct smtp_client_command *cmd,
unsigned int replies);
enum smtp_client_command_state
smtp_client_command_get_state(struct smtp_client_command *cmd) ATTR_PURE;
struct smtp_client_command *
smtp_client_command_noop_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_noop_submit_after(conn, flags, after, \
callback, context) \
smtp_client_command_noop_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, (smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_noop_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_noop_submit(conn, flags, callback, context) \
smtp_client_command_noop_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_vrfy_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
const char *param,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_vrfy_submit_after(conn, flags, after, param, \
callback, context) \
smtp_client_command_vrfy_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, param, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_vrfy_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
const char *param,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_vrfy_submit(conn, flags, param, callback, context) \
smtp_client_command_vrfy_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
param, (smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_rset_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_rset_submit_after(conn, flags, after, \
callback, context) \
smtp_client_command_rset_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, (smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_rset_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_rset_submit(conn, flags, callback, context) \
smtp_client_command_rset_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_mail_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
const struct smtp_address *from,
const struct smtp_params_mail *params,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_mail_submit_after(conn, flags, after, \
address, params, \
callback, context) \
smtp_client_command_mail_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, address, params, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_mail_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
const struct smtp_address *from,
const struct smtp_params_mail *params,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_mail_submit(conn, flags, address, params, \
callback, context) \
smtp_client_command_mail_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
address, params, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_rcpt_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
const struct smtp_address *to,
const struct smtp_params_rcpt *params,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_rcpt_submit_after(conn, flags, after, to, params, \
callback, context) \
smtp_client_command_rcpt_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, to, params, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_rcpt_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
const struct smtp_address *to,
const struct smtp_params_rcpt *params,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_rcpt_submit(conn, flags, to, params, \
callback, context) \
smtp_client_command_rcpt_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
to, params, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_data_submit_after(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct smtp_client_command *after,
struct istream *data,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_data_submit_after(conn, flags, after, data, \
callback, context) \
smtp_client_command_data_submit_after(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
after, data, \
(smtp_client_command_callback_t *)callback, context)
struct smtp_client_command *
smtp_client_command_data_submit(struct smtp_client_connection *conn,
enum smtp_client_command_flags flags,
struct istream *data,
smtp_client_command_callback_t *callback,
void *context);
#define smtp_client_command_data_submit(conn, flags, data, callback, context) \
smtp_client_command_data_submit(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
data, (smtp_client_command_callback_t *)callback, context)
#endif