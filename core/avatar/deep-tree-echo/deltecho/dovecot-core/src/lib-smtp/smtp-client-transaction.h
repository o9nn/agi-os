#ifndef SMTP_CLIENT_TRANSACTION_H
#define SMTP_CLIENT_TRANSACTION_H
#include "net.h"
#include "istream.h"
struct smtp_address;
struct smtp_client_transaction;
struct smtp_client_transaction_mail;
struct smtp_client_transaction_rcpt;
enum smtp_client_transaction_flags {
SMTP_CLIENT_TRANSACTION_FLAG_REPLY_PER_RCPT = BIT(0),
};
enum smtp_client_transaction_state {
SMTP_CLIENT_TRANSACTION_STATE_NEW = 0,
SMTP_CLIENT_TRANSACTION_STATE_PENDING,
SMTP_CLIENT_TRANSACTION_STATE_MAIL_FROM,
SMTP_CLIENT_TRANSACTION_STATE_RCPT_TO,
SMTP_CLIENT_TRANSACTION_STATE_DATA,
SMTP_CLIENT_TRANSACTION_STATE_RESET,
SMTP_CLIENT_TRANSACTION_STATE_FINISHED,
SMTP_CLIENT_TRANSACTION_STATE_ABORTED
};
extern const char *const smtp_client_transaction_state_names[];
struct smtp_client_transaction_times {
struct timeval started;
struct timeval finished;
};
typedef void
smtp_client_transaction_callback_t(void *context);
struct smtp_client_transaction *
smtp_client_transaction_create_empty(
struct smtp_client_connection *conn,
enum smtp_client_transaction_flags flags,
smtp_client_transaction_callback_t *callback, void *context)
ATTR_NULL(4);
#define smtp_client_transaction_create_empty(conn, flags, callback, context) \
smtp_client_transaction_create_empty(conn, flags - \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))), \
(smtp_client_transaction_callback_t *)callback, context)
struct smtp_client_transaction *
smtp_client_transaction_create(struct smtp_client_connection *conn,
const struct smtp_address *mail_from,
const struct smtp_params_mail *mail_params,
enum smtp_client_transaction_flags flags,
smtp_client_transaction_callback_t *callback, void *context)
ATTR_NULL(2, 3, 6);
#define smtp_client_transaction_create(conn, \
mail_from, mail_params, flags, callback, context) \
smtp_client_transaction_create(conn, mail_from, mail_params, flags - \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))), \
(smtp_client_transaction_callback_t *)callback, context)
void smtp_client_transaction_ref(struct smtp_client_transaction *trans);
void smtp_client_transaction_unref(struct smtp_client_transaction **_trans);
void smtp_client_transaction_destroy(struct smtp_client_transaction **trans);
void smtp_client_transaction_abort(struct smtp_client_transaction *trans);
void smtp_client_transaction_fail_reply(struct smtp_client_transaction *trans,
const struct smtp_reply *reply);
void smtp_client_transaction_fail(struct smtp_client_transaction *trans,
unsigned int status, const char *error);
void smtp_client_transaction_set_event(struct smtp_client_transaction *trans,
struct event *event);
void smtp_client_transaction_set_timeout(struct smtp_client_transaction *trans,
unsigned int timeout_msecs);
void smtp_client_transaction_start(struct smtp_client_transaction *trans,
smtp_client_command_callback_t *mail_callback, void *context);
#define smtp_client_transaction_start(trans, mail_callback, context) \
smtp_client_transaction_start(trans, \
(smtp_client_command_callback_t *)mail_callback, TRUE ? context : \
CALLBACK_TYPECHECK(mail_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))))
void smtp_client_transaction_start_empty(
struct smtp_client_transaction *trans,
const struct smtp_address *mail_from,
const struct smtp_params_mail *mail_params,
smtp_client_command_callback_t *mail_callback, void *context);
#define smtp_client_transaction_start_empty(trans, mail_from, mail_params, \
mail_callback, context) \
smtp_client_transaction_start_empty(trans, mail_from, mail_params, \
(smtp_client_command_callback_t *)mail_callback, TRUE ? context : \
CALLBACK_TYPECHECK(mail_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))))
struct smtp_client_transaction_mail *
smtp_client_transaction_add_mail(struct smtp_client_transaction *trans,
const struct smtp_address *mail_from,
const struct smtp_params_mail *mail_params,
smtp_client_command_callback_t *mail_callback,
void *context)
ATTR_NOWARN_UNUSED_RESULT ATTR_NULL(3,5);
#define smtp_client_transaction_add_mail(trans, \
mail_from, mail_params, mail_callback, context) \
smtp_client_transaction_add_mail(trans, mail_from - \
CALLBACK_TYPECHECK(mail_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
mail_params, \
(smtp_client_command_callback_t *)mail_callback, context)
void smtp_client_transaction_mail_abort(
struct smtp_client_transaction_mail **_mail);
struct smtp_client_transaction_rcpt *
smtp_client_transaction_add_rcpt(struct smtp_client_transaction *trans,
const struct smtp_address *rcpt_to,
const struct smtp_params_rcpt *rcpt_params,
smtp_client_command_callback_t *rcpt_callback,
smtp_client_command_callback_t *data_callback,
void *context)
ATTR_NOWARN_UNUSED_RESULT ATTR_NULL(3,5,6);
#define smtp_client_transaction_add_rcpt(trans, \
rcpt_to, rcpt_params, rcpt_callback, data_callback, context) \
smtp_client_transaction_add_rcpt(trans, rcpt_to - \
CALLBACK_TYPECHECK(rcpt_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))) - \
CALLBACK_TYPECHECK(data_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
rcpt_params, \
(smtp_client_command_callback_t *)rcpt_callback, \
(smtp_client_command_callback_t *)data_callback, context)
struct smtp_client_transaction_rcpt *
smtp_client_transaction_add_pool_rcpt(
struct smtp_client_transaction *trans, pool_t pool,
const struct smtp_address *rcpt_to,
const struct smtp_params_rcpt *rcpt_params,
smtp_client_command_callback_t *rcpt_callback, void *context)
ATTR_NOWARN_UNUSED_RESULT ATTR_NULL(4,6,7);
#define smtp_client_transaction_add_pool_rcpt(trans, pool, \
rcpt_to, rcpt_params, rcpt_callback, context) \
smtp_client_transaction_add_pool_rcpt(trans, pool, rcpt_to - \
CALLBACK_TYPECHECK(rcpt_callback, void (*)( \
const struct smtp_reply *reply, typeof(context))), \
rcpt_params, \
(smtp_client_command_callback_t *)rcpt_callback, context)
void smtp_client_transaction_rcpt_abort(
struct smtp_client_transaction_rcpt **_rcpt);
void smtp_client_transaction_rcpt_set_data_callback(
struct smtp_client_transaction_rcpt *rcpt,
smtp_client_command_callback_t *callback, void *context)
ATTR_NULL(3);
#define smtp_client_transaction_rcpt_set_data_callback(trans, \
callback, context) \
smtp_client_transaction_rcpt_set_data_callback(trans, \
(smtp_client_command_callback_t *)callback, \
(TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct smtp_reply *reply, typeof(context)))))
void smtp_client_transaction_send(
struct smtp_client_transaction *trans, struct istream *data_input,
smtp_client_command_callback_t *data_callback, void *data_context);
#define smtp_client_transaction_send(trans, \
data_input, data_callback, data_context) \
smtp_client_transaction_send(trans, data_input - \
CALLBACK_TYPECHECK(data_callback, void (*)( \
const struct smtp_reply *reply, typeof(data_context))), \
(smtp_client_command_callback_t *)data_callback, data_context)
void smtp_client_transaction_reset(
struct smtp_client_transaction *trans,
smtp_client_command_callback_t *reset_callback, void *reset_context);
#define smtp_client_transaction_reset(trans, reset_callback, reset_context) \
smtp_client_transaction_reset(trans, \
(smtp_client_command_callback_t *)reset_callback, \
TRUE ? reset_context : \
CALLBACK_TYPECHECK(reset_callback, void (*)( \
const struct smtp_reply *reply, typeof(reset_context))))
void smtp_client_transaction_set_immediate(
struct smtp_client_transaction *trans, bool immediate);
const struct smtp_client_transaction_times *
smtp_client_transaction_get_times(struct smtp_client_transaction *trans);
enum smtp_client_transaction_state
smtp_client_transaction_get_state(struct smtp_client_transaction *trans)
ATTR_PURE;
const char *
smtp_client_transaction_get_state_name(struct smtp_client_transaction *trans)
ATTR_PURE;
const char *
smtp_client_transaction_get_state_destription(
struct smtp_client_transaction *trans);
#endif