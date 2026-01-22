#ifndef MAIL_DELIVER_H
#define MAIL_DELIVER_H
#include "guid.h"
#include "mail-types.h"
#include "mail-error.h"
#include "smtp-params.h"
#include <sys/time.h>
struct smtp_address;
struct mail_storage;
struct mail_save_context;
struct mailbox;
enum mail_deliver_error {
MAIL_DELIVER_ERROR_NONE = 0,
MAIL_DELIVER_ERROR_TEMPORARY,
MAIL_DELIVER_ERROR_REJECTED,
MAIL_DELIVER_ERROR_NOQUOTA,
MAIL_DELIVER_ERROR_INTERNAL,
};
struct mail_deliver_session {
pool_t pool;
ARRAY(guid_128_t) inbox_guids;
};
struct mail_deliver_input {
const struct lda_settings *set;
const struct smtp_submit_settings *smtp_set;
struct mail_deliver_session *session;
struct event *event_parent;
unsigned int session_time_msecs;
struct timeval delivery_time_started;
const char *session_id;
struct mail *src_mail;
const struct smtp_address *mail_from;
struct smtp_params_mail mail_params;
const struct smtp_address *rcpt_to;
struct smtp_params_rcpt rcpt_params;
struct mail_user *rcpt_user;
const char *rcpt_default_mailbox;
bool save_dest_mail:1;
};
struct mail_deliver_fields {
const char *message_id;
const char *subject;
const char *from;
const char *from_envelope;
const char *storage_id;
uoff_t psize, vsize;
bool filled:1;
};
struct mail_deliver_context {
pool_t pool;
const struct lda_settings *set;
const struct smtp_submit_settings *smtp_set;
struct mail_deliver_session *session;
struct event *event;
unsigned int session_time_msecs;
struct timeval delivery_time_started;
struct mail_duplicate_db *dup_db;
const char *session_id;
struct mail *src_mail;
const struct smtp_address *mail_from;
struct smtp_params_mail mail_params;
const struct smtp_address *rcpt_to;
struct smtp_params_rcpt rcpt_params;
struct mail_user *rcpt_user;
const char *rcpt_default_mailbox;
struct mail *dest_mail;
struct mail_deliver_fields fields;
const char *tempfail_error;
bool tried_default_save;
bool saved_mail;
bool save_dest_mail;
bool mailbox_full;
bool dsn;
};
struct mail_deliver_save_open_context {
struct mail_user *user;
bool lda_mailbox_autocreate;
bool lda_mailbox_autosubscribe;
};
typedef int deliver_mail_func_t(struct mail_deliver_context *ctx,
struct mail_storage **storage_r);
extern deliver_mail_func_t *deliver_mail;
const struct var_expand_table *
mail_deliver_ctx_get_log_var_expand_table(struct mail_deliver_context *ctx,
const char *message);
void mail_deliver_log(struct mail_deliver_context *ctx, const char *fmt, ...)
ATTR_FORMAT(2, 3);
const struct smtp_address *
mail_deliver_get_address(struct mail *mail, const char *header);
const struct smtp_address *
mail_deliver_get_return_address(struct mail_deliver_context *ctx);
const char *mail_deliver_get_new_message_id(struct mail_deliver_context *ctx);
struct mail_deliver_session *mail_deliver_session_init(void);
void mail_deliver_session_deinit(struct mail_deliver_session **session);
void mail_deliver_init(struct mail_deliver_context *ctx,
struct mail_deliver_input *input);
void mail_deliver_deinit(struct mail_deliver_context *ctx);
int mail_deliver_save_open(struct mail_deliver_save_open_context *ctx,
const char *name, struct mailbox **box_r,
enum mail_error *error_r, const char **error_str_r);
int mail_deliver_save(struct mail_deliver_context *ctx, const char *mailbox,
enum mail_flags flags, const char *const *keywords,
struct mail_storage **storage_r) ATTR_NULL(4);
void mail_deliver_deduplicate_guid_if_needed(struct mail_deliver_session *session,
struct mail_save_context *save_ctx);
int mail_deliver(struct mail_deliver_context *ctx,
enum mail_deliver_error *error_code_r,
const char **error_r);
deliver_mail_func_t *mail_deliver_hook_set(deliver_mail_func_t *new_hook);
void mail_deliver_hooks_init(void);
#endif