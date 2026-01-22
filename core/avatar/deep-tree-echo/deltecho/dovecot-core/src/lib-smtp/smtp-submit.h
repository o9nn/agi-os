#ifndef SMTP_SUBMIT_H
#define SMTP_SUBMIT_H
#include "smtp-submit-settings.h"
struct ssl_iostream_settings;
struct smtp_address;
struct smtp_submit_settings;
struct smtp_submit_session;
struct smtp_submit;
struct smtp_submit_input {
const struct ssl_iostream_settings *ssl;
struct event *event_parent;
bool allow_root:1;
};
struct smtp_submit_result {
int status;
const char *error;
};
typedef void
smtp_submit_callback_t(const struct smtp_submit_result *result,
void *context);
struct smtp_submit_session *
smtp_submit_session_init(const struct smtp_submit_input *input,
const struct smtp_submit_settings *set);
void smtp_submit_session_deinit(struct smtp_submit_session **_session);
struct smtp_submit *
smtp_submit_init(struct smtp_submit_session *session,
const struct smtp_address *mail_from);
struct smtp_submit *
smtp_submit_init_simple(const struct smtp_submit_input *input,
const struct smtp_submit_settings *set,
const struct smtp_address *mail_from) ATTR_NULL(2);
void smtp_submit_deinit(struct smtp_submit **_submit);
void smtp_submit_add_rcpt(struct smtp_submit *subm,
const struct smtp_address *rcpt_to);
struct ostream *smtp_submit_send(struct smtp_submit *subm);
void smtp_submit_run_async(struct smtp_submit *subm,
smtp_submit_callback_t *callback, void *context);
#define smtp_submit_run_async(subm, callback, context) \
smtp_submit_run_async(subm, \
(smtp_submit_callback_t*)callback, \
(char*)context - CALLBACK_TYPECHECK(callback, \
void (*)(const struct smtp_submit_result *result, typeof(context))))
int smtp_submit_run(struct smtp_submit *subm, const char **error_r);
#endif