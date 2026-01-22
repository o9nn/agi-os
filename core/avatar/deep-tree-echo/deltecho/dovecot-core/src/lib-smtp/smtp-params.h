#ifndef SMTP_PARAMS_H
#define SMTP_PARAMS_H
#include "array-decl.h"
#include "smtp-common.h"
struct smtp_param;
ARRAY_DEFINE_TYPE(smtp_param, struct smtp_param);
enum smtp_param_mail_body_type {
SMTP_PARAM_MAIL_BODY_TYPE_UNSPECIFIED = 0,
SMTP_PARAM_MAIL_BODY_TYPE_7BIT,
SMTP_PARAM_MAIL_BODY_TYPE_8BITMIME,
SMTP_PARAM_MAIL_BODY_TYPE_BINARYMIME,
SMTP_PARAM_MAIL_BODY_TYPE_EXTENSION
};
enum smtp_param_mail_ret {
SMTP_PARAM_MAIL_RET_UNSPECIFIED = 0,
SMTP_PARAM_MAIL_RET_HDRS,
SMTP_PARAM_MAIL_RET_FULL,
};
enum smtp_param_rcpt_notify {
SMTP_PARAM_RCPT_NOTIFY_UNSPECIFIED = 0x00,
SMTP_PARAM_RCPT_NOTIFY_SUCCESS   	 = 0x01,
SMTP_PARAM_RCPT_NOTIFY_FAILURE     = 0x02,
SMTP_PARAM_RCPT_NOTIFY_DELAY       = 0x04,
SMTP_PARAM_RCPT_NOTIFY_NEVER       = 0x80
};
struct smtp_param {
const char *keyword;
const char *value;
};
struct smtp_params_mail {
const struct smtp_address *auth;
struct {
enum smtp_param_mail_body_type type;
const char *ext;
} body;
const char *envid;
enum smtp_param_mail_ret ret;
uoff_t size;
ARRAY_TYPE(smtp_param) extra_params;
};
struct smtp_params_rcpt {
struct {
const char *addr_type;
const struct smtp_address *addr;
const char *addr_raw;
} orcpt;
enum smtp_param_rcpt_notify notify;
ARRAY_TYPE(smtp_param) extra_params;
};
enum smtp_param_parse_error {
SMTP_PARAM_PARSE_ERROR_BAD_SYNTAX = 0,
SMTP_PARAM_PARSE_ERROR_NOT_SUPPORTED
};
int smtp_param_parse(pool_t pool, const char *text,
struct smtp_param *param_r, const char **error_r);
void smtp_params_copy(pool_t pool, ARRAY_TYPE(smtp_param) *dst,
const ARRAY_TYPE(smtp_param) *src) ATTR_NULL(3);
void smtp_params_add_one(ARRAY_TYPE(smtp_param) *params, pool_t pool,
const char *keyword, const char *value);
void smtp_params_add_encoded(ARRAY_TYPE(smtp_param) *params, pool_t pool,
const char *keyword, const unsigned char *value,
size_t value_len);
bool smtp_params_drop_one(ARRAY_TYPE(smtp_param) *params, const char *keyword,
const char **value_r);
void smtp_param_write(string_t *out, const struct smtp_param *param);
const struct smtp_param *
smtp_params_get_param(const ARRAY_TYPE(smtp_param) *params,
const char *keyword);
int smtp_params_decode_param(const ARRAY_TYPE(smtp_param) *params,
const char *keyword, string_t **value_r,
bool allow_nul, const char **error_r);
bool smtp_params_equal(const ARRAY_TYPE(smtp_param) *params1,
const ARRAY_TYPE(smtp_param) *params2);
int smtp_params_mail_parse(pool_t pool, const char *args,
enum smtp_capability caps,
const char *const *param_extensions,
const char *const *body_param_extensions,
struct smtp_params_mail *params_r,
enum smtp_param_parse_error *error_code_r,
const char **error_r) ATTR_NULL(4, 5);
void smtp_params_mail_copy(pool_t pool, struct smtp_params_mail *dst,
const struct smtp_params_mail *src);
void smtp_params_mail_add_extra(struct smtp_params_mail *params, pool_t pool,
const char *keyword, const char *value)
ATTR_NULL(4);
void smtp_params_mail_encode_extra(struct smtp_params_mail *params, pool_t pool,
const char *keyword,
const unsigned char *value,
size_t value_len);
bool smtp_params_mail_drop_extra(struct smtp_params_mail *params,
const char *keyword, const char **value_r)
ATTR_NULL(3);
void smtp_params_mail_write(string_t *buffer, enum smtp_capability caps,
const char *const *extra_params,
const struct smtp_params_mail *params) ATTR_NULL(3);
const struct smtp_param *
smtp_params_mail_get_extra(const struct smtp_params_mail *params,
const char *keyword);
int smtp_params_mail_decode_extra(const struct smtp_params_mail *params,
const char *keyword, string_t **value_r,
bool allow_nul, const char **error_r);
void smtp_params_mail_add_to_event(const struct smtp_params_mail *params,
struct event *event);
enum smtp_param_rcpt_parse_flags {
SMTP_PARAM_RCPT_FLAG_ORCPT_ALLOW_LOCALPART = BIT(0),
};
int smtp_params_rcpt_parse(pool_t pool, const char *args,
enum smtp_param_rcpt_parse_flags flags,
enum smtp_capability caps,
const char *const *param_extensions,
struct smtp_params_rcpt *params_r,
enum smtp_param_parse_error *error_code_r,
const char **error_r) ATTR_NULL(4);
void smtp_params_rcpt_copy(pool_t pool, struct smtp_params_rcpt *dst,
const struct smtp_params_rcpt *src) ATTR_NULL(3);
void smtp_params_rcpt_add_extra(struct smtp_params_rcpt *params, pool_t pool,
const char *keyword, const char *value)
ATTR_NULL(4);
void smtp_params_rcpt_encode_extra(struct smtp_params_rcpt *params, pool_t pool,
const char *keyword,
const unsigned char *value,
size_t value_len);
bool smtp_params_rcpt_drop_extra(struct smtp_params_rcpt *params,
const char *keyword, const char **value_r)
ATTR_NULL(3);
void smtp_params_rcpt_set_orcpt(struct smtp_params_rcpt *params, pool_t pool,
struct smtp_address *rcpt);
void smtp_params_rcpt_write(string_t *buffer, enum smtp_capability caps,
const char *const *extra_params,
const struct smtp_params_rcpt *params) ATTR_NULL(3);
const struct smtp_param *
smtp_params_rcpt_get_extra(const struct smtp_params_rcpt *params,
const char *keyword);
int smtp_params_rcpt_decode_extra(const struct smtp_params_rcpt *params,
const char *keyword, string_t **value_r,
bool allow_nul, const char **error_r);
bool smtp_params_rcpt_equal(const struct smtp_params_rcpt *params1,
const struct smtp_params_rcpt *params2);
static inline bool
smtp_params_rcpt_has_orcpt(const struct smtp_params_rcpt *params)
{
return (params->orcpt.addr_type != NULL);
}
void smtp_params_rcpt_add_to_event(const struct smtp_params_rcpt *params,
struct event *event);
#endif