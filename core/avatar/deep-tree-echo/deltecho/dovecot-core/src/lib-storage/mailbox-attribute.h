#ifndef MAILBOX_ATTRIBUTE_H
#define MAILBOX_ATTRIBUTE_H
struct mailbox;
struct mailbox_transaction_context;
#define MAILBOX_ATTRIBUTE_PREFIX_DOVECOT "vendor/vendor.dovecot/"
#define MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT \
MAILBOX_ATTRIBUTE_PREFIX_DOVECOT"pvt/"
#define MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER \
MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT"server/"
#define MAILBOX_ATTRIBUTE_KEY_IS_USER_ACCESSIBLE(key) \
(!str_begins_with(key, MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT) || \
(str_begins_with(key, MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER) && \
strncmp(key, MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT, \
strlen(MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT)) != 0))
enum mail_attribute_type {
MAIL_ATTRIBUTE_TYPE_PRIVATE,
MAIL_ATTRIBUTE_TYPE_SHARED
};
#define MAIL_ATTRIBUTE_TYPE_MASK 0x0f
#define MAIL_ATTRIBUTE_TYPE_FLAG_VALIDATED 0x80
enum mail_attribute_value_flags {
MAIL_ATTRIBUTE_VALUE_FLAG_READONLY = 0x01,
MAIL_ATTRIBUTE_VALUE_FLAG_INT_STREAMS = 0x02
};
struct mail_attribute_value {
const char *value;
struct istream *value_stream;
time_t last_change;
enum mail_attribute_value_flags flags;
};
enum mail_attribute_internal_rank {
MAIL_ATTRIBUTE_INTERNAL_RANK_DEFAULT = 0,
MAIL_ATTRIBUTE_INTERNAL_RANK_OVERRIDE,
MAIL_ATTRIBUTE_INTERNAL_RANK_AUTHORITY
};
enum mail_attribute_internal_flags {
MAIL_ATTRIBUTE_INTERNAL_FLAG_CHILDREN = 0x01,
MAIL_ATTRIBUTE_INTERNAL_FLAG_VALIDATED = 0x02,
};
struct mailbox_attribute_internal {
enum mail_attribute_type type;
const char *key;
enum mail_attribute_internal_rank rank;
enum mail_attribute_internal_flags flags;
int (*get)(struct mailbox *box, const char *key,
struct mail_attribute_value *value_r);
int (*set)(struct mailbox_transaction_context *t, const char *key,
const struct mail_attribute_value *value);
int (*iter)(struct mailbox *box, const char *key_prefix,
pool_t pool, ARRAY_TYPE(const_string) *keys);
};
void mailbox_attribute_register_internal(
const struct mailbox_attribute_internal *iattr);
void mailbox_attribute_register_internals(
const struct mailbox_attribute_internal *iattrs, unsigned int count);
void mailbox_attribute_unregister_internal(
const struct mailbox_attribute_internal *iattr);
void mailbox_attribute_unregister_internals(
const struct mailbox_attribute_internal *iattrs, unsigned int count);
int mailbox_attribute_set(struct mailbox_transaction_context *t,
enum mail_attribute_type type_flags, const char *key,
const struct mail_attribute_value *value);
int mailbox_attribute_unset(struct mailbox_transaction_context *t,
enum mail_attribute_type type_flags, const char *key);
int mailbox_attribute_get(struct mailbox *box,
enum mail_attribute_type type_flags, const char *key,
struct mail_attribute_value *value_r);
int mailbox_attribute_get_stream(struct mailbox *box,
enum mail_attribute_type type_flags,
const char *key,
struct mail_attribute_value *value_r);
struct mailbox_attribute_iter *
mailbox_attribute_iter_init(struct mailbox *box,
enum mail_attribute_type type_flags,
const char *prefix);
const char *mailbox_attribute_iter_next(struct mailbox_attribute_iter *iter);
int mailbox_attribute_iter_deinit(struct mailbox_attribute_iter **iter);
#endif