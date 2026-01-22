#ifndef IMAP_METADATA_H
#define IMAP_METADATA_H
#define IMAP_METADATA_PRIVATE_PREFIX "/private"
#define IMAP_METADATA_SHARED_PREFIX "/shared"
struct imap_metadata_iter;
struct imap_metadata_transaction;
bool imap_metadata_verify_entry_name(
const char *name, const char **client_error_r);
int imap_metadata_set(struct imap_metadata_transaction *imtrans,
const char *entry, const struct mail_attribute_value *value);
int imap_metadata_unset(struct imap_metadata_transaction *imtrans,
const char *entry);
int imap_metadata_get(struct imap_metadata_transaction *imtrans,
const char *entry, struct mail_attribute_value *value_r);
int imap_metadata_get_stream(struct imap_metadata_transaction *imtrans,
const char *entry, struct mail_attribute_value *value_r);
struct imap_metadata_iter *
imap_metadata_iter_init(struct imap_metadata_transaction *imtrans,
const char *entry);
const char *imap_metadata_iter_next(struct imap_metadata_iter *iter);
int imap_metadata_iter_deinit(struct imap_metadata_iter **_iter);
struct imap_metadata_transaction *
imap_metadata_transaction_begin(struct mailbox *box);
struct imap_metadata_transaction *
imap_metadata_transaction_begin_mailbox(struct mail_user *user,
const char *mailbox);
struct imap_metadata_transaction *
imap_metadata_transaction_begin_server(struct mail_user *user);
void imap_metadata_transaction_validated_only(struct imap_metadata_transaction *imtrans,
bool set);
int imap_metadata_transaction_commit(
struct imap_metadata_transaction **_imtrans,
enum mail_error *error_code_r, const char **client_error_r);
void imap_metadata_transaction_rollback(
struct imap_metadata_transaction **_imtrans);
const char *
imap_metadata_transaction_get_last_error(
struct imap_metadata_transaction *imtrans,
enum mail_error *error_code_r);
#endif