#ifndef DSYNC_MAIL_H
#define DSYNC_MAIL_H
#include "mail-types.h"
struct md5_context;
struct mail;
struct mailbox;
struct dsync_mail {
const char *guid;
uint32_t uid;
time_t saved_date;
struct mail *input_mail;
uint32_t input_mail_uid;
bool minimal_fields;
const char *pop3_uidl;
uint32_t pop3_order;
time_t received_date;
struct istream *input;
};
struct dsync_mail_request {
const char *guid;
uint32_t uid;
};
enum dsync_mail_change_type {
DSYNC_MAIL_CHANGE_TYPE_SAVE,
DSYNC_MAIL_CHANGE_TYPE_EXPUNGE,
DSYNC_MAIL_CHANGE_TYPE_FLAG_CHANGE,
DSYNC_MAIL_CHANGE_TYPE_COUNT
};
#define KEYWORD_CHANGE_ADD '+'
#define KEYWORD_CHANGE_REMOVE '-'
#define KEYWORD_CHANGE_FINAL '='
#define KEYWORD_CHANGE_ADD_AND_FINAL '&'
struct dsync_mail_change {
enum dsync_mail_change_type type;
uint32_t uid;
const char *guid;
const char *hdr_hash;
uint64_t modseq;
uint64_t pvt_modseq;
uint8_t add_flags, remove_flags, final_flags;
uint8_t add_pvt_flags, remove_pvt_flags;
bool keywords_reset;
ARRAY_TYPE(const_string) keyword_changes;
time_t received_timestamp;
uoff_t virtual_size;
};
struct mailbox_header_lookup_ctx *
dsync_mail_get_hash_headers(struct mailbox *box, const char *const *hashed_headers);
int dsync_mail_get_hdr_hash(struct mail *mail, unsigned int version,
const char *const *hashed_headers, const char **hdr_hash_r);
static inline bool dsync_mail_hdr_hash_is_empty(const char *hdr_hash)
{
return strcmp(hdr_hash, "68b329da9893e34099c7d8ad5cb9c940") == 0;
}
int dsync_mail_fill(struct mail *mail, bool minimal_fill,
struct dsync_mail *dmail_r, const char **error_field_r);
int dsync_mail_fill_nonminimal(struct mail *mail, struct dsync_mail *dmail_r,
const char **error_field_r);
void dsync_mail_change_dup(pool_t pool, const struct dsync_mail_change *src,
struct dsync_mail_change *dest_r);
#endif