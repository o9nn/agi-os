#ifndef IMAP_BASE_SUBJECT_H
#define IMAP_BASE_SUBJECT_H
const char *imap_get_base_subject_cased(pool_t pool, const char *subject,
bool *is_reply_or_forward_r);
#endif