#ifndef IMAP_MATCH_H
#define IMAP_MATCH_H
enum imap_match_result {
IMAP_MATCH_NO = 0x00,
IMAP_MATCH_YES = 0x01,
IMAP_MATCH_CHILDREN = 0x02,
IMAP_MATCH_PARENT = 0x04
};
struct imap_match_glob;
struct imap_match_glob *
imap_match_init(pool_t pool, const char *pattern,
bool inboxcase, char separator);
struct imap_match_glob *
imap_match_init_multiple(pool_t pool, const char *const *patterns,
bool inboxcase, char separator);
void imap_match_deinit(struct imap_match_glob **glob);
struct imap_match_glob *
imap_match_dup(pool_t pool, const struct imap_match_glob *glob);
bool imap_match_globs_equal(const struct imap_match_glob *glob1,
const struct imap_match_glob *glob2);
enum imap_match_result
imap_match(struct imap_match_glob *glob, const char *data);
#endif