#ifndef MAILDIRSTORAGE_H
#define MAILDIRSTORAGE_H
#include <libetpan/maildirdriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int maildir_mailstorage_init(struct mailstorage * storage,
const char * md_pathname, int md_cached,
const char * md_cache_directory, const char * md_flags_directory);
#ifdef __cplusplus
}
#endif
#endif