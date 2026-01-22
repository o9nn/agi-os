#ifndef MBOXSTORAGE_H
#define MBOXSTORAGE_H
#include <libetpan/mboxdriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int mbox_mailstorage_init(struct mailstorage * storage,
const char * mb_pathname, int mb_cached,
const char * mb_cache_directory, const char * mb_flags_directory);
#ifdef __cplusplus
}
#endif
#endif