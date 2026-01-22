#ifndef MHSTORAGE_H
#define MHSTORAGE_H
#include <libetpan/mhdriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int mh_mailstorage_init(struct mailstorage * storage,
const char * mh_pathname, int mh_cached,
const char * mh_cache_directory, const char * mh_flags_directory);
#ifdef __cplusplus
}
#endif
#endif