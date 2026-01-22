#ifndef FEEDSTORAGE_H
#define FEEDSTORAGE_H
#include <libetpan/feeddriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int feed_mailstorage_init(struct mailstorage * storage,
const char * feed_url,
int feed_cached, const char * feed_cache_directory,
const char * feed_flags_directory);
#ifdef __cplusplus
}
#endif
#endif