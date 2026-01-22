#ifndef FEEDDRIVER_TYPES_H
#define FEEDDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/mailstorage_types.h>
#include <libetpan/newsfeed.h>
#ifdef __cplusplus
extern "C" {
#endif
struct feed_session_state_data {
time_t feed_last_update;
struct newsfeed * feed_session;
int feed_error;
};
struct feed_mailstorage {
char * feed_url;
int feed_cached;
char * feed_cache_directory;
char * feed_flags_directory;
};
#ifdef __cplusplus
}
#endif
#endif