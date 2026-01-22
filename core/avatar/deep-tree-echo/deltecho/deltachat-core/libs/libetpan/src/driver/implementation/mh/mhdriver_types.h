#ifndef MHDRIVER_TYPES_H
#define MHDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/mailmh.h>
#include <libetpan/clist.h>
#include <libetpan/generic_cache_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct mh_session_state_data {
struct mailmh * mh_session;
struct mailmh_folder * mh_cur_folder;
clist * mh_subscribed_list;
};
enum {
MHDRIVER_CACHED_SET_CACHE_DIRECTORY = 1,
MHDRIVER_CACHED_SET_FLAGS_DIRECTORY
};
struct mh_cached_session_state_data {
mailsession * mh_ancestor;
char * mh_quoted_mb;
char mh_cache_directory[PATH_MAX];
char mh_flags_directory[PATH_MAX];
struct mail_flags_store * mh_flags_store;
};
struct mh_mailstorage {
char * mh_pathname;
int mh_cached;
char * mh_cache_directory;
char * mh_flags_directory;
};
#ifdef __cplusplus
}
#endif
#endif