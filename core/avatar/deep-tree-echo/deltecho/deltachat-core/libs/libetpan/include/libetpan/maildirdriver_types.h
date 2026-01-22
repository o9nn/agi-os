#ifndef MAILDIRDRIVER_TYPES_H
#define MAILDIRDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/maildir.h>
#include <libetpan/generic_cache_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct maildir_session_state_data {
struct maildir * md_session;
struct mail_flags_store * md_flags_store;
};
enum {
MAILDIRDRIVER_CACHED_SET_CACHE_DIRECTORY = 1,
MAILDIRDRIVER_CACHED_SET_FLAGS_DIRECTORY
};
struct maildir_cached_session_state_data {
mailsession * md_ancestor;
char * md_quoted_mb;
struct mail_flags_store * md_flags_store;
char md_cache_directory[PATH_MAX];
char md_flags_directory[PATH_MAX];
};
struct maildir_mailstorage {
char * md_pathname;
int md_cached;
char * md_cache_directory;
char * md_flags_directory;
};
#ifdef __cplusplus
}
#endif
#endif