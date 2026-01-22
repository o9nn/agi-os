#ifndef MBOXDRIVER_TYPES_H
#define MBOXDRIVER_TYPES_H
#include <libetpan/maildriver_types.h>
#include <libetpan/mailmbox.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
enum {
MBOXDRIVER_SET_READ_ONLY = 1,
MBOXDRIVER_SET_NO_UID
};
struct mbox_session_state_data {
struct mailmbox_folder * mbox_folder;
int mbox_force_read_only;
int mbox_force_no_uid;
};
enum {
MBOXDRIVER_CACHED_SET_READ_ONLY = 1,
MBOXDRIVER_CACHED_SET_NO_UID,
MBOXDRIVER_CACHED_SET_CACHE_DIRECTORY,
MBOXDRIVER_CACHED_SET_FLAGS_DIRECTORY
};
struct mbox_cached_session_state_data {
mailsession * mbox_ancestor;
char * mbox_quoted_mb;
char mbox_cache_directory[PATH_MAX];
char mbox_flags_directory[PATH_MAX];
struct mail_flags_store * mbox_flags_store;
};
struct mbox_mailstorage {
char * mbox_pathname;
int mbox_cached;
char * mbox_cache_directory;
char * mbox_flags_directory;
};
#ifdef __cplusplus
}
#endif
#endif