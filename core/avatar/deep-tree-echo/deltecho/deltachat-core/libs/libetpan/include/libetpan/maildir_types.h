#ifndef MAILDIR_TYPES_H
#define MAILDIR_TYPES_H
#include <sys/types.h>
#include <libetpan/libetpan-config.h>
#include <libetpan/chash.h>
#include <libetpan/carray.h>
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#include <libetpan/libetpan-config.h>
#define LIBETPAN_MAILDIR
enum {
MAILDIR_NO_ERROR = 0,
MAILDIR_ERROR_CREATE,
MAILDIR_ERROR_DIRECTORY,
MAILDIR_ERROR_MEMORY,
MAILDIR_ERROR_FILE,
MAILDIR_ERROR_NOT_FOUND,
MAILDIR_ERROR_FOLDER
};
#define MAILDIR_FLAG_NEW (1 << 0)
#define MAILDIR_FLAG_SEEN (1 << 1)
#define MAILDIR_FLAG_REPLIED (1 << 2)
#define MAILDIR_FLAG_FLAGGED (1 << 3)
#define MAILDIR_FLAG_TRASHED (1 << 4)
struct maildir_msg {
char * msg_uid;
char * msg_filename;
int msg_flags;
};
#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX 255
#endif
struct maildir {
pid_t mdir_pid;
char mdir_hostname[HOST_NAME_MAX];
char mdir_path[PATH_MAX];
uint32_t mdir_counter;
time_t mdir_mtime_new;
time_t mdir_mtime_cur;
carray * mdir_msg_list;
chash * mdir_msg_hash;
};
#endif