#ifndef _HURD_FSHELP_
#define _HURD_FSHELP_
#ifdef FSHELP_DEFINE_EXTERN_INLINE
#define FSHELP_EXTERN_INLINE
#else
#define FSHELP_EXTERN_INLINE __extern_inline
#endif
#include <errno.h>
#include <stdlib.h>
#include <mach.h>
#include <hurd/hurd_types.h>
#include <pthread.h>
#include <hurd/iohelp.h>
#include <sys/stat.h>
#include <maptime.h>
#include <stdlib.h>
#include <fcntl.h>
struct port_info;
struct transbox;
error_t
fshelp_set_active_translator (struct port_info *pi,
const char *name,
const struct transbox *transbox);
error_t
fshelp_remove_active_translator (mach_port_t active);
error_t
fshelp_get_active_translators (char **translators,
size_t *translators_len,
mach_port_t **controls,
size_t *controls_count);
error_t
fshelp_map_active_translators (error_t (*fun)(void *cookie,
const char *name,
mach_port_t control),
void *cookie);
typedef error_t (*fshelp_open_fn_t) (int flags,
file_t *node,
mach_msg_type_name_t *node_type,
task_t, void *cookie);
error_t
fshelp_start_translator_long (fshelp_open_fn_t underlying_open_fn,
void *cookie, char *name, char *argz,
mach_msg_type_number_t argz_len,
mach_port_t *fds,
mach_msg_type_name_t fds_type,
mach_msg_type_number_t fds_len,
mach_port_t *ports,
mach_msg_type_name_t ports_type,
mach_msg_type_number_t ports_len,
int *ints,
mach_msg_type_number_t ints_len,
uid_t owner_uid,
int timeout, fsys_t *control);
error_t
fshelp_start_translator (fshelp_open_fn_t underlying_open_fn, void *cookie,
char *name, char *argz,
mach_msg_type_number_t argz_len,
int timeout, fsys_t *control);
struct transbox
{
fsys_t active;
pthread_mutex_t *lock;
int flags;
pthread_cond_t wakeup;
void *cookie;
};
#define TRANSBOX_STARTING 1
#define TRANSBOX_WANTED 2
typedef error_t (*fshelp_fetch_root_callback1_t) (void *cookie1, void *cookie2,
uid_t *uid, gid_t *gid,
char **argz, mach_msg_type_number_t *argz_len);
struct fshelp_stat_cookie2
{
io_statbuf_t *statp;
mode_t *modep;
void *next;
};
error_t fshelp_short_circuited_callback1 (void *cookie1, void *cookie2,
uid_t *uid, gid_t *gid,
char **argz, mach_msg_type_number_t *argz_len);
typedef error_t (*fshelp_fetch_root_callback2_t) (void *cookie1, void *cookie2,
int flags,
mach_port_t *underlying,
mach_msg_type_name_t
*underlying_type);
error_t
fshelp_fetch_root (struct transbox *transbox, void *cookie,
file_t dotdot,
struct iouser *user,
int flags,
fshelp_fetch_root_callback1_t callback1,
fshelp_fetch_root_callback2_t callback2,
retry_type *retry, char *retryname, mach_port_t *root);
void
fshelp_transbox_init (struct transbox *transbox,
pthread_mutex_t *lock,
void *cookie);
int fshelp_translated (struct transbox *box);
error_t fshelp_set_active (struct transbox *box,
fsys_t newactive, int excl);
error_t fshelp_fetch_control (struct transbox *box,
mach_port_t *control);
void fshelp_drop_transbox (struct transbox *box);
struct lock_box
{
int type;
pthread_cond_t wait;
int waiting;
int shcount;
};
void fshelp_lock_init (struct lock_box *box);
error_t fshelp_acquire_lock (struct lock_box *box, int *user,
pthread_mutex_t *mut, int flags);
struct rlock_box
{
struct rlock_list *locks;
};
error_t fshelp_rlock_init (struct rlock_box *box);
#if defined(__USE_EXTERN_INLINES) || defined(FSHELP_DEFINE_EXTERN_INLINE)
FSHELP_EXTERN_INLINE
error_t fshelp_rlock_init (struct rlock_box *box)
{
box->locks = NULL;
return 0;
}
#endif
struct rlock_peropen
{
struct rlock_list **locks;
};
error_t fshelp_rlock_po_init (struct rlock_peropen *po);
#if defined(__USE_EXTERN_INLINES) || defined(FSHELP_DEFINE_EXTERN_INLINE)
FSHELP_EXTERN_INLINE
error_t fshelp_rlock_po_init (struct rlock_peropen *po)
{
po->locks = malloc (sizeof (struct rlock_list *));
if (! po->locks)
return ENOMEM;
*po->locks = NULL;
return 0;
}
#endif
error_t fshelp_rlock_drop_peropen (struct rlock_peropen *po);
error_t fshelp_rlock_po_fini (struct rlock_peropen *po);
#if defined(__USE_EXTERN_INLINES) || defined(FSHELP_DEFINE_EXTERN_INLINE)
FSHELP_EXTERN_INLINE
error_t fshelp_rlock_po_fini (struct rlock_peropen *po)
{
free (po->locks);
po->locks = NULL;
return 0;
}
#endif
error_t fshelp_rlock_tweak (struct rlock_box *box,
pthread_mutex_t *mutex,
struct rlock_peropen *po, int open_mode,
loff_t size, loff_t curpointer, int cmd,
struct flock64 *lock, mach_port_t rendezvous);
int fshelp_rlock_peropen_status (struct rlock_peropen *po);
int fshelp_rlock_node_status (struct rlock_box *box);
struct port_bucket;
error_t fshelp_get_identity (struct port_bucket *bucket,
ino64_t fileno, mach_port_t *pt);
error_t fshelp_delegate_translation (const char *server_name,
mach_port_t requestor, char **argv);
struct idvec;
error_t
fshelp_exec_reauth (int suid, uid_t uid, int sgid, gid_t gid,
auth_t auth,
error_t
(*get_file_ids)(struct idvec *uids, struct idvec *gids),
mach_port_t *ports, mach_msg_type_number_t num_ports,
mach_port_t *fds, mach_msg_type_number_t num_fds,
int *secure);
struct argp;
error_t fshelp_set_options (const struct argp *argp, int flags,
const char *argz, size_t argz_len, void *input);
error_t fshelp_isowner (io_statbuf_t *st, struct iouser *user);
error_t
fshelp_iscontroller (io_statbuf_t *st, struct iouser *user);
error_t fshelp_access (io_statbuf_t *st, int op, struct iouser *user);
error_t fshelp_checkdirmod (io_statbuf_t *dir, io_statbuf_t *st,
struct iouser *user);
#define TOUCH_ATIME 0x1
#define TOUCH_MTIME 0x2
#define TOUCH_CTIME 0x4
void fshelp_touch (io_statbuf_t *st, unsigned what,
volatile struct mapped_time_value *maptime);
#endif