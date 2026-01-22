#ifndef _HURD_DISKFS
#define _HURD_DISKFS
#include <assert-backtrace.h>
#include <unistd.h>
#include <pthread.h>
#include <hurd/ports.h>
#include <hurd/fshelp.h>
#include <hurd/ihash.h>
#include <hurd/iohelp.h>
#include <idvec.h>
#include <features.h>
#include <refcount.h>
#ifdef DISKFS_DEFINE_EXTERN_INLINE
#define DISKFS_EXTERN_INLINE
#else
#define DISKFS_EXTERN_INLINE __extern_inline
#endif
struct protid
{
struct port_info pi;
struct iouser *user;
struct peropen *po;
memory_object_t shared_object;
struct shared_io *mapped;
};
struct peropen
{
loff_t filepointer;
struct rlock_peropen lock_status;
refcount_t refcnt;
int openstat;
struct node *np;
mach_port_t root_parent;
mach_port_t shadow_root_parent;
struct node *shadow_root;
char *path;
};
struct node
{
hurd_ihash_locp_t slot;
struct disknode *dn;
io_statbuf_t dn_stat;
unsigned int
dn_set_ctime:1,
dn_set_atime:1,
dn_set_mtime:1,
dn_stat_dirty:1,
author_tracks_uid:1;
pthread_mutex_t lock;
refcounts_t refcounts;
mach_port_t sockaddr;
int owner;
struct transbox transbox;
struct rlock_box userlock;
struct conch conch;
struct modreq *dirmod_reqs;
unsigned int dirmod_tick;
struct modreq *filemod_reqs;
unsigned int filemod_tick;
loff_t allocsize;
ino64_t cache_id;
};
struct diskfs_control
{
struct port_info pi;
};
struct bootinfo
{
struct port_info pi;
};
enum lookup_type
{
LOOKUP,
CREATE,
REMOVE,
RENAME,
};
struct modreq
{
mach_port_t port;
struct modreq *next;
};
#define SPEC_DOTDOT 0x10000000
struct argp;
struct argp_child;
struct store;
struct store_parsed;
extern mach_port_t diskfs_default_pager;
extern auth_t diskfs_auth_server_port;
extern mach_port_t diskfs_fsys_identity;
extern char **diskfs_argv;
extern const char *diskfs_boot_command_line;
#define diskfs_boot_filesystem()	(diskfs_boot_command_line != 0)
extern int _diskfs_boot_pause;
extern const char *diskfs_boot_init_program;
extern pthread_rwlock_t diskfs_fsys_lock;
extern volatile struct mapped_time_value *diskfs_mtime;
extern int diskfs_synchronous;
extern int pager_port_type;
extern int diskfs_readonly;
struct pager;
extern struct port_class *diskfs_protid_class;
extern struct port_class *diskfs_control_class;
extern struct port_class *diskfs_execboot_class;
extern struct port_class *diskfs_shutdown_notification_class;
extern struct port_bucket *diskfs_port_bucket;
struct dirstat;
extern const size_t diskfs_dirstat_size;
extern int diskfs_link_max;
extern int diskfs_name_max;
extern int diskfs_maxsymlinks;
extern int diskfs_hard_readonly;
extern struct node *diskfs_root_node;
extern char *diskfs_server_name;
extern char *diskfs_server_version;
extern char *diskfs_extra_version;
extern int diskfs_shortcut_symlink;
extern int diskfs_shortcut_chrdev;
extern int diskfs_shortcut_blkdev;
extern int diskfs_shortcut_fifo;
extern int diskfs_shortcut_ifsock;
extern int diskfs_default_sync_interval;
extern char *diskfs_disk_name;
error_t diskfs_set_statfs (fsys_statfsbuf_t *statfsbuf);
error_t diskfs_lookup_hard (struct node *dp,
const char *name, enum lookup_type type,
struct node **np, struct dirstat *ds,
struct protid *cred);
error_t diskfs_direnter_hard (struct node *dp, const char *name,
struct node *np, struct dirstat *ds,
struct protid *cred);
error_t diskfs_dirrewrite_hard (struct node *dp, struct node *np,
struct dirstat *ds);
error_t diskfs_dirremove_hard (struct node *dp, struct dirstat *ds);
void diskfs_null_dirstat (struct dirstat *ds);
error_t diskfs_drop_dirstat (struct node *dp, struct dirstat *ds);
error_t diskfs_get_directs (struct node *dp, int entry, int n,
char **data, mach_msg_type_number_t *datacnt,
vm_size_t bufsiz, int *amt);
error_t diskfs_get_translator (struct node *np, char **namep, mach_msg_type_number_t *namelen);
error_t diskfs_set_translator (struct node *np,
const char *name, mach_msg_type_number_t namelen,
struct protid *cred);
error_t diskfs_truncate (struct node *np, loff_t size);
error_t diskfs_grow (struct node *np, loff_t size, struct protid *cred);
error_t diskfs_set_hypermetadata (int wait, int clean);
error_t diskfs_alloc_node (struct node *dp, mode_t mode, struct node **np);
void diskfs_free_node (struct node *np, mode_t mode);
void diskfs_node_norefs (struct node *np);
void diskfs_try_dropping_softrefs (struct node *np);
void diskfs_lost_hardrefs (struct node *np);
void diskfs_new_hardrefs (struct node *np);
int diskfs_dirempty (struct node *dp, struct protid *cred);
error_t diskfs_validate_mode_change (struct node *np, mode_t mode);
error_t diskfs_validate_owner_change (struct node *np, uid_t uid);
error_t diskfs_validate_group_change (struct node *np, gid_t gid);
error_t diskfs_validate_author_change (struct node *np, uid_t author);
error_t diskfs_validate_flags_change (struct node *np, int flags);
error_t diskfs_validate_rdev_change (struct node *np, dev_t rdev);
void diskfs_write_disknode (struct node *np, int wait);
void diskfs_file_update (struct node *np, int wait);
error_t diskfs_node_iterate (error_t (*fun)(struct node *));
void diskfs_sync_everything (int wait);
void diskfs_shutdown_pager (void);
mach_port_t diskfs_get_filemap (struct node *np, vm_prot_t prot);
int diskfs_pager_users (void);
vm_prot_t diskfs_max_user_pager_prot (void);
struct pager *diskfs_get_filemap_pager_struct (struct node *np);
void diskfs_readonly_changed (int readonly);
error_t diskfs_reload_global_state (void);
error_t diskfs_node_reload (struct node *node);
extern error_t (*diskfs_create_symlink_hook)(struct node *np, const char *target);
extern error_t (*diskfs_read_symlink_hook)(struct node *np, char *target);
error_t diskfs_get_source (char *source, size_t source_len);
struct lookup_context;
error_t diskfs_user_make_node (struct node **npp, struct lookup_context *ctx);
error_t diskfs_user_read_node (struct node *np, struct lookup_context *ctx);
void diskfs_user_try_dropping_softrefs (struct node *np);
struct node *diskfs_cached_ifind (ino64_t inum);
error_t diskfs_init_diskfs (void);
mach_port_t diskfs_startup_diskfs (mach_port_t bootstrap, int flags);
void diskfs_spawn_first_thread (ports_demuxer_type demuxer);
void diskfs_start_bootstrap (void);
void diskfs_drop_node (struct node *np);
void diskfs_node_update (struct node *np, int wait);
void diskfs_nref (struct node *np);
void diskfs_nput (struct node *np);
void diskfs_nrele (struct node *np);
void diskfs_nref_light (struct node *np);
void diskfs_nput_light (struct node *np);
void diskfs_nrele_light (struct node *np);
error_t
diskfs_node_rdwr (struct node *np, char *data, loff_t off,
mach_msg_type_number_t amt, int dir,
struct protid *cred,
mach_msg_type_number_t *amtread);
void
diskfs_notice_dirchange (struct node *dp, enum dir_changed_type type,
const char *name);
void
diskfs_notice_filechange (struct node *np, enum file_changed_type type,
loff_t start, loff_t end);
struct node *diskfs_make_node (struct disknode *dn);
struct node *diskfs_make_node_alloc (size_t size);
extern const size_t _diskfs_sizeof_struct_node;
struct disknode *diskfs_node_disknode (struct node *node);
struct node *diskfs_disknode_node (struct disknode *disknode);
#if defined(__USE_EXTERN_INLINES) || defined(DISKFS_DEFINE_EXTERN_INLINE)
DISKFS_EXTERN_INLINE struct disknode *
diskfs_node_disknode (struct node *node)
{
return (struct disknode *) ((char *) node + _diskfs_sizeof_struct_node);
}
DISKFS_EXTERN_INLINE struct node *
diskfs_disknode_node (struct disknode *disknode)
{
return (struct node *) ((char *) disknode - _diskfs_sizeof_struct_node);
}
#endif
error_t diskfs_lookup (struct node *dp,
const char *name, enum lookup_type type,
struct node **np, struct dirstat *ds,
struct protid *cred);
error_t
diskfs_direnter (struct node *dp, const char *name, struct node *np,
struct dirstat *ds, struct protid *cred);
error_t diskfs_dirrewrite (struct node *dp, struct node *oldnp,
struct node *np, const char *name,
struct dirstat *ds);
error_t diskfs_dirremove (struct node *dp, struct node *np,
const char *name, struct dirstat *ds);
error_t diskfs_cached_lookup (ino64_t cache_id, struct node **npp);
error_t diskfs_cached_lookup_context (ino64_t inum, struct node **npp,
struct lookup_context *ctx);
error_t
diskfs_create_node (struct node *dir, const char *name, mode_t mode,
struct node **newnode, struct protid *cred,
struct dirstat *ds);
error_t diskfs_create_protid (struct peropen *po, struct iouser *user,
struct protid **cred);
error_t diskfs_start_protid (struct peropen *po, struct protid **cred);
void diskfs_finish_protid (struct protid *cred, struct iouser *user);
extern struct protid * diskfs_begin_using_protid_port (file_t port);
extern struct protid *
diskfs_begin_using_protid_payload (uintptr_t payload);
extern struct diskfs_control * diskfs_begin_using_control_port (fsys_t port);
extern struct diskfs_control *
diskfs_begin_using_control_port_payload (uintptr_t payload);
extern struct bootinfo *diskfs_begin_using_bootinfo_port (exec_startup_t port);
struct bootinfo *
diskfs_begin_using_bootinfo_payload (uintptr_t payload);
extern void diskfs_end_using_protid_port (struct protid *cred);
extern void diskfs_end_using_control_port (struct diskfs_control *cred);
extern void diskfs_end_using_bootinfo (struct bootinfo *upt);
#if defined(__USE_EXTERN_INLINES) || defined(DISKFS_DEFINE_EXTERN_INLINE)
DISKFS_EXTERN_INLINE struct protid *
diskfs_begin_using_protid_port (file_t port)
{
return ports_lookup_port (diskfs_port_bucket, port, diskfs_protid_class);
}
DISKFS_EXTERN_INLINE struct protid *
diskfs_begin_using_protid_payload (uintptr_t payload)
{
return ports_lookup_payload (diskfs_port_bucket,
payload,
diskfs_protid_class);
}
DISKFS_EXTERN_INLINE struct diskfs_control *
diskfs_begin_using_control_port (fsys_t port)
{
return ports_lookup_port (diskfs_port_bucket, port, diskfs_control_class);
}
DISKFS_EXTERN_INLINE struct diskfs_control *
diskfs_begin_using_control_port_payload (uintptr_t payload)
{
return ports_lookup_payload (diskfs_port_bucket,
payload,
diskfs_control_class);
}
DISKFS_EXTERN_INLINE struct bootinfo *
diskfs_begin_using_bootinfo_port (exec_startup_t port)
{
return ports_lookup_port (diskfs_port_bucket, port, diskfs_execboot_class);
}
DISKFS_EXTERN_INLINE struct bootinfo *
diskfs_begin_using_bootinfo_payload (uintptr_t payload)
{
return ports_lookup_payload (diskfs_port_bucket,
payload,
diskfs_execboot_class);
}
DISKFS_EXTERN_INLINE void
diskfs_end_using_protid_port (struct protid *cred)
{
if (cred)
ports_port_deref (cred);
}
DISKFS_EXTERN_INLINE void
diskfs_end_using_control_port (struct diskfs_control *cred)
{
if (cred)
ports_port_deref (cred);
}
DISKFS_EXTERN_INLINE void
diskfs_end_using_bootinfo (struct bootinfo *b)
{
if (b)
ports_port_deref (b);
}
#endif
void diskfs_protid_rele (void *arg);
error_t
diskfs_make_peropen (struct node *np, int flags,
struct peropen *context, struct peropen **ppo);
void diskfs_release_peropen (struct peropen *po);
void diskfs_enter_lookup_cache (struct node *dir, struct node *np,
const char *name);
void diskfs_purge_lookup_cache (struct node *dp, struct node *np);
struct node *diskfs_check_lookup_cache (struct node *dir, const char *name);
error_t
diskfs_rename_dir (struct node *fdp, struct node *fnp, const char *fromname,
struct node *tdp, const char *toname,
struct protid *fromcred, struct protid *tocred, int excl);
error_t diskfs_clear_directory (struct node *dp, struct node *pdp,
struct protid *cred);
error_t
diskfs_init_dir (struct node *dp, struct node *pdp, struct protid *cred);
void diskfs_set_node_atime (struct node *np);
void diskfs_set_node_times (struct node *np);
error_t diskfs_shutdown (int flags);
error_t diskfs_set_readonly (int readonly);
error_t diskfs_remount (void);
error_t diskfs_execboot_fsys_startup (mach_port_t port, int flags,
mach_port_t ctl, mach_port_t *real,
mach_msg_type_name_t *realpoly);
error_t diskfs_set_sync_interval (int interval);
error_t diskfs_set_options (const char *argz, size_t argz_len);
error_t diskfs_append_args (char **argz, size_t *argz_len);
extern struct argp *diskfs_runtime_argp;
extern const struct argp diskfs_std_runtime_argp;
extern const struct argp diskfs_startup_argp;
extern const struct argp diskfs_store_startup_argp;
error_t diskfs_append_std_options (char **argz, size_t *argz_len);
int diskfs_demuxer (mach_msg_header_t *, mach_msg_header_t *);
int diskfs_check_readonly (void);
struct store *diskfs_init_main (struct argp *startup_argp,
int argc, char **argv,
struct store_parsed **store_parsed,
mach_port_t *bootstrap);
void diskfs_console_stdio (void);
typedef struct protid *protid_t;
kern_return_t diskfs_S_io_write (protid_t io_object,
const_data_t data,
mach_msg_type_number_t dataCnt,
loff_t offset,
vm_size_t *amount);
kern_return_t diskfs_S_io_read (protid_t io_object,
data_t *data,
mach_msg_type_number_t *dataCnt,
loff_t offset,
vm_size_t amount);
kern_return_t diskfs_S_io_seek (protid_t io_object,
loff_t offset,
int whence,
loff_t *newp);
kern_return_t diskfs_S_io_stat (protid_t stat_object,
io_statbuf_t *stat_info);
kern_return_t diskfs_S_file_set_size (protid_t trunc_file,
loff_t new_size);
kern_return_t diskfs_S_file_get_storage_info (protid_t file,
portarray_t *ports,
mach_msg_type_name_t *portsPoly,
mach_msg_type_number_t *portsCnt,
intarray_t *ints,
mach_msg_type_number_t *intsCnt,
off_array_t *offsets,
mach_msg_type_number_t *offsetsCnt,
data_t *data,
mach_msg_type_number_t *dataCnt);
kern_return_t diskfs_S_file_statfs (protid_t file,
fsys_statfsbuf_t *info);
#endif