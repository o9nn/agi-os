#ifndef _HURD_NETFS_H_
#define _HURD_NETFS_H_
#include <hurd/ports.h>
#include <hurd/fshelp.h>
#include <hurd/iohelp.h>
#include <assert-backtrace.h>
#include <pthread.h>
#include <refcount.h>
struct argp;
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
struct node *next, **prevp;
struct netnode *nn;
io_statbuf_t nn_stat;
mode_t nn_translated;
pthread_mutex_t lock;
refcounts_t refcounts;
mach_port_t sockaddr;
int owner;
struct transbox transbox;
struct rlock_box userlock;
struct conch conch;
struct dirmod *dirmod_reqs;
};
struct netfs_control
{
struct port_info pi;
};
extern char *netfs_server_name;
extern char *netfs_server_version;
error_t netfs_validate_stat (struct node *np, struct iouser *cred);
error_t netfs_attempt_chown (struct iouser *cred, struct node *np,
uid_t uid, uid_t gid);
error_t netfs_attempt_chauthor (struct iouser *cred, struct node *np,
uid_t author);
error_t netfs_attempt_chmod (struct iouser *cred, struct node *np,
mode_t mode);
error_t netfs_attempt_mksymlink (struct iouser *cred, struct node *np,
const char *name);
error_t netfs_attempt_mkdev (struct iouser *cred, struct node *np,
mode_t type, dev_t indexes);
error_t netfs_set_translator (struct iouser *cred, struct node *np,
const char *argz, mach_msg_type_number_t argzlen);
error_t netfs_get_translator (struct node *node, char **argz,
mach_msg_type_number_t *argz_len);
error_t netfs_attempt_chflags (struct iouser *cred, struct node *np,
int flags);
error_t netfs_attempt_utimes (struct iouser *cred, struct node *np,
struct timespec *atime, struct timespec *mtime);
error_t netfs_attempt_set_size (struct iouser *cred, struct node *np,
loff_t size);
error_t netfs_attempt_statfs (struct iouser *cred, struct node *np,
fsys_statfsbuf_t *st);
error_t netfs_attempt_sync (struct iouser *cred, struct node *np,
int wait);
error_t netfs_attempt_syncfs (struct iouser *cred, int wait);
error_t netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **np);
error_t netfs_attempt_unlink (struct iouser *user, struct node *dir,
const char *name);
error_t netfs_attempt_rename (struct iouser *user, struct node *fromdir,
const char *fromname, struct node *todir,
const char *toname, int excl);
error_t netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode);
error_t netfs_attempt_rmdir (struct iouser *user,
struct node *dir, const char *name);
error_t netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl);
error_t netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **np);
error_t netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **np);
error_t netfs_attempt_readlink (struct iouser *user, struct node *np,
char *buf);
error_t netfs_check_open_permissions (struct iouser *user, struct node *np,
int flags, int newnode);
error_t netfs_attempt_read (struct iouser *cred, struct node *np,
loff_t offset, size_t *len, void *data);
error_t netfs_attempt_write (struct iouser *cred, struct node *np,
loff_t offset, size_t *len, const void *data);
error_t netfs_report_access (struct iouser *cred, struct node *np,
int *types);
struct iouser *netfs_make_user (uid_t *uids, int nuids,
uid_t *gids, int ngids);
void netfs_node_norefs (struct node *np);
error_t netfs_get_dirents (struct iouser *cred, struct node *dir,
int entry, int nentries, char **data,
mach_msg_type_number_t *datacnt,
vm_size_t bufsize, int *amt);
mach_port_t netfs_get_filemap (struct node *np, vm_prot_t prot);
error_t netfs_file_get_storage_info (struct iouser *cred,
struct node *np,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints,
mach_msg_type_number_t *num_ints,
loff_t **offsets,
mach_msg_type_number_t *num_offsets,
char **data,
mach_msg_type_number_t *data_len);
error_t netfs_get_source (char *source, size_t source_len);
error_t netfs_set_options (const char *argz, size_t argz_len);
error_t netfs_append_args (char **argz, size_t *argz_len);
extern struct argp *netfs_runtime_argp;
extern const struct argp netfs_std_runtime_argp;
extern const struct argp netfs_std_startup_argp;
error_t netfs_append_std_options (char **argz, size_t *argz_len);
extern int netfs_maxsymlinks;
struct node *netfs_make_node (struct netnode *);
struct node *netfs_make_node_alloc (size_t size);
extern const size_t _netfs_sizeof_struct_node;
static inline struct netnode *
netfs_node_netnode (struct node *node)
{
return (struct netnode *) ((char *) node + _netfs_sizeof_struct_node);
}
static inline struct node *
netfs_netnode_node (struct netnode *netnode)
{
return (struct node *) ((char *) netnode - _netfs_sizeof_struct_node);
}
void netfs_init (void);
mach_port_t netfs_startup (mach_port_t bootstrap, int flags);
void netfs_server_loop (void);
struct protid *netfs_make_protid (struct peropen *po, struct iouser *user);
struct peropen *netfs_make_peropen (struct node *, int,
struct peropen *context);
void netfs_nref (struct node *np);
void netfs_nref_light (struct node *np);
void netfs_nrele (struct node *np);
void netfs_nrele_light (struct node *np);
void netfs_nput (struct node *np);
void netfs_try_dropping_softrefs (struct node *np);
void netfs_drop_node (struct node *np);
void netfs_release_protid (void *);
void netfs_release_peropen (struct peropen *);
int netfs_demuxer (mach_msg_header_t *, mach_msg_header_t *);
error_t netfs_shutdown (int flags);
extern struct port_class *netfs_protid_class;
extern struct port_class *netfs_control_class;
extern struct port_bucket *netfs_port_bucket;
extern struct node *netfs_root_node;
extern mach_port_t netfs_fsys_identity;
extern auth_t netfs_auth_server_port;
typedef struct protid *protid_t;
typedef struct netfs_control *control_t;
kern_return_t netfs_S_io_write (protid_t io_object,
const_data_t data,
mach_msg_type_number_t dataCnt,
loff_t offset,
vm_size_t *amount);
kern_return_t netfs_S_io_read (protid_t io_object,
data_t *data,
mach_msg_type_number_t *dataCnt,
loff_t offset,
vm_size_t amount);
kern_return_t netfs_S_io_seek (protid_t io_object,
loff_t offset,
int whence,
loff_t *newp);
kern_return_t netfs_S_io_stat (protid_t stat_object,
io_statbuf_t *stat_info);
kern_return_t netfs_S_file_set_size (protid_t trunc_file,
loff_t new_size);
kern_return_t netfs_S_file_get_storage_info (protid_t file,
portarray_t *ports,
mach_msg_type_name_t *portsPoly,
mach_msg_type_number_t *portsCnt,
intarray_t *ints,
mach_msg_type_number_t *intsCnt,
off_array_t *offsets,
mach_msg_type_number_t *offsetsCnt,
data_t *data,
mach_msg_type_number_t *dataCnt);
kern_return_t netfs_S_file_statfs (protid_t file,
fsys_statfsbuf_t *info);
#endif