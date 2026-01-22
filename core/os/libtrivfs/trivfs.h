#ifndef __TRIVFS_H__
#define __TRIVFS_H__
#include <errno.h>
#include <pthread.h>
#include <sys/types.h>
#include <mach/mach.h>
#include <hurd/ports.h>
#include <hurd/iohelp.h>
#include <hurd/fshelp.h>
#include <refcount.h>
struct trivfs_protid
{
struct port_info pi;
struct iouser *user;
int isroot;
mach_port_t realnode;
void *hook;
struct trivfs_peropen *po;
};
struct trivfs_peropen
{
void *hook;
int openmodes;
refcount_t refcnt;
struct trivfs_control *cntl;
struct rlock_peropen lock_status;
struct trivfs_node *tp;
};
struct trivfs_node
{
pthread_mutex_t lock;
int references;
struct transbox transbox;
struct rlock_box credlock;
};
struct trivfs_control
{
struct port_info pi;
struct port_class *protid_class;
struct port_bucket *protid_bucket;
mach_port_t filesys_id;
mach_port_t file_id;
mach_port_t underlying;
void *hook;
};
extern char *trivfs_server_name __attribute__((weak));
extern char *trivfs_server_version __attribute__((weak));
extern int trivfs_fstype;
extern int trivfs_fsid;
extern int trivfs_support_read;
extern int trivfs_support_write;
extern int trivfs_support_exec;
extern int trivfs_allow_open;
void trivfs_modify_stat (struct trivfs_protid *cred, io_statbuf_t *);
extern error_t (*trivfs_check_access_hook) (struct trivfs_control *cntl,
struct iouser *user,
mach_port_t realnode,
int *allowed);
extern error_t (*trivfs_check_open_hook) (struct trivfs_control *cntl,
struct iouser *user, int flags);
extern error_t (*trivfs_open_hook) (struct trivfs_control *fsys,
struct iouser *user,
mach_port_t dotdot,
int flags,
mach_port_t realnode,
struct trivfs_protid **cred);
extern error_t (*trivfs_protid_create_hook) (struct trivfs_protid *);
extern error_t (*trivfs_peropen_create_hook) (struct trivfs_peropen *);
extern void (*trivfs_protid_destroy_hook) (struct trivfs_protid *);
extern void (*trivfs_peropen_destroy_hook) (struct trivfs_peropen *);
typedef error_t (*trivfs_getroot_hook_fun) (struct trivfs_control *cntl,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
mach_port_t dotdot,
const uid_t *uids, mach_msg_type_number_t nuids, const uid_t *gids, mach_msg_type_number_t ngids,
int flags,
retry_type *do_retry, char *retry_name,
mach_port_t *node, mach_msg_type_name_t *node_type);
extern trivfs_getroot_hook_fun trivfs_getroot_hook;
error_t trivfs_startup (mach_port_t bootstrap, int flags,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control);
error_t
trivfs_startup_debug(const char *file_name,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control);
error_t
trivfs_create_control (mach_port_t underlying,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control);
void trivfs_clean_protid (void *);
void trivfs_clean_cntl (void *);
int trivfs_demuxer (mach_msg_header_t *, mach_msg_header_t *);
struct trivfs_node *trivfs_make_node (struct trivfs_peropen *po);
struct trivfs_peropen *trivfs_make_peropen (struct trivfs_protid *cred);
error_t trivfs_open (struct trivfs_control *fsys,
struct iouser *user,
unsigned flags,
mach_port_t realnode,
struct trivfs_protid **cred);
error_t trivfs_protid_dup (struct trivfs_protid *cred,
struct trivfs_protid **dup);
error_t trivfs_goaway (struct trivfs_control *cntl, int flags);
error_t trivfs_set_atime (struct trivfs_control *cntl);
error_t trivfs_set_mtime (struct trivfs_control *cntl);
extern struct argp *trivfs_runtime_argp;
error_t trivfs_set_options (struct trivfs_control *fsys,
const char *argz, size_t argz_len);
error_t trivfs_append_args (struct trivfs_control *fsys,
char **argz, size_t *argz_len);
error_t trivfs_get_source (char *source, size_t source_len);
error_t trivfs_add_control_port_class (struct port_class **class);
void trivfs_remove_control_port_class (struct port_class *class);
error_t trivfs_add_protid_port_class (struct port_class **class);
void trivfs_remove_protid_port_class (struct port_class *class);
error_t trivfs_add_port_bucket (struct port_bucket **bucket);
void trivfs_remove_port_bucket (struct port_bucket *bucket);
typedef struct trivfs_protid *trivfs_protid_t;
typedef struct trivfs_control *trivfs_control_t;
kern_return_t trivfs_S_io_write (trivfs_protid_t io_object,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
const_data_t data,
mach_msg_type_number_t dataCnt,
loff_t offset,
vm_size_t *amount);
kern_return_t trivfs_S_io_read (trivfs_protid_t io_object,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
data_t *data,
mach_msg_type_number_t *dataCnt,
loff_t offset,
vm_size_t amount);
kern_return_t trivfs_S_io_seek (trivfs_protid_t io_object,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
loff_t offset,
int whence,
loff_t *newp);
kern_return_t trivfs_S_io_stat (trivfs_protid_t stat_object,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
io_statbuf_t *stat_info);
kern_return_t trivfs_S_file_set_size (trivfs_protid_t trunc_file,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
loff_t new_size);
kern_return_t trivfs_S_file_get_storage_info (trivfs_protid_t file,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
portarray_t *ports,
mach_msg_type_name_t *portsPoly,
mach_msg_type_number_t *portsCnt,
intarray_t *ints,
mach_msg_type_number_t *intsCnt,
off_array_t *offsets,
mach_msg_type_number_t *offsetsCnt,
data_t *data,
mach_msg_type_number_t *dataCnt);
kern_return_t trivfs_S_file_statfs (trivfs_protid_t file,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
fsys_statfsbuf_t *info);
#endif