#include "9pfs.h"
#include <hurd/fshelp.h>
#include <fcntl.h>
#include <string.h>
error_t
netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **node)
{
(void)user; (void)dir; (void)name; (void)mode;
*node = NULL;
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser *user, struct node *node,
int flags, int newnode)
{
error_t err;
(void)newnode;
err = p9fs_refresh_node(node);
if (err)
return err;
if (flags & O_READ)
err = fshelp_access(&node->nn_stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access(&node->nn_stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access(&node->nn_stat, S_IEXEC, user);
return err;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *node,
struct timespec *atime, struct timespec *mtime)
{
(void)cred; (void)node; (void)atime; (void)mtime;
return EOPNOTSUPP;
}
error_t
netfs_report_access (struct iouser *cred, struct node *node, int *types)
{
error_t err = p9fs_refresh_node(node);
if (err)
return err;
*types = 0;
if (fshelp_access(&node->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access(&node->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access(&node->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
return 0;
}
error_t
netfs_attempt_read (struct iouser *cred, struct node *node,
loff_t offset, size_t len, void *data, size_t *bytes_read)
{
(void)cred;
return p9fs_file_read(node, offset, len, data, bytes_read);
}
error_t
netfs_attempt_write (struct iouser *cred, struct node *node,
loff_t offset, size_t len, const void *data,
size_t *bytes_written)
{
(void)cred;
return p9fs_file_write(node, offset, len, data, bytes_written);
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *node, int wait)
{
(void)cred; (void)node; (void)wait;
return 0;
}
error_t
netfs_attempt_unlink (struct iouser *user, struct node *dir, const char *name)
{
(void)user; (void)dir; (void)name;
return EOPNOTSUPP;
}
error_t
netfs_attempt_rename (struct iouser *user, struct node *from_dir,
const char *from_name, struct node *to_dir,
const char *to_name, int to_excl)
{
(void)user; (void)from_dir; (void)from_name; (void)to_dir; (void)to_name; (void)to_excl;
return EOPNOTSUPP;
}
error_t
netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **node)
{
error_t err;
(void)user;
err = p9fs_dir_lookup(dir, name, node);
pthread_mutex_unlock(&dir->lock);
if (!err && *node)
pthread_mutex_lock(&(*node)->lock);
return err;
}
error_t
netfs_attempt_remove_file (struct iouser *user, struct node *dir,
struct node *node)
{
(void)user; (void)dir; (void)node;
return EOPNOTSUPP;
}
error_t
netfs_attempt_chmod (struct iouser *cred, struct node *node, mode_t mode)
{
(void)cred; (void)node; (void)mode;
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdev (struct iouser *cred, struct node *node,
mode_t type, dev_t indexes)
{
(void)cred; (void)node; (void)type; (void)indexes;
return EOPNOTSUPP;
}
error_t
netfs_attempt_chown (struct iouser *cred, struct node *node,
uid_t uid, uid_t gid)
{
(void)cred; (void)node; (void)uid; (void)gid;
return EOPNOTSUPP;
}
error_t
netfs_attempt_chauthor (struct iouser *cred, struct node *node,
uid_t author)
{
(void)cred; (void)node; (void)author;
return EOPNOTSUPP;
}
error_t
netfs_attempt_chflags (struct iouser *cred, struct node *node, int flags)
{
(void)cred; (void)node; (void)flags;
return EOPNOTSUPP;
}
error_t
netfs_attempt_set_size (struct iouser *cred, struct node *node, loff_t size)
{
(void)cred; (void)node; (void)size;
return EOPNOTSUPP;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *node, fsys_statfsbuf_t *st)
{
(void)cred; (void)node;
memset(st, 0, sizeof(*st));
st->f_type = FSTYPE_MISC;
st->f_bsize = 4096;
st->f_blocks = 1000000;
st->f_bfree = 500000;
st->f_bavail = 500000;
st->f_files = 100000;
st->f_ffree = 50000;
st->f_fsid = 0x9999;
st->f_namelen = 255;
return 0;
}
error_t
netfs_validate_stat (struct node *node, struct iouser *cred)
{
(void)cred;
return p9fs_refresh_node(node);
}
error_t
netfs_attempt_syncfs (struct iouser *cred, int wait)
{
(void)cred; (void)wait;
return 0;
}
error_t
netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
(void)user; (void)dir; (void)file; (void)name; (void)excl;
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode)
{
(void)user; (void)dir; (void)name; (void)mode;
return EOPNOTSUPP;
}
error_t
netfs_attempt_rmdir (struct iouser *user, struct node *dir, const char *name)
{
(void)user; (void)dir; (void)name;
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **node)
{
(void)user; (void)dir; (void)mode;
*node = NULL;
return EOPNOTSUPP;
}
error_t
netfs_attempt_readlink (struct iouser *user, struct node *node, char *buf)
{
(void)user; (void)node; (void)buf;
return EOPNOTSUPP;
}
error_t
netfs_get_dirents (struct iouser *cred, struct node *dir,
int first_entry, int num_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_entries, int *data_entries)
{
(void)cred; (void)dir; (void)first_entry; (void)num_entries;
(void)max_entries;
*data = NULL;
*data_len = 0;
*data_entries = 0;
return 0;
}