#include <stddef.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <hurd/netfs.h>
#include "ftpfs.h"
#include "ccache.h"
error_t
netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **node)
{
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser *user, struct node *node,
int flags, int newnode)
{
error_t err = ftpfs_refresh_node (node);
if (!err && (flags & O_READ))
err = fshelp_access (&node->nn_stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&node->nn_stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&node->nn_stat, S_IEXEC, user);
return err;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *node,
struct timespec *atime, struct timespec *mtime)
{
error_t err = ftpfs_refresh_node (node);
int flags = TOUCH_CTIME;
if (! err)
err = fshelp_isowner (&node->nn_stat, cred);
if (! err)
{
if (atime)
node->nn_stat.st_atim = *atime;
if (mtime)
node->nn_stat.st_mtim = *mtime;
fshelp_touch (&node->nn_stat, flags, ftpfs_maptime);
}
return err;
}
error_t
netfs_report_access (struct iouser *cred, struct node *node, int *types)
{
error_t err = ftpfs_refresh_node (node);
if (! err)
{
*types = 0;
if (fshelp_access (&node->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access (&node->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access (&node->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
}
return err;
}
error_t
netfs_validate_stat (struct node *node, struct iouser *cred)
{
return ftpfs_refresh_node (node);
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *node, int wait)
{
return 0;
}
#define DIRENTS_CHUNK_SIZE	(8*1024)
#define DIRENT_ALIGN 4
#define DIRENT_NAME_OFFS offsetof (struct dirent, d_name)
#define DIRENT_LEN(name_len)						      \
((DIRENT_NAME_OFFS + (name_len) + 1 + (DIRENT_ALIGN - 1))		      \
& ~(DIRENT_ALIGN - 1))
static error_t
get_dirents (struct ftpfs_dir *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
struct ftpfs_dir_entry *e;
error_t err = 0;
if (! dir)
return ENOTDIR;
e = dir->ordered;
while (first_entry-- > 0)
if (! e)
{
max_entries = 0;
break;
}
else
e = e->ordered_next;
if (max_entries != 0)
{
size_t size =
(max_data_len == 0 || max_data_len > DIRENTS_CHUNK_SIZE
? DIRENTS_CHUNK_SIZE
: max_data_len);
*data = mmap (0, size, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
err = ((void *) *data == (void *) -1) ? errno : 0;
if (! err)
{
char *p = *data;
int count = 0;
while ((max_entries == -1 || count < max_entries) && e)
{
struct dirent hdr;
size_t name_len = strlen (e->name);
size_t sz = DIRENT_LEN (name_len);
int entry_type =
e->stat_timestamp ? IFTODT (e->stat.st_mode) : DT_UNKNOWN;
if ((p - *data) + sz > size)
{
if (max_data_len > 0)
break;
else
{
vm_address_t extension = (vm_address_t)(*data + size);
err = vm_allocate (mach_task_self (), &extension,
DIRENTS_CHUNK_SIZE, 0);
if (err)
break;
size += DIRENTS_CHUNK_SIZE;
}
}
hdr.d_namlen = name_len;
hdr.d_fileno = e->stat.st_ino;
hdr.d_reclen = sz;
hdr.d_type = entry_type;
memcpy (p, &hdr, DIRENT_NAME_OFFS);
strcpy (p + DIRENT_NAME_OFFS, e->name);
p += sz;
count++;
e = e->ordered_next;
}
if (err)
munmap (*data, size);
else
{
vm_address_t alloc_end = (vm_address_t)(*data + size);
vm_address_t real_end = round_page (p);
if (alloc_end > real_end)
munmap ((caddr_t) real_end, alloc_end - real_end);
*data_len = p - *data;
*data_entries = count;
}
}
}
else
{
*data_len = 0;
*data_entries = 0;
}
return err;
}
error_t
netfs_get_dirents (struct iouser *cred, struct node *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err = ftpfs_refresh_node (dir);
if (! err)
{
if (dir->nn->dir)
{
err = ftpfs_dir_refresh (dir->nn->dir);
if (! err)
err = get_dirents (dir->nn->dir, first_entry, max_entries,
data, data_len, max_entries, data_entries);
}
else
err = ENOTDIR;
}
return err;
}
error_t netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **node)
{
error_t err = ftpfs_refresh_node (dir);
if (! err)
err = ftpfs_dir_lookup (dir->nn->dir, name, node);
return err;
}
error_t netfs_attempt_unlink (struct iouser *user, struct node *dir,
const char *name)
{
return EROFS;
}
error_t netfs_attempt_rename (struct iouser *user, struct node *fromdir,
const char *fromname, struct node *todir,
const char *toname, int excl)
{
return EROFS;
}
error_t netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode)
{
return EROFS;
}
error_t netfs_attempt_rmdir (struct iouser *user,
struct node *dir, const char *name)
{
return EROFS;
}
error_t netfs_attempt_chown (struct iouser *cred, struct node *node,
uid_t uid, uid_t gid)
{
return EROFS;
}
error_t netfs_attempt_chauthor (struct iouser *cred, struct node *node,
uid_t author)
{
return EROFS;
}
error_t netfs_attempt_chmod (struct iouser *cred, struct node *node,
mode_t mode)
{
return EROFS;
}
error_t netfs_attempt_mksymlink (struct iouser *cred, struct node *node,
const char *name)
{
return EROFS;
}
error_t netfs_attempt_mkdev (struct iouser *cred, struct node *node,
mode_t type, dev_t indexes)
{
return EROFS;
}
error_t netfs_set_translator (struct iouser *cred, struct node *node,
const char *argz, mach_msg_type_number_t argzlen)
{
return EROFS;
}
error_t netfs_attempt_chflags (struct iouser *cred, struct node *node,
int flags)
{
return EROFS;
}
error_t netfs_attempt_set_size (struct iouser *cred, struct node *node,
off_t size)
{
return EROFS;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *node,
struct statfs *st)
{
memset (st, 0, sizeof *st);
st->f_type = FSTYPE_FTP;
st->f_fsid = getpid ();
return 0;
}
error_t netfs_attempt_syncfs (struct iouser *cred, int wait)
{
return 0;
}
error_t netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
return EROFS;
}
error_t netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **node)
{
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EROFS;
}
error_t netfs_attempt_readlink (struct iouser *user, struct node *node, char *buf)
{
error_t err = ftpfs_refresh_node (node);
if (! err)
{
struct ftpfs_dir_entry *e = node->nn->dir_entry;
if (e)
bcopy (e->symlink_target, buf, node->nn_stat.st_size);
else
err = EINVAL;
}
return err;
}
error_t netfs_attempt_read (struct iouser *cred, struct node *node,
off_t offset, size_t *len, void *data)
{
error_t err = 0;
if (! node->nn->contents)
err = ccache_create (node, &node->nn->contents);
if (! err)
{
if (*len > node->nn_stat.st_size - offset)
*len = node->nn_stat.st_size - offset;
if (*len > 0)
err = ccache_read (node->nn->contents, offset, *len, data);
}
return err;
}
error_t netfs_attempt_write (struct iouser *cred, struct node *node,
off_t offset, size_t *len, const void *data)
{
return EROFS;
}