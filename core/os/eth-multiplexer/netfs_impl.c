#include <fcntl.h>
#include <dirent.h>
#include <stddef.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <ctype.h>
#include <hurd/netfs.h>
#include "netfs_impl.h"
#include "vdev.h"
#include "util.h"
#define DIRENTS_CHUNK_SIZE      (8*1024)
#define DIRENT_ALIGN 4
#define DIRENT_NAME_OFFS offsetof (struct dirent, d_name)
#define DIRENT_LEN(name_len)                                                  \
((DIRENT_NAME_OFFS + (name_len) + 1 + (DIRENT_ALIGN - 1))                   \
& ~(DIRENT_ALIGN - 1))
extern struct stat underlying_node_stat;
int
is_num (char *str)
{
for (; *str; str++)
{
if (!isdigit (*str))
return 0;
}
return 1;
}
error_t
new_node (struct lnode *ln, struct node **np)
{
error_t err = 0;
struct netnode *nn = calloc (1, sizeof *nn);
struct node *node;
if (nn == 0)
return ENOMEM;
node = netfs_make_node (nn);
if (node == 0)
{
free (nn);
*np = NULL;
return ENOMEM;
}
if (ln)
ln->n = node;
nn->ln = ln;
*np = node;
return err;
}
struct node *
lookup (const char *name)
{
struct lnode *ln = (struct lnode *) lookup_dev_by_name (name);
char *copied_name = malloc (strlen (name) + 1);
strcpy (copied_name, name);
if (ln)
{
new_node (ln, &ln->n);
ln->n->nn->name = copied_name;
return ln->n;
}
else
{
struct node *n;
new_node (ln, &n);
n->nn->name = copied_name;
return n;
}
}
error_t
netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **node)
{
debug("");
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser *user, struct node *node,
int flags, int newnode)
{
error_t err = 0;
if(flags & O_READ)
err = fshelp_access(&node->nn_stat, S_IREAD, user);
if(!err && (flags & O_WRITE))
err = fshelp_access(&node->nn_stat, S_IWRITE, user);
if(!err && (flags & O_EXEC))
err = fshelp_access(&node->nn_stat, S_IEXEC, user);
debug("the mode of node: %o, return result: %d",
(node->nn_stat.st_mode & ~S_IFMT), err);
return err;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *node,
struct timespec *atime, struct timespec *mtime)
{
debug("");
return EOPNOTSUPP;
}
error_t
netfs_report_access (struct iouser *cred, struct node *node, int *types)
{
debug("");
*types = 0;
return 0;
}
error_t
netfs_validate_stat (struct node *node, struct iouser *cred)
{
struct stat st;
if (node->nn->ln)
st = node->nn->ln->st;
else
st = underlying_node_stat;
debug("node: %p", node);
node->nn_translated = S_ISLNK (st.st_mode) ? S_IFLNK : 0;
node->nn_stat = st;
return 0;
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *node, int wait)
{
debug("");
return 0;
}
error_t
netfs_get_dirents (struct iouser *cred, struct node *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t *data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err;
int count = 0;
char *data_p;
size_t size = (max_data_len == 0 || max_data_len > DIRENTS_CHUNK_SIZE
? DIRENTS_CHUNK_SIZE : max_data_len);
debug ("");
int
add_dirent (const char * name, ino_t ino, int type)
{
if((max_entries == -1) || (count < max_entries))
{
struct dirent hdr;
size_t name_len = strlen(name);
size_t sz = DIRENT_LEN(name_len);
if ((data_p - *data) + sz > size)
{
if (max_data_len > 0)
return 1;
else
{
error_t err;
vm_address_t extension = (vm_address_t)(*data + size);
err = vm_allocate (mach_task_self (), &extension,
DIRENTS_CHUNK_SIZE, 0);
if (err)
{
munmap (*data, size);
return 1;
}
size += DIRENTS_CHUNK_SIZE;
}
}
hdr.d_ino = ino;
hdr.d_reclen = sz;
hdr.d_type = type;
hdr.d_namlen = name_len;
memcpy(data_p, &hdr, DIRENT_NAME_OFFS);
strcpy(data_p + DIRENT_NAME_OFFS, name);
data_p += sz;
++count;
}
return 0;
}
int add_each_dev (struct vether_device *dev)
{
struct lnode *ln = (struct lnode *) dev;
add_dirent (ln->vdev.name, ln->st.st_ino, DT_CHR);
return 0;
}
if (dir != netfs_root_node)
return ENOTDIR;
*data = mmap (0, size, PROT_READ | PROT_WRITE, MAP_ANON, 0, 0);
err = ((void *) *data == (void *) -1) ? errno : 0;
if (!err)
{
data_p = *data;
if (first_entry < 2 + get_dev_num ())
{
add_dirent (".", 2, DT_DIR);
add_dirent ("..", 2, DT_DIR);
foreach_dev_do (add_each_dev);
}
vm_address_t alloc_end = (vm_address_t)(*data + size);
vm_address_t real_end = round_page (data_p);
if (alloc_end > real_end)
munmap ((caddr_t) real_end, alloc_end - real_end);
*data_entries = count;
debug ("first_entry is %d, count is %d", first_entry, count);
*data_len = data_p - *data;
}
return err;
}
error_t netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **node)
{
error_t err = 0;
debug ("dir: %p, file name: %s", dir, name);
if (strcmp(name, ".") == 0)
{
netfs_nref(dir);
*node = dir;
return 0;
}
else if (strcmp(name, "..") == 0)
{
err = ENOENT;
*node = NULL;
pthread_mutex_unlock (&dir->lock);
return err;
}
*node = lookup (name);
pthread_mutex_lock (&(*node)->lock);
pthread_mutex_unlock (&dir->lock);
return 0;
}
error_t netfs_attempt_unlink (struct iouser *user, struct node *dir,
const char *name)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_rename (struct iouser *user, struct node *fromdir,
const char *fromname, struct node *todir,
const char *toname, int excl)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_rmdir (struct iouser *user,
struct node *dir, const char *name)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_chown (struct iouser *cred, struct node *node,
uid_t uid, uid_t gid)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_chauthor (struct iouser *cred, struct node *node,
uid_t author)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_chmod (struct iouser *cred, struct node *node,
mode_t mode)
{
error_t err = 0;
debug("");
if (node->nn->ln == NULL)
return EOPNOTSUPP;
mode &= ~S_ITRANS;
err = fshelp_isowner (&node->nn->ln->st, cred);
if (err)
return err;
mode |= node->nn->ln->st.st_mode & S_IFMT;
node->nn->ln->st.st_mode = mode;
fshelp_touch (&node->nn_stat, TOUCH_CTIME, multiplexer_maptime);
return err;
}
error_t netfs_attempt_mksymlink (struct iouser *cred, struct node *node,
const char *name)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_mkdev (struct iouser *cred, struct node *node,
mode_t type, dev_t indexes)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_set_translator (struct iouser *cred, struct node *node,
const char *argz, mach_msg_type_number_t argzlen)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_chflags (struct iouser *cred, struct node *node,
int flags)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_set_size (struct iouser *cred, struct node *node,
off_t size)
{
debug("");
return EOPNOTSUPP;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *node,
struct statfs *st)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_syncfs (struct iouser *cred, int wait)
{
debug("");
return 0;
}
error_t netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **node)
{
debug("");
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t netfs_attempt_readlink (struct iouser *user, struct node *node, char *buf)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_read (struct iouser *cred, struct node *node,
off_t offset, size_t *len, void *data)
{
debug("");
return EOPNOTSUPP;
}
error_t netfs_attempt_write (struct iouser *cred, struct node *node,
off_t offset, size_t *len, const void *data)
{
debug("");
return EOPNOTSUPP;
}
void
netfs_node_norefs (struct node *node)
{
debug("node: %p", node);
if (node->nn->ln)
node->nn->ln->n = NULL;
free (node->nn->name);
free (node->nn);
free (node);
}