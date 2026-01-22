#include <hurd/netfs.h>
#include <hurd/fshelp.h>
#include <sys/mman.h>
#include <mach/vm_param.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/statvfs.h>
#include <unistd.h>
#include "procfs.h"
#define PROCFS_SERVER_NAME "procfs"
#define PROCFS_SERVER_VERSION "0.1.0"
#define PROCFS_MAXSYMLINKS 16
char *netfs_server_name = PROCFS_SERVER_NAME;
char *netfs_server_version = PROCFS_SERVER_VERSION;
int netfs_maxsymlinks = PROCFS_MAXSYMLINKS;
error_t netfs_validate_stat (struct node *np, struct iouser *cred)
{
char *contents;
ssize_t contents_len;
error_t err;
if (! S_ISLNK (np->nn_stat.st_mode))
return 0;
err = procfs_get_contents (np, &contents, &contents_len);
if (err)
return err;
np->nn_stat.st_size = contents_len;
return 0;
}
error_t netfs_attempt_read (struct iouser *cred, struct node *np,
loff_t offset, size_t *len, void *data)
{
char *contents;
ssize_t contents_len;
error_t err;
if (offset == 0)
procfs_refresh (np);
err = procfs_get_contents (np, &contents, &contents_len);
if (err)
return err;
contents += offset;
contents_len -= offset;
if (contents_len < 0)
contents_len = 0;
if (*len > contents_len)
*len = contents_len;
memcpy (data, contents, *len);
return 0;
}
error_t netfs_attempt_readlink (struct iouser *user, struct node *np,
char *buf)
{
char *contents;
ssize_t contents_len;
error_t err;
err = procfs_get_contents (np, &contents, &contents_len);
if (err)
return err;
assert_backtrace (contents_len == np->nn_stat.st_size);
memcpy (buf, contents, contents_len);
return 0;
}
static int putentries (char *contents, size_t contents_len, int nentries,
char *data, mach_msg_type_number_t *datacnt)
{
int align = __alignof (struct dirent);
int i;
*datacnt = 0;
for (i = 0; contents_len && (nentries < 0 || i < nentries); i++)
{
int namlen = strlen (contents);
int reclen = sizeof (struct dirent) + namlen;
int extra = reclen & (align - 1);
int pad = extra ? align - extra : 0;
reclen += pad;
if (data)
{
struct dirent *d = (struct dirent *) (data + *datacnt);
d->d_fileno = 42;
d->d_namlen = namlen;
d->d_reclen = reclen;
d->d_type = DT_UNKNOWN;
memcpy (d->d_name, contents, namlen + 1);
if (pad)
memset(d->d_name + namlen + 1, 0, pad);
}
*datacnt += reclen;
contents += namlen + 1;
contents_len -= namlen + 1;
}
return i;
}
error_t netfs_get_dirents (struct iouser *cred, struct node *dir,
int entry, int nentries, char **data,
mach_msg_type_number_t *datacnt,
vm_size_t bufsize, int *amt)
{
char *contents;
ssize_t contents_len;
error_t err;
if (entry == 0)
procfs_refresh (dir);
err = procfs_get_contents (dir, &contents, &contents_len);
if (err)
return err;
assert_backtrace (contents_len == 0 || contents[contents_len - 1] == '\0');
while (contents_len && entry--)
{
int ofs = strlen (contents) + 1;
contents += ofs;
contents_len -= ofs;
}
putentries (contents, contents_len, nentries, NULL, datacnt);
if (bufsize < *datacnt)
{
char *n = mmap (0, *datacnt, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, 0, 0);
if (n == MAP_FAILED)
return ENOMEM;
*data = n;
}
*amt = putentries (contents, contents_len, nentries, *data, datacnt);
return 0;
}
error_t netfs_attempt_lookup (struct iouser *user, struct node *dir,
const char *name, struct node **np)
{
error_t err;
err = procfs_lookup (dir, name, np);
pthread_mutex_unlock (&dir->lock);
if (! err)
pthread_mutex_lock (&(*np)->lock);
return err;
}
void netfs_node_norefs (struct node *np)
{
procfs_cleanup (np);
free (np);
}
error_t netfs_get_translator (struct node *np, char **argz,
mach_msg_type_number_t *argz_len)
{
return procfs_get_translator (np, argz, argz_len);
}
error_t netfs_check_open_permissions (struct iouser *user, struct node *np,
int flags, int newnode)
{
error_t err = 0;
if (!err && (flags & O_READ))
err = fshelp_access (&np->nn_stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&np->nn_stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&np->nn_stat, S_IEXEC, user);
return err;
}
error_t netfs_report_access (struct iouser *cred, struct node *np,
int *types)
{
*types = 0;
if (fshelp_access (&np->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access (&np->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access (&np->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
return 0;
}
error_t netfs_attempt_chown (struct iouser *cred, struct node *np,
uid_t uid, uid_t gid)
{
return EROFS;
}
error_t netfs_attempt_chauthor (struct iouser *cred, struct node *np,
uid_t author)
{
return EROFS;
}
error_t netfs_attempt_chmod (struct iouser *cred, struct node *np,
mode_t mode)
{
return EROFS;
}
error_t netfs_attempt_mksymlink (struct iouser *cred, struct node *np,
const char *name)
{
return EROFS;
}
error_t netfs_attempt_mkdev (struct iouser *cred, struct node *np,
mode_t type, dev_t indexes)
{
return EROFS;
}
error_t netfs_attempt_chflags (struct iouser *cred, struct node *np,
int flags)
{
return EROFS;
}
error_t netfs_attempt_utimes (struct iouser *cred, struct node *np,
struct timespec *atime, struct timespec *mtime)
{
return EROFS;
}
error_t netfs_attempt_set_size (struct iouser *cred, struct node *np,
loff_t size)
{
return EROFS;
}
error_t netfs_attempt_statfs (struct iouser *cred, struct node *np,
fsys_statfsbuf_t *st)
{
memset (st, 0, sizeof *st);
st->f_type = FSTYPE_PROC;
st->f_fsid = getpid ();
return 0;
}
error_t netfs_attempt_sync (struct iouser *cred, struct node *np,
int wait)
{
return 0;
}
error_t netfs_attempt_syncfs (struct iouser *cred, int wait)
{
return 0;
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
error_t netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
return EROFS;
}
error_t netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **np)
{
return EROFS;
}
error_t netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **np)
{
return EROFS;
}
error_t netfs_attempt_write (struct iouser *cred, struct node *np,
loff_t offset, size_t *len, const void *data)
{
return EROFS;
}