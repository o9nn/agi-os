#include "priv.h"
#include "fs_S.h"
#include <sys/sysmacros.h>
#include <hurd/paths.h>
#include <hurd/fsys.h>
kern_return_t
diskfs_S_file_set_translator (struct protid *cred,
int passive_flags,
int active_flags,
int killtrans_flags,
const_data_t passive,
mach_msg_type_number_t passivelen,
fsys_t active)
{
struct node *np;
error_t err;
mach_port_t control = MACH_PORT_NULL;
if (!cred)
return EOPNOTSUPP;
if (!(passive_flags & FS_TRANS_SET) && !(active_flags & FS_TRANS_SET))
return 0;
if ((passive_flags & FS_TRANS_SET) && diskfs_check_readonly ())
return EROFS;
if (passivelen && passive[passivelen - 1])
return EINVAL;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
err = fshelp_isowner (&np->dn_stat, cred->user);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
if ((active_flags & FS_TRANS_SET)
&& ! (active_flags & FS_TRANS_ORPHAN))
{
err = fshelp_fetch_control (&np->transbox, &control);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
if ((control != MACH_PORT_NULL) && ((active_flags & FS_TRANS_EXCL) == 0))
{
pthread_mutex_unlock (&np->lock);
err = fsys_goaway (control, killtrans_flags);
mach_port_deallocate (mach_task_self (), control);
if (err && (err != MIG_SERVER_DIED)
&& (err != MACH_SEND_INVALID_DEST))
return err;
err = 0;
pthread_mutex_lock (&np->lock);
}
else if (control != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), control);
}
if ((passive_flags & FS_TRANS_SET)
&& (passive_flags & FS_TRANS_EXCL)
&& (np->dn_stat.st_mode & S_IPTRANS))
{
pthread_mutex_unlock (&np->lock);
return EBUSY;
}
if (active_flags & FS_TRANS_SET)
{
err = fshelp_set_active (&np->transbox, active,
active_flags & FS_TRANS_EXCL);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
}
if (passive_flags & FS_TRANS_SET)
{
if (!(passive_flags & FS_TRANS_FORCE))
{
mode_t newmode = 0;
if (diskfs_shortcut_symlink && !strcmp (passive, _HURD_SYMLINK))
newmode = S_IFLNK;
else if (diskfs_shortcut_chrdev && !(strcmp (passive, _HURD_CHRDEV)))
newmode = S_IFCHR;
else if (diskfs_shortcut_blkdev && !strcmp (passive, _HURD_BLKDEV))
newmode = S_IFBLK;
else if (diskfs_shortcut_fifo && !strcmp (passive, _HURD_FIFO))
newmode = S_IFIFO;
else if (diskfs_shortcut_ifsock && !strcmp (passive, _HURD_IFSOCK))
newmode = S_IFSOCK;
if (newmode)
{
if (S_ISDIR (np->dn_stat.st_mode))
{
pthread_mutex_unlock (&np->lock);
return EISDIR;
}
if (newmode == S_IFBLK || newmode == S_IFCHR)
{
int major, minor;
const char *arg;
arg = passive + strlen (passive) + 1;
assert_backtrace (arg <= passive + passivelen);
if (arg == passive + passivelen)
{
pthread_mutex_unlock (&np->lock);
return EINVAL;
}
major = strtol (arg, 0, 0);
arg = arg + strlen (arg) + 1;
assert_backtrace (arg < passive + passivelen);
if (arg == passive + passivelen)
{
pthread_mutex_unlock (&np->lock);
return EINVAL;
}
minor = strtol (arg, 0, 0);
err = diskfs_validate_rdev_change (np,
gnu_dev_makedev (major, minor));
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
np->dn_stat.st_rdev = gnu_dev_makedev (major, minor);
}
err = diskfs_truncate (np, 0);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
err = diskfs_set_translator (np, NULL, 0, cred);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
if (newmode == S_IFLNK)
{
const char *arg = passive + strlen (passive) + 1;
assert_backtrace (arg <= passive + passivelen);
if (arg == passive + passivelen)
{
pthread_mutex_unlock (&np->lock);
return EINVAL;
}
if (diskfs_create_symlink_hook)
err = (*diskfs_create_symlink_hook)(np, arg);
if (!diskfs_create_symlink_hook || err == EINVAL)
err = diskfs_node_rdwr (np, (char*) arg, 0, strlen (arg),
1, cred, 0);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
}
newmode = (np->dn_stat.st_mode & ~S_IFMT) | newmode;
err = diskfs_validate_mode_change (np, newmode);
if (!err)
{
np->dn_stat.st_mode = newmode;
diskfs_node_update (np, diskfs_synchronous);
}
pthread_mutex_unlock (&np->lock);
return err;
}
}
err = diskfs_set_translator (np, passive, passivelen, cred);
}
pthread_mutex_unlock (&np->lock);
if (! err && cred->po->path && active_flags & FS_TRANS_SET)
err = fshelp_set_active_translator (cred->pi.bucket->notify_port,
cred->po->path, &np->transbox);
return err;
}