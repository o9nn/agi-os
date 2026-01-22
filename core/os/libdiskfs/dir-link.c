#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_dir_link (struct protid *dircred,
struct protid *filecred,
const_string_t name,
int excl)
{
struct node *np;
struct node *tnp;
struct node *dnp;
struct dirstat *ds = alloca (diskfs_dirstat_size);
error_t err;
if (!dircred)
return EOPNOTSUPP;
if (diskfs_check_readonly ())
return EROFS;
if (!filecred)
return EXDEV;
np = filecred->po->np;
pthread_mutex_lock (&np->lock);
if (S_ISDIR (np->dn_stat.st_mode))
{
pthread_mutex_unlock (&np->lock);
return EPERM;
}
pthread_mutex_unlock (&np->lock);
dnp = dircred->po->np;
pthread_mutex_lock (&dnp->lock);
err = diskfs_lookup (dnp, name, RENAME, &tnp, ds, dircred);
if (!err && excl)
{
err = EEXIST;
diskfs_nput (tnp);
}
if (err && err != ENOENT)
{
if (err == EAGAIN)
err = EINVAL;
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return err;
}
if (np == tnp)
{
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
pthread_mutex_unlock (&tnp->lock);
mach_port_deallocate (mach_task_self (), filecred->pi.port_right);
return 0;
}
if (tnp && S_ISDIR (tnp->dn_stat.st_mode))
{
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
pthread_mutex_unlock (&tnp->lock);
return EISDIR;
}
pthread_mutex_lock (&np->lock);
if (np->dn_stat.st_nlink == diskfs_link_max - 1)
{
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&np->lock);
pthread_mutex_unlock (&dnp->lock);
return EMLINK;
}
np->dn_stat.st_nlink++;
np->dn_set_ctime = 1;
diskfs_node_update (np, diskfs_synchronous);
if (tnp)
{
assert_backtrace (!excl);
err = diskfs_dirrewrite (dnp, tnp, np, name, ds);
if (!err)
{
tnp->dn_stat.st_nlink--;
tnp->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (tnp, 1);
}
diskfs_nput (tnp);
}
else
err = diskfs_direnter (dnp, name, np, ds, dircred);
if (diskfs_synchronous)
diskfs_node_update (dnp, 1);
pthread_mutex_unlock (&dnp->lock);
pthread_mutex_unlock (&np->lock);
if (!err)
mach_port_deallocate (mach_task_self (), filecred->pi.port_right);
return err;
}