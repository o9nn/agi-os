#include "priv.h"
#include "fs_S.h"
#include <hurd/fsys.h>
kern_return_t
diskfs_S_dir_unlink (struct protid *dircred,
const_string_t name)
{
struct node *dnp;
struct node *np;
struct dirstat *ds = alloca (diskfs_dirstat_size);
error_t err;
mach_port_t control = MACH_PORT_NULL;
if (!dircred)
return EOPNOTSUPP;
dnp = dircred->po->np;
if (diskfs_check_readonly ())
return EROFS;
pthread_mutex_lock (&dnp->lock);
err = diskfs_lookup (dnp, name, REMOVE, &np, ds, dircred);
if (err == EAGAIN)
err = EPERM;
if (err)
{
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return err;
}
if (S_ISDIR(np->dn_stat.st_mode))
{
if (np == dnp)
diskfs_nrele (np);
else
diskfs_nput (np);
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return EPERM;
}
err = diskfs_dirremove (dnp, np, name, ds);
if (diskfs_synchronous)
diskfs_node_update (dnp, 1);
if (err)
{
diskfs_nput (np);
pthread_mutex_unlock (&dnp->lock);
return err;
}
np->dn_stat.st_nlink--;
np->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (np, 1);
if (np->dn_stat.st_nlink == 0)
fshelp_fetch_control (&np->transbox, &control);
if (np == dnp)
diskfs_nrele (np);
else
diskfs_nput (np);
pthread_mutex_unlock (&dnp->lock);
if (control)
{
fsys_goaway (control, FSYS_GOAWAY_UNLINK);
mach_port_deallocate (mach_task_self (), control);
}
return err;
}