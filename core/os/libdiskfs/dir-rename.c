#include "priv.h"
#include "fs_S.h"
#include <string.h>
static pthread_mutex_t renamedirlock = PTHREAD_MUTEX_INITIALIZER;
kern_return_t
diskfs_S_dir_rename (struct protid *fromcred,
const_string_t fromname,
struct protid *tocred,
const_string_t toname,
int excl)
{
struct node *fdp, *tdp, *fnp, *tnp, *tmpnp;
error_t err;
struct dirstat *ds = alloca (diskfs_dirstat_size);
if (!fromcred)
return EOPNOTSUPP;
if (! tocred)
return EXDEV;
if (!strcmp (fromname, ".") || !strcmp (fromname, "..")
|| !strcmp (toname,   ".") || !strcmp (toname,   ".."))
return EINVAL;
if (tocred->po->shadow_root != fromcred->po->shadow_root)
return EXDEV;
if (diskfs_check_readonly ())
return EROFS;
fdp = fromcred->po->np;
tdp = tocred->po->np;
try_again:
pthread_mutex_lock (&fdp->lock);
err = diskfs_lookup (fdp, fromname, LOOKUP, &fnp, 0, fromcred);
pthread_mutex_unlock (&fdp->lock);
if (err == EAGAIN)
err = EINVAL;
if (err)
return err;
if (S_ISDIR (fnp->dn_stat.st_mode))
{
pthread_mutex_unlock (&fnp->lock);
if (pthread_mutex_trylock (&renamedirlock))
{
diskfs_nrele (fnp);
goto try_again;
}
err = diskfs_rename_dir (fdp, fnp, fromname, tdp, toname, fromcred,
tocred, excl);
if (diskfs_synchronous)
{
pthread_mutex_lock (&fdp->lock);
diskfs_file_update (fdp, 1);
pthread_mutex_unlock (&fdp->lock);
pthread_mutex_lock (&fnp->lock);
diskfs_file_update (fnp, 1);
pthread_mutex_unlock (&fnp->lock);
pthread_mutex_lock (&tdp->lock);
diskfs_file_update (tdp, 1);
pthread_mutex_unlock (&tdp->lock);
}
diskfs_nrele (fnp);
pthread_mutex_unlock (&renamedirlock);
if (!err)
mach_port_deallocate (mach_task_self (), tocred->pi.port_right);
return err;
}
pthread_mutex_unlock (&fnp->lock);
pthread_mutex_lock (&tdp->lock);
err = diskfs_lookup (tdp, toname, RENAME, &tnp, ds, tocred);
if (err == EAGAIN)
err = EINVAL;
else if (!err && excl)
{
err = EEXIST;
diskfs_nput (tnp);
}
if (err && err != ENOENT)
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nrele (fnp);
pthread_mutex_unlock (&tdp->lock);
return err;
}
if (tnp == fnp)
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nrele (fnp);
diskfs_nput (tnp);
pthread_mutex_unlock (&tdp->lock);
mach_port_deallocate (mach_task_self (), tocred->pi.port_right);
return 0;
}
if (tnp && S_ISDIR (tnp->dn_stat.st_mode))
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nrele (fnp);
diskfs_nput (tnp);
pthread_mutex_unlock (&tdp->lock);
return EISDIR;
}
pthread_mutex_lock (&fnp->lock);
if (fnp->dn_stat.st_nlink == diskfs_link_max - 1)
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nput (fnp);
if (tnp)
diskfs_nput (tnp);
pthread_mutex_unlock (&tdp->lock);
return EMLINK;
}
fnp->dn_stat.st_nlink++;
fnp->dn_set_ctime = 1;
diskfs_node_update (fnp, diskfs_synchronous);
if (tnp)
{
err = diskfs_dirrewrite (tdp, tnp, fnp, toname, ds);
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
err = diskfs_direnter (tdp, toname, fnp, ds, tocred);
if (diskfs_synchronous)
diskfs_node_update (tdp, 1);
pthread_mutex_unlock (&tdp->lock);
pthread_mutex_unlock (&fnp->lock);
if (err)
{
diskfs_nrele (fnp);
return err;
}
pthread_mutex_lock (&fdp->lock);
err = diskfs_lookup (fdp, fromname, REMOVE, &tmpnp, ds, fromcred);
if (err)
{
diskfs_drop_dirstat (tdp, ds);
pthread_mutex_unlock (&fdp->lock);
diskfs_nrele (fnp);
return err;
}
if (tmpnp != fnp)
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nput (tmpnp);
diskfs_nrele (fnp);
pthread_mutex_unlock (&fdp->lock);
mach_port_deallocate (mach_task_self (), tocred->pi.port_right);
return 0;
}
diskfs_nrele (tmpnp);
err = diskfs_dirremove (fdp, fnp, fromname, ds);
if (diskfs_synchronous)
diskfs_node_update (fdp, 1);
fnp->dn_stat.st_nlink--;
fnp->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (fnp, 1);
diskfs_nput (fnp);
pthread_mutex_unlock (&fdp->lock);
if (!err)
mach_port_deallocate (mach_task_self (), tocred->pi.port_right);
return err;
}