#include "priv.h"
#include "fs_S.h"
#include <hurd/fsys.h>
kern_return_t
diskfs_S_dir_rmdir (struct protid *dircred,
const_string_t name)
{
struct node *dnp;
struct node *np = NULL;
struct dirstat *ds = alloca (diskfs_dirstat_size);
error_t error;
inline error_t done (error_t error, struct node *np)
{
if (np)
diskfs_nput (np);
if (ds)
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return error;
}
if (!dircred)
return EOPNOTSUPP;
dnp = dircred->po->np;
if (diskfs_check_readonly ())
return EROFS;
pthread_mutex_lock (&dnp->lock);
error = diskfs_lookup (dnp, name, REMOVE, &np, ds, dircred);
if (error)
return done (error == EAGAIN ? ENOTEMPTY : error, 0);
if (dnp == np)
{
diskfs_nrele (np);
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return EINVAL;
}
if ((np->dn_stat.st_mode & S_IPTRANS) || fshelp_translated (&np->transbox))
return done (EBUSY, np);
if (!S_ISDIR (np->dn_stat.st_mode))
return done (ENOTDIR, np);
if (!diskfs_dirempty (np, dircred))
return done (ENOTEMPTY, np);
error = diskfs_dirremove (dnp, np, name, ds);
ds = 0;
if (!error)
{
np->dn_stat.st_nlink--;
np->dn_set_ctime = 1;
diskfs_clear_directory (np, dnp, dircred);
if (diskfs_synchronous)
diskfs_file_update (np, 1);
}
if (diskfs_synchronous)
diskfs_file_update (dnp, 1);
return done (error, np);
}