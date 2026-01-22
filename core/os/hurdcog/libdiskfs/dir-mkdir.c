#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_dir_mkdir (struct protid *dircred,
const_string_t name,
mode_t mode)
{
struct node *dnp;
struct node *np = 0;
struct dirstat *ds = alloca (diskfs_dirstat_size);
int error;
if (!dircred)
return EOPNOTSUPP;
dnp = dircred->po->np;
if (diskfs_check_readonly ())
return EROFS;
pthread_mutex_lock (&dnp->lock);
error = diskfs_lookup (dnp, name, CREATE, 0, ds, dircred);
if (error == EAGAIN)
error = EEXIST;
if (!error)
error =  EEXIST;
if (error != ENOENT)
{
diskfs_drop_dirstat (dnp, ds);
pthread_mutex_unlock (&dnp->lock);
return error;
}
mode &= ~(S_ISPARE | S_IFMT | S_ITRANS);
mode |= S_IFDIR;
error = diskfs_create_node (dnp, name, mode, &np, dircred, ds);
if (diskfs_synchronous)
{
diskfs_file_update (dnp, 1);
diskfs_file_update (np, 1);
}
if (!error)
diskfs_nput (np);
pthread_mutex_unlock (&dnp->lock);
return error;
}