#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_rmdir (struct protid *diruser, const_string_t name)
{
error_t err;
if (!diruser)
return EOPNOTSUPP;
pthread_mutex_lock (&diruser->po->np->lock);
err = netfs_attempt_rmdir (diruser->user, diruser->po->np, name);
pthread_mutex_unlock (&diruser->po->np->lock);
return err;
}