#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_check_access (struct protid *user,
int *types)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_report_access (user->user, user->po->np, types);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}