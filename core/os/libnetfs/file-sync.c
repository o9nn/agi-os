#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_sync (struct protid *user,
int wait,
int omitmeta)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_sync (user->user, user->po->np, wait);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}