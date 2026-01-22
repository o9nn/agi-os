#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_statfs (struct protid *user,
struct statfs *st)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_statfs (user->user, user->po->np, st);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}