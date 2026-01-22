#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_chflags (struct protid *user,
int flags)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_chflags (user->user, user->po->np, flags);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}