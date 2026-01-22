#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_chown (struct protid *user,
uid_t owner,
uid_t group)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_chown (user->user, user->po->np,
owner, group);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}