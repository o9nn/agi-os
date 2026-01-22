#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_set_size (struct protid *user,
off_t size)
{
error_t err;
if (!user)
return EOPNOTSUPP;
else if (size < 0)
return EINVAL;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_set_size (user->user, user->po->np, size);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}