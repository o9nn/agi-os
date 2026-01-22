#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_chmod (struct protid *user,
mode_t mode)
{
error_t err;
if (!user)
return EOPNOTSUPP;
mode &= ~(S_IFMT | S_ISPARE | S_ITRANS);
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_chmod (user->user, user->po->np, mode);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}