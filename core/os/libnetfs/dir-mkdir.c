#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_mkdir (struct protid *user, const_string_t name, mode_t mode)
{
error_t err;
if (!user)
return EOPNOTSUPP;
mode &= ~(S_IFMT|S_ISPARE|S_ISVTX);
mode |= S_IFDIR;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_mkdir (user->user, user->po->np, name, mode);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}