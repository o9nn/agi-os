#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_unlink (struct protid *user, const_string_t name)
{
error_t err;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_unlink (user->user, user->po->np, name);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}