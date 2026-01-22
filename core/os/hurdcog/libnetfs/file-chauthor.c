#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_chauthor (struct protid *user,
uid_t author)
{
error_t err;
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_chauthor (user->user, user->po->np, author);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}