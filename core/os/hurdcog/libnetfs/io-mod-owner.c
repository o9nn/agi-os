#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_mod_owner (struct protid *user, pid_t owner)
{
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
user->po->np->owner = owner;
pthread_mutex_unlock (&user->po->np->lock);
return 0;
}