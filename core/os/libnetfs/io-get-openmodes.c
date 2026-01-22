#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_get_openmodes (struct protid *user, int *bits)
{
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
*bits = user->po->openstat;
pthread_mutex_unlock (&user->po->np->lock);
return 0;
}