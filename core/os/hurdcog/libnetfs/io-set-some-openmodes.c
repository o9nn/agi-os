#include "netfs.h"
#include "io_S.h"
#include "modes.h"
kern_return_t
netfs_S_io_set_some_openmodes (struct protid *user, int bits)
{
if (!user)
return EOPNOTSUPP;
pthread_mutex_lock (&user->po->np->lock);
user->po->openstat |= (bits & HONORED_STATE_MODES);
pthread_mutex_unlock (&user->po->np->lock);
return 0;
}