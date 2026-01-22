#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_get_openmodes (struct protid *cred,
int *bits)
{
if (!cred)
return EOPNOTSUPP;
pthread_mutex_lock (&cred->po->np->lock);
*bits = cred->po->openstat;
pthread_mutex_unlock (&cred->po->np->lock);
return 0;
}