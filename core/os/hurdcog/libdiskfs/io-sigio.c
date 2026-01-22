#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_sigio (struct protid *cred)
{
if (!cred)
return EOPNOTSUPP;
pthread_mutex_lock (&cred->po->np->lock);
if ((cred->po->openstat & O_FSYNC) || diskfs_synchronous)
diskfs_file_update (cred->po->np, 1);
pthread_mutex_unlock (&cred->po->np->lock);
return 0;
}