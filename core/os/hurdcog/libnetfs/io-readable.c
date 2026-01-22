#include <fcntl.h>
#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_readable (struct protid *user,
vm_size_t *amount)
{
error_t err;
if (!user)
return EOPNOTSUPP;
if (!(user->po->openstat & O_READ))
return EINVAL;
pthread_mutex_lock (&user->po->np->lock);
err = netfs_validate_stat (user->po->np, user->user);
if (!err)
{
if (user->po->np->nn_stat.st_size > user->po->filepointer)
*amount = user->po->np->nn_stat.st_size - user->po->filepointer;
else
*amount = 0;
}
pthread_mutex_unlock (&user->po->np->lock);
return err;
}