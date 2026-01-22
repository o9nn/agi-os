#include "netfs.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
netfs_S_io_write (struct protid *user,
const_data_t data,
mach_msg_type_number_t datalen,
off_t offset,
vm_size_t *amount)
{
error_t err;
off_t off = offset;
struct node *np;
if (!user)
return EOPNOTSUPP;
if ((user->po->openstat & O_WRITE) == 0)
return EBADF;
*amount = datalen;
np = user->po->np;
pthread_mutex_lock (&np->lock);
if (off == -1)
{
if (user->po->openstat & O_APPEND)
{
err = netfs_validate_stat (np, user->user);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
user->po->filepointer = np->nn_stat.st_size;
}
off = user->po->filepointer;
}
err = netfs_attempt_write (user->user, np, off, amount, data);
if (offset == -1 && !err)
user->po->filepointer += *amount;
pthread_mutex_unlock (&np->lock);
return err;
}