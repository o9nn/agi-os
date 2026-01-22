#include <unistd.h>
#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_seek (struct protid *user,
off_t offset,
int whence,
off_t *newoffset)
{
error_t err = 0;
if (!user)
return EOPNOTSUPP;
switch (whence)
{
case SEEK_CUR:
offset += user->po->filepointer;
goto check;
case SEEK_END:
{
struct node *np;
np = user->po->np;
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, user->user);
if (!err)
offset += np->nn_stat.st_size;
pthread_mutex_unlock (&np->lock);
}
case SEEK_SET:
check:
if (offset >= 0)
{
*newoffset = user->po->filepointer = offset;
break;
}
default:
err = EINVAL;
break;
}
return err;
}