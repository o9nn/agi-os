#include "priv.h"
#include "io_S.h"
#include <unistd.h>
kern_return_t
diskfs_S_io_seek (struct protid *cred,
off_t offset,
int whence,
off_t *newoffset)
{
error_t err = 0;
struct node *np;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
iohelp_get_conch (&np->conch);
switch (whence)
{
case SEEK_CUR:
offset += cred->po->filepointer;
goto check;
case SEEK_END:
offset += np->dn_stat.st_size;
case SEEK_SET:
check:
if (sizeof(off_t) > sizeof(vm_offset_t) &&
offset > ((off_t) 1) << (sizeof(vm_offset_t) * 8))
{
err = EFBIG;
break;
}
if (offset >= 0)
{
*newoffset = cred->po->filepointer = offset;
break;
}
default:
err = EINVAL;
break;
}
pthread_mutex_unlock (&np->lock);
return err;
}