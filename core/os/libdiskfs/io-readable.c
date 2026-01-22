#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_readable (struct protid *cred,
vm_size_t *amount)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openstat & O_READ))
return EINVAL;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
iohelp_get_conch (&np->conch);
if (np->dn_stat.st_size > cred->po->filepointer)
*amount = np->dn_stat.st_size - cred->po->filepointer;
else
*amount = 0;
pthread_mutex_unlock (&np->lock);
return 0;
}