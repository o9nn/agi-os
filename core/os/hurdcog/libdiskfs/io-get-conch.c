#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_get_conch (struct protid *cred)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
if (!cred->mapped)
{
pthread_mutex_unlock (&np->lock);
return EINVAL;
}
iohelp_handle_io_get_conch (&np->conch, cred, cred->mapped);
pthread_mutex_unlock (&np->lock);
return 0;
}