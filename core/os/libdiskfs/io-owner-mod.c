#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_mod_owner (struct protid *cred,
pid_t owner)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
np->owner = owner;
pthread_mutex_unlock (&np->lock);
return 0;
}