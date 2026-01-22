#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_sync (struct protid *cred,
int wait,
int omitmetadata)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
if (diskfs_synchronous)
wait = 1;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
iohelp_get_conch (&np->conch);
pthread_mutex_unlock (&np->lock);
diskfs_file_update (np, wait);
return 0;
}