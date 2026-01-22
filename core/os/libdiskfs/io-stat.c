#include "priv.h"
#include "io_S.h"
#include <string.h>
kern_return_t
diskfs_S_io_stat (struct protid *cred,
io_statbuf_t *statbuf)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
iohelp_get_conch (&np->conch);
if (diskfs_synchronous)
diskfs_node_update (np, 1);
else
diskfs_set_node_times (np);
memcpy (statbuf, &np->dn_stat, sizeof (struct stat));
statbuf->st_mode &= ~(S_IATRANS | S_IROOT);
if (fshelp_translated (&np->transbox))
statbuf->st_mode |= S_IATRANS;
if (cred->po->shadow_root == np || np == diskfs_root_node)
statbuf->st_mode |= S_IROOT;
pthread_mutex_unlock (&np->lock);
return 0;
}