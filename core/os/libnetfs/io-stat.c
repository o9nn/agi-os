#include "netfs.h"
#include "io_S.h"
#include <string.h>
kern_return_t
netfs_S_io_stat (struct protid *user, io_statbuf_t *statbuf)
{
error_t err;
struct node *node;
if (! user)
return EOPNOTSUPP;
node = user->po->np;
pthread_mutex_lock (&node->lock);
err = netfs_validate_stat (node, user->user);
if (! err)
{
memcpy (statbuf, &node->nn_stat, sizeof (struct stat));
statbuf->st_mode &= ~(S_IATRANS | S_IROOT);
if (fshelp_translated (&node->transbox))
statbuf->st_mode |= S_IATRANS;
if (user->po->shadow_root == node || node == netfs_root_node)
statbuf->st_mode |= S_IROOT;
}
pthread_mutex_unlock (&node->lock);
return err;
}