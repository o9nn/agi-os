#include "netfs.h"
#include <hurd/fshelp.h>
error_t
netfs_S_file_record_lock (struct protid *cred,
int cmd,
struct flock64 *lock,
mach_port_t rendezvous)
{
struct node *node;
error_t err;
if (! cred)
return EOPNOTSUPP;
node = cred->po->np;
pthread_mutex_lock (&node->lock);
err = fshelp_rlock_tweak (&node->userlock, &node->lock,
&cred->po->lock_status, cred->po->openstat,
node->nn_stat.st_size, cred->po->filepointer,
cmd, lock, rendezvous);
pthread_mutex_unlock (&node->lock);
return err;
}