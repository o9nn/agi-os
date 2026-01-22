#include "priv.h"
#include "fs_S.h"
#include <fcntl.h>
#include <sys/file.h>
kern_return_t
diskfs_S_file_lock_stat (struct protid *cred,
int *mystatus,
int *otherstatus)
{
struct node *node;
if (!cred)
return EOPNOTSUPP;
node = cred->po->np;
pthread_mutex_lock (&node->lock);
*mystatus = fshelp_rlock_peropen_status (&cred->po->lock_status);
*otherstatus = fshelp_rlock_node_status (&node->userlock);
pthread_mutex_unlock (&node->lock);
return 0;
}