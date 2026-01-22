#include "netfs.h"
#include "fs_S.h"
#include <fcntl.h>
#include <sys/file.h>
kern_return_t
netfs_S_file_lock_stat (struct protid *user,
int *mystatus,
int *otherstatus)
{
struct node *node;
if (!user)
return EOPNOTSUPP;
node = user->po->np;
pthread_mutex_lock (&node->lock);
*mystatus = fshelp_rlock_peropen_status (&user->po->lock_status);
*otherstatus = fshelp_rlock_node_status (&node->userlock);
pthread_mutex_unlock (&node->lock);
return 0;
}