#include <fcntl.h>
#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_readdir (struct protid *user,
data_t *data,
mach_msg_type_number_t *datacnt,
boolean_t *data_dealloc,
int entry,
int nentries,
vm_size_t bufsiz,
int *amt)
{
error_t err;
struct node *np;
if (!user)
return EOPNOTSUPP;
np = user->po->np;
pthread_mutex_lock (&np->lock);
err = 0;
if ((user->po->openstat & O_READ) == 0)
err = EBADF;
if (!err)
err = netfs_validate_stat (np, user->user);
if (!err && (np->nn_stat.st_mode & S_IFMT) != S_IFDIR)
err = ENOTDIR;
if (!err)
err = netfs_get_dirents (user->user, np, entry, nentries, data,
datacnt, bufsiz, amt);
*data_dealloc = 1;
pthread_mutex_unlock (&np->lock);
return err;
}