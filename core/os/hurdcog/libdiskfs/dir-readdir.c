#include <fcntl.h>
#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_dir_readdir (struct protid *cred,
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
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
if ((cred->po->openstat & O_READ) == 0)
{
pthread_mutex_unlock (&np->lock);
return EBADF;
}
if ((np->dn_stat.st_mode & S_IFMT) != S_IFDIR)
{
pthread_mutex_unlock (&np->lock);
return ENOTDIR;
}
err = diskfs_get_directs (np, entry, nentries, data, datacnt, bufsiz, amt);
*data_dealloc = 1;
pthread_mutex_unlock (&np->lock);
return err;
}