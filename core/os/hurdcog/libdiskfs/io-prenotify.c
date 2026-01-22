#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_prenotify (struct protid *cred,
vm_offset_t start __attribute__ ((unused)),
vm_offset_t end)
{
struct node *np;
int err = 0;
if (!cred)
return EOPNOTSUPP;
if (diskfs_check_readonly ())
return EROFS;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
if (!cred->mapped)
{
err = EINVAL;
goto out;
}
err = iohelp_verify_user_conch (&np->conch, cred);
if (err)
goto out;
iohelp_fetch_shared_data (cred);
if ((off_t) end < np->allocsize)
{
pthread_spin_lock (&cred->mapped->lock);
iohelp_put_shared_data (cred);
pthread_spin_unlock (&cred->mapped->lock);
goto out;
}
err = diskfs_grow (np, end, cred);
if (diskfs_synchronous)
diskfs_node_update (np, 1);
if (!err && np->filemod_reqs)
diskfs_notice_filechange (np, FILE_CHANGED_EXTEND, 0, end);
out:
pthread_mutex_unlock (&np->lock);
return err;
}