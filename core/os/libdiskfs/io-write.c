#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_write (struct protid *cred,
const_data_t data,
mach_msg_type_number_t datalen,
off_t offset,
vm_size_t *amt)
{
struct node *np;
error_t err;
off_t off = offset;
mach_msg_type_number_t nwritten;
if (!cred)
return EOPNOTSUPP;
if (diskfs_check_readonly ())
return EROFS;
np = cred->po->np;
if (!(cred->po->openstat & O_WRITE))
return EBADF;
pthread_mutex_lock (&np->lock);
assert_backtrace (!S_ISDIR(np->dn_stat.st_mode));
iohelp_get_conch (&np->conch);
if (off == -1)
{
if (cred->po->openstat & O_APPEND)
cred->po->filepointer = np->dn_stat.st_size;
off = cred->po->filepointer;
}
if (off < 0)
{
err = EINVAL;
goto out;
}
while (off + (off_t) datalen > np->allocsize)
{
err = diskfs_grow (np, off + datalen, cred);
if (diskfs_synchronous)
diskfs_node_update (np, 1);
if (err)
goto out;
if (np->filemod_reqs)
diskfs_notice_filechange (np, FILE_CHANGED_EXTEND, 0, off + datalen);
}
if (off + (off_t) datalen > np->dn_stat.st_size)
{
np->dn_stat.st_size = off + datalen;
np->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (np, 1);
}
nwritten = datalen;
err = _diskfs_rdwr_internal (np, (char *) data, off, &nwritten, 1, 0);
if (!err)
*amt = nwritten;
if (!err && offset == -1)
cred->po->filepointer += nwritten;
if (!err
&& ((cred->po->openstat & O_FSYNC) || diskfs_synchronous))
diskfs_file_update (np, 1);
if (!err && np->filemod_reqs)
diskfs_notice_filechange (np, FILE_CHANGED_WRITE, off, off + nwritten);
out:
pthread_mutex_unlock (&np->lock);
return err;
}