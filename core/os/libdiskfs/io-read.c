#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_read (struct protid *cred,
data_t *data,
mach_msg_type_number_t *datalen,
off_t offset,
vm_size_t maxread)
{
struct node *np;
int err;
off_t off = offset;
char *buf;
int ourbuf = 0;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
if (!(cred->po->openstat & O_READ))
return EBADF;
pthread_mutex_lock (&np->lock);
iohelp_get_conch (&np->conch);
if (off == -1)
off = cred->po->filepointer;
if (off < 0)
{
pthread_mutex_unlock (&np->lock);
return EINVAL;
}
if (off > np->dn_stat.st_size)
maxread = 0;
else if (off + (off_t) maxread > np->dn_stat.st_size)
maxread = np->dn_stat.st_size - off;
if (maxread > *datalen)
{
ourbuf = 1;
buf = mmap (0, maxread, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
assert_backtrace (buf != MAP_FAILED);
*data = buf;
}
else
buf = *data;
*datalen = maxread;
if (maxread == 0)
err = 0;
else if (S_ISLNK (np->dn_stat.st_mode))
{
if (! diskfs_read_symlink_hook)
err = EINVAL;
else
{
if (off == 0 && maxread == np->dn_stat.st_size)
err = (*diskfs_read_symlink_hook)(np, buf);
else
{
char *whole_link = alloca (np->dn_stat.st_size);
err = (*diskfs_read_symlink_hook)(np, whole_link);
if (! err)
memcpy (buf, whole_link + off, maxread);
}
}
}
else
err = EINVAL;
if (err == EINVAL)
err = _diskfs_rdwr_internal (np, buf, off, datalen, 0,
cred->po->openstat & O_NOATIME);
if (diskfs_synchronous)
diskfs_node_update (np, 1);
if (offset == -1 && !err)
cred->po->filepointer += *datalen;
if (err && ourbuf)
munmap (buf, maxread);
pthread_mutex_unlock (&np->lock);
return err;
}