#include "priv.h"
#include <hurd/iohelp.h>
void
iohelp_fetch_shared_data (void *arg)
{
struct protid *cred = arg;
int mod = 0;
if (cred->mapped->file_size > cred->po->np->allocsize)
cred->mapped->file_size = cred->po->np->allocsize;
if (cred->mapped->file_size < cred->po->np->dn_stat.st_size)
cred->mapped->file_size = cred->po->np->dn_stat.st_size;
else if (cred->po->np->dn_stat.st_size != cred->mapped->file_size)
{
if (diskfs_check_readonly ())
cred->mapped->file_size = cred->po->np->dn_stat.st_size;
else
{
cred->po->np->dn_stat.st_size = cred->mapped->file_size;
cred->po->np->dn_set_ctime = 1;
mod = 1;
}
}
cred->po->filepointer = cred->mapped->xx_file_pointer;
if (!diskfs_check_readonly ())
{
if (cred->mapped->written)
{
cred->po->np->dn_set_mtime = 1;
mod = 1;
}
if (cred->mapped->accessed && atime_should_update (cred->po->np))
{
cred->po->np->dn_set_atime = 1;
mod = 1;
}
}
cred->mapped->written = 0;
cred->mapped->accessed = 0;
if (diskfs_synchronous && mod)
diskfs_node_update (cred->po->np, 1);
}