#include "priv.h"
error_t
diskfs_node_rdwr (struct node *np,
char *data,
off_t off,
mach_msg_type_number_t amt,
int dir,
struct protid *cred,
mach_msg_type_number_t *amtread)
{
error_t err;
iohelp_get_conch (&np->conch);
if (dir)
while (off + amt > np->allocsize)
{
err = diskfs_grow (np, off + amt, cred);
if (err)
return err;
if (np->filemod_reqs)
diskfs_notice_filechange (np, FILE_CHANGED_EXTEND, 0, off + amt);
}
if (off + amt > np->dn_stat.st_size)
{
if (dir)
{
np->dn_stat.st_size = off + amt;
np->dn_set_ctime = 1;
}
else
amt = np->dn_stat.st_size - off;
}
if (amtread)
*amtread = amt;
else
amtread = &amt;
err = _diskfs_rdwr_internal (np, data, off, amtread, dir, 0);
if (*amtread && diskfs_synchronous)
{
if (dir)
diskfs_file_update (np, 1);
else
diskfs_node_update (np, 1);
}
return err;
}