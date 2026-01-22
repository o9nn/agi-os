#include "priv.h"
static void
free_modreqs (struct modreq *mr)
{
struct modreq *tmp;
for (; mr; mr = tmp)
{
mach_port_deallocate (mach_task_self (), mr->port);
tmp = mr->next;
free (mr);
}
}
void
diskfs_drop_node (struct node *np)
{
mode_t savemode;
if (np->dn_stat.st_nlink == 0 && !diskfs_readonly)
{
diskfs_check_readonly ();
if (np->dn_stat.st_mode & S_IPTRANS)
diskfs_set_translator (np, 0, 0, 0);
if (np->allocsize != 0
|| (diskfs_create_symlink_hook
&& S_ISLNK (np->dn_stat.st_mode)
&& np->dn_stat.st_size))
{
refcounts_unsafe_ref (&np->refcounts, NULL);
diskfs_truncate (np, 0);
np->allocsize = 0;
diskfs_nput (np);
return;
}
assert_backtrace (np->dn_stat.st_size == 0);
savemode = np->dn_stat.st_mode;
np->dn_stat.st_mode = 0;
np->dn_stat.st_rdev = 0;
np->dn_set_ctime = np->dn_set_atime = 1;
diskfs_node_update (np, diskfs_synchronous);
diskfs_free_node (np, savemode);
}
else
diskfs_node_update (np, diskfs_synchronous);
fshelp_drop_transbox (&np->transbox);
if (np->dirmod_reqs)
free_modreqs (np->dirmod_reqs);
if (np->filemod_reqs)
free_modreqs (np->filemod_reqs);
assert_backtrace (!np->sockaddr);
pthread_mutex_unlock(&np->lock);
pthread_mutex_destroy(&np->lock);
diskfs_node_norefs (np);
}