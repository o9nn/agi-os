#include <fcntl.h>
#include <string.h>
#include <hurd/ihash.h>
#include <hurd/fshelp.h>
#include <hurd/iohelp.h>
#include <hurd/netfs.h>
#include "ftpfs.h"
#include "ccache.h"
error_t
ftpfs_create_node (struct ftpfs_dir_entry *e, const char *rmt_path,
struct node **node)
{
struct node *new;
struct netnode *nn = malloc (sizeof (struct netnode));
error_t err;
if (! nn)
return ENOMEM;
nn->fs = e->dir->fs;
nn->dir_entry = e;
nn->contents = 0;
nn->dir = 0;
nn->rmt_path = strdup (rmt_path);
nn->ncache_next = nn->ncache_prev = 0;
new = netfs_make_node (nn);
if (! new)
{
free (nn);
return ENOMEM;
}
fshelp_touch (&new->nn_stat, TOUCH_ATIME|TOUCH_MTIME|TOUCH_CTIME,
ftpfs_maptime);
pthread_spin_lock (&nn->fs->inode_mappings_lock);
err = hurd_ihash_add (&nn->fs->inode_mappings, e->stat.st_ino, e);
pthread_spin_unlock (&nn->fs->inode_mappings_lock);
if (err)
{
free (nn);
free (new);
return err;
}
e->node = new;
*node = new;
return 0;
}
void
netfs_node_norefs (struct node *node)
{
struct netnode *nn = node->nn;
netfs_nref (node);
ftpfs_detach_node (node);
if (nn->dir)
{
assert_backtrace (nn->dir->num_live_entries == 0);
ftpfs_dir_free (nn->dir);
}
pthread_spin_lock (&nn->fs->inode_mappings_lock);
hurd_ihash_locp_remove (&nn->fs->inode_mappings, nn->dir_entry->inode_locp);
pthread_spin_unlock (&nn->fs->inode_mappings_lock);
if (nn->contents)
ccache_free (nn->contents);
free (nn);
free (node);
}