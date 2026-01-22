#include <stddef.h>
#include <string.h>
#include <hurd/ihash.h>
#include <hurd/netfs.h>
#include "ftpfs.h"
error_t
ftpfs_create (char *rmt_path, int fsid,
struct ftp_conn_params *ftp_params,
struct ftp_conn_hooks *ftp_hooks,
struct ftpfs_params *params,
struct ftpfs **fs)
{
error_t err;
struct ftpfs_dir *super_root_dir;
struct node *super_root;
struct ftpfs *new = malloc (sizeof (struct ftpfs));
if (! new)
return ENOMEM;
new->free_conns = 0;
new->conns = 0;
pthread_spin_init (&new->conn_lock, PTHREAD_PROCESS_PRIVATE);
new->node_cache_mru = new->node_cache_lru = 0;
new->node_cache_len = 0;
pthread_mutex_init (&new->node_cache_lock, NULL);
new->fsid = fsid;
new->next_inode = 2;
new->params = *params;
new->ftp_params = ftp_params;
new->ftp_hooks = ftp_hooks;
hurd_ihash_init (&new->inode_mappings,
offsetof (struct ftpfs_dir_entry, inode_locp));
pthread_spin_init (&new->inode_mappings_lock, PTHREAD_PROCESS_PRIVATE);
super_root = netfs_make_node (0);
if (! super_root)
err = ENOMEM;
else
{
err = ftpfs_dir_create (new, super_root, rmt_path, &super_root_dir);
if (! err)
err = ftpfs_dir_null_lookup (super_root_dir, &new->root);
}
if (err)
{
hurd_ihash_destroy (&new->inode_mappings);
free (new);
}
else
*fs = new;
return err;
}