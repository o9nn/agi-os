#include "priv.h"
#include <fcntl.h>
static struct node *
init_node (struct node *np, struct disknode *dn)
{
np->dn = dn;
np->dn_set_ctime = 0;
np->dn_set_atime = 0;
np->dn_set_mtime = 0;
np->dn_stat_dirty = 0;
np->author_tracks_uid = 0;
pthread_mutex_init (&np->lock, NULL);
refcounts_init (&np->refcounts, 1, 0);
np->owner = 0;
np->sockaddr = MACH_PORT_NULL;
np->dirmod_reqs = 0;
np->dirmod_tick = 0;
np->filemod_reqs = 0;
np->filemod_tick = 0;
fshelp_transbox_init (&np->transbox, &np->lock, np);
iohelp_initialize_conch (&np->conch, &np->lock);
fshelp_rlock_init (&np->userlock);
return np;
}
struct node *
diskfs_make_node (struct disknode *dn)
{
struct node *np = malloc (sizeof (struct node));
if (np == 0)
return 0;
return init_node (np, dn);
}
struct node *
diskfs_make_node_alloc (size_t size)
{
struct node *np = malloc (sizeof (struct node) + size);
if (np == NULL)
return NULL;
return init_node (np, diskfs_node_disknode (np));
}