#include "netfs.h"
#include <hurd/fshelp.h>
static struct node *
init_node (struct node *np, struct netnode *nn)
{
np->nn = nn;
pthread_mutex_init (&np->lock, NULL);
refcounts_init (&np->refcounts, 1, 0);
np->sockaddr = MACH_PORT_NULL;
np->owner = 0;
fshelp_transbox_init (&np->transbox, &np->lock, np);
fshelp_rlock_init (&np->userlock);
return np;
}
struct node *
netfs_make_node (struct netnode *nn)
{
struct node *np = malloc (sizeof (struct node));
if (! np)
return NULL;
return init_node (np, nn);
}
struct node *
netfs_make_node_alloc (size_t size)
{
struct node *np = malloc (sizeof (struct node) + size);
if (np == NULL)
return NULL;
return init_node (np, netfs_node_netnode (np));
}