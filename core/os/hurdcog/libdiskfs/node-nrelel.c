#include "priv.h"
void
diskfs_nrele_light (struct node *np)
{
struct references result;
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
{
pthread_mutex_lock (&np->lock);
diskfs_drop_node (np);
}
}