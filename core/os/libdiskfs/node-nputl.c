#include "priv.h"
void
diskfs_nput_light (struct node *np)
{
struct references result;
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
diskfs_drop_node (np);
else
pthread_mutex_unlock (&np->lock);
}