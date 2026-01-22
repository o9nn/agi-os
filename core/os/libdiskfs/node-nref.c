#include "priv.h"
void
diskfs_nref (struct node *np)
{
struct references result;
refcounts_ref (&np->refcounts, &result);
if (result.hard == 1)
{
pthread_mutex_lock (&np->lock);
diskfs_new_hardrefs (np);
pthread_mutex_unlock (&np->lock);
}
}