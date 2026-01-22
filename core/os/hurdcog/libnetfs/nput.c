#include "priv.h"
void
netfs_nput (struct node *np)
{
struct references result;
refcounts_demote (&np->refcounts, &result);
if (result.hard == 0)
netfs_try_dropping_softrefs (np);
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
netfs_drop_node (np);
else
pthread_mutex_unlock (&np->lock);
}
void __attribute__ ((weak))
netfs_try_dropping_softrefs (struct node *np)
{
}