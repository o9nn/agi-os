#include "priv.h"
void
netfs_nrele (struct node *np)
{
struct references result;
int locked = FALSE;
refcounts_demote (&np->refcounts, &result);
if (result.hard == 0)
{
pthread_mutex_lock (&np->lock);
netfs_try_dropping_softrefs (np);
locked = TRUE;
}
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
{
if (! locked)
pthread_mutex_lock (&np->lock);
netfs_drop_node (np);
} else if (locked)
pthread_mutex_unlock (&np->lock);
}
void
netfs_nrele_light (struct node *np)
{
struct references result;
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
{
pthread_mutex_lock (&np->lock);
netfs_drop_node (np);
}
}