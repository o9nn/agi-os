#include "priv.h"
void
diskfs_nrele (struct node *np)
{
int locked = FALSE;
struct references result;
refcounts_demote (&np->refcounts, &result);
if (result.hard == 0)
{
locked = TRUE;
pthread_mutex_lock (&np->lock);
_diskfs_lastref (np);
}
refcounts_deref_weak (&np->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
{
if (! locked)
pthread_mutex_lock (&np->lock);
diskfs_drop_node (np);
}
else if (locked)
pthread_mutex_unlock (&np->lock);
}