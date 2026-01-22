#include "priv.h"
void
netfs_nref (struct node *np)
{
refcounts_ref (&np->refcounts, NULL);
}
void
netfs_nref_light (struct node *np)
{
refcounts_ref_weak (&np->refcounts, NULL);
}