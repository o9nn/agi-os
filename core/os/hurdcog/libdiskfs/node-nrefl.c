#include "priv.h"
void
diskfs_nref_light (struct node *np)
{
refcounts_ref_weak (&np->refcounts, NULL);
}