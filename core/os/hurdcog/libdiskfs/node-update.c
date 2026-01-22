#include "priv.h"
void diskfs_node_update (struct node *np, int wait)
{
diskfs_set_node_times (np);
if (np->dn_stat_dirty)
diskfs_write_disknode (np, wait);
}