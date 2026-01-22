#include "netfs.h"
void
netfs_drop_node (struct node *np)
{
fshelp_drop_transbox (&np->transbox);
netfs_node_norefs (np);
}