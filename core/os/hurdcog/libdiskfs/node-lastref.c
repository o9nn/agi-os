#include "priv.h"
void
_diskfs_lastref (struct node *np)
{
diskfs_set_node_times (np);
diskfs_lost_hardrefs (np);
if (!np->dn_stat.st_nlink)
{
if (np->sockaddr != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), np->sockaddr);
np->sockaddr = MACH_PORT_NULL;
}
diskfs_try_dropping_softrefs (np);
}
}