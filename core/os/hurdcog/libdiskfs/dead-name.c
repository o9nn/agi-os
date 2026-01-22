#include "priv.h"
void
ports_dead_name (void *notify, mach_port_t dead_name)
{
#if 0
struct protid *pi = ports_lookup_port (diskfs_port_bucket, dead_name,
diskfs_protid_class);
struct node *np;
if (pi)
{
np = pi->po->np;
pthread_mutex_lock (&np->lock);
if (dead_name == np->sockaddr)
{
mach_port_deallocate (mach_task_self (), np->sockaddr);
np->sockaddr = MACH_PORT_NULL;
diskfs_nput (np);
}
else
pthread_mutex_unlock (&np->lock);
}
#endif
fshelp_remove_active_translator (dead_name);
ports_interrupt_notified_rpcs (notify, dead_name, MACH_NOTIFY_DEAD_NAME);
}