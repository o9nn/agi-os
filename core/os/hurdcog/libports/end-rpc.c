#include "ports.h"
void
ports_end_rpc (void *port, struct rpc_info *info)
{
struct port_info *pi = port;
pthread_mutex_lock (&_ports_lock);
if (info->notifies)
_ports_remove_notified_rpc (info);
*info->prevp = info->next;
if (info->next)
info->next->prevp = info->prevp;
pi->class->rpcs--;
_ports_total_rpcs--;
pi->bucket->rpcs--;
if ((pi->flags & PORT_INHIBIT_WAIT)
|| (pi->bucket->flags & PORT_BUCKET_INHIBIT_WAIT)
|| (pi->class->flags & PORT_CLASS_INHIBIT_WAIT)
|| (_ports_flags & _PORTS_INHIBIT_WAIT))
pthread_cond_broadcast (&_ports_block);
ports_self_interrupted ();
hurd_check_cancel ();
pthread_mutex_unlock (&_ports_lock);
}