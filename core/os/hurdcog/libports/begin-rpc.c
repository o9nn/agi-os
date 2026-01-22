#include "ports.h"
#define INHIBITED (PORTS_INHIBITED | PORTS_INHIBIT_WAIT)
error_t
ports_begin_rpc (void *portstruct, mach_msg_id_t msg_id, struct rpc_info *info)
{
int *block_flags = 0;
struct port_info *pi = portstruct;
pthread_mutex_lock (&_ports_lock);
do
{
if (pi->port_right == MACH_PORT_NULL)
{
pthread_mutex_unlock (&_ports_lock);
return EOPNOTSUPP;
}
if (_ports_flags & INHIBITED)
block_flags = &_ports_flags;
else if (pi->bucket->flags & INHIBITED)
block_flags = &pi->bucket->flags;
else if (pi->class->flags & INHIBITED)
block_flags = &pi->class->flags;
else if (pi->flags & INHIBITED)
block_flags = &pi->flags;
else
block_flags = 0;
if (block_flags)
{
if (msg_id)
{
struct ports_msg_id_range *range = pi->class->uninhibitable_rpcs;
while (range)
if (msg_id >= range->start && msg_id < range->end)
{
block_flags = 0;
break;
}
else
range = range->next;
}
if (block_flags)
{
*block_flags |= PORTS_BLOCKED;
if (pthread_hurd_cond_wait_np (&_ports_block, &_ports_lock))
{
pthread_mutex_unlock (&_ports_lock);
return EINTR;
}
}
}
}
while (block_flags);
info->thread = hurd_thread_self ();
info->next = pi->current_rpcs;
info->notifies = 0;
if (pi->current_rpcs)
pi->current_rpcs->prevp = &info->next;
info->prevp = &pi->current_rpcs;
pi->current_rpcs = info;
pi->class->rpcs++;
pi->bucket->rpcs++;
_ports_total_rpcs++;
pthread_mutex_unlock (&_ports_lock);
return 0;
}