#include "ports.h"
#include <hurd.h>
error_t
ports_inhibit_port_rpcs (void *portstruct)
{
error_t err = 0;
struct port_info *pi = portstruct;
pthread_mutex_lock (&_ports_lock);
if (pi->flags & (PORT_INHIBITED | PORT_INHIBIT_WAIT))
err = EBUSY;
else
{
struct rpc_info *rpc;
struct rpc_info *this_rpc = 0;
for (rpc = pi->current_rpcs; rpc; rpc = rpc->next)
{
if (rpc->thread == hurd_thread_self ())
this_rpc = rpc;
else
hurd_thread_cancel (rpc->thread);
}
while (pi->current_rpcs
&& !(pi->current_rpcs == this_rpc && ! this_rpc->next))
{
pi->flags |= PORT_INHIBIT_WAIT;
if (pthread_hurd_cond_wait_np (&_ports_block, &_ports_lock))
{
err = EINTR;
break;
}
}
pi->flags &= ~PORT_INHIBIT_WAIT;
if (! err)
pi->flags |= PORT_INHIBITED;
}
pthread_mutex_unlock (&_ports_lock);
return err;
}