#include "ports.h"
#include <hurd.h>
void
ports_interrupt_rpcs (void *portstruct)
{
struct port_info *pi = portstruct;
struct rpc_info *rpc;
thread_t self = hurd_thread_self ();
pthread_mutex_lock (&_ports_lock);
for (rpc = pi->current_rpcs; rpc; rpc = rpc->next)
{
if (rpc->thread != self)
{
hurd_thread_cancel (rpc->thread);
_ports_record_interruption (rpc);
}
}
pthread_mutex_unlock (&_ports_lock);
}