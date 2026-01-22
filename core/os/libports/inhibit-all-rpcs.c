#include "ports.h"
#include <hurd.h>
#include <hurd/ihash.h>
error_t
ports_inhibit_all_rpcs (void)
{
error_t err = 0;
pthread_mutex_lock (&_ports_lock);
if (_ports_flags & (_PORTS_INHIBITED | _PORTS_INHIBIT_WAIT))
err = EBUSY;
else
{
int this_one = 0;
pthread_rwlock_rdlock (&_ports_htable_lock);
HURD_IHASH_ITERATE (&_ports_htable, portstruct)
{
struct rpc_info *rpc;
struct port_info *pi = portstruct;
for (rpc = pi->current_rpcs; rpc; rpc = rpc->next)
{
if (rpc->thread == hurd_thread_self ())
this_one = 1;
else
hurd_thread_cancel (rpc->thread);
}
}
pthread_rwlock_unlock (&_ports_htable_lock);
while (_ports_total_rpcs > this_one)
{
_ports_flags |= _PORTS_INHIBIT_WAIT;
if (pthread_hurd_cond_wait_np (&_ports_block, &_ports_lock))
{
err = EINTR;
break;
}
}
_ports_flags &= ~_PORTS_INHIBIT_WAIT;
if (! err)
_ports_flags |= _PORTS_INHIBITED;
}
pthread_mutex_unlock (&_ports_lock);
return err;
}