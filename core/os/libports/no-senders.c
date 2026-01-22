#include "ports.h"
#include <mach/notify.h>
void
ports_no_senders (void *portstruct,
mach_port_mscount_t mscount)
{
struct port_info *pi = portstruct;
int dealloc;
mach_port_t old;
pthread_mutex_lock (&_ports_lock);
if ((pi->flags & PORT_HAS_SENDRIGHTS) == 0)
{
pthread_mutex_unlock (&_ports_lock);
return;
}
if (mscount >= pi->mscount)
{
dealloc = 1;
pi->flags &= ~PORT_HAS_SENDRIGHTS;
}
else
{
mach_port_request_notification (mach_task_self (), pi->port_right,
MACH_NOTIFY_NO_SENDERS, pi->mscount,
pi->port_right,
MACH_MSG_TYPE_MAKE_SEND_ONCE, &old);
if (old)
mach_port_deallocate (mach_task_self (), old);
dealloc = 0;
}
pthread_mutex_unlock (&_ports_lock);
if (dealloc)
{
ports_interrupt_notified_rpcs (portstruct, pi->port_right,
MACH_NOTIFY_NO_SENDERS);
ports_interrupt_rpcs (pi);
ports_port_deref (pi);
}
}