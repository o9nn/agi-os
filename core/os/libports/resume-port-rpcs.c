#include "ports.h"
#include <assert-backtrace.h>
void
ports_resume_port_rpcs (void *portstruct)
{
struct port_info *pi = portstruct;
pthread_mutex_lock (&_ports_lock);
assert_backtrace (pi->flags & PORT_INHIBITED);
pi->flags &= ~PORT_INHIBITED;
if (pi->flags & PORT_BLOCKED)
{
pi->flags &= ~PORT_BLOCKED;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}