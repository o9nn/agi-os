#include "ports.h"
#include <assert-backtrace.h>
void
ports_resume_all_rpcs (void)
{
pthread_mutex_lock (&_ports_lock);
assert_backtrace (_ports_flags & _PORTS_INHIBITED);
_ports_flags &= ~_PORTS_INHIBITED;
if (_ports_flags & _PORTS_BLOCKED)
{
_ports_flags &= ~_PORTS_BLOCKED;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}