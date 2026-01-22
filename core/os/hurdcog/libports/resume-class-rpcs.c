#include "ports.h"
#include <assert-backtrace.h>
void
ports_resume_class_rpcs (struct port_class *class)
{
pthread_mutex_lock (&_ports_lock);
assert_backtrace (class->flags & PORT_CLASS_INHIBITED);
class->flags &= ~PORT_CLASS_INHIBITED;
if (class->flags & PORT_CLASS_BLOCKED)
{
class->flags &= ~PORT_CLASS_BLOCKED;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}