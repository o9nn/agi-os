#include "ports.h"
void
ports_enable_class (struct port_class *class)
{
pthread_mutex_lock (&_ports_lock);
class->flags &= ~PORT_CLASS_NO_ALLOC;
if (class->flags & PORT_CLASS_ALLOC_WAIT)
{
class->flags &= ~PORT_CLASS_ALLOC_WAIT;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}