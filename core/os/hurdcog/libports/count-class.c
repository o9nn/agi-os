#include "ports.h"
int
ports_count_class (struct port_class *class)
{
int ret;
pthread_mutex_lock (&_ports_lock);
ret = class->count;
class->flags |= PORT_CLASS_NO_ALLOC;
pthread_mutex_unlock (&_ports_lock);
return ret;
}