#include "ports.h"
error_t
ports_class_iterate (struct port_class *class,
error_t (*fun)(void *))
{
return _ports_bucket_class_iterate (&_ports_htable, class, fun);
}