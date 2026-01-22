#include "ports.h"
error_t
ports_create_port_noinstall (struct port_class *class,
struct port_bucket *bucket,
size_t size, void *result)
{
return _ports_create_port_internal (class, bucket, size, result, 0);
}