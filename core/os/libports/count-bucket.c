#include "ports.h"
int
ports_count_bucket (struct port_bucket *bucket)
{
int ret;
pthread_mutex_lock (&_ports_lock);
ret = bucket->count - 1;
bucket->flags |= PORT_BUCKET_NO_ALLOC;
pthread_mutex_unlock (&_ports_lock);
return ret;
}