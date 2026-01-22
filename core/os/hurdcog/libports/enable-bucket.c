#include "ports.h"
void
ports_enable_bucket (struct port_bucket *bucket)
{
pthread_mutex_lock (&_ports_lock);
bucket->flags &= ~PORT_BUCKET_NO_ALLOC;
if (bucket->flags & PORT_BUCKET_ALLOC_WAIT)
{
bucket->flags &= ~PORT_BUCKET_ALLOC_WAIT;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}