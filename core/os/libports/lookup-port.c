#include "ports.h"
#include <hurd/ihash.h>
void *
ports_lookup_port (struct port_bucket *bucket,
mach_port_t port,
struct port_class *class)
{
struct port_info *pi;
pthread_rwlock_rdlock (&_ports_htable_lock);
pi = hurd_ihash_find (&_ports_htable, port);
if (pi
&& ((class && pi->class != class)
|| (bucket && pi->bucket != bucket)))
pi = 0;
if (pi)
refcounts_unsafe_ref (&pi->refcounts, NULL);
pthread_rwlock_unlock (&_ports_htable_lock);
return pi;
}