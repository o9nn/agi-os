#include "ports.h"
#include <assert-backtrace.h>
void
ports_resume_bucket_rpcs (struct port_bucket *bucket)
{
pthread_mutex_lock (&_ports_lock);
assert_backtrace (bucket->flags & PORT_BUCKET_INHIBITED);
bucket->flags &= ~PORT_BUCKET_INHIBITED;
if (bucket->flags & PORT_BUCKET_BLOCKED)
{
bucket->flags &= ~PORT_BUCKET_BLOCKED;
pthread_cond_broadcast (&_ports_block);
}
pthread_mutex_unlock (&_ports_lock);
}