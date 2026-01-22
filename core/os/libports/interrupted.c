#include "ports.h"
static pthread_spinlock_t interrupted_lock = PTHREAD_SPINLOCK_INITIALIZER;
static struct rpc_info *interrupted = 0;
int
ports_self_interrupted (void)
{
struct rpc_info **rpc_p, *rpc;
thread_t self = hurd_thread_self ();
pthread_spin_lock (&interrupted_lock);
for (rpc_p = &interrupted; *rpc_p; rpc_p = &rpc->interrupted_next)
{
rpc = *rpc_p;
if (rpc->thread == self)
{
*rpc_p = rpc->interrupted_next;
pthread_spin_unlock (&interrupted_lock);
rpc->interrupted_next = 0;
return 1;
}
}
pthread_spin_unlock (&interrupted_lock);
return 0;
}
void
_ports_record_interruption (struct rpc_info *rpc)
{
struct rpc_info *i;
pthread_spin_lock (&interrupted_lock);
for (i = interrupted; i; i = i->interrupted_next)
if (i == rpc)
{
pthread_spin_unlock (&interrupted_lock);
return;
}
rpc->interrupted_next = interrupted;
interrupted = rpc;
pthread_spin_unlock (&interrupted_lock);
}