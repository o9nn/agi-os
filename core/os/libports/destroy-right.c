#include "ports.h"
#include <hurd/ihash.h>
#include <assert-backtrace.h>
#include <pthread.h>
#include <error.h>
#include <time.h>
#include <unistd.h>
error_t
ports_destroy_right (void *portstruct)
{
struct port_info *pi = portstruct;
mach_port_t port_right;
int defer = 0;
error_t err;
pthread_mutex_lock (&_ports_lock);
port_right = pi->port_right;
pi->port_right = MACH_PORT_DEAD;
if (pi->flags & PORT_HAS_SENDRIGHTS)
{
pi->flags &= ~PORT_HAS_SENDRIGHTS;
defer = 1;
}
if (MACH_PORT_VALID (port_right))
{
mach_port_clear_protected_payload (mach_task_self (), port_right);
pthread_rwlock_wrlock (&_ports_htable_lock);
hurd_ihash_locp_remove (&_ports_htable, pi->ports_htable_entry);
hurd_ihash_locp_remove (&pi->bucket->htable, pi->hentry);
pthread_rwlock_unlock (&_ports_htable_lock);
}
pthread_mutex_unlock (&_ports_lock);
if (MACH_PORT_VALID (port_right))
{
err = mach_port_mod_refs (mach_task_self (), port_right,
MACH_PORT_RIGHT_RECEIVE, -1);
assert_perror_backtrace (err);
}
if (defer)
_ports_port_deref_deferred (pi);
return 0;
}