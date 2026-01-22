#include "ports.h"
#include <assert-backtrace.h>
#include <hurd/ihash.h>
void
_ports_complete_deallocate (struct port_info *pi)
{
assert_backtrace ((pi->flags & PORT_HAS_SENDRIGHTS) == 0);
if (MACH_PORT_VALID (pi->port_right))
{
struct references result;
pthread_rwlock_wrlock (&_ports_htable_lock);
refcounts_references (&pi->refcounts, &result);
if (result.hard > 0 || result.weak > 0)
{
assert_backtrace (! "reacquired reference w/o send rights");
pthread_rwlock_unlock (&_ports_htable_lock);
return;
}
hurd_ihash_locp_remove (&_ports_htable, pi->ports_htable_entry);
hurd_ihash_locp_remove (&pi->bucket->htable, pi->hentry);
pthread_rwlock_unlock (&_ports_htable_lock);
mach_port_mod_refs (mach_task_self (), pi->port_right,
MACH_PORT_RIGHT_RECEIVE, -1);
pi->port_right = MACH_PORT_NULL;
}
pthread_mutex_lock (&_ports_lock);
pi->bucket->count--;
pi->class->count--;
pthread_mutex_unlock (&_ports_lock);
if (pi->class->clean_routine)
(*pi->class->clean_routine)(pi);
assert_backtrace (pi->current_rpcs == NULL);
free (pi);
}