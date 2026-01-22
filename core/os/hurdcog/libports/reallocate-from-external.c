#include "ports.h"
#include <assert-backtrace.h>
#include <hurd/ihash.h>
#include <mach/notify.h>
void
ports_reallocate_from_external (void *portstruct, mach_port_t receive)
{
struct port_info *pi = portstruct;
mach_port_status_t stat;
int dropref = 0;
mach_port_t foo;
error_t err;
err = mach_port_get_receive_status (mach_task_self (), receive, &stat);
assert_perror_backtrace (err);
pthread_mutex_lock (&_ports_lock);
assert_backtrace (pi->port_right);
err = mach_port_mod_refs (mach_task_self (), pi->port_right,
MACH_PORT_RIGHT_RECEIVE, -1);
assert_perror_backtrace (err);
pthread_rwlock_wrlock (&_ports_htable_lock);
hurd_ihash_locp_remove (&_ports_htable, pi->ports_htable_entry);
hurd_ihash_locp_remove (&pi->bucket->htable, pi->hentry);
pthread_rwlock_unlock (&_ports_htable_lock);
if ((pi->flags & PORT_HAS_SENDRIGHTS) && !stat.mps_srights)
{
dropref = 1;
pi->flags &= ~PORT_HAS_SENDRIGHTS;
}
else if (((pi->flags & PORT_HAS_SENDRIGHTS) == 0) && stat.mps_srights)
{
pi->flags |= PORT_HAS_SENDRIGHTS;
refcounts_ref (&pi->refcounts, NULL);
}
pi->port_right = receive;
pi->cancel_threshold = 0;
pi->mscount = stat.mps_mscount;
pthread_rwlock_wrlock (&_ports_htable_lock);
err = hurd_ihash_add (&_ports_htable, receive, pi);
assert_perror_backtrace (err);
err = hurd_ihash_add (&pi->bucket->htable, receive, pi);
pthread_rwlock_unlock (&_ports_htable_lock);
pthread_mutex_unlock (&_ports_lock);
assert_perror_backtrace (err);
mach_port_set_protected_payload (mach_task_self (), pi->port_right,
(unsigned long) pi);
mach_port_move_member (mach_task_self (), receive, pi->bucket->portset);
if (stat.mps_srights)
{
err = mach_port_request_notification (mach_task_self (), receive,
MACH_NOTIFY_NO_SENDERS,
stat.mps_mscount, receive,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&foo);
assert_perror_backtrace (err);
if (foo != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), foo);
}
if (dropref)
ports_port_deref (pi);
}