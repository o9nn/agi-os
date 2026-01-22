#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_reauthenticate (struct protid *user, mach_port_t rend_port)
{
error_t err;
struct protid *newpi;
mach_port_t newright;
if (!user)
return EOPNOTSUPP;
refcount_ref (&user->po->refcnt);
pthread_mutex_lock (&user->po->np->lock);
do
newpi = netfs_make_protid (user->po, 0);
while (! newpi && errno == EINTR);
if (! newpi)
{
refcount_deref (&user->po->refcnt);
pthread_mutex_unlock (&user->po->np->lock);
return errno;
}
newright = ports_get_send_right (newpi);
assert_backtrace (newright != MACH_PORT_NULL);
pthread_mutex_unlock (&user->po->np->lock);
err = iohelp_reauth (&newpi->user, netfs_auth_server_port, rend_port,
newright, 1);
pthread_mutex_lock (&user->po->np->lock);
if (!err)
mach_port_deallocate (mach_task_self (), rend_port);
mach_port_deallocate (mach_task_self (), newright);
mach_port_move_member (mach_task_self (), newpi->pi.port_right,
netfs_port_bucket->portset);
pthread_mutex_unlock (&user->po->np->lock);
ports_port_deref (newpi);
return err;
}