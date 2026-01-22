#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_reauthenticate (struct protid *cred,
mach_port_t rend_port)
{
struct protid *newcred;
error_t err;
mach_port_t newright;
struct iouser *user;
if (cred == 0)
return EOPNOTSUPP;
pthread_mutex_lock (&cred->po->np->lock);
refcount_ref (&cred->po->refcnt);
do
err = diskfs_start_protid (cred->po, &newcred);
while (err == EINTR);
if (err)
{
refcount_deref (&cred->po->refcnt);
pthread_mutex_unlock (&cred->po->np->lock);
return err;
}
newright = ports_get_send_right (newcred);
assert_backtrace (newright != MACH_PORT_NULL);
pthread_mutex_unlock (&cred->po->np->lock);
err = iohelp_reauth (&user, diskfs_auth_server_port, rend_port,
newright, 1);
pthread_mutex_lock (&cred->po->np->lock);
if (! err)
{
diskfs_finish_protid (newcred, user);
iohelp_free_iouser (user);
mach_port_deallocate (mach_task_self (), rend_port);
}
mach_port_deallocate (mach_task_self (), newright);
pthread_mutex_unlock (&cred->po->np->lock);
ports_port_deref (newcred);
return err;
}