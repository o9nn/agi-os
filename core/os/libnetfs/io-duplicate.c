#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_duplicate (struct protid *user,
mach_port_t *newport,
mach_msg_type_name_t *newporttp)
{
error_t err;
struct protid *newpi;
struct iouser *clone;
err = iohelp_dup_iouser (&clone, user->user);
if (err)
return err;
refcount_ref (&user->po->refcnt);
pthread_mutex_lock (&user->po->np->lock);
newpi = netfs_make_protid (user->po, clone);
*newport = ports_get_right (newpi);
pthread_mutex_unlock (&user->po->np->lock);
*newporttp = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
return 0;
}