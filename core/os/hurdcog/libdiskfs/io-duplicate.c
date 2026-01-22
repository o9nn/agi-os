#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_duplicate (struct protid *cred,
mach_port_t *port,
mach_msg_type_name_t *portpoly)
{
error_t err;
struct protid *newpi;
if (!cred)
return EOPNOTSUPP;
pthread_mutex_lock (&cred->po->np->lock);
refcount_ref (&cred->po->refcnt);
err = diskfs_create_protid (cred->po, cred->user, &newpi);
if (! err)
{
*port = ports_get_right (newpi);
*portpoly = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
}
else
refcount_deref (&cred->po->refcnt);
pthread_mutex_unlock (&cred->po->np->lock);
return err;
}