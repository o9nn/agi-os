#include "netfs.h"
struct protid *
netfs_make_protid (struct peropen *po, struct iouser *cred)
{
struct protid *pi;
if (cred)
errno = ports_create_port (netfs_protid_class, netfs_port_bucket,
sizeof (struct protid), &pi);
else
errno = ports_create_port_noinstall (netfs_protid_class,
netfs_port_bucket,
sizeof (struct protid), &pi);
if (errno)
return 0;
pi->po = po;
pi->user = cred;
pi->shared_object = MACH_PORT_NULL;
pi->mapped = 0;
return pi;
}