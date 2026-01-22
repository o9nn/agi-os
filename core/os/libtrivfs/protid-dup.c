#include <string.h>
#include "priv.h"
error_t
trivfs_protid_dup (struct trivfs_protid *cred, struct trivfs_protid **dup)
{
struct trivfs_protid *new;
error_t err = ports_create_port (cred->po->cntl->protid_class,
cred->po->cntl->protid_bucket,
sizeof (struct trivfs_protid),
&new);
if (! err)
{
new->po = cred->po;
refcount_ref (&new->po->refcnt);
new->isroot = cred->isroot;
err = iohelp_dup_iouser (&new->user, cred->user);
if (err)
{
ports_port_deref (new);
return err;
}
new->realnode = cred->realnode;
mach_port_mod_refs (mach_task_self (), new->realnode,
MACH_PORT_RIGHT_SEND, 1);
new->hook = cred->hook;
if (trivfs_protid_create_hook)
err = (*trivfs_protid_create_hook) (new);
if (err)
{
mach_port_deallocate (mach_task_self (), new->realnode);
new->realnode = MACH_PORT_NULL;
ports_port_deref (new);
}
else
*dup = new;
}
return err;
}