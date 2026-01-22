#include <string.h>
#include "priv.h"
error_t
trivfs_open (struct trivfs_control *cntl,
struct iouser *user,
unsigned flags,
mach_port_t realnode,
struct trivfs_protid **cred)
{
error_t err = 0;
struct trivfs_peropen *po = malloc (sizeof (struct trivfs_peropen));
if (!po)
return ENOMEM;
ports_port_ref (cntl);
refcount_init (&po->refcnt, 1);
po->cntl = cntl;
po->openmodes = flags;
po->hook = 0;
if (trivfs_peropen_create_hook)
err = (*trivfs_peropen_create_hook) (po);
if (!err)
{
struct trivfs_protid *new;
err = ports_create_port (cntl->protid_class, cntl->protid_bucket,
sizeof (struct trivfs_protid), &new);
if (! err)
{
new->user = user;
new->isroot = _is_privileged (user->uids);
new->po = po;
new->hook = 0;
new->realnode = realnode;
if (!err && trivfs_protid_create_hook)
err = (*trivfs_protid_create_hook) (new);
if (err)
{
new->realnode = MACH_PORT_NULL;
ports_port_deref (new);
}
else
*cred = new;
}
}
if (err)
{
ports_port_deref (cntl);
free (po);
}
return err;
}