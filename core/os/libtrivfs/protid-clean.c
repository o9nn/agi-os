#include "priv.h"
void
trivfs_clean_protid (void *arg)
{
struct trivfs_protid *cred = arg;
struct trivfs_control *cntl = cred->po->cntl;
if (trivfs_protid_destroy_hook && cred->realnode != MACH_PORT_NULL)
(*trivfs_protid_destroy_hook) (cred);
if (trivfs_peropen_destroy_hook)
{
if (refcount_deref (&cred->po->refcnt) == 0)
{
refcount_unsafe_ref (&cred->po->refcnt);
(*trivfs_peropen_destroy_hook) (cred->po);
if (refcount_deref (&cred->po->refcnt) == 0)
{
ports_port_deref (cntl);
free (cred->po);
}
}
}
else
if (refcount_deref (&cred->po->refcnt) == 0)
{
ports_port_deref (cntl);
free (cred->po);
}
iohelp_free_iouser (cred->user);
if (cred->realnode != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), cred->realnode);
}