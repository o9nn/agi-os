#include "priv.h"
void
diskfs_protid_rele (void *arg)
{
struct protid *cred = arg;
iohelp_free_iouser (cred->user);
if (cred->shared_object)
mach_port_deallocate (mach_task_self (), cred->shared_object);
if (cred->mapped)
munmap (cred->mapped, vm_page_size);
diskfs_release_peropen (cred->po);
}