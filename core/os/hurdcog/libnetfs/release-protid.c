#include "netfs.h"
#include <sys/mman.h>
void
netfs_release_protid (void *arg)
{
struct protid *user = arg;
iohelp_free_iouser (user->user);
if (user->shared_object)
mach_port_deallocate (mach_task_self (), user->shared_object);
if (user->mapped)
munmap (user->mapped, vm_page_size);
netfs_release_peropen (user->po);
}