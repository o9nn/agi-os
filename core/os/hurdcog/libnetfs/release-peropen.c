#include <sys/file.h>
#include "netfs.h"
void
netfs_release_peropen (struct peropen *po)
{
if (refcount_deref (&po->refcnt) > 0)
return;
pthread_mutex_lock (&po->np->lock);
if (po->root_parent)
mach_port_deallocate (mach_task_self (), po->root_parent);
if (po->shadow_root && po->shadow_root != po->np)
{
pthread_mutex_lock (&po->shadow_root->lock);
netfs_nput (po->shadow_root);
}
if (po->shadow_root_parent)
mach_port_deallocate (mach_task_self (), po->shadow_root_parent);
fshelp_rlock_drop_peropen (&po->lock_status);
fshelp_rlock_po_fini (&po->lock_status);
netfs_nput (po->np);
free (po->path);
free (po);
}