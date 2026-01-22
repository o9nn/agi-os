#include <sys/file.h>
#include "priv.h"
void
diskfs_release_peropen (struct peropen *po)
{
if (refcount_deref (&po->refcnt) > 0)
return;
if (po->root_parent)
mach_port_deallocate (mach_task_self (), po->root_parent);
if (po->shadow_root && po->shadow_root != po->np)
diskfs_nrele (po->shadow_root);
if (po->shadow_root_parent)
mach_port_deallocate (mach_task_self (), po->shadow_root_parent);
pthread_mutex_lock (&po->np->lock);
fshelp_rlock_drop_peropen (&po->lock_status);
diskfs_nput (po->np);
fshelp_rlock_po_fini (&po->lock_status);
free (po->path);
free (po);
}