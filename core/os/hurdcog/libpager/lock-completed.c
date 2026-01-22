#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
kern_return_t
_pager_S_memory_object_lock_completed (struct pager *p,
mach_port_t control,
vm_offset_t offset,
vm_size_t length)
{
error_t err = 0;
struct lock_request *lr;
if (!p
|| p->port.class != _pager_class)
return EOPNOTSUPP;
pthread_mutex_lock (&p->interlock);
if (control != p->memobjcntl)
{
printf ("lock_completed: bad control port\n");
err = EPERM;
goto out;
}
mach_port_deallocate (mach_task_self (), control);
for (lr = p->lock_requests; lr; lr = lr->next)
if (lr->start == offset && lr->end == offset + length)
{
if (lr->locks_pending)
--lr->locks_pending;
if (!lr->locks_pending && !lr->pending_writes)
pthread_cond_broadcast (&p->wakeup);
break;
}
out:
pthread_mutex_unlock (&p->interlock);
return err;
}