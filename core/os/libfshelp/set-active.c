#include "fshelp.h"
#include <hurd/fsys.h>
error_t
fshelp_set_active (struct transbox *box,
mach_port_t active,
int excl)
{
int cancel;
if (excl)
{
if (box->flags & TRANSBOX_STARTING)
return EBUSY;
if (box->active != MACH_PORT_NULL)
{
mach_port_urefs_t dead_refs;
error_t err =
mach_port_get_refs (mach_task_self (),
box->active, MACH_PORT_RIGHT_DEAD_NAME,
&dead_refs);
if (!err && dead_refs == 0)
return EBUSY;
}
}
while (box->flags & TRANSBOX_STARTING)
{
box->flags |= TRANSBOX_WANTED;
cancel = pthread_hurd_cond_wait_np (&box->wakeup, box->lock);
if (cancel)
return EINTR;
}
if (box->active != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), box->active);
box->active = active;
return 0;
}