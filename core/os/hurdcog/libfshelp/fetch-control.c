#include "fshelp.h"
#include <assert-backtrace.h>
error_t
fshelp_fetch_control (struct transbox *box,
mach_port_t *control)
{
error_t err = 0;
*control = box->active;
if (*control != MACH_PORT_NULL)
err = mach_port_mod_refs (mach_task_self (), *control,
MACH_PORT_RIGHT_SEND, 1);
if (err == KERN_INVALID_RIGHT)
{
err = mach_port_deallocate (mach_task_self (), *control);
assert_perror_backtrace (err);
*control = box->active = MACH_PORT_NULL;
}
return err;
}