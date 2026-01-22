#include "ports.h"
#include <mach/notify.h>
error_t
ports_request_dead_name_notification (void *object, mach_port_t name,
mach_port_t *previous)
{
error_t err;
mach_port_t notify_port;
mach_port_t prev;
if (object)
notify_port = ports_port_notify_right (object);
else
notify_port = MACH_PORT_NULL;
err = mach_port_request_notification (mach_task_self (), name,
MACH_NOTIFY_DEAD_NAME, 1,
notify_port,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&prev);
if (err)
return err;
if (previous != NULL)
*previous = prev;
else if (MACH_PORT_VALID (prev))
{
err = mach_port_deallocate (mach_task_self (), prev);
assert_perror_backtrace (err);
}
return 0;
}