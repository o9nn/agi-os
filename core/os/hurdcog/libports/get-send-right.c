#include "ports.h"
#include <assert-backtrace.h>
mach_port_t
ports_get_send_right (void *port)
{
error_t err;
mach_port_t right;
right = ports_get_right (port);
if (right == MACH_PORT_NULL)
return MACH_PORT_NULL;
err = mach_port_insert_right (mach_task_self (),
right, right, MACH_MSG_TYPE_MAKE_SEND);
assert_perror_backtrace (err);
return right;
}