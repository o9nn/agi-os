#include "ports.h"
#include "notify_S.h"
kern_return_t
ports_do_mach_notify_dead_name (struct port_info *pi,
mach_port_t dead_name)
{
if (!ports_port_is_notify (pi))
return EOPNOTSUPP;
ports_dead_name (pi, dead_name);
mach_port_deallocate (mach_task_self (), dead_name);
return 0;
}