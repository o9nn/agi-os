#include "ports.h"
#include "notify_S.h"
kern_return_t
ports_do_mach_notify_port_destroyed (struct port_info *pi,
mach_port_t name)
{
return EOPNOTSUPP;
}