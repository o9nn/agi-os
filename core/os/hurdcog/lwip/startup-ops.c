#include <lwip_startup_notify_S.h>
#include <lwip-hurd.h>
error_t
lwip_S_startup_dosync (mach_port_t handle)
{
struct port_info *inpi = ports_lookup_port (lwip_bucket, handle,
shutdown_notify_class);
if (!inpi)
return EOPNOTSUPP;
ports_class_iterate (socketport_class, ports_destroy_right);
ports_class_iterate (addrport_class, ports_destroy_right);
return 0;
}