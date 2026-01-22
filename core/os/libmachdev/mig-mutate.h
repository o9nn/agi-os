#define NOTIFY_INTRAN \
port_info_t begin_using_port_info_port (mach_port_t)
#define NOTIFY_INTRAN_PAYLOAD \
port_info_t begin_using_port_info_payload
#define NOTIFY_DESTRUCTOR \
end_using_port_info (port_info_t)
#define NOTIFY_IMPORTS \
import "libports/mig-decls.h";
#define DEVICE_INTRAN \
mach_device_t begin_using_device_port (mach_port_t)
#define DEVICE_INTRAN_PAYLOAD \
mach_device_t begin_using_device_payload
#define DEVICE_DESTRUCTOR \
end_using_device (mach_device_t)
#define DEVICE_IMPORTS \
import "libmachdev/mig-decls.h";