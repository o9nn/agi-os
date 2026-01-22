#define NOTIFY_INTRAN \
port_info_t begin_using_port_info_port (mach_port_t)
#define NOTIFY_INTRAN_PAYLOAD \
port_info_t begin_using_port_info_payload
#define NOTIFY_DESTRUCTOR \
end_using_port_info (port_info_t)
#define NOTIFY_IMPORTS \
import "libports/mig-decls.h";
#define DEVICE_IMPORTS \
import "libports/ports.h";