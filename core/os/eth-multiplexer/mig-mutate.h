#define DEVICE_INTRAN \
vether_device_t begin_using_device_port (mach_port_t)
#define DEVICE_INTRAN_PAYLOAD \
vether_device_t begin_using_device_payload
#define DEVICE_DESTRUCTOR \
end_using_device (vether_device_t)
#define DEVICE_IMPORTS \
import "eth-multiplexer/mig-decls.h";