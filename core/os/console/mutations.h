#define IO_INTRAN protid_t begin_using_protid_port (io_t)
#define IO_INTRAN_PAYLOAD protid_t begin_using_protid_payload
#define IO_DESTRUCTOR end_using_protid_port (protid_t)
#define TIOCTL_IMPORTS import "libnetfs/priv.h";
#define NOTIFY_INTRAN \
port_info_t begin_using_port_info_port (mach_port_t)
#define NOTIFY_INTRAN_PAYLOAD \
port_info_t begin_using_port_info_payload
#define NOTIFY_DESTRUCTOR \
end_using_port_info (port_info_t)
#define NOTIFY_IMPORTS \
import "libports/mig-decls.h";