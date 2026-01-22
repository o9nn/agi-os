#define IO_INTRAN trivfs_protid_t trivfs_begin_using_protid (io_t)
#define IO_INTRAN_PAYLOAD trivfs_protid_t trivfs_begin_using_protid_payload
#define IO_DESTRUCTOR trivfs_end_using_protid (trivfs_protid_t)
#define IO_OUTTRAN io_t trivfs_convert_to_port (trivfs_protid_t)
#define CTTY_INTRAN \
port_info_t begin_using_ctty_port (mach_port_t)
#define CTTY_INTRAN_PAYLOAD \
port_info_t begin_using_ctty_payload
#define CTTY_DESTRUCTOR \
end_using_ctty (port_info_t)
#define TIOCTL_IMPORTS import "../libtrivfs/mig-decls.h";
#define TERM_IMPORTS \
import "../libtrivfs/mig-decls.h"; \
import "mig-decls.h";