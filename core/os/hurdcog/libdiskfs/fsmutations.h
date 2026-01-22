#define FILE_INTRAN protid_t diskfs_begin_using_protid_port (file_t)
#define FILE_INTRAN_PAYLOAD protid_t diskfs_begin_using_protid_payload
#define FILE_DESTRUCTOR diskfs_end_using_protid_port (protid_t)
#define IO_INTRAN protid_t diskfs_begin_using_protid_port (io_t)
#define IO_INTRAN_PAYLOAD protid_t diskfs_begin_using_protid_payload
#define IO_DESTRUCTOR diskfs_end_using_protid_port (protid_t)
#define FSYS_INTRAN control_t diskfs_begin_using_control_port (fsys_t)
#define FSYS_INTRAN_PAYLOAD control_t diskfs_begin_using_control_port_payload
#define FSYS_DESTRUCTOR diskfs_end_using_control_port (control_t)
#define FILE_IMPORTS import "libdiskfs/priv.h";
#define IO_IMPORTS import "libdiskfs/priv.h";
#define FSYS_IMPORTS import "libdiskfs/priv.h";
#define IFSOCK_IMPORTS import "libdiskfs/priv.h";
#define EXEC_STARTUP_INTRAN \
bootinfo_t diskfs_begin_using_bootinfo_port (exec_startup_t)
#define EXEC_STARTUP_INTRAN_PAYLOAD \
bootinfo_t diskfs_begin_using_bootinfo_payload
#define EXEC_STARTUP_DESTRUCTOR \
diskfs_end_using_bootinfo (bootinfo_t)
#define EXEC_STARTUP_IMPORTS \
import "libdiskfs/priv.h";