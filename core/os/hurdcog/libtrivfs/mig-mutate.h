#define REPLY_PORTS
#define FILE_INTRAN trivfs_protid_t trivfs_begin_using_protid (file_t)
#define FILE_INTRAN_PAYLOAD trivfs_protid_t trivfs_begin_using_protid_payload
#define FILE_DESTRUCTOR trivfs_end_using_protid (trivfs_protid_t)
#define FILE_IMPORTS import "libtrivfs/mig-decls.h";
#define IO_INTRAN trivfs_protid_t trivfs_begin_using_protid (io_t)
#define IO_INTRAN_PAYLOAD trivfs_protid_t trivfs_begin_using_protid_payload
#define IO_DESTRUCTOR trivfs_end_using_protid (trivfs_protid_t)
#define IO_IMPORTS import "libtrivfs/mig-decls.h";
#define FSYS_INTRAN trivfs_control_t trivfs_begin_using_control (fsys_t)
#define FSYS_INTRAN_PAYLOAD trivfs_control_t trivfs_begin_using_control_payload
#define FSYS_DESTRUCTOR trivfs_end_using_control (trivfs_control_t)
#define FSYS_IMPORTS import "libtrivfs/mig-decls.h";