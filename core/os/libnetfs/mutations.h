#define IO_SELECT_REPLY_PORT
#define FILE_INTRAN protid_t begin_using_protid_port (file_t)
#define FILE_INTRAN_PAYLOAD protid_t begin_using_protid_payload
#define FILE_DESTRUCTOR end_using_protid_port (protid_t)
#define IO_INTRAN protid_t begin_using_protid_port (io_t)
#define IO_INTRAN_PAYLOAD protid_t begin_using_protid_payload
#define IO_DESTRUCTOR end_using_protid_port (protid_t)
#define FSYS_INTRAN control_t begin_using_control_port (fsys_t)
#define FSYS_INTRAN_PAYLOAD control_t begin_using_control_payload
#define FSYS_DESTRUCTOR end_using_control_port (control_t)
#define FILE_IMPORTS import "libnetfs/netfs.h"; import "libnetfs/priv.h";
#define IO_IMPORTS import "libnetfs/netfs.h"; import "libnetfs/priv.h";
#define FSYS_IMPORTS import "libnetfs/netfs.h"; import "libnetfs/priv.h";
#define IFSOCK_IMPORTS import "libnetfs/netfs.h"; import "libnetfs/priv.h";