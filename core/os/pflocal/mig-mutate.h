#define IO_SELECT_REPLY_PORT
#define IO_INTRAN sock_user_t begin_using_sock_user_port (io_t)
#define IO_INTRAN_PAYLOAD sock_user_t begin_using_sock_user_payload
#define IO_DESTRUCTOR end_using_sock_user_port (sock_user_t)
#define IO_IMPORTS import "mig-decls.h";
#define FILE_INTRAN sock_user_t begin_using_sock_user_port (io_t)
#define FILE_INTRAN_PAYLOAD sock_user_t begin_using_sock_user_payload
#define FILE_DESTRUCTOR end_using_sock_user_port (sock_user_t)
#define FILE_IMPORTS import "mig-decls.h";
#define SOCKET_INTRAN sock_user_t begin_using_sock_user_port (socket_t)
#define SOCKET_INTRAN_PAYLOAD sock_user_t begin_using_sock_user_payload
#define SOCKET_DESTRUCTOR end_using_sock_user_port (sock_user_t)
#define ADDRPORT_INTRAN addr_t begin_using_addr_port (addr_port_t)
#define ADDRPORT_INTRAN_PAYLOAD addr_t begin_using_addr_payload
#define ADDRPORT_DESTRUCTOR end_using_addr_port (addr_t)
#define SOCKET_IMPORTS \
import "mig-decls.h"; \
import "../libtrivfs/mig-decls.h"; \
#define PF_INTRAN trivfs_protid_t trivfs_begin_using_protid (pf_t)
#define PF_INTRAN_PAYLOAD trivfs_protid_t trivfs_begin_using_protid_payload
#define PF_DESTRUCTOR trivfs_end_using_protid (trivfs_protid_t)