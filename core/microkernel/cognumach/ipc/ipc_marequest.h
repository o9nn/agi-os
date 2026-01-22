#ifndef	_IPC_IPC_MAREQUEST_H_
#define _IPC_IPC_MAREQUEST_H_
#include <mach/kern_return.h>
#include <mach/port.h>
#include <mach_debug/hash_info.h>
#include <ipc/ipc_types.h>
typedef struct ipc_marequest {
struct ipc_space *imar_space;
mach_port_name_t imar_name;
struct ipc_port *imar_soright;
struct ipc_marequest *imar_next;
} *ipc_marequest_t;
#define	IMAR_NULL		((ipc_marequest_t) 0)
#define	IPC_MAREQUEST_SIZE	16
extern void
ipc_marequest_init(void);
#if	MACH_IPC_DEBUG
extern unsigned int
ipc_marequest_info(unsigned int *, hash_info_bucket_t *, unsigned int);
#endif
extern mach_msg_return_t
ipc_marequest_create(ipc_space_t space, ipc_port_t port,
mach_port_name_t notify, ipc_marequest_t *marequestp);
extern void
ipc_marequest_cancel(ipc_space_t space, mach_port_name_t name);
extern void
ipc_marequest_rename(ipc_space_t space,
mach_port_name_t old, mach_port_name_t new);
extern void
ipc_marequest_destroy(ipc_marequest_t marequest);
#endif