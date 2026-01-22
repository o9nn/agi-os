#ifndef _IPC_PRINT_H_
#define	_IPC_PRINT_H_
#if MACH_KDB
#include <mach/mach_types.h>
#include <mach/message.h>
#include <ipc/ipc_types.h>
#include <ipc/ipc_pset.h>
extern void ipc_port_print(const ipc_port_t);
extern void ipc_pset_print(const ipc_pset_t);
extern void ipc_kmsg_print(const ipc_kmsg_t);
extern void ipc_msg_print(mach_msg_header_t*);
#endif
#endif