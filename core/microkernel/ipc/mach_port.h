#ifndef _IPC_MACH_PORT_H_
#define _IPC_MACH_PORT_H_
#include <sys/types.h>
#include <ipc/ipc_types.h>
#include <ipc/ipc_entry.h>
#if	MACH_KDB
void db_debug_port_references (boolean_t enable);
#endif
#endif