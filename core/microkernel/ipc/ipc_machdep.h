#ifndef _IPC_IPC_MACHDEP_H_
#define _IPC_IPC_MACHDEP_H_
#include <mach/message.h>
#define PORT_T_SIZE_IN_BITS (sizeof(mach_port_t)*8)
#define PORT_NAME_T_SIZE_IN_BITS (sizeof(mach_port_name_t)*8)
#endif