#ifndef	_MACH_MACH_TRAPS_H_
#define _MACH_MACH_TRAPS_H_
#include <mach/port.h>
mach_port_name_t mach_reply_port (void);
mach_port_name_t mach_thread_self (void);
mach_port_name_t mach_task_self (void);
mach_port_name_t mach_host_self (void);
#endif