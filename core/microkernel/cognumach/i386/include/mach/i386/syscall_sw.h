#ifndef _MACH_I386_SYSCALL_SW_H_
#define _MACH_I386_SYSCALL_SW_H_
#include <mach/machine/asm.h>
#define kernel_trap(trap_name,trap_number,number_args) \
ENTRY(trap_name) \
movl $ trap_number,%eax; \
SVC; \
ret; \
END(trap_name)
#endif