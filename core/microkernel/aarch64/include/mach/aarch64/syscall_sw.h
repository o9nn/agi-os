#ifndef _MACH_AARCH64_SYSCALL_SW_H_
#define _MACH_AARCH64_SYSCALL_SW_H_
#include <mach/machine/asm.h>
#define kernel_trap(trap_name,trap_number,number_args) \
ENTRY(trap_name) \
MACH_BTI_C; \
mov w8, #(trap_number); \
SVC; \
ret; \
END(trap_name)
#endif