#ifndef	_MACH_X86_64_SYSCALL_SW_H_
#define _MACH_X86_64_SYSCALL_SW_H_
#include <mach/machine/asm.h>
#define kernel_trap(trap_name,trap_number,number_args)  \
ENTRY(trap_name)					\
movq	$ trap_number,%rax;			\
movq	%rcx,%r10;				\
syscall;					\
ret;						\
END(trap_name)
#endif