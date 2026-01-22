#ifndef	_KERNEL_I386_EFLAGS_H_
#define	_KERNEL_I386_EFLAGS_H_
#include <mach/machine/eflags.h>
#define	EFL_USER_SET	(EFL_IF)
#define	EFL_USER_CLEAR	(EFL_IOPL|EFL_NT|EFL_RF)
#endif