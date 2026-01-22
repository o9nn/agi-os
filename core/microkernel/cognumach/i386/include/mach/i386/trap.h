#ifndef	_MACH_I386_TRAP_H_
#define	_MACH_I386_TRAP_H_
#define	T_DIVIDE_ERROR		0
#define	T_DEBUG			1
#define	T_NMI			2
#define	T_INT3			3
#define	T_OVERFLOW		4
#define	T_OUT_OF_BOUNDS		5
#define	T_INVALID_OPCODE	6
#define	T_NO_FPU		7
#define	T_DOUBLE_FAULT		8
#define	T_FPU_FAULT		9
#define	T_SEGMENT_NOT_PRESENT	11
#define	T_STACK_FAULT		12
#define	T_GENERAL_PROTECTION	13
#define	T_PAGE_FAULT		14
#define	T_FLOATING_POINT_ERROR	16
#define	T_WATCHPOINT		17
#define	T_PF_PROT		0x1
#define	T_PF_WRITE		0x2
#define	T_PF_USER		0x4
#endif