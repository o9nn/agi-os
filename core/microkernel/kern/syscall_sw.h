#ifndef	_KERN_SYSCALL_SW_H_
#define	_KERN_SYSCALL_SW_H_
#include <mach/boolean.h>
typedef void (*generic_trap_function)(void);
typedef struct {
int		mach_trap_arg_count;
generic_trap_function mach_trap_function;
boolean_t	mach_trap_stack;
const char	*mach_trap_name;
} mach_trap_t;
extern mach_trap_t	mach_trap_table[];
extern int		mach_trap_count;
#define	MACH_TRAP(name, arg_count)		\
{ (arg_count), (generic_trap_function) (name), FALSE, #name }
#define	MACH_TRAP_STACK(name, arg_count)	\
{ (arg_count), (generic_trap_function) (name), TRUE, #name }
#endif