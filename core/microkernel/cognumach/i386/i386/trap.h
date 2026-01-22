#ifndef	_I386_TRAP_H_
#define	_I386_TRAP_H_
#include <mach/machine/trap.h>
#ifndef __ASSEMBLER__
#include <i386/thread.h>
#include <mach/mach_types.h>
char *trap_name(unsigned int trapnum);
unsigned int interrupted_pc(thread_t);
void
i386_exception(
int	exc,
int	code,
long	subcode) __attribute__ ((noreturn));
extern void
thread_kdb_return(void);
void kernel_trap(struct i386_saved_state *regs);
int user_trap(struct i386_saved_state *regs);
void i386_astintr(void);
#endif
#endif