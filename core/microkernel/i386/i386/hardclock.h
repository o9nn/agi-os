#ifndef _I386_HARDCLOCK_H_
#define _I386_HARDCLOCK_H_
void hardclock(
int iunit,
int old_ipl,
const char *ret_addr,
struct i386_interrupt_state *regs);
#endif