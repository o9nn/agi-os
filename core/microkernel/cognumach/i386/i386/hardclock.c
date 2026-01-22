#include <mach/machine/eflags.h>
#include <kern/mach_clock.h>
#include <i386/thread.h>
#include <i386/hardclock.h>
#if	defined(AT386) || defined(ATX86_64)
#include <i386/ipl.h>
#endif
#ifdef LINUX_DEV
#include <linux/dev/glue/glue.h>
#endif
extern char	return_to_iret[];
void
hardclock(int iunit,
int old_ipl,
const char *ret_addr,
struct i386_interrupt_state *regs
)
{
if (ret_addr == return_to_iret)
clock_interrupt(tick,
(regs->efl & EFL_VM) ||
((regs->cs & 0x03) != 0),
#if defined(LINUX_DEV)
FALSE,
#else
old_ipl == SPL0,
#endif
regs->eip);
else
clock_interrupt(tick,
FALSE,
FALSE,
0);
#ifdef LINUX_DEV
linux_timer_intr();
#endif
}