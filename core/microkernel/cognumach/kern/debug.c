#include <mach/xen.h>
#include <kern/printf.h>
#include <stdarg.h>
#include "cpu_number.h"
#include <kern/lock.h>
#include <kern/thread.h>
#include <kern/debug.h>
#include <kern/constants.h>
#include <machine/loose_ends.h>
#include <machine/model_dep.h>
#include <device/cons.h>
#if NCPUS > 1
simple_lock_irq_data_t Assert_print_lock;
#endif
static void
do_cnputc(char c, vm_offset_t offset)
{
cnputc(c);
}
void
Assert(const char *exp, const char *file, int line, const char *fun)
{
#if NCPUS > 1
spl_t s = simple_lock_irq(&Assert_print_lock);
printf("{cpu%d} %s:%d: %s: Assertion `%s' failed.",
cpu_number(), file, line, fun, exp);
simple_unlock_irq(s, &Assert_print_lock);
#else
printf("%s:%d: %s: Assertion `%s' failed.",
file, line, fun, exp);
#endif
Debugger("assertion failure");
}
void SoftDebugger(const char *message)
{
printf("Debugger invoked: %s\n", message);
#if	!MACH_KDB
printf("But no debugger, continuing.\n");
return;
#endif
#if	defined(vax) || defined(PC532)
asm("bpt");
#endif
#ifdef	sun3
current_thread()->pcb->flag |= TRACE_KDB;
asm("orw  #0x00008000,sr");
#endif
#ifdef	sun4
current_thread()->pcb->pcb_flag |= TRACE_KDB;
asm("ta 0x81");
#endif
#if	defined(mips ) || defined(i860) || defined(alpha)
gimmeabreak();
#endif
#if defined(__i386__) || defined(__x86_64__)
asm("int3");
#endif
}
void Debugger(const char *message)
{
#if	!MACH_KDB
panic("Debugger invoked, but there isn't one!");
#endif
SoftDebugger(message);
panic("Debugger returned!");
}
def_simple_lock_irq_data(static,	panic_lock)
const char     		*panicstr;
int			paniccpu;
void
panic_init(void)
{
}
#if ! MACH_KBD
extern boolean_t reboot_on_panic;
#endif
void
Panic(const char *file, int line, const char *fun, const char *s, ...)
{
va_list	listp;
spl_t spl;
panic_init();
spl = simple_lock_irq(&panic_lock);
if (panicstr) {
if (cpu_number() != paniccpu) {
simple_unlock_irq(spl, &panic_lock);
halt_cpu();
}
}
else {
panicstr = s;
paniccpu = cpu_number();
}
simple_unlock_irq(spl, &panic_lock);
printf("panic ");
#if	NCPUS > 1
printf("{cpu%d} ", paniccpu);
#endif
printf("%s:%d: %s: ",file, line, fun);
va_start(listp, s);
_doprnt(s, listp, do_cnputc, 16, 0);
va_end(listp);
printf("\n");
#if	MACH_KDB
Debugger("panic");
#else
# ifdef	MACH_HYP
hyp_crash();
# else
{
int i = 1000;
while (i--)
delay (MICROSECONDS_PER_SECOND);
}
halt_all_cpus (reboot_on_panic);
# endif
#endif
}
void
log(int level, const char *fmt, ...)
{
va_list	listp;
va_start(listp, fmt);
_doprnt(fmt, listp, do_cnputc, 16, 0);
va_end(listp);
}
#define STACK_CHK_GUARD_CANARY 0xff
unsigned char __stack_chk_guard [ sizeof (vm_offset_t) ] =
{
[ sizeof (vm_offset_t) - 3 ] = '\r',
[ sizeof (vm_offset_t) - 2 ] = '\n',
[ sizeof (vm_offset_t) - 1 ] = STACK_CHK_GUARD_CANARY,
};
void __stack_chk_fail (void);
void
__stack_chk_fail (void)
{
panic("stack smashing detected");
}