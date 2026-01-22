#ifndef _X86_CPU_H
#define _X86_CPU_H
#include <kern/macros.h>
#define CPU_EFL_ONE 0x00000002
#define CPU_EFL_IF 0x00000200
static __always_inline unsigned long
cpu_get_eflags(void)
{
unsigned long eflags;
asm volatile("pushf\n"
"pop %0\n"
: "=r" (eflags)
: : "memory");
return eflags;
}
static __always_inline void
cpu_intr_enable(void)
{
asm volatile("sti" : : : "memory");
}
static __always_inline void
cpu_intr_disable(void)
{
asm volatile("cli" : : : "memory");
}
static __always_inline void
cpu_intr_restore(unsigned long flags)
{
asm volatile("push %0\n"
"popf\n"
: : "r" (flags)
: "memory");
}
static __always_inline void
cpu_intr_save(unsigned long *flags)
{
*flags = cpu_get_eflags();
cpu_intr_disable();
}
static __always_inline int
cpu_intr_enabled(void)
{
unsigned long eflags;
eflags = cpu_get_eflags();
return (eflags & CPU_EFL_IF) ? 1 : 0;
}
#endif