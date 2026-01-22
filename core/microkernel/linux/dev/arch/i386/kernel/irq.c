#include <sys/types.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <kern/assert.h>
#include <kern/cpu_number.h>
#include <i386/spl.h>
#include <i386/irq.h>
#include <i386/pit.h>
#define MACH_INCLUDE
#include <linux/mm.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <linux/delay.h>
#include <linux/kernel_stat.h>
#include <linux/malloc.h>
#include <linux/ioport.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <asm/irq.h>
#include <asm/io.h>
#include <asm/hardirq.h>
#include <linux/dev/glue/glue.h>
#include <machine/spl.h>
#include <device/intr.h>
#if 0
unsigned int local_bh_count[NR_CPUS];
unsigned int local_irq_count[NR_CPUS];
#else
#define local_bh_count (&intr_count)
#define local_irq_count (&intr_count)
#endif
int EISA_bus = 0;
unsigned int intr_count = 0;
struct linux_action
{
void (*handler) (int, void *, struct pt_regs *);
void *dev_id;
struct linux_action *next;
unsigned long flags;
user_intr_t *user_intr;
};
static struct linux_action *irq_action[NINTR] = {0};
static void
linux_intr (int irq)
{
struct pt_regs regs;
struct linux_action *action = *(irq_action + irq);
struct linux_action **prev = &irq_action[irq];
unsigned long flags;
kstat.interrupts[irq]++;
intr_count++;
save_flags (flags);
if (action && (action->flags & SA_INTERRUPT))
cli ();
while (action)
{
if (action->user_intr)
{
if (!deliver_user_intr(&irqtab, irq, action->user_intr))
{
*prev = action->next;
linux_kfree(action);
action = *prev;
continue;
}
}
else if (action->handler)
action->handler (irq, action->dev_id, &regs);
prev = &action->next;
action = action->next;
}
if (!irq_action[irq])
{
mask_irq (irq);
ivect[irq] = intnull;
iunit[irq] = irq;
}
restore_flags (flags);
intr_count--;
}
static unsigned linux_pic_mask;
void
disable_irq (unsigned int irq_nr)
{
unsigned long flags;
unsigned mask = 1U << irq_nr;
save_flags (flags);
cli ();
if (!(linux_pic_mask & mask))
{
linux_pic_mask |= mask;
__disable_irq(irq_nr);
}
restore_flags (flags);
}
void
enable_irq (unsigned int irq_nr)
{
unsigned long flags;
unsigned mask = 1U << irq_nr;
save_flags (flags);
cli ();
if (linux_pic_mask & mask)
{
linux_pic_mask &= ~mask;
__enable_irq(irq_nr);
}
restore_flags (flags);
}
static int
setup_x86_irq (int irq, struct linux_action *new)
{
int shared = 0;
struct linux_action *old, **p;
unsigned long flags;
p = irq_action + irq;
if ((old = *p) != NULL)
{
if (!(old->flags & new->flags & SA_SHIRQ))
return (-EBUSY);
if ((old->flags ^ new->flags) & SA_INTERRUPT)
return (-EBUSY);
do
{
p = &old->next;
old = *p;
}
while (old);
shared = 1;
}
save_flags (flags);
cli ();
*p = new;
if (!shared)
{
ivect[irq] = linux_intr;
iunit[irq] = irq;
unmask_irq (irq);
}
restore_flags (flags);
return 0;
}
int
install_user_intr_handler (struct irqdev *dev, int id, unsigned long flags,
user_intr_t *user_intr)
{
struct linux_action *action;
struct linux_action *old;
int retval;
unsigned int irq = dev->irq[id];
assert (irq < NINTR);
old = irq_action[irq];
while (old)
{
if (old->user_intr && old->user_intr->dst_port == user_intr->dst_port)
{
printk ("The interrupt handler has already been installed on line %d", irq);
return linux_to_mach_error (-EAGAIN);
}
old = old->next;
}
action = (struct linux_action *)
linux_kmalloc (sizeof (struct linux_action), GFP_KERNEL);
if (action == NULL)
return linux_to_mach_error (-ENOMEM);
action->handler = NULL;
action->next = NULL;
action->dev_id = NULL;
action->flags = SA_SHIRQ;
action->user_intr = user_intr;
retval = setup_x86_irq (irq, action);
if (retval)
linux_kfree (action);
return linux_to_mach_error (retval);
}
int
request_irq (unsigned int irq, void (*handler) (int, void *, struct pt_regs *),
unsigned long flags, const char *device, void *dev_id)
{
struct linux_action *action;
int retval;
assert (irq < NINTR);
if (!handler)
return -EINVAL;
action = (struct linux_action *)
linux_kmalloc (sizeof (struct linux_action), GFP_KERNEL);
if (action == NULL)
return -ENOMEM;
action->handler = handler;
action->next = NULL;
action->dev_id = dev_id;
action->flags = flags;
action->user_intr = NULL;
retval = setup_x86_irq (irq, action);
if (retval)
linux_kfree (action);
return retval;
}
void
free_irq (unsigned int irq, void *dev_id)
{
struct linux_action *action, **p;
unsigned long flags;
if (irq >= NINTR)
panic ("free_irq: bad irq number");
for (p = irq_action + irq; (action = *p) != NULL; p = &action->next)
{
if (action->dev_id != dev_id)
continue;
save_flags (flags);
cli ();
*p = action->next;
if (!irq_action[irq])
{
mask_irq (irq);
ivect[irq] = intnull;
iunit[irq] = irq;
}
restore_flags (flags);
linux_kfree (action);
return;
}
panic ("free_irq: bad irq number");
}
unsigned long
probe_irq_on (void)
{
unsigned i, irqs = 0;
unsigned long delay;
assert (curr_ipl[cpu_number()] == 0);
for (i = NINTR - 1; i > 0; i--)
{
if (!irq_action[i] && ivect[i] == intnull)
{
enable_irq (i);
irqs |= 1 << i;
}
}
for (delay = jiffies + HZ / 10; delay > jiffies;)
;
return (irqs & ~linux_pic_mask);
}
int
probe_irq_off (unsigned long irqs)
{
unsigned int i;
assert (curr_ipl[cpu_number()] == 0);
irqs &= linux_pic_mask;
for (i = NINTR - 1; i > 0; i--)
{
if (!irq_action[i] && ivect[i] == intnull)
{
disable_irq (i);
}
}
if (!irqs)
return 0;
i = ffz (~irqs);
if (irqs != (irqs & (1 << i)))
i = -i;
return i;
}
static void reserved_mach_handler (int line, void *cookie, struct pt_regs *regs)
{
assert (! "reached");
}
static const struct linux_action reserved_mach =
{
reserved_mach_handler, NULL, NULL, 0
};
static void
reserve_mach_irqs (void)
{
unsigned int i;
for (i = 0; i < NINTR; i++)
{
if (ivect[i] != intnull)
irq_action[i] = (struct linux_action *) &reserved_mach;
}
}
#ifdef __SMP__
unsigned char global_irq_holder = NO_PROC_ID;
unsigned volatile int global_irq_lock;
atomic_t global_irq_count;
atomic_t global_bh_count;
atomic_t global_bh_lock;
#if 0
static inline void check_smp_invalidate(int cpu)
{
if (test_bit(cpu, &smp_invalidate_needed)) {
clear_bit(cpu, &smp_invalidate_needed);
local_flush_tlb();
}
}
#endif
static void show(char * str)
{
int i;
unsigned long *stack;
int cpu = smp_processor_id();
printk("\n%s, CPU %d:\n", str, cpu);
printk("irq:  %d [%d %d]\n",
atomic_read(&global_irq_count), local_irq_count[0], local_irq_count[1]);
printk("bh:   %d [%d %d]\n",
atomic_read(&global_bh_count), local_bh_count[0], local_bh_count[1]);
stack = (unsigned long *) &stack;
for (i = 40; i ; i--) {
unsigned long x = *++stack;
printk("<[%08lx]> ", x);
}
}
#define MAXCOUNT 100000000
static inline void wait_on_bh(void)
{
int count = MAXCOUNT;
do {
if (!--count) {
show("wait_on_bh");
count = ~0;
}
} while (atomic_read(&global_bh_count) != 0);
}
#define SUSPECTED_CPU_OR_CHIPSET_BUG_WORKAROUND 1
#if SUSPECTED_CPU_OR_CHIPSET_BUG_WORKAROUND
# define SYNC_OTHER_CORES(x) udelay(x+1)
#else
# define SYNC_OTHER_CORES(x) __asm__ __volatile__ ("nop")
#endif
static inline void wait_on_irq(int cpu)
{
int count = MAXCOUNT;
for (;;) {
if (!atomic_read(&global_irq_count)) {
if (local_bh_count[cpu] || !atomic_read(&global_bh_count))
break;
}
clear_bit(0,&global_irq_lock);
for (;;) {
if (!--count) {
show("wait_on_irq");
count = ~0;
}
__sti();
SYNC_OTHER_CORES(cpu);
__cli();
if (atomic_read(&global_irq_count))
continue;
if (global_irq_lock)
continue;
if (!local_bh_count[cpu] && atomic_read(&global_bh_count))
continue;
if (!test_and_set_bit(0,&global_irq_lock))
break;
}
}
}
void synchronize_bh(void)
{
if (atomic_read(&global_bh_count) && !in_interrupt())
wait_on_bh();
}
void synchronize_irq(void)
{
if (atomic_read(&global_irq_count)) {
cli();
sti();
}
}
static inline void get_irqlock(int cpu)
{
if (test_and_set_bit(0,&global_irq_lock)) {
if ((unsigned char) cpu == global_irq_holder)
return;
do {
do {
} while (test_bit(0,&global_irq_lock));
} while (test_and_set_bit(0,&global_irq_lock));
}
wait_on_irq(cpu);
global_irq_holder = cpu;
}
#define EFLAGS_IF_SHIFT 9
void __global_cli(void)
{
unsigned int flags;
__save_flags(flags);
if (flags & (1 << EFLAGS_IF_SHIFT)) {
int cpu = smp_processor_id();
__cli();
if (!local_irq_count[cpu])
get_irqlock(cpu);
}
}
void __global_sti(void)
{
int cpu = smp_processor_id();
if (!local_irq_count[cpu])
release_irqlock(cpu);
__sti();
}
unsigned long __global_save_flags(void)
{
int retval;
int local_enabled;
unsigned long flags;
__save_flags(flags);
local_enabled = (flags >> EFLAGS_IF_SHIFT) & 1;
retval = 2 + local_enabled;
if (!local_irq_count[smp_processor_id()]) {
if (local_enabled)
retval = 1;
if (global_irq_holder == (unsigned char) smp_processor_id())
retval = 0;
}
return retval;
}
void __global_restore_flags(unsigned long flags)
{
switch (flags) {
case 0:
__global_cli();
break;
case 1:
__global_sti();
break;
case 2:
__cli();
break;
case 3:
__sti();
break;
default:
printk("global_restore_flags: %08lx (%08lx)\n",
flags, (&flags)[-1]);
}
}
#endif
static void (*old_clock_handler) ();
void
init_IRQ (void)
{
char *p;
int latch = (CLKNUM + hz / 2) / hz;
(void) splhigh ();
#ifndef APIC
outb_p (PIT_C0 | PIT_SQUAREMODE | PIT_READMODE, PITCTL_PORT);
outb_p (latch & 0xff, PITCTR0_PORT);
outb (latch >> 8, PITCTR0_PORT);
old_clock_handler = ivect[0];
ivect[0] = linux_timer_intr;
#endif
reserve_mach_irqs ();
(void) spl0 ();
p = (char *) phystokv(0x0FFFD9);
if (*p++ == 'E' && *p++ == 'I' && *p++ == 'S' && *p == 'A')
EISA_bus = 1;
request_region (0x00, 0x20, "dma1");
request_region (0x20, 0x20, "pic1");
request_region (0x40, 0x20, "timer");
request_region (0x70, 0x10, "rtc");
request_region (0x80, 0x20, "dma page reg");
request_region (0xa0, 0x20, "pic2");
request_region (0xc0, 0x20, "dma2");
request_region (0xf0, 0x10, "npu");
}
void
restore_IRQ (void)
{
(void) splhigh ();
#ifndef APIC
ivect[0] = old_clock_handler;
#endif
}