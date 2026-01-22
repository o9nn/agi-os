#ifndef _LINUX_INTERRUPT_H
#define _LINUX_INTERRUPT_H
#include <linux/kernel.h>
#include <asm/bitops.h>
struct irqaction {
void (*handler)(int, void *, struct pt_regs *);
unsigned long flags;
unsigned long mask;
const char *name;
void *dev_id;
struct irqaction *next;
};
extern unsigned int intr_count;
extern int bh_mask_count[32];
extern unsigned int bh_active;
extern unsigned int bh_mask;
extern void (*bh_base[32])(void);
asmlinkage void do_bottom_half(void);
enum {
TIMER_BH = 0,
CONSOLE_BH,
TQUEUE_BH,
DIGI_BH,
SERIAL_BH,
RISCOM8_BH,
SPECIALIX_BH,
BAYCOM_BH,
NET_BH,
IMMEDIATE_BH,
KEYBOARD_BH,
CYCLADES_BH,
CM206_BH,
ISICOM_BH
};
static inline void init_bh(int nr, void (*routine)(void))
{
bh_base[nr] = routine;
bh_mask_count[nr] = 0;
bh_mask |= 1 << nr;
}
static inline void mark_bh(int nr)
{
set_bit(nr, &bh_active);
}
static inline void disable_bh(int nr)
{
bh_mask &= ~(1 << nr);
bh_mask_count[nr]++;
}
static inline void enable_bh(int nr)
{
if (!--bh_mask_count[nr])
bh_mask |= 1 << nr;
}
static inline void start_bh_atomic(void)
{
intr_count++;
barrier();
}
static inline void end_bh_atomic(void)
{
barrier();
intr_count--;
}
extern unsigned long probe_irq_on(void);
extern int probe_irq_off(unsigned long);
#endif