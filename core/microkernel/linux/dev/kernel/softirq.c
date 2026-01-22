#define MACH_INCLUDE
#include <linux/ptrace.h>
#include <linux/interrupt.h>
#include <asm/system.h>
#include <linux/dev/glue/glue.h>
int bh_mask_count[32];
unsigned int bh_active = 0;
unsigned int bh_mask = 0;
void (*bh_base[32]) (void);
void
linux_soft_intr (void)
{
unsigned int active;
unsigned int mask, left;
void (**bh) (void);
sti ();
bh = bh_base;
active = bh_active & bh_mask;
for (mask = 1, left = ~0; left & active; bh++, mask += mask, left += left)
{
if (mask & active)
{
void (*fn) (void);
bh_active &= ~mask;
fn = *bh;
if (!fn)
goto bad_bh;
fn ();
}
}
return;
bad_bh:
printk ("linux_soft_intr:bad interrupt handler entry %08x\n", mask);
}