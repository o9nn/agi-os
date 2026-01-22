#ifdef version
static const char *version =
"auto_irq.c:v1.11 Donald Becker (becker@cesdis.gsfc.nasa.gov)";
#endif
#include <sys/types.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <mach/message.h>
#include <vm/vm_map.h>
#define MACH_INCLUDE
#include <linux/sched.h>
#include <linux/delay.h>
#include <asm/bitops.h>
#include <asm/io.h>
#include <asm/irq.h>
#include <linux/netdevice.h>
void *irq2dev_map[NR_IRQS] = {0, 0,  };
unsigned long irqs_busy = 0x2147;
unsigned long irqs_used = 0x0001;
unsigned long irqs_reserved = 0x0000;
unsigned long irqs_shared = 0x0000;
static volatile unsigned long irq_bitmap;
static unsigned long irq_handled;
static volatile int irq_number;
static void
autoirq_probe (int irq, void *dev_id, struct pt_regs *regs)
{
irq_number = irq;
set_bit (irq, (void *) &irq_bitmap);
free_irq (irq, dev_id);
return;
}
int
autoirq_setup (int waittime)
{
int i;
unsigned long timeout = jiffies + waittime;
unsigned long boguscount = (waittime * loops_per_sec) / 100;
irq_handled = 0;
irq_bitmap = 0;
for (i = 0; i < 16; i++)
{
if (test_bit (i, &irqs_busy) == 0
&& request_irq (i, autoirq_probe, SA_INTERRUPT, "irq probe", NULL) == 0)
set_bit (i, (void *) &irq_handled);
}
irqs_used |= ~irq_handled;
while (timeout > jiffies && --boguscount > 0)
;
irq_handled &= ~irq_bitmap;
irq_number = 0;
return irq_handled;
}
int
autoirq_report (int waittime)
{
int i;
unsigned long timeout = jiffies + waittime;
unsigned long boguscount = (waittime * loops_per_sec) / 100;
while (timeout > jiffies && --boguscount > 0)
if (irq_number)
break;
irq_handled &= ~irq_bitmap;
for (i = 0; i < 16; i++)
{
if (test_bit (i, (void *) &irq_handled))
free_irq (i, NULL);
}
return irq_number;
}