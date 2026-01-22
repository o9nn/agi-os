#define AUTOSENSE
#define PSEUDO_DMA
#include <asm/system.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <asm/io.h>
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include "t128.h"
#define AUTOPROBE_IRQ
#include "NCR5380.h"
#include "constants.h"
#include "sd.h"
#include<linux/stat.h>
struct proc_dir_entry proc_scsi_t128 = {
PROC_SCSI_T128, 4, "t128",
S_IFDIR | S_IRUGO | S_IXUGO, 2
};
static struct override {
unsigned char *address;
int irq;
} overrides
#ifdef T128_OVERRIDE
[] = T128_OVERRIDE;
#else
[4] = {{NULL,IRQ_AUTO}, {NULL,IRQ_AUTO}, {NULL,IRQ_AUTO},
{NULL,IRQ_AUTO}};
#endif
#define NO_OVERRIDES (sizeof(overrides) / sizeof(struct override))
static struct base {
unsigned char *address;
int noauto;
} bases[] = {{(unsigned char *) 0xcc000, 0}, {(unsigned char *) 0xc8000, 0},
{(unsigned char *) 0xdc000, 0}, {(unsigned char *) 0xd8000, 0}};
#define NO_BASES (sizeof (bases) / sizeof (struct base))
static const struct signature {
const char *string;
int offset;
} signatures[] = {
{"TSROM: SCSI BIOS, Version 1.12", 0x36},
};
#define NO_SIGNATURES (sizeof (signatures) /  sizeof (struct signature))
void t128_setup(char *str, int *ints) {
static int commandline_current = 0;
int i;
if (ints[0] != 2)
printk("t128_setup : usage t128=address,irq\n");
else
if (commandline_current < NO_OVERRIDES) {
overrides[commandline_current].address = (unsigned char *) ints[1];
overrides[commandline_current].irq = ints[2];
for (i = 0; i < NO_BASES; ++i)
if (bases[i].address == (unsigned char *) ints[1]) {
bases[i].noauto = 1;
break;
}
++commandline_current;
}
}
int t128_detect(Scsi_Host_Template * tpnt) {
static int current_override = 0, current_base = 0;
struct Scsi_Host *instance;
unsigned char *base;
int sig, count;
tpnt->proc_dir = &proc_scsi_t128;
tpnt->proc_info = &t128_proc_info;
for (count = 0; current_override < NO_OVERRIDES; ++current_override) {
base = NULL;
if (overrides[current_override].address)
base = overrides[current_override].address;
else
for (; !base && (current_base < NO_BASES); ++current_base) {
#if (TDEBUG & TDEBUG_INIT)
printk("scsi : probing address %08x\n", (unsigned int) bases[current_base].address);
#endif
for (sig = 0; sig < NO_SIGNATURES; ++sig)
if (!bases[current_base].noauto && !memcmp
(bases[current_base].address + signatures[sig].offset,
signatures[sig].string, strlen(signatures[sig].string))) {
base = bases[current_base].address;
#if (TDEBUG & TDEBUG_INIT)
printk("scsi-t128 : detected board.\n");
#endif
break;
}
}
#if defined(TDEBUG) && (TDEBUG & TDEBUG_INIT)
printk("scsi-t128 : base = %08x\n", (unsigned int) base);
#endif
if (!base)
break;
instance = scsi_register (tpnt, sizeof(struct NCR5380_hostdata));
instance->base = base;
NCR5380_init(instance, 0);
if (overrides[current_override].irq != IRQ_AUTO)
instance->irq = overrides[current_override].irq;
else
instance->irq = NCR5380_probe_irq(instance, T128_IRQS);
if (instance->irq != IRQ_NONE)
if (request_irq(instance->irq, t128_intr, SA_INTERRUPT, "t128", NULL)) {
printk("scsi%d : IRQ%d not free, interrupts disabled\n",
instance->host_no, instance->irq);
instance->irq = IRQ_NONE;
}
if (instance->irq == IRQ_NONE) {
printk("scsi%d : interrupts not enabled. for better interactive performance,\n", instance->host_no);
printk("scsi%d : please jumper the board for a free IRQ.\n", instance->host_no);
}
#if defined(TDEBUG) && (TDEBUG & TDEBUG_INIT)
printk("scsi%d : irq = %d\n", instance->host_no, instance->irq);
#endif
printk("scsi%d : at 0x%08x", instance->host_no, (int)
instance->base);
if (instance->irq == IRQ_NONE)
printk (" interrupts disabled");
else
printk (" irq %d", instance->irq);
printk(" options CAN_QUEUE=%d  CMD_PER_LUN=%d release=%d",
CAN_QUEUE, CMD_PER_LUN, T128_PUBLIC_RELEASE);
NCR5380_print_options(instance);
printk("\n");
++current_override;
++count;
}
return count;
}
int t128_biosparam(Disk * disk, kdev_t dev, int * ip)
{
int size = disk->capacity;
ip[0] = 64;
ip[1] = 32;
ip[2] = size >> 11;
return 0;
}
static inline int NCR5380_pread (struct Scsi_Host *instance, unsigned char *dst,
int len) {
register unsigned char *reg = (unsigned char *) (instance->base +
T_DATA_REG_OFFSET), *d = dst;
register int i = len;
#if 0
for (; i; --i) {
while (!(instance->base[T_STATUS_REG_OFFSET]) & T_ST_RDY) barrier();
#else
while (!((instance->base[T_STATUS_REG_OFFSET]) & T_ST_RDY)) barrier();
for (; i; --i) {
#endif
*d++ = *reg;
}
if (*(instance->base + T_STATUS_REG_OFFSET) & T_ST_TIM) {
unsigned char tmp;
volatile unsigned char *foo;
foo = instance->base + T_CONTROL_REG_OFFSET;
tmp = *foo;
*foo = tmp | T_CR_CT;
*foo = tmp;
printk("scsi%d : watchdog timer fired in NCR5380_pread()\n",
instance->host_no);
return -1;
} else
return 0;
}
static inline int NCR5380_pwrite (struct Scsi_Host *instance, unsigned char *src,
int len) {
register unsigned char *reg = (unsigned char *) (instance->base +
T_DATA_REG_OFFSET), *s = src;
register int i = len;
#if 0
for (; i; --i) {
while (!(instance->base[T_STATUS_REG_OFFSET]) & T_ST_RDY) barrier();
#else
while (!((instance->base[T_STATUS_REG_OFFSET]) & T_ST_RDY)) barrier();
for (; i; --i) {
#endif
*reg = *s++;
}
if (*(instance->base + T_STATUS_REG_OFFSET) & T_ST_TIM) {
unsigned char tmp;
volatile unsigned char *foo;
foo = instance->base + T_CONTROL_REG_OFFSET;
tmp = *foo;
*foo = tmp | T_CR_CT;
*foo = tmp;
printk("scsi%d : watchdog timer fired in NCR5380_pwrite()\n",
instance->host_no);
return -1;
} else
return 0;
}
#include "NCR5380.c"
#ifdef MODULE
Scsi_Host_Template driver_template = TRANTOR_T128;
#include "scsi_module.c"
#endif