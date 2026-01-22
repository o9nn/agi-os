#include <sys/types.h>
#include <mach/vm_param.h>
#include <mach/vm_prot.h>
#include <mach/machine.h>
#include <vm/vm_page.h>
#include <kern/kalloc.h>
#include <machine/spl.h>
#include <machine/pmap.h>
#include <machine/vm_param.h>
#include <machine/model_dep.h>
#define MACH_INCLUDE
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/interrupt.h>
#include <linux/delay.h>
#include <linux/ioport.h>
#include <linux/string.h>
#include <linux/pci.h>
#include <linux/dev/glue/glue.h>
#include <asm/system.h>
#include <asm/io.h>
unsigned long loops_per_sec = 1;
#if defined(__SMP__) && defined(__i386__)
unsigned long smp_loops_per_tick = 1000000;
#endif
unsigned long high_memory;
int linux_auto_config = 1;
struct drive_info_struct
{
char dummy[32];
} drive_info;
static void calibrate_delay (void);
#define CONTIG_ALLOC (512 * 1024)
void
linux_init (void)
{
int addr;
unsigned long memory_start, memory_end;
vm_page_t pages;
high_memory = vm_page_seg_end(VM_PAGE_SEL_DIRECTMAP);
init_IRQ ();
linux_sched_init ();
calibrate_delay ();
addr = *((unsigned *) phystokv (0x104));
memcpy (&drive_info,
(void *) ((addr & 0xffff) + ((addr >> 12) & 0xffff0)), 16);
addr = *((unsigned *) phystokv (0x118));
memcpy ((char *) &drive_info + 16,
(void *) ((addr & 0xffff) + ((addr >> 12) & 0xffff0)), 16);
linux_kmem_init ();
memory_start = alloc_contig_mem (CONTIG_ALLOC, 16 * 1024 * 1024, 0, &pages);
if (memory_start == 0)
panic ("linux_init: alloc_contig_mem failed");
memory_end = memory_start + CONTIG_ALLOC;
memory_start = pci_init (memory_start, memory_end);
if (memory_start > memory_end)
panic ("linux_init: ran out memory");
#ifdef CONFIG_INET
linux_net_emulation_init ();
#endif
device_setup ();
#ifdef CONFIG_PCMCIA
pcmcia_init ();
#endif
restore_IRQ ();
linux_auto_config = 0;
}
#ifndef NBPW
#define NBPW 32
#endif
unsigned long
alloc_contig_mem (unsigned size, unsigned limit,
unsigned mask, vm_page_t * pages)
{
vm_page_t p;
p = vm_page_grab_contig(size, VM_PAGE_SEL_DMA);
if (p == NULL)
return 0;
if (pages)
*pages = p;
return phystokv(vm_page_to_pa(p));
}
void
free_contig_mem (vm_page_t pages, unsigned size)
{
vm_page_free_contig(pages, size);
}
#define LPS_PREC 8
static void
calibrate_delay (void)
{
int ticks;
int loopbit;
int lps_precision = LPS_PREC;
loops_per_sec = (1 << 12);
#ifndef MACH
printk ("Calibrating delay loop.. ");
#endif
while (loops_per_sec <<= 1)
{
ticks = jiffies;
while (ticks == jiffies)
;
ticks = jiffies;
__delay (loops_per_sec);
ticks = jiffies - ticks;
if (ticks)
break;
}
loops_per_sec >>= 1;
loopbit = loops_per_sec;
while (lps_precision-- && (loopbit >>= 1))
{
loops_per_sec |= loopbit;
ticks = jiffies;
while (ticks == jiffies);
ticks = jiffies;
__delay (loops_per_sec);
if (jiffies != ticks)
loops_per_sec &= ~loopbit;
}
loops_per_sec *= HZ;
#ifndef MACH
printk ("ok - %lu.%02lu BogoMIPS\n",
(loops_per_sec + 2500) / 500000,
((loops_per_sec + 2500) / 5000) % 100);
#endif
#if defined(__SMP__) && defined(__i386__)
smp_loops_per_tick = loops_per_sec / 400;
#endif
}