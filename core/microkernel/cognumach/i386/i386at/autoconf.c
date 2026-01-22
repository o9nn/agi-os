#include <kern/printf.h>
#include <mach/std_types.h>
#include <i386at/autoconf.h>
#include <i386/irq.h>
#include <i386/ipl.h>
#ifdef APIC
# include <i386/apic.h>
#else
# include <i386/pic.h>
#endif
#include <chips/busses.h>
#define SPL_FIVE (vm_offset_t)SPL5
#define SPL_SIX (vm_offset_t)SPL6
#define SPL_TTY (vm_offset_t)SPLTTY
#if NCOM > 0
extern struct bus_driver comdriver;
#include <i386at/com.h>
#endif
#if NLPR > 0
extern struct bus_driver lprdriver;
#include <i386at/lpr.h>
#endif
struct bus_ctlr bus_master_init[] = {
{0}
};
struct bus_device bus_device_init[] = {
#if NCOM > 0
{&comdriver, "com", 0, comintr, 0x3f8, 8, 0x3f8,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 4},
{&comdriver, "com", 1, comintr, 0x2f8, 8, 0x2f8,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 3},
{&comdriver, "com", 2, comintr, 0x3e8, 8, 0x3e8,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 5},
#endif
#ifdef MACH_LPR
#if NLPR > 0
{&lprdriver, "lpr", 0, lprintr, 0x378, 3, 0x378,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 7},
{&lprdriver, "lpr", 0, lprintr, 0x278, 3, 0x278,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 7},
{&lprdriver, "lpr", 0, lprintr, 0x3bc, 3, 0x3bc,
'?', 0, -1, -1, 0, 0, 0, SPL_TTY, 7},
#endif
#endif
{0}
};
void probeio(void)
{
struct bus_device *device;
struct bus_ctlr *master;
int i = 0;
for (master = bus_master_init; master->driver; master++)
{
if (configure_bus_master(master->name, master->address,
master->phys_address, i, "atbus"))
i++;
}
for (device = bus_device_init; device->driver; device++)
{
if (device->alive || device->ctlr >= 0)
continue;
if (configure_bus_device(device->name, device->address,
device->phys_address, i, "atbus"))
i++;
}
#if MACH_TTD
ttd_init();
#endif
}
void take_dev_irq(
const struct bus_device *dev)
{
int pic = (int)dev->sysdep1;
if (ivect[pic] == intnull) {
iunit[pic] = dev->unit;
ivect[pic] = dev->intr;
} else {
printf("The device below will clobber IRQ %d (%p).\n", pic, ivect[pic]);
printf("You have two devices at the same IRQ.\n");
printf("This won't work.  Reconfigure your hardware and try again.\n");
printf("%s%d: port = %zx, spl = %zd, pic = %d.\n",
dev->name, dev->unit, dev->address,
dev->sysdep, dev->sysdep1);
while (1);
}
unmask_irq(pic);
}