#include <mach/machine/vm_types.h>
#include <device/conf.h>
#include <kern/mach_clock.h>
#include <i386at/model_dep.h>
#define timename "time"
#ifndef MACH_HYP
#include <i386at/kd.h>
#define kdname "kd"
#if NCOM > 0
#include <i386at/com.h>
#define comname "com"
#endif
#if NLPR > 0
#include <i386at/lpr.h>
#define lprname "lpr"
#endif
#endif
#include <i386at/kd_event.h>
#define kbdname "kbd"
#ifndef MACH_HYP
#include <i386at/kd_mouse.h>
#define mousename "mouse"
#include <i386at/mem.h>
#define memname "mem"
#endif
#include <device/kmsg.h>
#define kmsgname "kmsg"
#ifdef MACH_HYP
#include <xen/console.h>
#define hypcnname "hyp"
#endif
#include <device/intr.h>
#define irqname "irq"
#include <i386at/mbinfo.h>
#define mbinfoname "mbinfo"
struct dev_ops dev_name_list[] =
{
{ "cn", nulldev_open, nulldev_close, nulldev_read,
nulldev_write, nulldev_getstat, nulldev_setstat, nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info},
#ifndef MACH_HYP
#if ENABLE_IMMEDIATE_CONSOLE
{ "immc", nulldev_open, nulldev_close, nulldev_read,
nulldev_write, nulldev_getstat, nulldev_setstat,
nomap, nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
#endif
{ kdname, kdopen, kdclose, kdread,
kdwrite, kdgetstat, kdsetstat, kdmmap,
nodev_async_in, nulldev_reset, kdportdeath, 0,
nodev_info },
#endif
{ timename, timeopen, timeclose, nulldev_read,
nulldev_write, nulldev_getstat, nulldev_setstat, timemmap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
#ifndef MACH_HYP
#if NCOM > 0
{ comname, comopen, comclose, comread,
comwrite, comgetstat, comsetstat, nomap,
nodev_async_in, nulldev_reset, comportdeath, 0,
nodev_info },
#endif
#ifdef MACH_LPR
{ lprname, lpropen, lprclose, lprread,
lprwrite, lprgetstat, lprsetstat, nomap,
nodev_async_in, nulldev_reset, lprportdeath, 0,
nodev_info },
#endif
{ mousename, mouseopen, mouseclose, mouseread,
nulldev_write, mousegetstat, nulldev_setstat, nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
{ kbdname, kbdopen, kbdclose, kbdread,
nulldev_write, kbdgetstat, kbdsetstat, nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
{ memname, nulldev_open, nulldev_close, nulldev_read,
nulldev_write, nulldev_getstat, nulldev_setstat, memmmap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
#endif
#ifdef MACH_KMSG
{ kmsgname, kmsgopen, kmsgclose, kmsgread,
nulldev_write, kmsggetstat, nulldev_setstat, nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath, 0,
nodev_info },
#endif
#ifdef MACH_HYP
{ hypcnname, hypcnopen, hypcnclose, hypcnread,
hypcnwrite, hypcngetstat, hypcnsetstat, nomap,
nodev_async_in, nulldev_reset, hypcnportdeath, 0,
nodev_info },
#endif
{ irqname, nulldev_open, nulldev_close, nulldev_read,
nulldev_write,nulldev_getstat,nulldev_setstat, nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath,0,
nodev_info },
#ifndef MACH_HYP
{ mbinfoname, nulldev_open, nulldev_close, mbinforead,
nulldev_write,nulldev_getstat,nulldev_setstat,nomap,
nodev_async_in, nulldev_reset, nulldev_portdeath,0,
nodev_info },
#endif
};
int dev_name_count = sizeof(dev_name_list)/sizeof(dev_name_list[0]);
struct dev_indirect dev_indirect_list[] = {
{ "console", &dev_name_list[0], 0 }
};
int dev_indirect_count = sizeof(dev_indirect_list)
/ sizeof(dev_indirect_list[0]);