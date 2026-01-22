#ifndef _AUTOCONF_H_
#define _AUTOCONF_H_
#include <mach/std_types.h>
#include <chips/busses.h>
void probeio(void);
void take_dev_irq(
const struct bus_device *dev);
#endif