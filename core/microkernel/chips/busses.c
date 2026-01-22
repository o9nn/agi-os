#include <string.h>
#include <kern/printf.h>
#include <mach/boolean.h>
#include <mach/std_types.h>
#include <chips/busses.h>
boolean_t configure_bus_master(
const char	*name,
vm_offset_t	 virt,
vm_offset_t	 phys,
int		 adpt_no,
const char	*bus_name)
{
struct bus_device *device;
struct bus_ctlr *master;
struct bus_driver *driver;
boolean_t             found = FALSE;
for (master = bus_master_init; master->driver; master++) {
if (master->alive)
continue;
if (((master->adaptor == adpt_no) || (master->adaptor == '?')) &&
(strcmp(master->name, name) == 0)) {
found = TRUE;
break;
}
}
if (!found)
return FALSE;
driver = master->driver;
if ((*driver->probe) (virt, master) == 0)
return FALSE;
master->alive = 1;
master->adaptor = adpt_no;
driver->minfo[master->unit] = master;
printf("%s%d: at %s%d\n", master->name, master->unit, bus_name, adpt_no);
for (device = bus_device_init; device->driver; device++) {
int	ctlr;
if (device->alive || device->driver != driver ||
(device->adaptor != '?' && device->adaptor != adpt_no))
continue;
ctlr = device->ctlr;
if (ctlr == '?') device->ctlr = master->unit;
if ((device->ctlr != master->unit) ||
((*driver->slave) (device, virt) == 0)) {
device->ctlr = ctlr;
continue;
}
device->alive = 1;
device->adaptor = adpt_no;
device->ctlr = master->unit;
device->mi = master;
driver->dinfo[device->unit] = device;
if (device->slave >= 0)
printf(" %s%d: at %s%d slave %d",
device->name, device->unit,
driver->mname, master->unit, device->slave);
else
printf(" %s%d: at %s%d",
device->name, device->unit,
driver->mname, master->unit);
(*driver->attach) (device);
printf("\n");
}
return TRUE;
}
boolean_t configure_bus_device(
const char	*name,
vm_offset_t	 virt,
vm_offset_t	 phys,
int 		 adpt_no,
const char	*bus_name)
{
struct bus_device *device;
struct bus_driver *driver;
boolean_t             found = FALSE;
for (device = bus_device_init; device->driver; device++) {
if (device->alive)
continue;
if (((device->adaptor == adpt_no) || (device->adaptor == '?')) &&
(device->slave == -1) &&
((!device->phys_address) ||
((device->phys_address == phys) && (device->address == virt))) &&
(strcmp(device->name, name) == 0)) {
found = TRUE;
break;
}
}
if (!found)
return FALSE;
driver = device->driver;
if ((*driver->probe) (virt, (struct bus_ctlr *)device) == 0)
return FALSE;
device->alive = 1;
device->adaptor = adpt_no;
printf("%s%d: at %s%d", device->name, device->unit, bus_name, adpt_no);
driver->dinfo[device->unit] = device;
(*driver->attach) (device);
printf("\n");
return TRUE;
}