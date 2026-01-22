#include <fcntl.h>
#include <hurd.h>
#include <device/device.h>
#include "maptime.h"
error_t
maptime_map (int use_mach_dev, char *dev_name,
volatile struct mapped_time_value **mtime)
{
error_t err;
mach_port_t memobj;
if (use_mach_dev)
{
device_t device;
mach_port_t device_master;
err = get_privileged_ports (0, &device_master);
if (err)
return err;
err = device_open (device_master, 0, dev_name ?: "time", &device);
mach_port_deallocate (mach_task_self (), device_master);
if (err)
return err;
err = device_map (device, VM_PROT_READ, 0, sizeof *mtime, &memobj, 0);
mach_port_deallocate (mach_task_self (), device);
}
else
{
mach_port_t wr_memobj;
file_t node = file_name_lookup (dev_name ?: "/dev/time", O_RDONLY, 0);
if (node == MACH_PORT_NULL)
return errno;
err = io_map (node, &memobj, &wr_memobj);
if (!err && wr_memobj != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), wr_memobj);
mach_port_deallocate (mach_task_self (), node);
}
if (! err)
{
*mtime = 0;
err =
vm_map (mach_task_self (), (vm_address_t *)mtime, sizeof *mtime, 0, 1,
memobj, 0, 0, VM_PROT_READ, VM_PROT_READ, VM_INHERIT_NONE);
mach_port_deallocate (mach_task_self (), memobj);
}
return err;
}