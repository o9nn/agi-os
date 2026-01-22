#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <kern/debug.h>
#include <kern/task.h>
#include <xen/xen.h>
#include <device/device_types.h>
#include <device/device_port.h>
#include <device/tty.h>
#include <device/device_init.h>
#include <device/ds_routines.h>
#include <device/net_io.h>
#include <device/chario.h>
ipc_port_t	master_device_port;
void
device_service_create(void)
{
master_device_port = ipc_port_alloc_kernel();
if (master_device_port == IP_NULL)
panic("can't allocate master device port");
mach_device_init();
#ifdef MACH_HYP
hyp_dev_init();
#endif
dev_lookup_init();
net_io_init();
device_pager_init();
chario_init();
(void) kernel_thread(kernel_task, "io_done", io_done_thread, 0);
(void) kernel_thread(kernel_task, "net", net_thread, 0);
}