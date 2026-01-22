#include <string.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <kern/slab.h>
#include <kern/kalloc.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <kern/thread.h>
#include <device/dev_hdr.h>
#include <device/device_emul.h>
#include <device/device_port.h>
#include <i386/i386/mach_i386.server.h>
#include "io_perm.h"
#include "gdt.h"
#include "pcb.h"
#define PCI_CFG1_START	0xcf8
#define PCI_CFG1_END	0xcff
#define CONTAINS_PCI_CFG(from, to) \
( ( from <= PCI_CFG1_END ) && ( to >= PCI_CFG1_START ) )
static struct device_emulation_ops io_perm_device_emulation_ops;
static boolean_t taken_pci_cfg = FALSE;
ipc_port_t
convert_io_perm_to_port (io_perm_t io_perm)
{
if (io_perm == IO_PERM_NULL)
return IP_NULL;
ipc_port_t port;
port = ipc_port_make_send (io_perm->port);
return port;
}
io_perm_t
convert_port_to_io_perm (ipc_port_t port)
{
device_t device;
device = dev_port_lookup (port);
if (device == DEVICE_NULL)
return IO_PERM_NULL;
io_perm_t io_perm;
io_perm = device->emul_data;
return io_perm;
}
void
io_perm_deallocate (io_perm_t io_perm)
{
if (CONTAINS_PCI_CFG(io_perm->from, io_perm->to))
taken_pci_cfg = FALSE;
}
static
void
no_senders (mach_no_senders_notification_t *notification)
{
io_perm_t io_perm;
io_perm = convert_port_to_io_perm
((ipc_port_t) notification->not_header.msgh_remote_port);
assert (io_perm != IO_PERM_NULL);
ipc_kobject_set (io_perm->port, IKO_NULL, IKOT_NONE);
ipc_port_dealloc_kernel (io_perm->port);
kfree ((vm_offset_t) io_perm, sizeof *io_perm);
}
static inline void
io_bitmap_init (unsigned char *iopb)
{
memset (iopb, ~0, IOPB_BYTES);
}
static inline void
io_bitmap_set (unsigned char *iopb, io_port_t from, io_port_t to)
{
do
iopb[from >> 3] &= ~(1 << (from & 0x7));
while (from++ != to);
}
static inline void
io_bitmap_clear (unsigned char *iopb, io_port_t from, io_port_t to)
{
do
iopb[from >> 3] |= (1 << (from & 0x7));
while (from++ != to);
}
kern_return_t
i386_io_perm_create (const ipc_port_t master_port, io_port_t from, io_port_t to,
io_perm_t *new)
{
if (master_port != master_device_port)
return KERN_INVALID_ARGUMENT;
if (from > to)
return KERN_INVALID_ARGUMENT;
if (taken_pci_cfg && CONTAINS_PCI_CFG(from, to))
return KERN_PROTECTION_FAILURE;
io_perm_t io_perm;
io_perm = (io_perm_t) kalloc (sizeof *io_perm);
if (io_perm == NULL)
return KERN_RESOURCE_SHORTAGE;
io_perm->from = from;
io_perm->to = to;
io_perm->port = ipc_port_alloc_kernel ();
if (io_perm->port == IP_NULL)
{
kfree ((vm_offset_t) io_perm, sizeof *io_perm);
return KERN_RESOURCE_SHORTAGE;
}
ipc_kobject_set(io_perm->port,
(ipc_kobject_t) &io_perm->device, IKOT_DEVICE);
io_perm->device.emul_data = io_perm;
io_perm->device.emul_ops = &io_perm_device_emulation_ops;
ipc_port_t notify;
notify = ipc_port_make_sonce(io_perm->port);
ip_lock(io_perm->port);
ipc_port_nsrequest(io_perm->port, 1, notify, &notify);
assert(notify == IP_NULL);
*new = io_perm;
if (CONTAINS_PCI_CFG(from, to))
taken_pci_cfg = TRUE;
return KERN_SUCCESS;
}
kern_return_t
i386_io_perm_modify (task_t target_task, io_perm_t io_perm, boolean_t enable)
{
io_port_t from, to;
unsigned char *iopb;
io_port_t iopb_size;
if (target_task == TASK_NULL || io_perm == IO_PERM_NULL)
return KERN_INVALID_ARGUMENT;
from = io_perm->from;
to = io_perm->to;
simple_lock (&target_task->machine.iopb_lock);
iopb = target_task->machine.iopb;
iopb_size = target_task->machine.iopb_size;
if (!enable && !iopb_size)
{
simple_unlock (&target_task->machine.iopb_lock);
return KERN_SUCCESS;
}
if (!iopb)
{
simple_unlock (&target_task->machine.iopb_lock);
iopb = (unsigned char *) kmem_cache_alloc (&machine_task_iopb_cache);
simple_lock (&target_task->machine.iopb_lock);
if (target_task->machine.iopb)
{
if (iopb)
kmem_cache_free (&machine_task_iopb_cache, (vm_offset_t) iopb);
iopb = target_task->machine.iopb;
iopb_size = target_task->machine.iopb_size;
}
else if (iopb)
{
target_task->machine.iopb = iopb;
io_bitmap_init (iopb);
}
else
{
simple_unlock (&target_task->machine.iopb_lock);
return KERN_RESOURCE_SHORTAGE;
}
}
if (enable)
{
io_bitmap_set (iopb, from, to);
if ((to >> 3) + 1 > iopb_size)
target_task->machine.iopb_size = (to >> 3) + 1;
}
else
{
if ((from >> 3) + 1 > iopb_size)
{
simple_unlock (&target_task->machine.iopb_lock);
return KERN_SUCCESS;
}
io_bitmap_clear (iopb, from, to);
while (iopb_size > 0 && iopb[iopb_size - 1] == 0xff)
iopb_size--;
target_task->machine.iopb_size = iopb_size;
}
#if NCPUS>1
#warning SMP support missing (notify all CPUs running threads in that of the I/O bitmap change).
#endif
if (target_task == current_task())
update_ktss_iopb (iopb, target_task->machine.iopb_size);
simple_unlock (&target_task->machine.iopb_lock);
return KERN_SUCCESS;
}
static struct device_emulation_ops io_perm_device_emulation_ops =
{
.no_senders = no_senders
};