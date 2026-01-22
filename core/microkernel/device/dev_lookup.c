#include <mach/port.h>
#include <mach/vm_param.h>
#include <kern/queue.h>
#include <kern/slab.h>
#include <device/device_types.h>
#include <device/dev_hdr.h>
#include <device/conf.h>
#include <device/param.h>
#include <ipc/ipc_port.h>
#include <kern/ipc_kobject.h>
#include <device/device_emul.h>
#include <device/ds_routines.h>
#define NDEVHASH 8
#define DEV_NUMBER_HASH(dev) ((dev) & (NDEVHASH-1))
queue_head_t dev_number_hash_table[NDEVHASH];
def_simple_lock_data(static, dev_number_lock)
struct kmem_cache dev_hdr_cache;
static void
dev_number_enter(const mach_device_t device)
{
queue_t q;
q = &dev_number_hash_table[DEV_NUMBER_HASH(device->dev_number)];
queue_enter(q, device, mach_device_t, number_chain);
}
static void
dev_number_remove(const mach_device_t device)
{
queue_t q;
q = &dev_number_hash_table[DEV_NUMBER_HASH(device->dev_number)];
queue_remove(q, device, mach_device_t, number_chain);
}
static mach_device_t
dev_number_lookup(const dev_ops_t ops, int devnum)
{
queue_t q;
mach_device_t device;
q = &dev_number_hash_table[DEV_NUMBER_HASH(devnum)];
queue_iterate(q, device, mach_device_t, number_chain) {
if (device->dev_ops == ops && device->dev_number == devnum) {
return (device);
}
}
return (MACH_DEVICE_NULL);
}
mach_device_t
device_lookup(const char *name)
{
dev_ops_t dev_ops;
int dev_minor;
mach_device_t device;
mach_device_t new_device;
if (!dev_name_lookup(name, &dev_ops, &dev_minor))
return (MACH_DEVICE_NULL);
new_device = MACH_DEVICE_NULL;
simple_lock(&dev_number_lock);
while ((device = dev_number_lookup(dev_ops, dev_minor))
== MACH_DEVICE_NULL) {
if (new_device != MACH_DEVICE_NULL)
break;
simple_unlock(&dev_number_lock);
new_device = (mach_device_t) kmem_cache_alloc(&dev_hdr_cache);
simple_lock_init(&new_device->ref_lock);
new_device->ref_count = 1;
simple_lock_init(&new_device->lock);
new_device->state = DEV_STATE_INIT;
new_device->flag = 0;
new_device->open_count = 0;
new_device->io_in_progress = 0;
new_device->io_wait = FALSE;
new_device->port = IP_NULL;
new_device->dev_ops = dev_ops;
new_device->dev_number = dev_minor;
new_device->bsize = DEV_BSIZE;
simple_lock(&dev_number_lock);
}
if (device == MACH_DEVICE_NULL) {
assert(new_device != MACH_DEVICE_NULL);
device = new_device;
dev_number_enter(device);
simple_unlock(&dev_number_lock);
}
else {
mach_device_reference(device);
simple_unlock(&dev_number_lock);
if (new_device != MACH_DEVICE_NULL)
kmem_cache_free(&dev_hdr_cache, (vm_offset_t)new_device);
}
return (device);
}
void
mach_device_reference(mach_device_t device)
{
simple_lock(&device->ref_lock);
device->ref_count++;
simple_unlock(&device->ref_lock);
}
void
mach_device_deallocate(mach_device_t device)
{
simple_lock(&device->ref_lock);
if (--device->ref_count > 0) {
simple_unlock(&device->ref_lock);
return;
}
device->ref_count = 1;
simple_unlock(&device->ref_lock);
simple_lock(&dev_number_lock);
simple_lock(&device->ref_lock);
if (--device->ref_count > 0) {
simple_unlock(&device->ref_lock);
simple_unlock(&dev_number_lock);
return;
}
dev_number_remove(device);
simple_unlock(&device->ref_lock);
simple_unlock(&dev_number_lock);
kmem_cache_free(&dev_hdr_cache, (vm_offset_t)device);
}
void
dev_port_enter(mach_device_t device)
{
mach_device_reference(device);
ipc_kobject_set(device->port,
(ipc_kobject_t) &device->dev, IKOT_DEVICE);
device->dev.emul_data = device;
{
extern struct device_emulation_ops mach_device_emulation_ops;
device->dev.emul_ops = &mach_device_emulation_ops;
}
}
void
dev_port_remove(mach_device_t device)
{
ipc_kobject_set(device->port, IKO_NULL, IKOT_NONE);
mach_device_deallocate(device);
}
device_t
dev_port_lookup(ipc_port_t port)
{
device_t device;
if (!IP_VALID(port))
return (DEVICE_NULL);
ip_lock(port);
if (ip_active(port) && (ip_kotype(port) == IKOT_DEVICE)) {
device = (device_t) port->ip_kobject;
if (device->emul_ops->reference)
(*device->emul_ops->reference)(device->emul_data);
}
else
device = DEVICE_NULL;
ip_unlock(port);
return (device);
}
ipc_port_t
convert_device_to_port(const device_t device)
{
if (device == DEVICE_NULL)
return IP_NULL;
return (*device->emul_ops->dev_to_port) (device->emul_data);
}
boolean_t
dev_map(
dev_map_fn routine,
mach_port_t port)
{
int i;
queue_t q;
mach_device_t dev, prev_dev;
for (i = 0, q = &dev_number_hash_table[0];
i < NDEVHASH;
i++, q++) {
prev_dev = MACH_DEVICE_NULL;
simple_lock(&dev_number_lock);
queue_iterate(q, dev, mach_device_t, number_chain) {
mach_device_reference(dev);
simple_unlock(&dev_number_lock);
if (prev_dev != MACH_DEVICE_NULL)
mach_device_deallocate(prev_dev);
if ((*routine)(dev, port)) {
mach_device_deallocate(dev);
return (TRUE);
}
simple_lock(&dev_number_lock);
prev_dev = dev;
}
simple_unlock(&dev_number_lock);
if (prev_dev != MACH_DEVICE_NULL)
mach_device_deallocate(prev_dev);
}
return (FALSE);
}
void
dev_lookup_init(void)
{
int i;
simple_lock_init(&dev_number_lock);
for (i = 0; i < NDEVHASH; i++)
queue_init(&dev_number_hash_table[i]);
kmem_cache_init(&dev_hdr_cache, "mach_device",
sizeof(struct mach_device), 0, NULL, 0);
}