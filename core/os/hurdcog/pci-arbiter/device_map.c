#include <pciaccess.h>
#include "device_map.h"
error_t
device_map_region (struct pci_device *device, struct pci_mem_region *region,
void **addr)
{
error_t err = 0;
if (*addr == 0)
{
if (region->base_addr > 0x100000
|| region->base_addr + region->size > 0x100000)
err = pci_device_map_range (device, region->base_addr, region->size,
PCI_DEV_MAP_FLAG_WRITABLE, addr);
else
err = pci_device_map_legacy (device, region->base_addr, region->size,
PCI_DEV_MAP_FLAG_WRITABLE, addr);
}
return err;
}
error_t
device_map_rom (struct pci_device *device, void **addr)
{
error_t err = 0;
vm_address_t fullrom;
if (*addr == 0)
{
err = vm_allocate (mach_task_self (), &fullrom, device->rom_size, 1);
if (err)
return ENOMEM;
err = pci_device_read_rom (device, (void *) fullrom);
if (err)
return err;
*addr = (void *) fullrom;
}
return err;
}