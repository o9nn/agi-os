#ifndef DEVICE_MAP_H
#define DEVICE_MAP_H
#include <hurd.h>
#include <pciaccess.h>
error_t device_map_region (struct pci_device *device,
struct pci_mem_region *region, void **addr);
error_t device_map_rom (struct pci_device *device, void **addr);
#endif