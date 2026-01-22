#ifndef FUNC_FILES_H
#define FUNC_FILES_H
#include "pcifs.h"
#include <pciaccess.h>
typedef int (*pci_io_op_t) (struct pci_device *dev, void *data,
pciaddr_t reg, pciaddr_t width, pciaddr_t *bytes);
#define FILE_CONFIG_NAME "config"
#define FILE_ROM_NAME "rom"
#define FILE_REGION_NAME "region"
error_t io_config_file (struct pci_device * dev, off_t offset, size_t * len,
void *data, pci_io_op_t op);
error_t read_rom_file (struct pcifs_dirent * e, off_t offset, size_t * len,
void *data);
error_t io_region_file (struct pcifs_dirent *e, off_t offset, size_t * len,
void *data, int read);
#endif