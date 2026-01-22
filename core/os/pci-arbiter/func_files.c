#include "func_files.h"
#include <assert.h>
#include <sys/io.h>
#include <pciaccess.h>
#include "device_map.h"
static error_t
config_block_op (struct pci_device *dev, off_t offset, size_t * len,
void *data, pci_io_op_t op)
{
error_t err;
size_t pendent = *len;
pciaddr_t actual = 0;
while (pendent >= 4)
{
err = op (dev, data, offset, 4, &actual);
if (err)
return err;
offset += 4;
data += 4;
pendent -= 4;
}
if (pendent >= 2)
{
err = op (dev, data, offset, 2, &actual);
if (err)
return err;
offset += 2;
data += 2;
pendent -= 2;
}
if (pendent)
{
err = op (dev, data, offset, 1, &actual);
if (err)
return err;
offset++;
data++;
pendent--;
}
*len -= pendent;
return 0;
}
error_t
io_config_file (struct pci_device * dev, off_t offset, size_t * len,
void *data, pci_io_op_t op)
{
error_t err;
assert_backtrace (dev != 0);
if (offset > PCI_CONFIG_SIZE)
return EINVAL;
if ((offset + *len) > PCI_CONFIG_SIZE)
*len = PCI_CONFIG_SIZE - offset;
pthread_mutex_lock (&fs->pci_conf_lock);
err = config_block_op (dev, offset, len, data, op);
pthread_mutex_unlock (&fs->pci_conf_lock);
return err;
}
error_t
read_rom_file (struct pcifs_dirent * e, off_t offset, size_t * len,
void *data)
{
error_t err;
assert_backtrace (e->device != 0);
if (offset > e->device->rom_size)
return EINVAL;
if ((offset + *len) > e->device->rom_size)
*len = e->device->rom_size - offset;
err = device_map_rom (e->device, &e->rom_map);
if (err)
return err;
memcpy (data, e->rom_map + offset, *len);
return 0;
}
static error_t
region_block_ioport_op (uint16_t port, off_t offset, size_t * len,
void *data, int read)
{
size_t pending = *len;
while (pending >= 4)
{
if (read)
*((unsigned int *) data) = inl (port + offset);
else
outl (*((unsigned int *) data), port + offset);
offset += 4;
data += 4;
pending -= 4;
}
if (pending >= 2)
{
if (read)
*((unsigned short *) data) = inw (port + offset);
else
outw (*((unsigned short *) data), port + offset);
offset += 2;
data += 2;
pending -= 2;
}
if (pending)
{
if (read)
*((unsigned char *) data) = inb (port + offset);
else
outb (*((unsigned char *) data), port + offset);
offset++;
data++;
pending--;
}
*len -= pending;
return 0;
}
error_t
io_region_file (struct pcifs_dirent * e, off_t offset, size_t * len,
void *data, int read)
{
error_t err = 0;
size_t reg_num;
struct pci_mem_region *region;
assert_backtrace (e->device != 0);
reg_num = strtol (&e->name[strlen (e->name) - 1], 0, 16);
region = &e->device->regions[reg_num];
if (offset > region->size)
return EINVAL;
if ((offset + *len) > region->size)
*len = region->size - offset;
if (region->is_IO)
region_block_ioport_op (region->base_addr, offset, len, data, read);
else
{
err = device_map_region (e->device, region, &e->region_maps[reg_num]);
if (err)
return err;
if (read)
memcpy (data, e->region_maps[reg_num] + offset, *len);
else
memcpy (e->region_maps[reg_num] + offset, data, *len);
}
return err;
}