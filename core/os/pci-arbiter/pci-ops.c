#include <pci_S.h>
#include <fcntl.h>
#include <hurd/netfs.h>
#include <sys/mman.h>
#include <pciaccess.h>
#include "pcifs.h"
#include "func_files.h"
static error_t
check_permissions (struct protid *master, int flags)
{
error_t err = 0;
struct node *node;
struct pcifs_dirent *e;
node = master->po->np;
e = node->nn->ln;
err = entry_check_perms (master->user, e, flags);
if (err)
return err;
if (e->domain != 0
|| e->bus < 0 || e->dev < 0 || e->func < 0)
err = EINVAL;
return err;
}
static size_t
calculate_ndevs (struct iouser *user)
{
size_t ndevs = 0;
int i;
struct pcifs_dirent *e;
for (i = 0, e = fs->entries; i < fs->num_entries; i++, e++)
{
if (e->func < 0
|| !S_ISDIR (e->stat.st_mode))
continue;
if (!entry_check_perms (user, e, O_READ))
ndevs++;
}
return ndevs;
}
kern_return_t
S_pci_conf_read (struct protid * master, int reg, char **data,
mach_msg_type_number_t * datalen, vm_size_t amount)
{
error_t err;
pthread_mutex_t *lock;
struct pcifs_dirent *e;
pciaddr_t actual_len;
if (!master)
return EOPNOTSUPP;
e = master->po->np->nn->ln;
if (strncmp (e->name, FILE_CONFIG_NAME, NAME_SIZE))
return EINVAL;
lock = &fs->pci_conf_lock;
err = check_permissions (master, O_READ);
if (err)
return err;
if (amount > *datalen)
amount = *datalen;
pthread_mutex_lock (lock);
err = pci_device_cfg_read (e->device, *data, reg, amount, &actual_len);
pthread_mutex_unlock (lock);
if (!err)
{
*datalen = (size_t)actual_len;
UPDATE_TIMES (e, TOUCH_ATIME);
}
return err;
}
kern_return_t
S_pci_conf_write (struct protid * master, int reg, const char *data, mach_msg_type_number_t datalen,
vm_size_t * amount)
{
error_t err;
pthread_mutex_t *lock;
struct pcifs_dirent *e;
pciaddr_t actual_len;
if (!master)
return EOPNOTSUPP;
e = master->po->np->nn->ln;
if (strncmp (e->name, FILE_CONFIG_NAME, NAME_SIZE))
return EINVAL;
lock = &fs->pci_conf_lock;
err = check_permissions (master, O_WRITE);
if (err)
return err;
pthread_mutex_lock (lock);
err = pci_device_cfg_write (e->device, data, reg, datalen, &actual_len);
pthread_mutex_unlock (lock);
if (!err)
{
*amount = (size_t)actual_len;
UPDATE_TIMES (e, TOUCH_MTIME | TOUCH_CTIME);
}
return err;
}
kern_return_t
S_pci_get_ndevs (struct protid * master, mach_msg_type_number_t * amount)
{
if (master->po->np != fs->root)
return EINVAL;
*amount = calculate_ndevs (master->user);
return 0;
}
kern_return_t
S_pci_get_dev_regions (struct protid * master, char **data, mach_msg_type_number_t * datalen)
{
error_t err;
struct pcifs_dirent *e;
struct pci_bar regions[6], *r;
size_t size;
int i;
if (!master)
return EOPNOTSUPP;
e = master->po->np->nn->ln;
if (strncmp (e->name, FILE_CONFIG_NAME, NAME_SIZE))
return EINVAL;
err = check_permissions (master, O_READ);
if (err)
return err;
size = sizeof (regions);
if (size > *datalen)
{
*data = mmap (0, size, PROT_READ | PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
return ENOMEM;
}
for (i = 0, r = (struct pci_bar *) *data; i < 6; i++, r++)
{
r->base_addr = e->device->regions[i].base_addr;
r->size = e->device->regions[i].size;
r->is_IO = e->device->regions[i].is_IO;
r->is_prefetchable = e->device->regions[i].is_prefetchable;
r->is_64 = e->device->regions[i].is_64;
}
UPDATE_TIMES (e, TOUCH_ATIME);
*datalen = size;
return 0;
}
kern_return_t
S_pci_get_dev_rom (struct protid * master, char **data, mach_msg_type_number_t * datalen)
{
error_t err;
struct pcifs_dirent *e;
struct pci_xrom_bar rom;
size_t size;
if (!master)
return EOPNOTSUPP;
e = master->po->np->nn->ln;
if (strncmp (e->name, FILE_CONFIG_NAME, NAME_SIZE))
return EINVAL;
err = check_permissions (master, O_READ);
if (err)
return err;
size = sizeof (rom);
if (size > *datalen)
{
*data = mmap (0, size, PROT_READ | PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
return ENOMEM;
}
rom.base_addr = 0;
rom.size = e->device->rom_size;
memcpy (*data, &rom, size);
UPDATE_TIMES (e, TOUCH_ATIME);
*datalen = size;
return 0;
}