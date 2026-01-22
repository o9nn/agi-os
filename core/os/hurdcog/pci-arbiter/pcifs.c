#include "pcifs.h"
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <hurd/netfs.h>
#include "ncache.h"
#include "func_files.h"
static error_t
create_dir_entry (int32_t domain, int16_t bus, int16_t dev,
int16_t func, int32_t device_class, char *name,
struct pcifs_dirent *parent, io_statbuf_t stat,
struct node *node, struct pci_device *device,
struct pcifs_dirent *entry)
{
uint16_t parent_num_entries;
memset (entry, 0, sizeof (struct pcifs_dirent));
entry->domain = domain;
entry->bus = bus;
entry->dev = dev;
entry->func = func;
entry->device_class = device_class;
strncpy (entry->name, name, NAME_SIZE - 1);
entry->name[NAME_SIZE - 1] = '\0';
entry->parent = parent;
entry->stat = stat;
entry->dir = 0;
entry->node = node;
entry->device = device;
if (entry->parent)
{
if (!entry->parent->dir)
{
entry->parent->dir = calloc (1, sizeof (struct pcifs_dir));
if (!entry->parent->dir)
return ENOMEM;
}
parent_num_entries = entry->parent->dir->num_entries++;
entry->parent->dir->entries = realloc (entry->parent->dir->entries,
entry->parent->dir->num_entries *
sizeof (struct pcifs_dirent *));
if (!entry->parent->dir->entries)
return ENOMEM;
entry->parent->dir->entries[parent_num_entries] = entry;
}
return 0;
}
error_t
alloc_file_system (struct pcifs ** fs)
{
*fs = calloc (1, sizeof (struct pcifs));
if (!*fs)
return ENOMEM;
return 0;
}
error_t
init_root_node (file_t underlying_node)
{
error_t err;
struct node *np;
io_statbuf_t underlying_node_stat = { 0 };
if (underlying_node != MACH_PORT_NULL)
{
err = io_stat (underlying_node, &underlying_node_stat);
if (err)
return err;
}
np = netfs_make_node_alloc (sizeof (struct netnode));
if (!np)
return ENOMEM;
np->nn_stat = underlying_node_stat;
np->nn_stat.st_fsid = getpid ();
np->nn_stat.st_mode =
S_IFDIR | S_IROOT | S_IRUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH |
S_IXOTH;
np->nn_translated = np->nn_stat.st_mode;
fshelp_touch (&np->nn_stat, TOUCH_ATIME | TOUCH_MTIME | TOUCH_CTIME,
pcifs_maptime);
netfs_root_node = np;
return 0;
}
error_t
init_file_system (struct pcifs * fs)
{
error_t err;
struct node *np = netfs_root_node;
fs->entries = calloc (1, sizeof (struct pcifs_dirent));
if (!fs->entries)
return ENOMEM;
err =
create_dir_entry (-1, -1, -1, -1, -1, "", 0, np->nn_stat, np, 0,
fs->entries);
if (err)
return err;
fs->num_entries = 1;
fs->root = np;
fs->root->nn->ln = fs->entries;
pthread_mutex_init (&fs->node_cache_lock, 0);
pthread_mutex_init (&fs->pci_conf_lock, 0);
return 0;
}
error_t
create_fs_tree (struct pcifs * fs)
{
error_t err = 0;
int c_domain, c_bus, c_dev, i, j;
size_t nentries;
struct pci_device *device;
struct pcifs_dirent *e, *domain_parent, *bus_parent, *dev_parent,
*func_parent, *list;
struct stat e_stat;
char entry_name[NAME_SIZE];
const struct pci_slot_match match =
{ PCI_MATCH_ANY, PCI_MATCH_ANY, PCI_MATCH_ANY, PCI_MATCH_ANY, 0 };
struct pci_device_iterator *iter;
nentries = 1;
c_domain = c_bus = c_dev = -1;
iter = pci_slot_match_iterator_create(&match);
device = pci_device_next(iter);
for (i = 0; device != NULL; i++, device = pci_device_next(iter) )
{
if (device->domain != c_domain)
{
c_domain = device->domain;
c_bus = -1;
c_dev = -1;
nentries++;
}
if (device->bus != c_bus)
{
c_bus = device->bus;
c_dev = -1;
nentries++;
}
if (device->dev != c_dev)
{
c_dev = device->dev;
nentries++;
}
nentries += 2;
err = pci_device_probe (device);
if (!err)
{
for (j = 0; j < 6; j++)
if (device->regions[j].size > 0)
nentries++;
if (device->rom_size)
nentries++;
}
}
pci_iterator_destroy(iter);
if (nentries == 1)
{
return 0;
}
list = realloc (fs->entries, nentries * sizeof (struct pcifs_dirent));
if (!list)
return ENOMEM;
e = list + 1;
c_domain = c_bus = c_dev = -1;
domain_parent = bus_parent = dev_parent = func_parent = 0;
iter = pci_slot_match_iterator_create(&match);
device = pci_device_next(iter);
for (i = 0; device != NULL; i++, device = pci_device_next(iter))
{
if (device->domain != c_domain)
{
e_stat = list->stat;
e_stat.st_mode &= ~S_IROOT;
snprintf (entry_name, NAME_SIZE, "%04x", device->domain);
err =
create_dir_entry (device->domain, -1, -1, -1, -1, entry_name,
list, e_stat, 0, 0, e);
if (err)
return err;
domain_parent = e++;
c_domain = device->domain;
c_bus = -1;
c_dev = -1;
}
if (device->bus != c_bus)
{
snprintf (entry_name, NAME_SIZE, "%02x", device->bus);
err =
create_dir_entry (device->domain, device->bus, -1, -1, -1,
entry_name, domain_parent, domain_parent->stat,
0, 0, e);
if (err)
return err;
bus_parent = e++;
c_bus = device->bus;
c_dev = -1;
}
if (device->dev != c_dev)
{
snprintf (entry_name, NAME_SIZE, "%02x", device->dev);
err =
create_dir_entry (device->domain, device->bus, device->dev, -1,
-1, entry_name, bus_parent, bus_parent->stat, 0,
0, e);
if (err)
return err;
dev_parent = e++;
c_dev = device->dev;
}
e_stat = dev_parent->stat;
e_stat.st_mode &= ~(S_IROTH | S_IWOTH | S_IXOTH);
snprintf (entry_name, NAME_SIZE, "%01u", device->func);
err =
create_dir_entry (device->domain, device->bus, device->dev,
device->func, device->device_class, entry_name,
dev_parent, e_stat, 0, device, e);
if (err)
return err;
func_parent = e++;
e_stat = func_parent->stat;
e_stat.st_mode &= ~(S_IFDIR | S_IXUSR | S_IXGRP);
e_stat.st_mode |= S_IFREG | S_IWUSR | S_IWGRP;
e_stat.st_size = PCI_CONFIG_SIZE;
strncpy (entry_name, FILE_CONFIG_NAME, NAME_SIZE - 1);
err =
create_dir_entry (device->domain, device->bus, device->dev,
device->func, device->device_class, entry_name,
func_parent, e_stat, 0, device, e++);
if (err)
return err;
for (j = 0; j < 6; j++)
{
if (device->regions[j].size > 0)
{
e_stat.st_size = device->regions[j].size;
snprintf (entry_name, NAME_SIZE, "%s%01u", FILE_REGION_NAME, j);
err =
create_dir_entry (device->domain, device->bus, device->dev,
device->func, device->device_class,
entry_name, func_parent, e_stat, 0, device,
e++);
if (err)
return err;
}
}
if (device->rom_size)
{
e_stat.st_mode &= ~(S_IWUSR | S_IWGRP);
e_stat.st_size = device->rom_size;
strncpy (entry_name, FILE_ROM_NAME, NAME_SIZE - 1);
err =
create_dir_entry (device->domain, device->bus, device->dev,
device->func, device->device_class, entry_name,
func_parent, e_stat, 0, device, e++);
if (err)
return err;
}
}
pci_iterator_destroy(iter);
fs->entries = list;
fs->num_entries = nentries;
fs->root->nn->ln = fs->entries;
return err;
}
error_t
entry_check_perms (struct iouser * user, struct pcifs_dirent * e, int flags)
{
error_t err = 0;
if (!err && (flags & O_READ))
err = fshelp_access (&e->stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&e->stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&e->stat, S_IEXEC, user);
return err;
}
static void
entry_default_perms (struct pcifs *fs, struct pcifs_dirent *e)
{
UPDATE_OWNER (e, fs->root->nn->ln->stat.st_uid);
UPDATE_GROUP (e, fs->root->nn->ln->stat.st_gid);
UPDATE_TIMES (e, TOUCH_CTIME);
return;
}
static void
entry_set_perms (struct pcifs *fs, struct pcifs_dirent *e)
{
int i;
struct pcifs_perm *perms = fs->params.perms, *p;
size_t num_perms = fs->params.num_perms;
for (i = 0, p = perms; i < num_perms; i++, p++)
{
uint8_t e_class = e->device_class >> 16;
uint8_t e_subclass = ((e->device_class >> 8) & 0xFF);
if (p->d_class >= 0 && e_class != p->d_class)
continue;
if (p->d_subclass >= 0 && e_subclass != p->d_subclass)
continue;
if (p->domain >= 0 && p->domain != e->domain)
continue;
if (p->bus >= 0 && e->bus != p->bus)
continue;
if (p->dev >= 0 && e->dev != p->dev)
continue;
if (p->func >= 0 && e->func != p->func)
continue;
if (p->uid >= 0)
UPDATE_OWNER (e, p->uid);
if (p->gid >= 0)
UPDATE_GROUP (e, p->gid);
UPDATE_TIMES (e, TOUCH_CTIME);
break;
}
return;
}
error_t
fs_set_permissions (struct pcifs * fs)
{
int i;
struct pcifs_dirent *e;
for (i = 0, e = fs->entries; i < fs->num_entries; i++, e++)
{
entry_default_perms (fs, e);
entry_set_perms (fs, e);
}
return 0;
}