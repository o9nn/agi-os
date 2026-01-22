#include "netfs_impl.h"
#include <stddef.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <hurd/netfs.h>
#include <hurd/paths.h>
#include <mach/mach4.h>
#include <device/device.h>
#include <pciaccess.h>
#include "pcifs.h"
#include "ncache.h"
#include "func_files.h"
#include "device_map.h"
#define DIRENTS_CHUNK_SIZE (8*1024)
#define DIRENT_ALIGN 4
#define DIRENT_NAME_OFFS offsetof (struct dirent, d_name)
#define DIRENT_LEN(name_len) \
((DIRENT_NAME_OFFS + (name_len) + 1 + (DIRENT_ALIGN - 1)) \
& ~(DIRENT_ALIGN - 1))
static error_t
get_dirents (struct pcifs_dirent *dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t * data_len,
vm_size_t max_data_len, int *data_entries)
{
struct pcifs_dirent *e;
error_t err = 0;
int i, count;
size_t size;
char *p;
int nentries = (int)dir->dir->num_entries;
if (first_entry >= nentries)
{
*data_len = 0;
*data_entries = 0;
return 0;
}
count = nentries - first_entry;
if (max_entries >= 0 && count > max_entries)
count = max_entries;
size = count * DIRENTS_CHUNK_SIZE;
if (max_data_len && size > max_data_len)
size = max_data_len;
*data = mmap (0, size, PROT_READ | PROT_WRITE, MAP_ANON, 0, 0);
err = ((void *) *data == (void *) -1) ? errno : 0;
if (err)
return err;
p = *data;
for (i = 0; i < count; i++)
{
struct dirent hdr;
size_t name_len;
size_t sz;
int entry_type;
e = dir->dir->entries[i + first_entry];
name_len = strlen (e->name) + 1;
sz = DIRENT_LEN (name_len);
entry_type = IFTODT (e->stat.st_mode);
hdr.d_namlen = name_len;
hdr.d_fileno = e->stat.st_ino;
hdr.d_reclen = sz;
hdr.d_type = entry_type;
memcpy (p, &hdr, DIRENT_NAME_OFFS);
strncpy (p + DIRENT_NAME_OFFS, e->name, name_len);
p += sz;
}
vm_address_t alloc_end = (vm_address_t) (*data + size);
vm_address_t real_end = round_page (p);
if (alloc_end > real_end)
munmap ((caddr_t) real_end, alloc_end - real_end);
*data_len = p - *data;
*data_entries = count;
return err;
}
static struct pcifs_dirent *
lookup (struct node *np, const char *name)
{
int i;
struct pcifs_dirent *ret = 0, *e;
for (i = 0; i < np->nn->ln->dir->num_entries; i++)
{
e = np->nn->ln->dir->entries[i];
if (!strncmp (e->name, name, NAME_SIZE))
{
ret = e;
break;
}
}
return ret;
}
static error_t
create_node (struct pcifs_dirent * e, struct node ** node)
{
struct node *np;
struct netnode *nn;
np = netfs_make_node_alloc (sizeof (struct netnode));
if (!np)
return ENOMEM;
np->nn_stat = e->stat;
np->nn_translated = np->nn_stat.st_mode;
nn = netfs_node_netnode (np);
memset (nn, 0, sizeof (struct netnode));
nn->ln = e;
*node = e->node = np;
return 0;
}
static void
destroy_node (struct node *node)
{
if (node->nn->ln)
node->nn->ln->node = 0;
free (node);
}
error_t
netfs_attempt_create_file (struct iouser * user, struct node * dir,
const char *name, mode_t mode, struct node ** node)
{
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser * user, struct node * node,
int flags, int newnode)
{
return entry_check_perms (user, node->nn->ln, flags);
}
error_t
netfs_attempt_utimes (struct iouser * cred, struct node * node,
struct timespec * atime, struct timespec * mtime)
{
return EOPNOTSUPP;
}
error_t
netfs_report_access (struct iouser * cred, struct node * node, int *types)
{
return EOPNOTSUPP;
}
error_t
netfs_validate_stat (struct node * node, struct iouser * cred)
{
return 0;
}
error_t
netfs_attempt_sync (struct iouser * cred, struct node * node, int wait)
{
return EOPNOTSUPP;
}
error_t
netfs_get_dirents (struct iouser * cred, struct node * dir,
int first_entry, int max_entries, char **data,
mach_msg_type_number_t * data_len,
vm_size_t max_data_len, int *data_entries)
{
error_t err = 0;
if (dir->nn->ln->dir)
{
err = get_dirents (dir->nn->ln, first_entry, max_entries,
data, data_len, max_data_len, data_entries);
}
else
err = ENOTDIR;
if (!err)
UPDATE_TIMES (dir->nn->ln, TOUCH_ATIME);
return err;
}
error_t
netfs_attempt_lookup (struct iouser * user, struct node * dir,
const char *name, struct node ** node)
{
error_t err = 0;
struct pcifs_dirent *entry;
if (*name == '\0' || strcmp (name, ".") == 0)
{
netfs_nref (dir);
*node = dir;
return 0;
}
else if (strcmp (name, "..") == 0)
{
if (dir->nn->ln->parent)
{
*node = dir->nn->ln->parent->node;
pthread_mutex_lock (&(*node)->lock);
netfs_nref (*node);
}
else
{
err = ENOENT;
*node = 0;
}
pthread_mutex_unlock (&dir->lock);
return err;
}
if (dir->nn->ln->dir)
{
err = entry_check_perms (user, dir->nn->ln, O_READ | O_EXEC);
if (!err)
{
entry = lookup (dir, name);
if (!entry)
{
err = ENOENT;
}
else
{
if (entry->node)
{
netfs_nref (entry->node);
}
else
{
err = create_node (entry, node);
}
if (!err)
{
*node = entry->node;
pthread_mutex_unlock (&dir->lock);
pthread_mutex_lock (&(*node)->lock);
}
}
}
}
else
{
err = ENOTDIR;
}
if (err)
{
*node = 0;
pthread_mutex_unlock (&dir->lock);
}
else
{
node_cache (*node);
}
return err;
}
error_t
netfs_attempt_unlink (struct iouser * user, struct node * dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rename (struct iouser * user, struct node * fromdir,
const char *fromname, struct node * todir,
const char *toname, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdir (struct iouser * user, struct node * dir,
const char *name, mode_t mode)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rmdir (struct iouser * user, struct node * dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chown (struct iouser * cred, struct node * node,
uid_t uid, uid_t gid)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chauthor (struct iouser * cred, struct node * node,
uid_t author)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chmod (struct iouser * cred, struct node * node, mode_t mode)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mksymlink (struct iouser * cred, struct node * node, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdev (struct iouser * cred, struct node * node,
mode_t type, dev_t indexes)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chflags (struct iouser * cred, struct node * node, int flags)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_set_size (struct iouser * cred, struct node * node, off_t size)
{
return 0;
}
error_t
netfs_attempt_statfs (struct iouser * cred, struct node * node,
struct statfs * st)
{
memset (st, 0, sizeof *st);
st->f_type = FSTYPE_PCI;
st->f_fsid = getpid ();
return 0;
}
error_t
netfs_attempt_syncfs (struct iouser * cred, int wait)
{
return 0;
}
error_t
netfs_attempt_link (struct iouser * user, struct node * dir,
struct node * file, const char *name, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkfile (struct iouser * user, struct node * dir,
mode_t mode, struct node ** node)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_readlink (struct iouser * user, struct node * node, char *buf)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_read (struct iouser * cred, struct node * node,
off_t offset, size_t * len, void *data)
{
error_t err;
if (!strncmp (node->nn->ln->name, FILE_CONFIG_NAME, NAME_SIZE))
{
err =
io_config_file (node->nn->ln->device, offset, len, data,
pci_device_cfg_read);
if (!err)
UPDATE_TIMES (node->nn->ln, TOUCH_ATIME);
}
else if (!strncmp (node->nn->ln->name, FILE_ROM_NAME, NAME_SIZE))
{
err = read_rom_file (node->nn->ln, offset, len, data);
if (!err)
UPDATE_TIMES (node->nn->ln, TOUCH_ATIME);
}
else if (!strncmp
(node->nn->ln->name, FILE_REGION_NAME, strlen (FILE_REGION_NAME)))
{
err = io_region_file (node->nn->ln, offset, len, data, 1);
if (!err)
UPDATE_TIMES (node->nn->ln, TOUCH_ATIME);
}
else
return EOPNOTSUPP;
return err;
}
error_t
netfs_attempt_write (struct iouser * cred, struct node * node,
off_t offset, size_t * len, const void *data)
{
error_t err;
if (!strncmp (node->nn->ln->name, FILE_CONFIG_NAME, NAME_SIZE))
{
err =
io_config_file (node->nn->ln->device, offset, len, (void*) data,
(pci_io_op_t) pci_device_cfg_write);
if (!err)
{
UPDATE_TIMES (node->nn->ln, TOUCH_MTIME | TOUCH_CTIME);
}
}
else if (!strncmp
(node->nn->ln->name, FILE_REGION_NAME, strlen (FILE_REGION_NAME)))
{
err = io_region_file (node->nn->ln, offset, len, (void*) data, 0);
if (!err)
UPDATE_TIMES (node->nn->ln, TOUCH_MTIME | TOUCH_CTIME);
}
else
return EOPNOTSUPP;
return err;
}
void
netfs_node_norefs (struct node *node)
{
destroy_node (node);
}
static mach_port_t
get_filemap_region (struct node *node, vm_prot_t prot)
{
error_t err;
memory_object_t proxy;
vm_prot_t max_prot;
size_t reg_num;
struct pci_mem_region *region;
size_t rounded_size;
reg_num =
strtol (&node->nn->ln->name[strlen (node->nn->ln->name) - 1], 0, 16);
region = &node->nn->ln->device->regions[reg_num];
if (region->is_IO)
goto error;
err = device_map_region (node->nn->ln->device, region,
&node->nn->ln->region_maps[reg_num]);
if (err)
goto error;
rounded_size = round_page (region->size);
max_prot = (VM_PROT_READ | VM_PROT_WRITE) & prot;
err =
vm_region_create_proxy (mach_task_self (),
(vm_address_t) node->nn->ln->region_maps[reg_num],
max_prot, rounded_size, &proxy);
if (err)
goto error;
return proxy;
error:
errno = EOPNOTSUPP;
return MACH_PORT_NULL;
}
static mach_port_t
get_filemap_rom (struct node *node, vm_prot_t prot)
{
error_t err;
memory_object_t proxy;
vm_prot_t max_prot;
err = device_map_rom (node->nn->ln->device, &node->nn->ln->rom_map);
if (err)
goto error;
max_prot = (VM_PROT_READ) & prot;
err =
vm_region_create_proxy (mach_task_self (),
(vm_address_t) node->nn->ln->rom_map,
max_prot, node->nn->ln->device->rom_size, &proxy);
if (err)
goto error;
return proxy;
error:
errno = EOPNOTSUPP;
return MACH_PORT_NULL;
}
mach_port_t
netfs_get_filemap (struct node *node, vm_prot_t prot)
{
if (!strncmp
(node->nn->ln->name, FILE_REGION_NAME, strlen (FILE_REGION_NAME)))
{
return get_filemap_region (node, prot);
}
if (!strncmp (node->nn->ln->name, FILE_ROM_NAME, strlen (FILE_ROM_NAME)))
{
return get_filemap_rom (node, prot);
}
errno = EOPNOTSUPP;
return MACH_PORT_NULL;
}