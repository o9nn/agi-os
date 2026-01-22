#include <error.h>
#include <string.h>
#include "isofs.h"
pthread_spinlock_t node2pagelock = PTHREAD_SPINLOCK_INITIALIZER;
struct port_bucket *pager_bucket;
void *disk_image;
size_t disk_image_len;
error_t
pager_read_page (struct user_pager_info *upi,
vm_offset_t page,
vm_address_t *buf,
int *writelock)
{
error_t err;
daddr_t addr;
struct node *np = upi->np;
size_t read = 0;
size_t overrun = 0;
*writelock = 1;
if (upi->type == FILE_DATA)
{
addr = np->dn->file_start + (page >> store->log2_block_size);
if (page >= np->dn_stat.st_size)
{
*buf = (vm_address_t) mmap (0, vm_page_size, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
return 0;
}
if (page + vm_page_size > np->dn_stat.st_size)
overrun = page + vm_page_size - np->dn_stat.st_size;
}
else
{
assert_backtrace (upi->type == DISK);
addr = page >> store->log2_block_size;
}
err = store_read (store, addr, vm_page_size, (void **) buf, &read);
if (err)
return err;
if (read != vm_page_size)
return EIO;
if (overrun)
memset ((void *)*buf + vm_page_size - overrun, 0, overrun);
return 0;
}
error_t
pager_write_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t buf)
{
assert_backtrace (0);
}
error_t
pager_unlock_page (struct user_pager_info *pager,
vm_offset_t address)
{
return EROFS;
}
void
pager_notify_evict (struct user_pager_info *pager,
vm_offset_t page)
{
assert_backtrace (!"unrequested notification on eviction");
}
error_t
pager_report_extent (struct user_pager_info *pager,
vm_address_t *offset,
vm_size_t *size)
{
*offset = 0;
*size = pager->np->dn_stat.st_size;
return 0;
}
void
pager_clear_user_data (struct user_pager_info *upi)
{
if (upi->type == FILE_DATA)
{
pthread_spin_lock (&node2pagelock);
if (upi->np->dn->fileinfo == upi)
upi->np->dn->fileinfo = 0;
pthread_spin_unlock (&node2pagelock);
diskfs_nrele_light (upi->np);
}
}
void
pager_dropweak (struct user_pager_info *upi)
{
}
void
create_disk_pager (void)
{
struct user_pager_info *upi = malloc (sizeof (struct user_pager_info));
if (!upi)
error (1, errno, "Could not create disk pager");
upi->type = DISK;
upi->np = 0;
pager_bucket = ports_create_bucket ();
diskfs_start_disk_pager (upi, pager_bucket, 1, 0, store->size, &disk_image);
disk_image_len = store->size;
upi->p = diskfs_disk_pager;
}
void
diskfs_file_update (struct node *np,
int wait)
{
}
mach_port_t
diskfs_get_filemap (struct node *np, vm_prot_t prot)
{
struct user_pager_info *upi;
mach_port_t right;
assert_backtrace (S_ISDIR (np->dn_stat.st_mode)
|| S_ISREG (np->dn_stat.st_mode)
|| S_ISLNK (np->dn_stat.st_mode));
assert_backtrace (prot == VM_PROT_READ);
pthread_spin_lock (&node2pagelock);
do
if (!np->dn->fileinfo)
{
struct pager *p;
p = pager_create_alloc (sizeof *upi, pager_bucket, 1,
MEMORY_OBJECT_COPY_DELAY, 0);
if (p == NULL)
{
diskfs_nrele_light (np);
pthread_spin_unlock (&node2pagelock);
return MACH_PORT_NULL;
}
upi = pager_get_upi (p);
upi->type = FILE_DATA;
upi->np = np;
diskfs_nref_light (np);
upi->p = p;
np->dn->fileinfo = upi;
right = pager_get_port (np->dn->fileinfo->p);
ports_port_deref (np->dn->fileinfo->p);
}
else
{
right = pager_get_port (np->dn->fileinfo->p);
if (right == MACH_PORT_NULL)
np->dn->fileinfo = 0;
}
while (right == MACH_PORT_NULL);
pthread_spin_unlock (&node2pagelock);
mach_port_insert_right (mach_task_self (), right, right,
MACH_MSG_TYPE_MAKE_SEND);
return right;
}
void
drop_pager_softrefs (struct node *np)
{
struct user_pager_info *upi;
pthread_spin_lock (&node2pagelock);
upi = np->dn->fileinfo;
if (upi)
ports_port_ref (upi->p);
pthread_spin_unlock (&node2pagelock);
if (upi)
{
pager_change_attributes (upi->p, 0, MEMORY_OBJECT_COPY_DELAY, 0);
ports_port_deref (upi->p);
}
}
void
allow_pager_softrefs (struct node *np)
{
struct user_pager_info *upi;
pthread_spin_lock (&node2pagelock);
upi = np->dn->fileinfo;
if (upi)
ports_port_ref (upi->p);
pthread_spin_unlock (&node2pagelock);
if (upi)
{
pager_change_attributes (upi->p, 1, MEMORY_OBJECT_COPY_DELAY, 0);
ports_port_deref (upi->p);
}
}
static void
block_caching (void)
{
error_t block_cache (void *arg)
{
struct pager *p = arg;
pager_change_attributes (p, 0, MEMORY_OBJECT_COPY_DELAY, 1);
return 0;
}
ports_bucket_iterate (pager_bucket, block_cache);
}
static void
enable_caching (void)
{
error_t enable_cache (void *arg)
{
struct pager *p = arg;
struct user_pager_info *upi = pager_get_upi (p);
pager_change_attributes (p, 1, MEMORY_OBJECT_COPY_DELAY, 0);
if (upi->type == FILE_DATA)
{
diskfs_nref (upi->np);
diskfs_nrele (upi->np);
}
return 0;
}
ports_bucket_iterate (pager_bucket, enable_cache);
}
int
diskfs_pager_users (void)
{
int npagers = ports_count_bucket (pager_bucket);
if (npagers <= 1)
return 0;
block_caching ();
sleep (1);
npagers = ports_count_bucket (pager_bucket);
if (npagers <= 1)
return 0;
enable_caching ();
ports_enable_bucket (pager_bucket);
return 1;
}
vm_prot_t
diskfs_max_user_pager_prot (void)
{
return VM_PROT_READ | VM_PROT_EXECUTE;
}
struct pager *
diskfs_get_filemap_pager_struct (struct node *np)
{
return np->dn->fileinfo->p;
}
void
diskfs_shutdown_pager (void)
{
}
void
diskfs_sync_everything (int wait)
{
}