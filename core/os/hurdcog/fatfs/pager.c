#include <error.h>
#include <string.h>
#include <hurd/store.h>
#include "fatfs.h"
struct port_bucket *disk_pager_bucket;
struct port_bucket *file_pager_bucket;
struct pager_requests *file_pager_requests;
void *fat_image;
pthread_spinlock_t node_to_page_lock = PTHREAD_SPINLOCK_INITIALIZER;
#ifdef DONT_CACHE_MEMORY_OBJECTS
#define MAY_CACHE 0
#else
#define MAY_CACHE 1
#endif
#define STAT_INC(field) (void) 0
#define MAX_FREE_PAGE_BUFS 32
static pthread_spinlock_t free_page_bufs_lock = PTHREAD_SPINLOCK_INITIALIZER;
static void *free_page_bufs = 0;
static int num_free_page_bufs = 0;
static void *
get_page_buf (void)
{
void *buf;
pthread_spin_lock (&free_page_bufs_lock);
buf = free_page_bufs;
if (buf == 0)
{
pthread_spin_unlock (&free_page_bufs_lock);
buf = mmap (0, vm_page_size, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf == (void *) -1)
buf = 0;
}
else
{
free_page_bufs = *(void **)buf;
num_free_page_bufs--;
pthread_spin_unlock (&free_page_bufs_lock);
}
return buf;
}
static void
free_page_buf (void *buf)
{
pthread_spin_lock (&free_page_bufs_lock);
if (num_free_page_bufs < MAX_FREE_PAGE_BUFS)
{
*(void **)buf = free_page_bufs;
free_page_bufs = buf;
num_free_page_bufs++;
pthread_spin_unlock (&free_page_bufs_lock);
}
else
{
pthread_spin_unlock (&free_page_bufs_lock);
munmap (buf, vm_page_size);
}
}
static error_t
find_cluster (struct node *node, vm_offset_t offset,
cluster_t *cluster, pthread_rwlock_t **lock)
{
error_t err;
if (!*lock)
{
*lock = &node->dn->alloc_lock;
pthread_rwlock_rdlock (*lock);
}
if (round_cluster (offset) > node->allocsize)
return EIO;
err = fat_getcluster (node, offset >> log2_bytes_per_cluster, 0, cluster);
return err;
}
static error_t
root_dir_pager_read_page (vm_offset_t page, void **buf, int *writelock)
{
error_t err;
daddr_t addr;
int overrun = 0;
size_t read = 0;
*writelock = 0;
if (page >= diskfs_root_node->allocsize)
{
return EIO;
}
pthread_rwlock_rdlock (&diskfs_root_node->dn->alloc_lock);
addr = first_root_dir_byte + page;
if (page + vm_page_size > diskfs_root_node->allocsize)
overrun = page + vm_page_size - diskfs_root_node->allocsize;
err = store_read (store, addr >> store->log2_block_size,
vm_page_size, (void **) buf, &read);
if (!err && read != vm_page_size)
err = EIO;
pthread_rwlock_unlock (&diskfs_root_node->dn->alloc_lock);
if (overrun)
memset ((void *)*buf + vm_page_size - overrun, 0, overrun);
return err;
}
static error_t
file_pager_read_small_page (struct node *node, vm_offset_t page,
void **buf, int *writelock)
{
error_t err;
pthread_rwlock_t *lock = NULL;
cluster_t cluster;
size_t read = 0;
*writelock = 0;
if (page >= node->allocsize)
{
return EIO;
}
err = find_cluster (node, page, &cluster, &lock);
if (!err)
{
err = store_read (store,
FAT_FIRST_CLUSTER_BLOCK(cluster)
+ ((page % bytes_per_cluster)
>> store->log2_block_size),
vm_page_size, (void **) buf, &read);
if (read != vm_page_size)
err = EIO;
}
if (lock)
pthread_rwlock_unlock (lock);
return err;
}
static error_t
file_pager_read_huge_page (struct node *node, vm_offset_t page,
void **buf, int *writelock)
{
error_t err = 0;
int offs = 0;
pthread_rwlock_t *lock = NULL;
int left = vm_page_size;
cluster_t pending_clusters = 0;
int num_pending_clusters = 0;
error_t do_pending_reads (void)
{
if (num_pending_clusters > 0)
{
size_t dev_block = FAT_FIRST_CLUSTER_BLOCK(pending_clusters);
size_t amount = num_pending_clusters << log2_bytes_per_cluster;
void *new_buf = *buf + offs;
size_t new_len = offs == 0 ? 0 : vm_page_size - offs;
STAT_INC (file_pagein_reads);
err = store_read (store, dev_block, amount, &new_buf, &new_len);
if (err)
return err;
else if (amount != new_len)
return EIO;
if (new_buf != *buf + offs)
{
if (offs == 0)
*buf = new_buf;
else
{
memcpy (*buf + offs, new_buf, new_len);
free_page_buf (new_buf);
STAT_INC (file_pagein_freed_bufs);
}
}
offs += new_len;
num_pending_clusters = 0;
}
return 0;
}
STAT_INC (file_pageins);
*writelock = 0;
if (page >= node->allocsize)
{
err = EIO;
left = 0;
}
else if (page + left > node->allocsize)
left = node->allocsize - page;
while (left > 0)
{
cluster_t cluster;
err = find_cluster (node, page, &cluster, &lock);
if (err)
break;
if (cluster != pending_clusters + num_pending_clusters)
{
err = do_pending_reads ();
if (err)
break;
pending_clusters = cluster;
}
num_pending_clusters++;
page += bytes_per_cluster;
left -= bytes_per_cluster;
}
if (!err && num_pending_clusters > 0)
err = do_pending_reads();
if (lock)
pthread_rwlock_unlock (lock);
return err;
}
struct pending_clusters
{
cluster_t cluster;
loff_t num;
void *buf;
int offs;
};
static error_t
pending_clusters_write (struct pending_clusters *pc)
{
if (pc->num > 0)
{
error_t err;
size_t dev_block = FAT_FIRST_CLUSTER_BLOCK(pc->cluster);
size_t length = pc->num << log2_bytes_per_cluster, amount;
if (pc->offs > 0)
{
void *page_buf = get_page_buf ();
memcpy ((void *) page_buf, pc->buf + pc->offs, length);
err = store_write (store, dev_block, page_buf, length, &amount);
free_page_buf (page_buf);
}
else
err = store_write (store, dev_block, pc->buf, length, &amount);
if (err)
return err;
else if (amount != length)
return EIO;
pc->offs += length;
pc->num = 0;
}
return 0;
}
static void
pending_clusters_init (struct pending_clusters *pc, void *buf)
{
pc->buf = buf;
pc->cluster = 0;
pc->num = 0;
pc->offs = 0;
}
static error_t
pending_clusters_add (struct pending_clusters *pc, cluster_t cluster)
{
if (cluster != pc->cluster + pc->num)
{
error_t err = pending_clusters_write (pc);
if (err)
return err;
pc->cluster = cluster;
}
pc->num++;
return 0;
}
static error_t
file_pager_write_huge_page (struct node *node, vm_offset_t offset, void *buf)
{
error_t err = 0;
struct pending_clusters pc;
pthread_rwlock_t *lock = &node->dn->alloc_lock;
cluster_t cluster;
int left = vm_page_size;
pending_clusters_init (&pc, buf);
pthread_rwlock_rdlock (&node->dn->alloc_lock);
if (offset >= node->allocsize)
left = 0;
else if (offset + left > node->allocsize)
left = node->allocsize - offset;
STAT_INC (file_pageouts);
while (left > 0)
{
err = find_cluster (node, offset, &cluster, &lock);
if (err)
break;
pending_clusters_add (&pc, cluster);
offset += bytes_per_cluster;
left -= bytes_per_cluster;
}
if (!err)
pending_clusters_write (&pc);
pthread_rwlock_unlock (&node->dn->alloc_lock);
return err;
}
static error_t
root_dir_pager_write_page (vm_offset_t offset, void *buf)
{
error_t err;
daddr_t addr;
size_t length;
size_t write = 0;
if (offset >= diskfs_root_node->allocsize)
return 0;
pthread_rwlock_rdlock (&diskfs_root_node->dn->alloc_lock);
addr = first_root_dir_byte + offset;
if (offset + vm_page_size > diskfs_root_node->allocsize)
length = diskfs_root_node->allocsize - offset;
else
length = vm_page_size;
err = store_write (store, addr >> store->log2_block_size, (void **) buf,
length, &write);
if (!err && write != length)
err = EIO;
pthread_rwlock_unlock (&diskfs_root_node->dn->alloc_lock);
return err;
}
static error_t
file_pager_write_small_page (struct node *node, vm_offset_t offset, void *buf)
{
error_t err;
pthread_rwlock_t *lock = NULL;
cluster_t cluster;
size_t write = 0;
if (offset >= node->allocsize)
return 0;
pthread_rwlock_rdlock (&node->dn->alloc_lock);
err = find_cluster (node, offset, &cluster, &lock);
if (!err)
{
err = store_write (store, FAT_FIRST_CLUSTER_BLOCK(cluster)
+ ((offset % bytes_per_cluster)
>> store->log2_block_size),
(void **) buf, vm_page_size, &write);
if (write != vm_page_size)
err = EIO;
}
if (lock)
pthread_rwlock_unlock (lock);
return err;
}
static error_t
fat_pager_read_page (vm_offset_t page, void **buf, int *writelock)
{
error_t err;
size_t length = vm_page_size, read = 0;
vm_size_t fat_end = bytes_per_sector * sectors_per_fat;
if (page + vm_page_size > fat_end)
length = fat_end - page;
page += first_fat_sector * bytes_per_sector;
err = store_read (store, page >> store->log2_block_size, length, buf, &read);
if (read != length)
return EIO;
if (!err && length != vm_page_size)
memset ((void *)(*buf + length), 0, vm_page_size - length);
*writelock = 0;
return err;
}
static error_t
fat_pager_write_page (vm_offset_t page, void *buf)
{
error_t err = 0;
size_t length = vm_page_size, amount;
vm_size_t fat_end = bytes_per_sector * sectors_per_fat;
if (page + vm_page_size > fat_end)
length = fat_end - page;
page += first_fat_sector * bytes_per_sector;
err = store_write (store, page >> store->log2_block_size,
buf, length, &amount);
if (!err && length != amount)
err = EIO;
return err;
}
error_t
pager_read_page (struct user_pager_info *pager, vm_offset_t page,
vm_address_t *buf, int *writelock)
{
if (pager->type == FAT)
return fat_pager_read_page (page, (void **)buf, writelock);
else
{
if (pager->node == diskfs_root_node
&& (fat_type == FAT12 || fat_type == FAT16))
return root_dir_pager_read_page (page, (void **)buf, writelock);
else
{
if (bytes_per_cluster < vm_page_size)
return file_pager_read_huge_page (pager->node, page,
(void **)buf, writelock);
else
return file_pager_read_small_page (pager->node, page,
(void **)buf, writelock);
}
}
}
error_t
pager_write_page (struct user_pager_info *pager, vm_offset_t page,
vm_address_t buf)
{
if (pager->type == FAT)
return fat_pager_write_page (page, (void *)buf);
else
{
if (pager->node == diskfs_root_node
&& (fat_type == FAT12 || fat_type == FAT16))
return root_dir_pager_write_page (page, (void *)buf);
else
{
if (bytes_per_cluster < vm_page_size)
return file_pager_write_huge_page (pager->node, page,
(void *)buf);
else
return file_pager_write_small_page (pager->node, page,
(void *)buf);
}
}
}
error_t
pager_unlock_page (struct user_pager_info *pager,
vm_offset_t page)
{
return 0;
}
void
pager_notify_evict (struct user_pager_info *pager,
vm_offset_t page)
{
assert_backtrace (!"unrequested notification on eviction");
}
error_t
diskfs_grow (struct node *node, loff_t size, struct protid *cred)
{
diskfs_check_readonly ();
assert_backtrace (!diskfs_readonly);
if (size > node->allocsize)
{
error_t err = 0;
loff_t old_size;
volatile loff_t new_size;
volatile cluster_t end_cluster;
cluster_t new_end_cluster;
struct disknode *dn = node->dn;
pthread_rwlock_wrlock (&dn->alloc_lock);
old_size = node->allocsize;
new_size = ((size + bytes_per_cluster - 1) >> log2_bytes_per_cluster)
<< log2_bytes_per_cluster;
end_cluster = old_size >> log2_bytes_per_cluster;
new_end_cluster = new_size >> log2_bytes_per_cluster;
if (new_end_cluster > end_cluster)
{
err = diskfs_catch_exception ();
if (! err)
{
while (!err && end_cluster < new_end_cluster)
{
cluster_t disk_cluster;
err = fat_getcluster (node, end_cluster++, 1, &disk_cluster);
}
diskfs_end_catch_exception ();
}
if (err)
new_size = (end_cluster - 1) >> log2_bytes_per_cluster;
}
STAT_INC (file_grows);
node->allocsize = new_size;
pthread_rwlock_unlock (&dn->alloc_lock);
return err;
}
else
return 0;
}
void
diskfs_file_update (struct node *node, int wait)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = node->dn->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (pager)
{
pager_sync (pager, wait);
ports_port_deref (pager);
}
diskfs_node_update (node, wait);
}
void
flush_node_pager (struct node *node)
{
struct pager *pager;
struct disknode *dn = node->dn;
pthread_spin_lock (&node_to_page_lock);
pager = dn->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (pager)
{
pager_flush (pager, 1);
ports_port_deref (pager);
}
}
inline error_t
pager_report_extent (struct user_pager_info *pager,
vm_address_t *offset, vm_size_t *size)
{
assert_backtrace (pager->type == FAT || pager->type == FILE_DATA);
*offset = 0;
if (pager->type == FAT)
*size = bytes_per_sector * sectors_per_fat;
else
*size = pager->node->allocsize;
return 0;
}
void
pager_clear_user_data (struct user_pager_info *upi)
{
if (upi->type == FILE_DATA)
diskfs_nrele_light (upi->node);
}
void
pager_dropweak (struct user_pager_info *upi)
{
struct pager *pager;
if (upi->type != FILE_DATA)
return;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (upi->node)->pager;
if (pager)
{
assert_backtrace (pager_get_upi (pager) == upi);
diskfs_node_disknode (upi->node)->pager = NULL;
ports_port_deref_weak (pager);
}
pthread_spin_unlock (&node_to_page_lock);
}
void
create_fat_pager (void)
{
error_t err;
struct user_pager_info *upi = malloc (sizeof (struct user_pager_info));
upi->type = FAT;
disk_pager_bucket = ports_create_bucket ();
diskfs_start_disk_pager (upi, disk_pager_bucket, MAY_CACHE, 0,
bytes_per_sector * sectors_per_fat,
&fat_image);
file_pager_bucket = ports_create_bucket ();
err = pager_start_workers (file_pager_bucket, &file_pager_requests);
if (err)
error (2, err, "can't create libpager worker threads");
}
error_t
inhibit_fat_pager (void)
{
error_t err;
err = pager_inhibit_workers (file_pager_requests);
if (err)
return err;
err = pager_inhibit_workers (diskfs_disk_pager_requests);
if (err)
pager_resume_workers (file_pager_requests);
return err;
}
void
resume_fat_pager (void)
{
pager_resume_workers (diskfs_disk_pager_requests);
pager_resume_workers (file_pager_requests);
}
mach_port_t
diskfs_get_filemap (struct node *node, vm_prot_t prot)
{
struct pager *pager;
mach_port_t right;
assert_backtrace (S_ISDIR (node->dn_stat.st_mode)
|| S_ISREG (node->dn_stat.st_mode)
|| (S_ISLNK (node->dn_stat.st_mode)));
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (node)->pager;
if (pager)
{
ports_port_ref (pager);
pager_get_upi (pager)->max_prot |= prot;
}
else
{
struct user_pager_info *upi;
pager = pager_create_alloc (sizeof *upi, file_pager_bucket,
MAY_CACHE, MEMORY_OBJECT_COPY_DELAY, 0);
if (pager == NULL)
{
pthread_spin_unlock (&node_to_page_lock);
return MACH_PORT_NULL;
}
upi = pager_get_upi (pager);
upi->type = FILE_DATA;
upi->node = node;
upi->max_prot = prot;
diskfs_nref_light (node);
diskfs_node_disknode (node)->pager = pager;
ports_port_ref_weak (pager);
}
pthread_spin_unlock (&node_to_page_lock);
if (prot & VM_PROT_WRITE)
right = ports_get_send_right (pager);
else
right = pager_create_ro_port (pager);
ports_port_deref (pager);
assert_backtrace (MACH_PORT_VALID (right));
return right;
}
void
drop_pager_softrefs (struct node *node)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = node->dn->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (MAY_CACHE && pager)
pager_change_attributes (pager, 0, MEMORY_OBJECT_COPY_DELAY, 0);
if (pager)
ports_port_deref (pager);
}
void
allow_pager_softrefs (struct node *node)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = node->dn->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (MAY_CACHE && pager)
pager_change_attributes (pager, 1, MEMORY_OBJECT_COPY_DELAY, 0);
if (pager)
ports_port_deref (pager);
}
struct pager *
diskfs_get_filemap_pager_struct (struct node *node)
{
return node->dn->pager;
}
void
diskfs_shutdown_pager (void)
{
error_t shutdown_one (void *v_p)
{
struct pager *p = v_p;
pager_shutdown (p);
return 0;
}
write_all_disknodes ();
ports_bucket_iterate (file_pager_bucket, shutdown_one);
pager_sync (diskfs_disk_pager, 1);
}
void
diskfs_sync_everything (int wait)
{
error_t sync_one (void *v_p)
{
struct pager *p = v_p;
pager_sync (p, wait);
return 0;
}
write_all_disknodes ();
ports_bucket_iterate (file_pager_bucket, sync_one);
pager_sync (diskfs_disk_pager, wait);
}
static void
disable_caching (void)
{
error_t block_cache (void *arg)
{
struct pager *p = arg;
pager_change_attributes (p, 0, MEMORY_OBJECT_COPY_DELAY, 1);
return 0;
}
ports_bucket_iterate (disk_pager_bucket, block_cache);
ports_bucket_iterate (file_pager_bucket, block_cache);
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
diskfs_nref (upi->node);
diskfs_nrele (upi->node);
}
return 0;
}
ports_bucket_iterate (disk_pager_bucket, enable_cache);
ports_bucket_iterate (file_pager_bucket, enable_cache);
}
int
diskfs_pager_users (void)
{
int npagers = ports_count_bucket (file_pager_bucket);
if (npagers == 0)
return 0;
if (MAY_CACHE)
{
disable_caching ();
sleep (1);
npagers = ports_count_bucket (file_pager_bucket);
if (npagers == 0)
return 0;
enable_caching ();
}
ports_enable_bucket (file_pager_bucket);
return 1;
}
vm_prot_t
diskfs_max_user_pager_prot (void)
{
vm_prot_t max_prot = 0;
int npagers = ports_count_bucket (file_pager_bucket);
if (npagers > 0)
{
error_t add_pager_max_prot (void *v_p)
{
struct pager *p = v_p;
struct user_pager_info *upi = pager_get_upi (p);
max_prot |= upi->max_prot;
return max_prot == (VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE);
}
disable_caching ();
sleep (1);
ports_bucket_iterate (file_pager_bucket, add_pager_max_prot);
enable_caching ();
}
ports_enable_bucket (file_pager_bucket);
return max_prot;
}