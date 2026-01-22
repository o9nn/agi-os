#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <error.h>
#include <inttypes.h>
#include <hurd/store.h>
#include "ext2fs.h"
#include "../libpager/priv.h"
struct port_bucket *disk_pager_bucket;
struct port_bucket *file_pager_bucket;
struct pager_requests *file_pager_requests;
pthread_spinlock_t node_to_page_lock = PTHREAD_SPINLOCK_INITIALIZER;
static int disk_cache_initialized;
#ifdef DONT_CACHE_MEMORY_OBJECTS
#define MAY_CACHE 0
#else
#define MAY_CACHE 1
#endif
#define STATS
#ifdef STATS
struct ext2fs_pager_stats
{
pthread_spinlock_t lock;
unsigned long disk_pageins;
unsigned long disk_pageouts;
unsigned long file_pageins;
unsigned long file_pagein_reads;
unsigned long file_pagein_freed_bufs;
unsigned long file_pagein_alloced_bufs;
unsigned long file_pageouts;
unsigned long file_page_unlocks;
unsigned long file_grows;
};
static struct ext2fs_pager_stats ext2s_pager_stats =
{ .lock = PTHREAD_SPINLOCK_INITIALIZER };
#define STAT_INC(field) \
do { pthread_spin_lock (&ext2s_pager_stats.lock); \
ext2s_pager_stats.field++; \
pthread_spin_unlock (&ext2s_pager_stats.lock); } while (0)
#else
#define STAT_INC(field) 0
#endif
static void
disk_cache_info_free_push (struct disk_cache_info *p);
#define FREE_PAGE_BUFS 24
static void *
get_page_buf (void)
{
static pthread_mutex_t free_page_bufs_lock = PTHREAD_MUTEX_INITIALIZER;
static void *free_page_bufs;
static int num_free_page_bufs;
void *buf;
pthread_mutex_lock (&free_page_bufs_lock);
if (num_free_page_bufs > 0)
{
buf = free_page_bufs;
num_free_page_bufs --;
if (num_free_page_bufs > 0)
free_page_bufs += vm_page_size;
#ifndef NDEBUG
else
free_page_bufs = 0;
#endif
}
else
{
assert_backtrace (free_page_bufs == 0);
buf = mmap (0, vm_page_size * FREE_PAGE_BUFS,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (buf == MAP_FAILED)
buf = 0;
else
{
free_page_bufs = buf + vm_page_size;
num_free_page_bufs = FREE_PAGE_BUFS - 1;
}
}
pthread_mutex_unlock (&free_page_bufs_lock);
return buf;
}
static inline void
free_page_buf (void *buf)
{
munmap (buf, vm_page_size);
}
static error_t
find_block (struct node *node, vm_offset_t offset,
block_t *block, pthread_rwlock_t **lock)
{
error_t err;
if (!*lock)
{
*lock = &diskfs_node_disknode (node)->alloc_lock;
pthread_rwlock_rdlock (*lock);
}
if (offset + block_size > node->allocsize)
return EIO;
err = ext2_getblk (node, offset >> log2_block_size, 0, block);
if (err == EINVAL)
{
*block = 0;
err = 0;
}
return err;
}
static error_t
file_pager_read_page (struct node *node, vm_offset_t page,
void **buf, int *writelock)
{
error_t err = 0;
int offs = 0;
int partial = 0;
pthread_rwlock_t *lock = NULL;
int left = vm_page_size;
block_t pending_blocks = 0;
int num_pending_blocks = 0;
ext2_debug ("reading inode %llu page %lu[%u]",
node->cache_id, page, vm_page_size);
error_t do_pending_reads (void)
{
if (num_pending_blocks > 0)
{
store_offset_t dev_block = (store_offset_t) pending_blocks
<< log2_dev_blocks_per_fs_block;
size_t amount = num_pending_blocks << log2_block_size;
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
num_pending_blocks = 0;
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
{
left = node->allocsize - page;
partial = 1;
}
while (left > 0)
{
block_t block;
err = find_block (node, page, &block, &lock);
if (err)
break;
if (block != pending_blocks + num_pending_blocks)
{
err = do_pending_reads ();
if (err)
break;
pending_blocks = block;
}
if (block == 0)
{
*writelock = 1;
if (offs == 0)
{
*buf = get_page_buf ();
if (! *buf)
break;
STAT_INC (file_pagein_alloced_bufs);
}
memset (*buf + offs, 0, block_size);
offs += block_size;
}
else
num_pending_blocks++;
page += block_size;
left -= block_size;
}
if (!err && num_pending_blocks > 0)
err = do_pending_reads();
if (!err && partial && !*writelock)
diskfs_node_disknode (node)->last_page_partially_writable = 1;
if (lock)
pthread_rwlock_unlock (lock);
return err;
}
struct pending_blocks
{
block_t block;
off_t num;
void *buf;
int offs;
};
static error_t
pending_blocks_write (struct pending_blocks *pb)
{
if (pb->num > 0)
{
error_t err;
store_offset_t dev_block = (store_offset_t) pb->block
<< log2_dev_blocks_per_fs_block;
size_t length = pb->num << log2_block_size, amount;
ext2_debug ("writing block %u[%ld]", pb->block, pb->num);
if (pb->offs > 0)
{
void *page_buf = get_page_buf ();
memcpy ((void *)page_buf, pb->buf + pb->offs, length);
err = store_write (store, dev_block, page_buf, length, &amount);
free_page_buf (page_buf);
}
else
err = store_write (store, dev_block, pb->buf, length, &amount);
if (err)
return err;
else if (amount != length)
return EIO;
pb->offs += length;
pb->num = 0;
}
return 0;
}
static void
pending_blocks_init (struct pending_blocks *pb, void *buf)
{
pb->buf = buf;
pb->block = 0;
pb->num = 0;
pb->offs = 0;
}
static error_t
pending_blocks_skip (struct pending_blocks *pb)
{
error_t err = pending_blocks_write (pb);
pb->offs += block_size;
return err;
}
static error_t
pending_blocks_add (struct pending_blocks *pb, block_t block)
{
if (block != pb->block + pb->num)
{
error_t err = pending_blocks_write (pb);
if (err)
return err;
pb->block = block;
}
pb->num++;
return 0;
}
static error_t
file_pager_write_page (struct node *node, vm_offset_t offset, void *buf)
{
error_t err = 0;
struct pending_blocks pb;
pthread_rwlock_t *lock = &diskfs_node_disknode (node)->alloc_lock;
block_t block;
int left = vm_page_size;
pending_blocks_init (&pb, buf);
pthread_rwlock_rdlock (&diskfs_node_disknode (node)->alloc_lock);
if (offset >= node->allocsize)
left = 0;
else if (offset + left > node->allocsize)
left = node->allocsize - offset;
ext2_debug ("writing inode %d page %d[%d]", node->cache_id, offset, left);
STAT_INC (file_pageouts);
while (left > 0)
{
err = find_block (node, offset, &block, &lock);
if (err)
break;
assert_backtrace (block);
pending_blocks_add (&pb, block);
offset += block_size;
left -= block_size;
}
if (!err)
pending_blocks_write (&pb);
pthread_rwlock_unlock (&diskfs_node_disknode (node)->alloc_lock);
return err;
}
static error_t
disk_pager_read_page (vm_offset_t page, void **buf, int *writelock)
{
error_t err;
size_t length = vm_page_size, read = 0;
store_offset_t offset = page, dev_end = store->size;
int index = offset >> log2_block_size;
pthread_mutex_lock (&disk_cache_lock);
offset = ((store_offset_t) disk_cache_info[index].block << log2_block_size)
+ offset % block_size;
disk_cache_info[index].flags |= DC_INCORE;
disk_cache_info[index].flags &=~ DC_UNTOUCHED;
#ifdef DEBUG_DISK_CACHE
disk_cache_info[index].last_read = disk_cache_info[index].block;
disk_cache_info[index].last_read_xor
= disk_cache_info[index].block ^ DISK_CACHE_LAST_READ_XOR;
#endif
pthread_mutex_unlock (&disk_cache_lock);
ext2_debug ("(%lld)", offset >> log2_block_size);
if (offset + vm_page_size > dev_end)
length = dev_end - offset;
err = store_read (store, offset >> store->log2_block_size, length,
buf, &read);
if (read != length)
return EIO;
if (!err && length != vm_page_size)
memset ((void *)(*buf + length), 0, vm_page_size - length);
*writelock = 0;
return err;
}
static error_t
disk_pager_write_page (vm_offset_t page, void *buf)
{
error_t err = 0;
size_t length = vm_page_size, amount;
store_offset_t offset = page, dev_end = store->size;
int index = offset >> log2_block_size;
pthread_mutex_lock (&disk_cache_lock);
assert_backtrace (disk_cache_info[index].block != DC_NO_BLOCK);
offset = ((store_offset_t) disk_cache_info[index].block << log2_block_size)
+ offset % block_size;
#ifdef DEBUG_DISK_CACHE
assert_backtrace ((disk_cache_info[index].last_read ^ DISK_CACHE_LAST_READ_XOR)
== disk_cache_info[index].last_read_xor);
assert_backtrace (disk_cache_info[index].last_read
== disk_cache_info[index].block);
#endif
pthread_mutex_unlock (&disk_cache_lock);
if (offset + vm_page_size > dev_end)
length = dev_end - offset;
ext2_debug ("writing disk page %lld[%zu]", offset, length);
STAT_INC (disk_pageouts);
if (modified_global_blocks)
{
struct pending_blocks pb;
pending_blocks_init (&pb, buf);
while (length > 0 && !err)
{
block_t block = boffs_block (offset);
if (test_bit (block, modified_global_blocks))
err = pending_blocks_add (&pb, block);
else
err = pending_blocks_skip (&pb);
offset += block_size;
length -= block_size;
}
if (!err)
err = pending_blocks_write (&pb);
}
else
{
err = store_write (store, offset >> store->log2_block_size,
buf, length, &amount);
if (!err && length != amount)
err = EIO;
}
return err;
}
static void
disk_pager_notify_evict (vm_offset_t page)
{
unsigned long index = page >> log2_block_size;
ext2_debug ("(block %lu)", index);
pthread_mutex_lock (&disk_cache_lock);
disk_cache_info[index].flags &= ~DC_INCORE;
if (disk_cache_info[index].ref_count == 0 &&
!(disk_cache_info[index].flags & DC_DONT_REUSE))
disk_cache_info_free_push (&disk_cache_info[index]);
pthread_mutex_unlock (&disk_cache_lock);
}
error_t
pager_read_page (struct user_pager_info *pager, vm_offset_t page,
vm_address_t *buf, int *writelock)
{
if (pager->type == DISK)
return disk_pager_read_page (page, (void **)buf, writelock);
else
return file_pager_read_page (pager->node, page, (void **)buf, writelock);
}
error_t
pager_write_page (struct user_pager_info *pager, vm_offset_t page,
vm_address_t buf)
{
if (pager->type == DISK)
return disk_pager_write_page (page, (void *)buf);
else
return file_pager_write_page (pager->node, page, (void *)buf);
}
void
pager_notify_evict (struct user_pager_info *pager, vm_offset_t page)
{
if (pager->type == DISK)
disk_pager_notify_evict (page);
}
error_t
pager_unlock_page (struct user_pager_info *pager, vm_offset_t page)
{
if (pager->type == DISK)
return 0;
else
{
error_t err;
volatile int partial_page;
struct node *node = pager->node;
struct disknode *dn = diskfs_node_disknode (node);
pthread_rwlock_wrlock (&dn->alloc_lock);
partial_page = (page + vm_page_size > node->allocsize);
err = diskfs_catch_exception ();
if (!err)
{
block_t block = page >> log2_block_size;
int left = (partial_page ? node->allocsize - page : vm_page_size);
while (left > 0)
{
block_t disk_block;
err = ext2_getblk (node, block++, 1, &disk_block);
if (err)
break;
left -= block_size;
}
diskfs_end_catch_exception ();
}
if (partial_page)
dn->last_page_partially_writable = !err;
else if (page + vm_page_size == node->allocsize)
dn->last_page_partially_writable = 0;
#ifdef EXT2FS_DEBUG
if (dn->last_page_partially_writable)
ext2_debug ("made page %u[%lu] in inode %d partially writable",
page, node->allocsize - page, node->cache_id);
else
ext2_debug ("made page %u[%u] in inode %d writable",
page, vm_page_size, node->cache_id);
#endif
STAT_INC (file_page_unlocks);
pthread_rwlock_unlock (&dn->alloc_lock);
if (err == ENOSPC)
ext2_warning ("This filesystem is out of space.");
else if (err)
ext2_warning ("inode=%" PRIu64 ", page=0x%lx: %s",
node->cache_id, (unsigned long)page, strerror (err));
return err;
}
}
error_t
diskfs_grow (struct node *node, off_t size, struct protid *cred)
{
diskfs_check_readonly ();
assert_backtrace (!diskfs_readonly);
if (size > node->allocsize)
{
error_t err = 0;
off_t old_size;
volatile off_t new_size;
volatile block_t end_block;
block_t new_end_block;
struct disknode *dn = diskfs_node_disknode (node);
pthread_rwlock_wrlock (&dn->alloc_lock);
old_size = node->allocsize;
new_size = round_block (size);
end_block = old_size >> log2_block_size;
new_end_block = new_size >> log2_block_size;
if (new_end_block > end_block)
{
block_t old_page_end_block =
round_page (old_size) >> log2_block_size;
ext2_debug ("growing inode %d to %lu bytes (from %lu)", node->cache_id,
new_size, old_size);
if (dn->last_page_partially_writable
&& old_page_end_block > end_block)
{
volatile block_t writable_end =
(old_page_end_block > new_end_block
? new_end_block
: old_page_end_block);
ext2_debug ("extending writable page %u by %d blocks"
"; first new block = %u",
trunc_page (old_size),
writable_end - end_block,
end_block);
err = diskfs_catch_exception ();
if (! err)
{
while (!err && end_block < writable_end)
{
block_t disk_block;
err = ext2_getblk (node, end_block++, 1, &disk_block);
}
diskfs_end_catch_exception ();
}
if (! err)
new_size = end_block << log2_block_size;
else
dn->last_page_partially_writable =
(old_page_end_block > end_block);
}
}
STAT_INC (file_grows);
ext2_debug ("new size: %ld%s.", new_size,
dn->last_page_partially_writable
? " (last page writable)": "");
if (err)
ext2_warning ("inode=%" PRIu64 ", target=%" PRIi64 ": %s",
node->cache_id, new_size, strerror (err));
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
pager = diskfs_node_disknode (node)->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (pager)
{
pager_sync (pager, wait);
ports_port_deref (pager);
}
pokel_sync (&diskfs_node_disknode (node)->indir_pokel, wait);
diskfs_node_update (node, wait);
}
void
flush_node_pager (struct node *node)
{
struct pager *pager;
struct disknode *dn = diskfs_node_disknode (node);
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
assert_backtrace (pager->type == DISK || pager->type == FILE_DATA);
*offset = 0;
if (pager->type == DISK)
*size = store->size;
else
*size = pager->node->allocsize;
return 0;
}
void
pager_clear_user_data (struct user_pager_info *upi)
{
if (upi->type == FILE_DATA)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (upi->node)->pager;
assert_backtrace (!pager || pager_get_upi (pager) != upi);
pthread_spin_unlock (&node_to_page_lock);
diskfs_nrele_light (upi->node);
}
}
void
pager_dropweak (struct user_pager_info *upi)
{
if (upi->type == FILE_DATA)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (upi->node)->pager;
if (pager && pager_get_upi (pager) == upi)
{
diskfs_node_disknode (upi->node)->pager = NULL;
ports_port_deref_weak (pager);
}
pthread_spin_unlock (&node_to_page_lock);
}
}
void *disk_cache;
store_offset_t disk_cache_size;
int disk_cache_blocks;
hurd_ihash_t disk_cache_bptr;
struct disk_cache_info *disk_cache_info;
pthread_mutex_t disk_cache_lock;
pthread_cond_t disk_cache_reassociation;
static struct disk_cache_info *disk_cache_info_free;
static pthread_mutex_t disk_cache_info_free_lock;
static struct disk_cache_info *
disk_cache_info_free_pop (void)
{
struct disk_cache_info *p;
do
{
pthread_mutex_lock (&disk_cache_info_free_lock);
p = disk_cache_info_free;
if (p)
{
disk_cache_info_free = p->next;
p->next = NULL;
}
pthread_mutex_unlock (&disk_cache_info_free_lock);
}
while (p && (p->flags & DC_DONT_REUSE || p->ref_count > 0));
return p;
}
static void
disk_cache_info_free_push (struct disk_cache_info *p)
{
pthread_mutex_lock (&disk_cache_info_free_lock);
if (! p->next)
{
p->next = disk_cache_info_free;
disk_cache_info_free = p;
}
pthread_mutex_unlock (&disk_cache_info_free_lock);
}
static void
disk_cache_init (void)
{
if (block_size != vm_page_size)
ext2_panic ("Block size %u != vm_page_size %lu",
block_size, (unsigned long)vm_page_size);
pthread_mutex_init (&disk_cache_lock, NULL);
pthread_cond_init (&disk_cache_reassociation, NULL);
pthread_mutex_init (&disk_cache_info_free_lock, NULL);
if (hurd_ihash_create (&disk_cache_bptr, HURD_IHASH_NO_LOCP))
ext2_panic ("Can't allocate memory for disk_pager_bptr");
disk_cache_info = malloc ((sizeof *disk_cache_info) * disk_cache_blocks);
if (!disk_cache_info)
ext2_panic ("Cannot allocate space for disk cache info");
for (int i = disk_cache_blocks - 1; i >= 0; i--)
{
disk_cache_info[i].block = DC_NO_BLOCK;
disk_cache_info[i].flags = 0;
disk_cache_info[i].ref_count = 0;
disk_cache_info[i].next = NULL;
disk_cache_info_free_push (&disk_cache_info[i]);
#ifdef DEBUG_DISK_CACHE
disk_cache_info[i].last_read = DC_NO_BLOCK;
disk_cache_info[i].last_read_xor
= DC_NO_BLOCK ^ DISK_CACHE_LAST_READ_XOR;
#endif
}
block_t fixed_first = boffs_block (SBLOCK_OFFS);
block_t fixed_last = fixed_first
+ (round_block ((sizeof *group_desc_image) * groups_count)
>> log2_block_size);
ext2_debug ("%u-%u\n", fixed_first, fixed_last);
assert_backtrace (fixed_last - fixed_first + 1 <= (block_t)disk_cache_blocks + 3);
for (block_t i = fixed_first; i <= fixed_last; i++)
{
disk_cache_block_ref (i);
assert_backtrace (disk_cache_info[i-fixed_first].block == i);
disk_cache_info[i-fixed_first].flags |= DC_FIXED;
}
disk_cache_initialized = 1;
}
static void
disk_cache_return_unused (void)
{
int index;
for (vm_offset_t i = 0; i < disk_cache_size; i += vm_page_size)
*(volatile char *)(disk_cache + i);
pokel_sync (&global_pokel, 1);
int pending_begin = -1, pending_end = -1;
pthread_mutex_lock (&disk_cache_lock);
for (index = 0; index < disk_cache_blocks; index++)
if (! (disk_cache_info[index].flags & (DC_DONT_REUSE & ~DC_INCORE))
&& ! disk_cache_info[index].ref_count)
{
ext2_debug ("return %u -> %d",
disk_cache_info[index].block, index);
if (index != pending_end)
{
if (pending_end >= 0)
{
pthread_mutex_unlock (&disk_cache_lock);
pager_return_some (diskfs_disk_pager,
pending_begin * vm_page_size,
(pending_end - pending_begin)
* vm_page_size, 1);
pthread_mutex_lock (&disk_cache_lock);
}
pending_begin = index;
}
pending_end = index + 1;
}
pthread_mutex_unlock (&disk_cache_lock);
if (pending_end >= 0)
pager_return_some (diskfs_disk_pager,
pending_begin * vm_page_size,
(pending_end - pending_begin) * vm_page_size,
1);
else
{
ext2_debug ("ext2fs: disk cache is starving\n");
sleep (1);
}
}
void *
disk_cache_block_ref (block_t block)
{
struct disk_cache_info *info;
int index;
void *bptr;
hurd_ihash_locp_t slot;
if (disk_cache_initialized)
assert_backtrace (block >= group_desc_block_end
&& block < store->size >> log2_block_size);
ext2_debug ("(%u)", block);
retry_ref:
pthread_mutex_lock (&disk_cache_lock);
bptr = hurd_ihash_locp_find (disk_cache_bptr, block, &slot);
if (bptr)
{
index = bptr_index (bptr);
if (disk_cache_info[index].flags & DC_UNTOUCHED)
{
pthread_cond_wait (&disk_cache_reassociation, &disk_cache_lock);
pthread_mutex_unlock (&disk_cache_lock);
#if 0
printf ("Re-association -- wait finished.\n");
#endif
goto retry_ref;
}
assert_backtrace (disk_cache_info[index].ref_count + 1
> disk_cache_info[index].ref_count);
disk_cache_info[index].ref_count++;
ext2_debug ("cached %u -> %d (ref_count = %hu, flags = %#hx, ptr = %p)",
disk_cache_info[index].block, index,
disk_cache_info[index].ref_count,
disk_cache_info[index].flags, bptr);
pthread_mutex_unlock (&disk_cache_lock);
return bptr;
}
info = disk_cache_info_free_pop ();
if (info == NULL)
{
ext2_debug ("flush %u -> %d", disk_cache_info[index].block, index);
pthread_mutex_unlock (&disk_cache_lock);
disk_cache_return_unused ();
goto retry_ref;
}
index = info - disk_cache_info;
bptr = (char *)disk_cache + (index << log2_block_size);
ext2_debug ("map %u -> %d (%p)", block, index, bptr);
disk_cache_info[index].flags |= DC_UNTOUCHED;
#if 0
pthread_mutex_unlock (&disk_cache_lock);
pager_return_some (diskfs_disk_pager, bptr - disk_cache, vm_page_size, 1);
pthread_mutex_lock (&disk_cache_lock);
if ((! (disk_cache_info[index].flags & DC_UNTOUCHED))
|| hurd_ihash_find (disk_cache_bptr, block))
{
pthread_mutex_unlock (&disk_cache_lock);
goto retry_ref;
}
#elif 0
pthread_mutex_lock (&diskfs_disk_pager->interlock);
int page = (bptr - disk_cache) / vm_page_size;
assert_backtrace (page >= 0);
int is_incore = (page < diskfs_disk_pager->pagemapsize
&& (diskfs_disk_pager->pagemap[page] & PM_INCORE));
pthread_mutex_unlock (&diskfs_disk_pager->interlock);
if (is_incore)
{
pthread_mutex_unlock (&disk_cache_lock);
printf ("INCORE\n");
goto retry_ref;
}
#endif
if (hurd_ihash_locp_add (disk_cache_bptr, slot, block, bptr))
ext2_panic ("Couldn't hurd_ihash_locp_add new disk block");
if (disk_cache_info[index].block != DC_NO_BLOCK)
hurd_ihash_remove (disk_cache_bptr, disk_cache_info[index].block);
assert_backtrace (! (disk_cache_info[index].flags & DC_DONT_REUSE & ~DC_UNTOUCHED));
disk_cache_info[index].block = block;
assert_backtrace (! disk_cache_info[index].ref_count);
disk_cache_info[index].ref_count = 1;
pthread_mutex_unlock (&disk_cache_lock);
*(volatile char *) bptr;
pthread_mutex_lock (&disk_cache_lock);
if (disk_cache_info[index].flags & DC_UNTOUCHED)
{
hurd_ihash_remove (disk_cache_bptr, block);
disk_cache_info[index].block = DC_NO_BLOCK;
disk_cache_info[index].flags &=~ DC_UNTOUCHED;
disk_cache_info[index].ref_count = 0;
pthread_mutex_unlock (&disk_cache_lock);
pager_flush_some (diskfs_disk_pager, bptr - disk_cache,
vm_page_size, 0);
#if 0
printf ("Re-association failed.\n");
#endif
goto retry_ref;
}
pthread_cond_broadcast (&disk_cache_reassociation);
pthread_mutex_unlock (&disk_cache_lock);
ext2_debug ("(%u) = %p", block, bptr);
return bptr;
}
void
disk_cache_block_ref_ptr (void *ptr)
{
int index;
pthread_mutex_lock (&disk_cache_lock);
index = bptr_index (ptr);
assert_backtrace (disk_cache_info[index].ref_count >= 1);
assert_backtrace (disk_cache_info[index].ref_count + 1
> disk_cache_info[index].ref_count);
disk_cache_info[index].ref_count++;
assert_backtrace (! (disk_cache_info[index].flags & DC_UNTOUCHED));
ext2_debug ("(%p) (ref_count = %hu, flags = %#hx)",
ptr,
disk_cache_info[index].ref_count,
disk_cache_info[index].flags);
pthread_mutex_unlock (&disk_cache_lock);
}
void
_disk_cache_block_deref (void *ptr)
{
int index;
assert_backtrace (disk_cache <= ptr && ptr <= disk_cache + disk_cache_size);
pthread_mutex_lock (&disk_cache_lock);
index = bptr_index (ptr);
ext2_debug ("(%p) (ref_count = %hu, flags = %#hx)",
ptr,
disk_cache_info[index].ref_count - 1,
disk_cache_info[index].flags);
assert_backtrace (! (disk_cache_info[index].flags & DC_UNTOUCHED));
assert_backtrace (disk_cache_info[index].ref_count >= 1);
disk_cache_info[index].ref_count--;
if (disk_cache_info[index].ref_count == 0 &&
!(disk_cache_info[index].flags & DC_DONT_REUSE))
disk_cache_info_free_push (&disk_cache_info[index]);
pthread_mutex_unlock (&disk_cache_lock);
}
int
disk_cache_block_is_ref (block_t block)
{
int ref;
void *ptr;
pthread_mutex_lock (&disk_cache_lock);
ptr = hurd_ihash_find (disk_cache_bptr, block);
if (ptr == NULL)
ref = 0;
else
ref = disk_cache_info[bptr_index (ptr)].ref_count;
pthread_mutex_unlock (&disk_cache_lock);
return ref;
}
void
create_disk_pager (void)
{
error_t err;
struct user_pager_info *upi = malloc (sizeof (struct user_pager_info));
if (!upi)
ext2_panic ("can't create disk pager: %s", strerror (errno));
upi->type = DISK;
disk_pager_bucket = ports_create_bucket ();
get_hypermetadata ();
disk_cache_blocks = DISK_CACHE_BLOCKS;
disk_cache_size = disk_cache_blocks << log2_block_size;
diskfs_start_disk_pager (upi, disk_pager_bucket, MAY_CACHE, 1,
disk_cache_size, &disk_cache);
disk_cache_init ();
file_pager_bucket = ports_create_bucket ();
err = pager_start_workers (file_pager_bucket, &file_pager_requests);
if (err)
ext2_panic ("can't create libpager worker threads: %s", strerror (err));
}
error_t
inhibit_ext2_pager (void)
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
resume_ext2_pager (void)
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
pager = diskfs_node_disknode (node)->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (MAY_CACHE && pager)
{
pager_sync (pager, 0);
pager_change_attributes (pager, 0, MEMORY_OBJECT_COPY_DELAY, 0);
}
if (pager)
ports_port_deref (pager);
}
void
allow_pager_softrefs (struct node *node)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (node)->pager;
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
return diskfs_node_disknode (node)->pager;
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
sync_global (1);
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
sync_global (wait);
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