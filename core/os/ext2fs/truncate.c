#include "ext2fs.h"
#ifdef DONT_CACHE_MEMORY_OBJECTS
#define MAY_CACHE 0
#else
#define MAY_CACHE 1
#endif
struct free_block_run
{
block_t first_block;
unsigned long num_blocks;
struct node *node;
};
static inline void
free_block_run_init (struct free_block_run *fbr, struct node *node)
{
fbr->num_blocks = 0;
fbr->node = node;
}
static inline void
_free_block_run_flush (struct free_block_run *fbr, unsigned long count)
{
fbr->node->dn_stat.st_blocks -= count << log2_stat_blocks_per_fs_block;
fbr->node->dn_stat_dirty = 1;
ext2_free_blocks (fbr->first_block, count);
}
static inline void
free_block_run_add (struct free_block_run *fbr, block_t block)
{
unsigned long count = fbr->num_blocks;
if (count == 0)
{
fbr->first_block = block;
fbr->num_blocks++;
}
else if (count > 0 && fbr->first_block == block - count)
fbr->num_blocks++;
else
{
_free_block_run_flush (fbr, count);
fbr->first_block = block;
fbr->num_blocks = 1;
}
}
static inline void
free_block_run_free_ptr (struct free_block_run *fbr, block_t *p)
{
block_t block = *p;
if (block)
{
*p = 0;
free_block_run_add (fbr, block);
}
}
static inline void
free_block_run_finish (struct free_block_run *fbr)
{
unsigned long count = fbr->num_blocks;
if (count > 0)
_free_block_run_flush (fbr, count);
}
static void
trunc_direct (struct node *node, block_t end, struct free_block_run *fbr)
{
block_t *blocks = diskfs_node_disknode (node)->info.i_data;
ext2_debug ("truncating direct blocks from %d", end);
while (end < EXT2_NDIR_BLOCKS)
free_block_run_free_ptr (fbr, blocks + end++);
}
static void
trunc_indirect (struct node *node, block_t end,
block_t *p, block_t offset,
void (*free_block)(block_t *p, unsigned index),
struct free_block_run *fbr)
{
if (*p)
{
unsigned index;
int modified = 0, all_freed = 1;
block_t *ind_bh = (block_t *) disk_cache_block_ref (*p);
unsigned first = end < offset ? 0 : end - offset;
for (index = first; index < addr_per_block; index++)
if (ind_bh[index])
{
(*free_block)(ind_bh + index, index);
if (ind_bh[index])
all_freed = 0;
else
modified = 1;
}
if (first == 0 && all_freed)
{
pager_flush_some (diskfs_disk_pager,
bptr_index (ind_bh) << log2_block_size,
block_size, 1);
free_block_run_free_ptr (fbr, p);
disk_cache_block_deref (ind_bh);
}
else if (modified)
record_indir_poke (node, ind_bh);
else
disk_cache_block_deref (ind_bh);
}
}
static void
trunc_single_indirect (struct node *node, block_t end,
block_t *p, block_t offset,
struct free_block_run *fbr)
{
void free_block (block_t *p, unsigned index)
{
free_block_run_free_ptr (fbr, p);
}
trunc_indirect (node, end, p, offset, free_block, fbr);
}
static void
trunc_double_indirect (struct node *node, block_t end,
block_t *p, block_t offset,
struct free_block_run *fbr)
{
void free_block (block_t *p, unsigned index)
{
block_t entry_offs = offset + (index * addr_per_block);
trunc_single_indirect (node, end, p, entry_offs, fbr);
}
trunc_indirect (node, end, p, offset, free_block, fbr);
}
static void
trunc_triple_indirect (struct node *node, block_t end,
block_t *p, block_t offset,
struct free_block_run *fbr)
{
void free_block (block_t *p, unsigned index)
{
block_t entry_offs = offset + (index * addr_per_block * addr_per_block);
trunc_double_indirect (node, end, p, entry_offs, fbr);
}
trunc_indirect (node, end, p, offset, free_block, fbr);
}
static void
poke_pages (memory_object_t obj, vm_offset_t start, vm_offset_t end)
{
while (start < end)
{
error_t err;
vm_size_t len = 8 * vm_page_size;
vm_address_t addr = 0;
if (len > end - start)
len = end - start;
err = vm_map (mach_task_self (), &addr, len, 0, 1, obj, start, 0,
VM_PROT_WRITE|VM_PROT_READ, VM_PROT_READ|VM_PROT_WRITE, 0);
if (!err)
{
vm_address_t poke;
for (poke = addr; poke < addr + len; poke += vm_page_size)
*(volatile int *)poke = *(volatile int *)poke;
munmap ((caddr_t) addr, len);
}
start += len;
}
}
static void
force_delayed_copies (struct node *node, off_t length)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (node)->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (pager)
{
mach_port_t obj;
pager_change_attributes (pager, MAY_CACHE, MEMORY_OBJECT_COPY_NONE, 1);
obj = diskfs_get_filemap (node, VM_PROT_READ | VM_PROT_WRITE);
if (obj != MACH_PORT_NULL)
{
poke_pages (obj, round_page (length), round_page (node->allocsize));
mach_port_deallocate (mach_task_self (), obj);
pager_flush_some (pager, round_page(length),
node->allocsize - length, 1);
}
ports_port_deref (pager);
}
}
static void
enable_delayed_copies (struct node *node)
{
struct pager *pager;
pthread_spin_lock (&node_to_page_lock);
pager = diskfs_node_disknode (node)->pager;
if (pager)
ports_port_ref (pager);
pthread_spin_unlock (&node_to_page_lock);
if (pager)
{
pager_change_attributes (pager, MAY_CACHE, MEMORY_OBJECT_COPY_DELAY, 0);
ports_port_deref (pager);
}
}
error_t
diskfs_truncate (struct node *node, off_t length)
{
error_t err;
off_t offset;
diskfs_check_readonly ();
assert_backtrace (!diskfs_readonly);
if (length >= node->dn_stat.st_size)
return 0;
if (! node->dn_stat.st_blocks
&& !S_ISREG (node->dn_stat.st_mode)
&& !S_ISDIR (node->dn_stat.st_mode))
{
off_t froblen = node->dn_stat.st_size;
off_t frobmax = sizeof(diskfs_node_disknode (node)->info.i_data);
if (froblen > frobmax)
{
ext2_warning ("inline data was %lld, more than max %lld",
(long long) froblen, (long long) frobmax);
froblen = frobmax;
}
froblen -= length;
memset (((char *) (diskfs_node_disknode (node)->info.i_data)) + length,
0, froblen);
node->dn_stat.st_size = length;
node->dn_set_mtime = 1;
node->dn_set_ctime = 1;
diskfs_node_update (node, diskfs_synchronous);
return 0;
}
offset = length & (block_size - 1);
if (offset > 0)
{
diskfs_node_rdwr (node, (void *)zeroblock, length, block_size - offset,
1, 0, 0);
diskfs_file_update (node, 1);
}
ext2_discard_prealloc (node);
force_delayed_copies (node, length);
pthread_rwlock_wrlock (&diskfs_node_disknode (node)->alloc_lock);
node->dn_stat.st_size = length;
node->dn_set_mtime = 1;
node->dn_set_ctime = 1;
diskfs_node_update (node, diskfs_synchronous);
err = diskfs_catch_exception ();
if (!err)
{
block_t end = boffs_block (round_block (length)), offs;
block_t *bptrs = diskfs_node_disknode (node)->info.i_data;
struct free_block_run fbr;
free_block_run_init (&fbr, node);
trunc_direct (node, end, &fbr);
offs = EXT2_NDIR_BLOCKS;
trunc_single_indirect (node, end, bptrs + EXT2_IND_BLOCK, offs, &fbr);
offs += addr_per_block;
trunc_double_indirect (node, end, bptrs + EXT2_DIND_BLOCK, offs, &fbr);
offs += addr_per_block * addr_per_block;
trunc_triple_indirect (node, end, bptrs + EXT2_TIND_BLOCK, offs, &fbr);
free_block_run_finish (&fbr);
node->allocsize = round_block (length);
diskfs_node_disknode (node)->last_page_partially_writable =
trunc_page (node->allocsize) != node->allocsize;
diskfs_end_catch_exception ();
}
node->dn_set_mtime = 1;
node->dn_set_ctime = 1;
node->dn_stat_dirty = 1;
enable_delayed_copies (node);
pthread_rwlock_unlock (&diskfs_node_disknode (node)->alloc_lock);
return err;
}