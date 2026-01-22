#ifndef _EXT2FS_H
#define _EXT2FS_H
#include <mach.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/pager.h>
#include <hurd/fshelp.h>
#include <hurd/iohelp.h>
#include <hurd/store.h>
#include <hurd/diskfs.h>
#include <hurd/ihash.h>
#include <assert-backtrace.h>
#include <pthread.h>
#include <sys/mman.h>
#include <endian.h>
typedef u_int32_t __u32;
typedef int32_t __s32;
typedef u_int16_t __u16;
typedef int16_t __s16;
typedef u_int8_t __u8;
typedef int8_t __s8;
#include "ext2_fs.h"
#undef ext2_debug
#ifdef EXT2FS_DEBUG
#include <stdio.h>
extern int ext2_debug_flag;
#define ext2_debug_(f, a...) \
fprintf (stderr, "ext2fs: (debug) %s: " f "\n", __FUNCTION__ , ## a)
#define ext2_debug(f, a...) \
do { if (ext2_debug_flag) ext2_debug_(f, ## a); } while (0)
#else
#define ext2_debug(f, a...) (void)0
#endif
#undef DONT_CACHE_MEMORY_OBJECTS
typedef __u32 block_t;
struct poke
{
vm_offset_t offset;
vm_size_t length;
struct poke *next;
};
struct pokel
{
struct poke *pokes, *free_pokes;
pthread_spinlock_t lock;
struct pager *pager;
void *image;
};
void pokel_init (struct pokel *pokel, struct pager *pager, void *image);
void pokel_finalize (struct pokel *pokel);
void pokel_add (struct pokel *pokel, void *loc, vm_size_t length);
void pokel_sync (struct pokel *pokel, int wait);
void pokel_flush (struct pokel *pokel);
void pokel_inherit (struct pokel *pokel, struct pokel *from);
#include <features.h>
#ifdef EXT2FS_DEFINE_EI
#define EXT2FS_EI
#else
#define EXT2FS_EI __extern_inline
#endif
#include <stdint.h>
extern int test_bit (unsigned num, unsigned char *bitmap);
extern int set_bit (unsigned num, unsigned char *bitmap);
extern int clear_bit (unsigned num, unsigned char *bitmap);
#if defined(__USE_EXTERN_INLINES) || defined(EXT2FS_DEFINE_EI)
EXT2FS_EI int
test_bit (unsigned num, unsigned char *bitmap)
{
const uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
const uint_fast32_t mask = 1U << (num & 31);
return *bw & mask;
}
EXT2FS_EI int
set_bit (unsigned num, unsigned char *bitmap)
{
uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
const uint_fast32_t mask = 1U << (num & 31);
return (*bw & mask) ?: (*bw |= mask, 0);
}
EXT2FS_EI int
clear_bit (unsigned num, unsigned char *bitmap)
{
uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
const uint_fast32_t mask = 1U << (num & 31);
return (*bw & mask) ? (*bw &= ~mask, mask) : 0;
}
#endif
struct disknode
{
int *dirents;
pthread_rwlock_t alloc_lock;
struct pokel indir_pokel;
struct ext2_inode_info info;
uint32_t info_i_translator;
struct pager *pager;
int last_page_partially_writable;
int dir_idx;
};
struct user_pager_info
{
enum pager_type
{
DISK,
FILE_DATA,
} type;
struct node *node;
vm_prot_t max_prot;
};
#define DISK_CACHE_BLOCKS 65536
#include <hurd/diskfs-pager.h>
void create_disk_pager (void);
error_t inhibit_ext2_pager (void);
void resume_ext2_pager (void);
void drop_pager_softrefs (struct node *node);
void allow_pager_softrefs (struct node *node);
void flush_node_pager (struct node *node);
extern struct store *store;
extern struct store_parsed *store_parsed;
extern void *disk_cache;
extern store_offset_t disk_cache_size;
extern int disk_cache_blocks;
#define DC_INCORE 0x01
#define DC_UNTOUCHED 0x02
#define DC_FIXED 0x04
#define DC_DONT_REUSE (DC_INCORE | DC_UNTOUCHED | DC_FIXED)
#define DC_NO_BLOCK ((block_t) -1L)
#ifdef DEBUG_DISK_CACHE
#define DISK_CACHE_LAST_READ_XOR 0xDEADBEEF
#endif
struct disk_cache_info
{
block_t block;
uint16_t flags;
uint16_t ref_count;
struct disk_cache_info *next;
#ifdef DEBUG_DISK_CACHE
block_t last_read, last_read_xor;
#endif
};
extern hurd_ihash_t disk_cache_bptr;
extern struct disk_cache_info *disk_cache_info;
extern pthread_mutex_t disk_cache_lock;
extern pthread_cond_t disk_cache_reassociation;
void *disk_cache_block_ref (block_t block);
void disk_cache_block_ref_ptr (void *ptr);
void _disk_cache_block_deref (void *ptr);
#define disk_cache_block_deref(PTR) \
do { _disk_cache_block_deref (PTR); PTR = NULL; } while (0)
int disk_cache_block_is_ref (block_t block);
extern struct ext2_super_block *sblock;
extern int sblock_dirty;
#define SBLOCK_BLOCK 1
#define SBLOCK_SIZE (sizeof (struct ext2_super_block))
extern unsigned int sblock_block;
#define SBLOCK_OFFS (sblock_block << 10)
extern unsigned int block_size;
extern unsigned int log2_block_size;
#define BLOCKSIZE_SCALE (le32toh (sblock->s_log_block_size))
extern unsigned log2_dev_blocks_per_fs_block;
extern unsigned log2_stat_blocks_per_fs_block;
extern vm_address_t zeroblock;
void get_hypermetadata (void);
void map_hypermetadata (void);
extern unsigned long frag_size;
extern unsigned long frags_per_block;
extern unsigned long inodes_per_block;
extern unsigned long itb_per_group;
extern unsigned long db_per_group;
extern unsigned long desc_per_block;
extern unsigned long addr_per_block;
extern unsigned long groups_count;
extern pthread_spinlock_t node_to_page_lock;
extern pthread_spinlock_t generation_lock;
extern unsigned long next_generation;
#define trunc_block(offs) \
((off_t) ((offs) >> log2_block_size) << log2_block_size)
#define round_block(offs) \
((off_t) (((offs) + block_size - 1) >> log2_block_size) << log2_block_size)
#define boffs(block) ((off_t) (block) << log2_block_size)
#define boffs_block(offs) ((offs) >> log2_block_size)
#define bptr_index(ptr) (((char *)ptr - (char *)disk_cache) >> log2_block_size)
extern char *boffs_ptr (off_t offset);
extern off_t bptr_offs (void *ptr);
#if defined(__USE_EXTERN_INLINES) || defined(EXT2FS_DEFINE_EI)
EXT2FS_EI char *
boffs_ptr (off_t offset)
{
block_t block = boffs_block (offset);
pthread_mutex_lock (&disk_cache_lock);
char *ptr = hurd_ihash_find (disk_cache_bptr, block);
pthread_mutex_unlock (&disk_cache_lock);
assert_backtrace (ptr);
ptr += offset % block_size;
ext2_debug ("(%lld) = %p", offset, ptr);
return ptr;
}
EXT2FS_EI off_t
bptr_offs (void *ptr)
{
vm_offset_t mem_offset = (char *)ptr - (char *)disk_cache;
off_t offset;
assert_backtrace (mem_offset < disk_cache_size);
pthread_mutex_lock (&disk_cache_lock);
offset = (off_t) disk_cache_info[boffs_block (mem_offset)].block
<< log2_block_size;
assert_backtrace (offset || mem_offset < block_size);
offset += mem_offset % block_size;
pthread_mutex_unlock (&disk_cache_lock);
ext2_debug ("(%p) = %lld", ptr, offset);
return offset;
}
#endif
#define bptr(block) boffs_ptr(boffs(block))
#define bptr_block(ptr) boffs_block(bptr_offs(ptr))
#define group_desc(num) (&group_desc_image[num])
extern struct ext2_group_desc *group_desc_image;
#define group_desc_block (boffs_block (SBLOCK_OFFS) + 1)
#define group_desc_size (groups_count * sizeof(struct ext2_group_desc))
#define group_desc_block_end (group_desc_block + boffs_block(round_block(group_desc_size)))
#define inode_group_num(inum) (((inum) - 1) / le32toh (sblock->s_inodes_per_group))
extern struct ext2_inode * dino_ref (ino_t inum);
extern void _dino_deref (struct ext2_inode *inode);
#if defined(__USE_EXTERN_INLINES) || defined(EXT2FS_DEFINE_EI)
EXT2FS_EI struct ext2_inode *
dino_ref (ino_t inum)
{
unsigned long inodes_per_group = le32toh (sblock->s_inodes_per_group);
unsigned long bg_num = (inum - 1) / inodes_per_group;
unsigned long group_inum = (inum - 1) % inodes_per_group;
struct ext2_group_desc *bg = group_desc (bg_num);
block_t block = le32toh (bg->bg_inode_table) + (group_inum / inodes_per_block);
struct ext2_inode *inode = disk_cache_block_ref (block);
inode += group_inum % inodes_per_block;
ext2_debug ("(%llu) = %p", inum, inode);
return inode;
}
EXT2FS_EI void
_dino_deref (struct ext2_inode *inode)
{
ext2_debug ("(%p)", inode);
disk_cache_block_deref (inode);
}
#endif
#define dino_deref(INODE) \
do { _dino_deref (INODE); INODE = NULL; } while (0)
void write_all_disknodes (void);
extern pthread_spinlock_t global_lock;
extern struct pokel global_pokel;
extern unsigned char *modified_global_blocks;
extern pthread_spinlock_t modified_global_blocks_lock;
extern int global_block_modified (block_t block);
extern void record_global_poke (void *ptr);
extern void sync_global_ptr (void *bptr, int wait);
extern void record_indir_poke (struct node *node, void *ptr);
extern void sync_global (int wait);
extern void alloc_sync (struct node *np);
#if defined(__USE_EXTERN_INLINES) || defined(EXT2FS_DEFINE_EI)
EXT2FS_EI int
global_block_modified (block_t block)
{
if (modified_global_blocks)
{
int was_clean;
pthread_spin_lock (&modified_global_blocks_lock);
was_clean = !set_bit(block, modified_global_blocks);
pthread_spin_unlock (&modified_global_blocks_lock);
return was_clean;
}
else
return 1;
}
EXT2FS_EI void
record_global_poke (void *ptr)
{
block_t block = boffs_block (bptr_offs (ptr));
void *block_ptr = bptr (block);
ext2_debug ("(%p = %p)", ptr, block_ptr);
#ifdef EXT2FS_DEBUG
assert_backtrace (disk_cache_block_is_ref (block));
#endif
global_block_modified (block);
pokel_add (&global_pokel, block_ptr, block_size);
}
EXT2FS_EI void
sync_global_ptr (void *ptr, int wait)
{
block_t block = boffs_block (bptr_offs (ptr));
void *block_ptr = bptr (block);
ext2_debug ("(%p -> %u)", ptr, block);
global_block_modified (block);
disk_cache_block_deref (block_ptr);
pager_sync_some (diskfs_disk_pager,
block_ptr - disk_cache, block_size, wait);
}
EXT2FS_EI void
record_indir_poke (struct node *node, void *ptr)
{
block_t block = boffs_block (bptr_offs (ptr));
void *block_ptr = bptr (block);
ext2_debug ("(%llu, %p)", node->cache_id, ptr);
#ifdef EXT2FS_DEBUG
assert_backtrace (disk_cache_block_is_ref (block));
#endif
global_block_modified (block);
pokel_add (&diskfs_node_disknode (node)->indir_pokel, block_ptr, block_size);
}
EXT2FS_EI void
sync_global (int wait)
{
ext2_debug ("%d", wait);
pokel_sync (&global_pokel, wait);
}
EXT2FS_EI void
alloc_sync (struct node *np)
{
if (diskfs_synchronous)
{
if (np)
{
diskfs_node_update (np, 1);
pokel_sync (&diskfs_node_disknode (np)->indir_pokel, 1);
}
diskfs_set_hypermetadata (1, 0);
}
}
#endif
void ext2_discard_prealloc (struct node *node);
error_t ext2_getblk (struct node *node, block_t block, int create, block_t *disk_block);
block_t ext2_new_block (block_t goal,
block_t prealloc_goal,
block_t *prealloc_count, block_t *prealloc_block);
void ext2_free_blocks (block_t block, unsigned long count);
error_t dev_write_sync (block_t addr, vm_address_t data, long len);
error_t dev_write (block_t addr, vm_address_t data, long len);
error_t dev_read_sync (block_t addr, vm_address_t *data, long len);
#define ext2_error(fmt, args...) _ext2_error (__FUNCTION__, fmt , ##args)
extern void _ext2_error (const char *, const char *, ...)
__attribute__ ((format (printf, 2, 3)));
#define ext2_panic(fmt, args...) _ext2_panic (__FUNCTION__, fmt , ##args)
extern void _ext2_panic (const char *, const char *, ...)
__attribute__ ((format (printf, 2, 3)));
extern void ext2_warning (const char *, ...)
__attribute__ ((format (printf, 1, 2)));
error_t ext2_list_xattr (struct node *np, char *buffer, size_t *len);
error_t ext2_get_xattr (struct node *np, const char *name, char *value, size_t *len);
error_t ext2_set_xattr (struct node *np, const char *name, const char *value, size_t len, int flags);
error_t ext2_free_xattr_block (struct node *np);
extern int use_xattr_translator_records;
#endif