#include <string.h>
#include <stdio.h>
#include <error.h>
#include <inttypes.h>
#include <hurd/store.h>
#include "ext2fs.h"
vm_address_t zeroblock;
unsigned char *modified_global_blocks;
static void
allocate_mod_map (void)
{
static vm_size_t mod_map_size;
if (modified_global_blocks && mod_map_size)
munmap (modified_global_blocks, mod_map_size);
if (!diskfs_readonly && block_size < vm_page_size)
{
mod_map_size = le32toh (sblock->s_blocks_count) >> 3;
modified_global_blocks = mmap (0, mod_map_size, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
assert_backtrace (modified_global_blocks != MAP_FAILED);
}
else
modified_global_blocks = 0;
}
unsigned int sblock_block = SBLOCK_BLOCK;
static int ext2fs_clean;
void
get_hypermetadata (void)
{
error_t err;
size_t read = 0;
u_int32_t features;
if (sblock != NULL)
munmap (sblock, SBLOCK_SIZE);
err = store_read (store, SBLOCK_OFFS >> store->log2_block_size,
SBLOCK_SIZE, (void **)&sblock, &read);
if (err || read != SBLOCK_SIZE)
ext2_panic ("Cannot read hypermetadata");
if (sblock->s_magic != htole16 (EXT2_SUPER_MAGIC)
#ifdef EXT2FS_PRE_02B_COMPAT
&& sblock->s_magic != htole16 (EXT2_PRE_02B_MAGIC)
#endif
)
ext2_panic ("bad magic number %#x (should be %#x)",
le16toh (sblock->s_magic), EXT2_SUPER_MAGIC);
log2_block_size = EXT2_MIN_BLOCK_LOG_SIZE + le32toh(sblock->s_log_block_size);
block_size = 1 << log2_block_size;
if (block_size > EXT2_MAX_BLOCK_SIZE)
ext2_panic ("block size %d is too big (max is %d bytes)",
block_size, EXT2_MAX_BLOCK_SIZE);
if (log2_block_size < store->log2_block_size)
ext2_panic ("block size %d isn't a power-of-two multiple of the device"
" block size (%zd)!",
block_size, store->block_size);
log2_dev_blocks_per_fs_block = log2_block_size - store->log2_block_size;
log2_stat_blocks_per_fs_block = 0;
while ((512 << log2_stat_blocks_per_fs_block) < block_size)
log2_stat_blocks_per_fs_block++;
if ((512 << log2_stat_blocks_per_fs_block) != block_size)
ext2_panic ("block size %d isn't a power-of-two multiple of 512!",
block_size);
if ((store->size >> log2_block_size) < le32toh (sblock->s_blocks_count))
ext2_panic ("disk size (%qd bytes) too small; superblock says we need %qd",
(long long int) store->size,
(long long int) le32toh (sblock->s_blocks_count) << log2_block_size);
if (log2_dev_blocks_per_fs_block != 0
&& (store->size & ((1 << log2_dev_blocks_per_fs_block) - 1)) != 0)
ext2_warning ("%" PRIi64 " (%zd byte) device blocks "
" unused after last filesystem (%d byte) block",
(store->size & ((1 << log2_dev_blocks_per_fs_block) - 1)),
store->block_size, block_size);
inodes_per_block = block_size / EXT2_INODE_SIZE (sblock);
frag_size = EXT2_MIN_FRAG_SIZE << le32toh (sblock->s_log_frag_size);
if (frag_size == 0)
ext2_panic ("frag size is zero!");
frags_per_block = block_size / frag_size;
if (le32toh (sblock->s_rev_level) > EXT2_GOOD_OLD_REV)
{
features = EXT2_HAS_INCOMPAT_FEATURE(sblock, EXT2_FEATURE_INCOMPAT_UNSUPPORTED);
if (features)
ext2_panic ("could not mount because of unsupported optional features "
"(0x%x)",
features);
features = EXT2_HAS_RO_COMPAT_FEATURE(sblock, EXT2_FEATURE_RO_COMPAT_UNSUPPORTED);
if (features)
{
ext2_warning ("mounted readonly because of "
"unsupported optional features (0x%x)",
features);
diskfs_readonly = 1;
}
if (le16toh (sblock->s_inode_size) != EXT2_GOOD_OLD_INODE_SIZE)
ext2_panic ("inode size %d isn't supported, only %d is supported", le16toh (sblock->s_inode_size), EXT2_GOOD_OLD_INODE_SIZE);
if (EXT2_HAS_COMPAT_FEATURE (sblock, EXT3_FEATURE_COMPAT_HAS_JOURNAL))
ext2_warning ("mounting ext3 filesystem as ext2");
}
groups_count =
((le32toh (sblock->s_blocks_count) - le32toh (sblock->s_first_data_block) +
le32toh (sblock->s_blocks_per_group) - 1)
/ le32toh (sblock->s_blocks_per_group));
itb_per_group = le32toh (sblock->s_inodes_per_group) / inodes_per_block;
desc_per_block = block_size / sizeof (struct ext2_group_desc);
addr_per_block = block_size / sizeof (block_t);
db_per_group = (groups_count + desc_per_block - 1) / desc_per_block;
ext2fs_clean = sblock->s_state & htole16 (EXT2_VALID_FS);
if (! ext2fs_clean)
{
ext2_warning ("FILESYSTEM NOT UNMOUNTED CLEANLY; PLEASE fsck");
if (! diskfs_readonly)
{
diskfs_readonly = 1;
ext2_warning ("MOUNTED READ-ONLY; MUST USE `fsysopts --writable'");
}
}
allocate_mod_map ();
if (zeroblock == 0)
{
zeroblock = (vm_address_t) mmap (0, block_size, PROT_READ, MAP_ANON, 0, 0);
assert_backtrace (zeroblock != (vm_address_t) MAP_FAILED);
}
}
static struct ext2_super_block *mapped_sblock;
void
map_hypermetadata (void)
{
mapped_sblock = (struct ext2_super_block *) boffs_ptr (SBLOCK_OFFS);
group_desc_image =
(struct ext2_group_desc *) bptr (group_desc_block);
}
error_t
diskfs_set_hypermetadata (int wait, int clean)
{
if (clean && ext2fs_clean && !(sblock->s_state & htole16 (EXT2_VALID_FS)))
{
sblock->s_state |= htole16 (EXT2_VALID_FS);
sblock_dirty = 1;
}
else if (!clean && (sblock->s_state & htole16 (EXT2_VALID_FS)))
{
sblock->s_state &= htole16 (~EXT2_VALID_FS);
sblock_dirty = 1;
wait = 1;
}
if (sblock_dirty)
{
if (diskfs_readonly)
return EROFS;
sblock->s_wtime = htole32 (diskfs_mtime->seconds);
sblock_dirty = 0;
memcpy (mapped_sblock, sblock, SBLOCK_SIZE);
disk_cache_block_ref_ptr (mapped_sblock);
record_global_poke (mapped_sblock);
}
sync_global (wait);
return 0;
}
void
diskfs_readonly_changed (int readonly)
{
allocate_mod_map ();
(*(readonly ? store_set_flags : store_clear_flags)) (store, STORE_READONLY);
mprotect (disk_cache, disk_cache_size,
PROT_READ | (readonly ? 0 : PROT_WRITE));
if (!readonly && !(sblock->s_state & htole16 (EXT2_VALID_FS)))
ext2_warning ("UNCLEANED FILESYSTEM NOW WRITABLE");
}