#ifndef _LINUX_EXT2_FS_SB
#define _LINUX_EXT2_FS_SB
#include <linux/ext2_fs.h>
#define EXT2_MAX_GROUP_LOADED 8
struct ext2_sb_info {
unsigned long s_frag_size;
unsigned long s_frags_per_block;
unsigned long s_inodes_per_block;
unsigned long s_frags_per_group;
unsigned long s_blocks_per_group;
unsigned long s_inodes_per_group;
unsigned long s_itb_per_group;
unsigned long s_db_per_group;
unsigned long s_desc_per_block;
unsigned long s_groups_count;
struct buffer_head * s_sbh;
struct ext2_super_block * s_es;
struct buffer_head ** s_group_desc;
unsigned short s_loaded_inode_bitmaps;
unsigned short s_loaded_block_bitmaps;
unsigned long s_inode_bitmap_number[EXT2_MAX_GROUP_LOADED];
struct buffer_head * s_inode_bitmap[EXT2_MAX_GROUP_LOADED];
unsigned long s_block_bitmap_number[EXT2_MAX_GROUP_LOADED];
struct buffer_head * s_block_bitmap[EXT2_MAX_GROUP_LOADED];
unsigned long s_mount_opt;
unsigned short s_resuid;
unsigned short s_resgid;
unsigned short s_mount_state;
unsigned short s_pad;
int s_addr_per_block_bits;
int s_desc_per_block_bits;
int s_inode_size;
int s_first_ino;
int s_feature_compat;
int s_feature_incompat;
int s_feature_ro_compat;
};
#endif