#ifndef __EFS_FS_SB_H__
#define __EFS_FS_SB_H__
#define EFS_SUPER_MAGIC	0x414A53
#define EFS_MAGIC	0x072959
#define EFS_NEWMAGIC	0x07295a
#define IS_EFS_MAGIC(x)	((x == EFS_MAGIC) || (x == EFS_NEWMAGIC))
#define EFS_SUPER		1
#define EFS_ROOTINODE		2
struct efs_super {
int32_t		fs_size;
int32_t		fs_firstcg;
int32_t		fs_cgfsize;
short		fs_cgisize;
short		fs_sectors;
short		fs_heads;
short		fs_ncg;
short		fs_dirty;
short		fs_filler;
int32_t		fs_time;
int32_t		fs_magic;
char		fs_fname[6];
char		fs_fpack[6];
int32_t		fs_bmsize;
int32_t		fs_tfree;
int32_t		fs_tinode;
int32_t		fs_bmblock;
int32_t		fs_replsb;
int32_t		fs_lastialloc;
char		fs_spare[20];
int32_t		fs_checksum;
};
struct efs_sb_info {
int32_t	fs_magic;
int32_t	fs_start;
int32_t	first_block;
int32_t	total_blocks;
int32_t	group_size;
int32_t	data_free;
int32_t	inode_free;
short	inode_blocks;
short	total_groups;
};
#endif