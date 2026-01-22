#ifndef _AFFS_FS_SB
#define _AFFS_FS_SB
#define MAX_ZONES		8
#define AFFS_DATA_MIN_FREE	512
#define AFFS_HDR_MIN_FREE	128
#define AFFS_ZONE_SIZE		1024
struct affs_bm_info {
struct buffer_head *bm_bh;
s32 bm_firstblk;
s32 bm_key;
int bm_count;
};
struct affs_alloc_zone {
short az_size;
short az_count;
int az_free;
};
struct affs_zone {
unsigned long z_ino;
struct affs_bm_info *z_bm;
int z_start;
int z_end;
int z_az_no;
unsigned long z_lru_time;
};
struct affs_sb_info {
int s_partition_size;
int s_blksize;
s32 s_root_block;
int s_hashsize;
unsigned long s_flags;
s16 s_uid;
s16 s_gid;
umode_t s_mode;
int s_reserved;
struct buffer_head *s_root_bh;
struct affs_bm_info *s_bitmap;
int s_bm_count;
int s_nextzone;
int s_num_az;
struct affs_zone *s_zones;
struct affs_alloc_zone *s_alloc;
char *s_zonemap;
char *s_prefix;
int s_prefix_len;
char s_volume[32];
};
#define SF_INTL		0x0001
#define SF_BM_VALID	0x0002
#define SF_IMMUTABLE	0x0004
#define SF_QUIET	0x0008
#define SF_SETUID	0x0010
#define SF_SETGID	0x0020
#define SF_SETMODE	0x0040
#define SF_MUFS		0x0100
#define SF_OFS		0x0200
#define SF_PREFIX	0x0400
#define SF_VERBOSE	0x0800
#define SF_READONLY	0x1000
#endif