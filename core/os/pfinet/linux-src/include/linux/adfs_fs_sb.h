#ifndef _ADFS_FS_SB
#define _ADFS_FS_SB
#include <linux/adfs_fs.h>
struct adfs_sb_info {
struct buffer_head *s_sbh;
struct adfs_discrecord *s_dr;
uid_t	s_uid;
gid_t	s_gid;
int	s_owner_mask;
int	s_other_mask;
__u16	s_zone_size;
__u16	s_ids_per_zone;
__u32	s_idlen;
__u32	s_map_size;
__u32	s_zonesize;
__u32	s_map_block;
struct buffer_head **s_map;
__u32	s_root;
__s8	s_map2blk;
};
#endif