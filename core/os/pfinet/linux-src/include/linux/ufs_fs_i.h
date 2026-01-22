#ifndef _LINUX_UFS_FS_I_H
#define _LINUX_UFS_FS_I_H
struct ufs_inode_info {
union {
__u32	i_data[15];
__u8	i_symlink[4*15];
} i_u1;
__u64	i_size;
__u32	i_flags;
__u32	i_gen;
__u32	i_shadow;
__u32	i_uid;
__u32	i_gid;
__u32	i_oeftflag;
__u16	i_osync;
__u32	i_lastfrag;
};
#endif