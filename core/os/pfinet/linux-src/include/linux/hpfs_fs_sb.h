#ifndef _HPFS_FS_SB
#define _HPFS_FS_SB
struct hpfs_sb_info {
ino_t sb_root;
unsigned sb_fs_size;
unsigned sb_bitmaps;
unsigned sb_dirband_size;
unsigned sb_dmap;
unsigned sb_n_free;
unsigned sb_n_free_dnodes;
uid_t sb_uid;
gid_t sb_gid;
umode_t sb_mode;
unsigned sb_lowercase : 1;
unsigned sb_conv : 2;
};
#define s_hpfs_root u.hpfs_sb.sb_root
#define s_hpfs_fs_size u.hpfs_sb.sb_fs_size
#define s_hpfs_bitmaps u.hpfs_sb.sb_bitmaps
#define s_hpfs_dirband_size u.hpfs_sb.sb_dirband_size
#define s_hpfs_dmap u.hpfs_sb.sb_dmap
#define s_hpfs_uid u.hpfs_sb.sb_uid
#define s_hpfs_gid u.hpfs_sb.sb_gid
#define s_hpfs_mode u.hpfs_sb.sb_mode
#define s_hpfs_n_free u.hpfs_sb.sb_n_free
#define s_hpfs_n_free_dnodes u.hpfs_sb.sb_n_free_dnodes
#define s_hpfs_lowercase u.hpfs_sb.sb_lowercase
#define s_hpfs_conv u.hpfs_sb.sb_conv
#endif