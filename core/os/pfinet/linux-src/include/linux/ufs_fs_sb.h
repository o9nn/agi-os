#ifndef __LINUX_UFS_FS_SB_H
#define __LINUX_UFS_FS_SB_H
#include <linux/ufs_fs.h>
struct ufs_buffer_head {
unsigned fragment;
unsigned count;
struct buffer_head * bh[UFS_MAXFRAG];
};
struct ufs_cg_private_info {
struct ufs_cylinder_group ucg;
__u32	c_cgx;
__u16	c_ncyl;
__u16	c_niblk;
__u32	c_ndblk;
__u32	c_rotor;
__u32	c_frotor;
__u32	c_irotor;
__u32	c_btotoff;
__u32	c_boff;
__u32	c_iusedoff;
__u32	c_freeoff;
__u32	c_nextfreeoff;
__u32	c_clustersumoff;
__u32	c_clusteroff;
__u32	c_nclusterblks;
};
struct ufs_sb_private_info {
struct ufs_buffer_head s_ubh;
__u32	s_sblkno;
__u32	s_cblkno;
__u32	s_iblkno;
__u32	s_dblkno;
__u32	s_cgoffset;
__u32	s_cgmask;
__u32	s_size;
__u32	s_dsize;
__u32	s_ncg;
__u32	s_bsize;
__u32	s_fsize;
__u32	s_fpb;
__u32	s_minfree;
__u32	s_bmask;
__u32	s_fmask;
__u32	s_bshift;
__u32   s_fshift;
__u32	s_fpbshift;
__u32	s_fsbtodb;
__u32	s_sbsize;
__u32   s_csmask;
__u32	s_csshift;
__u32	s_nindir;
__u32	s_inopb;
__u32	s_nspf;
__u32	s_npsect;
__u32	s_interleave;
__u32	s_trackskew;
__u32	s_csaddr;
__u32	s_cssize;
__u32	s_cgsize;
__u32	s_ntrak;
__u32	s_nsect;
__u32	s_spc;
__u32	s_ipg;
__u32	s_fpg;
__u32	s_cpc;
__s32	s_contigsumsize;
__s64	s_qbmask;
__s64	s_qfmask;
__s32	s_postblformat;
__s32	s_nrpos;
__s32	s_postbloff;
__s32	s_rotbloff;
__u32	s_fpbmask;
__u32	s_apb;
__u32	s_2apb;
__u32	s_3apb;
__u32	s_apbmask;
__u32	s_apbshift;
__u32	s_2apbshift;
__u32	s_3apbshift;
__u32	s_nspfshift;
__u32	s_nspb;
__u32	s_inopf;
__u32	s_sbbase;
__u32	s_bpf;
__u32	s_bpfshift;
__u32	s_bpfmask;
};
#define UFS_MAX_GROUP_LOADED 8
#define UFS_CGNO_EMPTY ((unsigned)-1)
struct ufs_sb_info {
struct ufs_sb_private_info * s_uspi;
struct ufs_csum	* s_csp[UFS_MAXCSBUFS];
unsigned s_swab;
unsigned s_flags;
struct buffer_head ** s_ucg;
struct ufs_cg_private_info * s_ucpi[UFS_MAX_GROUP_LOADED];
unsigned s_cgno[UFS_MAX_GROUP_LOADED];
unsigned short s_cg_loaded;
unsigned s_mount_opt;
};
struct ufs_super_block_first {
__u32	fs_link;
__u32	fs_rlink;
__u32	fs_sblkno;
__u32	fs_cblkno;
__u32	fs_iblkno;
__u32	fs_dblkno;
__u32	fs_cgoffset;
__u32	fs_cgmask;
__u32	fs_time;
__u32	fs_size;
__u32	fs_dsize;
__u32	fs_ncg;
__u32	fs_bsize;
__u32	fs_fsize;
__u32	fs_frag;
__u32	fs_minfree;
__u32	fs_rotdelay;
__u32	fs_rps;
__u32	fs_bmask;
__u32	fs_fmask;
__u32	fs_bshift;
__u32	fs_fshift;
__u32	fs_maxcontig;
__u32	fs_maxbpg;
__u32	fs_fragshift;
__u32	fs_fsbtodb;
__u32	fs_sbsize;
__u32	fs_csmask;
__u32	fs_csshift;
__u32	fs_nindir;
__u32	fs_inopb;
__u32	fs_nspf;
__u32	fs_optim;
union {
struct {
__u32	fs_npsect;
} fs_sun;
struct {
__s32	fs_state;
} fs_sunx86;
} fs_u1;
__u32	fs_interleave;
__u32	fs_trackskew;
__u32	fs_id[2];
__u32	fs_csaddr;
__u32	fs_cssize;
__u32	fs_cgsize;
__u32	fs_ntrak;
__u32	fs_nsect;
__u32	fs_spc;
__u32	fs_ncyl;
__u32	fs_cpg;
__u32	fs_ipg;
__u32	fs_fpg;
struct ufs_csum fs_cstotal;
__s8	fs_fmod;
__s8	fs_clean;
__s8	fs_ronly;
__s8	fs_flags;
__s8	fs_fsmnt[UFS_MAXMNTLEN - 212];
};
struct ufs_super_block_second {
__s8	fs_fsmnt[212];
__u32	fs_cgrotor;
__u32	fs_csp[UFS_MAXCSBUFS];
__u32	fs_maxcluster;
__u32	fs_cpc;
__u16	fs_opostbl[82];
};
struct ufs_super_block_third {
__u16	fs_opostbl[46];
union {
struct {
__s32	fs_sparecon[53];
__s32	fs_reclaim;
__s32	fs_sparecon2[1];
__s32	fs_state;
__u32	fs_qbmask[2];
__u32	fs_qfmask[2];
} fs_sun;
struct {
__s32	fs_sparecon[53];
__s32	fs_reclaim;
__s32	fs_sparecon2[1];
__u32	fs_npsect;
__u32	fs_qbmask[2];
__u32	fs_qfmask[2];
} fs_sunx86;
struct {
__s32	fs_sparecon[50];
__s32	fs_contigsumsize;
__s32	fs_maxsymlinklen;
__s32	fs_inodefmt;
__u32	fs_maxfilesize[2];
__u32	fs_qbmask[2];
__u32	fs_qfmask[2];
__s32	fs_state;
} fs_44;
} fs_u2;
__s32	fs_postblformat;
__s32	fs_nrpos;
__s32	fs_postbloff;
__s32	fs_rotbloff;
__s32	fs_magic;
__u8	fs_space[1];
};
#endif