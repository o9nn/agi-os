#ifndef __LINUX_UFS_FS_H
#define __LINUX_UFS_FS_H
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/time.h>
#include <linux/stat.h>
#define UFS_BBLOCK 0
#define UFS_BBSIZE 8192
#define UFS_SBLOCK 8192
#define UFS_SBSIZE 8192
#define UFS_SECTOR_SIZE 512
#define UFS_SECTOR_BITS 9
#define UFS_MAGIC 0x00011954
#define UFS_CIGAM 0x54190100
#define UFS_BSIZE	8192
#define UFS_MINBSIZE	4096
#define UFS_FSIZE	1024
#define UFS_MAXFRAG	(UFS_BSIZE / UFS_FSIZE)
#define UFS_NDADDR 12
#define UFS_NINDIR 3
#define UFS_IND_BLOCK	(UFS_NDADDR + 0)
#define UFS_DIND_BLOCK	(UFS_NDADDR + 1)
#define UFS_TIND_BLOCK	(UFS_NDADDR + 2)
#define UFS_NDIR_FRAGMENT (UFS_NDADDR << uspi->s_fpbshift)
#define UFS_IND_FRAGMENT (UFS_IND_BLOCK << uspi->s_fpbshift)
#define UFS_DIND_FRAGMENT (UFS_DIND_BLOCK << uspi->s_fpbshift)
#define UFS_TIND_FRAGMENT (UFS_TIND_BLOCK << uspi->s_fpbshift)
#define UFS_ROOTINO 2
#define UFS_FIRST_INO (UFS_ROOTINO + 1)
#define UFS_USEEFT  ((__u16)65535)
#define UFS_FSOK      0x7c269d38
#define UFS_FSACTIVE  ((char)0x00)
#define UFS_FSCLEAN   ((char)0x01)
#define UFS_FSSTABLE  ((char)0x02)
#define UFS_FSOSF1    ((char)0x03)
#define UFS_FSBAD     ((char)0xff)
#define UFS_BYTESEX             0x00000001
#if defined(__LITTLE_ENDIAN) || defined(__BIG_ENDIAN)
#define UFS_NATIVE_ENDIAN	0x00000000
#define UFS_SWABBED_ENDIAN	0x00000001
#else
#define UFS_LITTLE_ENDIAN	0x00000000
#define UFS_BIG_ENDIAN		0x00000001
#endif
#define UFS_DE_MASK		0x00000010
#define UFS_DE_OLD		0x00000000
#define UFS_DE_44BSD		0x00000010
#define UFS_UID_MASK		0x00000060
#define UFS_UID_OLD		0x00000000
#define UFS_UID_44BSD		0x00000020
#define UFS_UID_EFT		0x00000040
#define UFS_ST_MASK		0x00000700
#define UFS_ST_OLD		0x00000000
#define UFS_ST_44BSD		0x00000100
#define UFS_ST_SUN		0x00000200
#define UFS_ST_SUNx86		0x00000400
#define UFS_CG_MASK		0x00003000
#define UFS_CG_OLD		0x00000000
#define UFS_CG_44BSD		0x00002000
#define UFS_CG_SUN		0x00001000
#define UFS_42INODEFMT	-1
#define UFS_44INODEFMT	2
#define UFS_MOUNT_ONERROR		0x0000000F
#define UFS_MOUNT_ONERROR_PANIC		0x00000001
#define UFS_MOUNT_ONERROR_LOCK		0x00000002
#define UFS_MOUNT_ONERROR_UMOUNT	0x00000004
#define UFS_MOUNT_ONERROR_REPAIR	0x00000008
#define UFS_MOUNT_UFSTYPE		0x000007F0
#define UFS_MOUNT_UFSTYPE_OLD		0x00000010
#define UFS_MOUNT_UFSTYPE_44BSD		0x00000020
#define UFS_MOUNT_UFSTYPE_SUN		0x00000040
#define UFS_MOUNT_UFSTYPE_NEXTSTEP	0x00000080
#define UFS_MOUNT_UFSTYPE_NEXTSTEP_CD	0x00000100
#define UFS_MOUNT_UFSTYPE_OPENSTEP	0x00000200
#define UFS_MOUNT_UFSTYPE_SUNx86	0x00000400
#define ufs_clear_opt(o,opt)	o &= ~UFS_MOUNT_##opt
#define ufs_set_opt(o,opt)	o |= UFS_MOUNT_##opt
#define ufs_test_opt(o,opt)	((o) & UFS_MOUNT_##opt)
#define UFS_MINFREE         5
#define UFS_DEFAULTOPT      UFS_OPTTIME
#define ufs_fsbtodb(uspi, b)	((b) << (uspi)->s_fsbtodb)
#define	ufs_dbtofsb(uspi, b)	((b) >> (uspi)->s_fsbtodb)
#define	ufs_cgbase(c)	(uspi->s_fpg * (c))
#define ufs_cgstart(c)	(ufs_cgbase(c)  + uspi->s_cgoffset * ((c) & ~uspi->s_cgmask))
#define	ufs_cgsblock(c)	(ufs_cgstart(c) + uspi->s_sblkno)
#define	ufs_cgcmin(c)	(ufs_cgstart(c) + uspi->s_cblkno)
#define	ufs_cgimin(c)	(ufs_cgstart(c) + uspi->s_iblkno)
#define	ufs_cgdmin(c)	(ufs_cgstart(c) + uspi->s_dblkno)
#define	ufs_inotocg(x)		((x) / uspi->s_ipg)
#define	ufs_inotocgoff(x)	((x) % uspi->s_ipg)
#define	ufs_inotofsba(x)	(ufs_cgimin(ufs_inotocg(x)) + ufs_inotocgoff(x) / uspi->s_inopf)
#define	ufs_inotofsbo(x)	((x) % uspi->s_inopf)
#define	ufs_dtog(d)	((d) / uspi->s_fpg)
#define	ufs_dtogd(d)	((d) % uspi->s_fpg)
#define ufs_cbtocylno(bno) \
((bno) * uspi->s_nspf / uspi->s_spc)
#define ufs_cbtorpos(bno) \
((((bno) * uspi->s_nspf % uspi->s_spc / uspi->s_nsect \
* uspi->s_trackskew + (bno) * uspi->s_nspf % uspi->s_spc \
% uspi->s_nsect * uspi->s_interleave) % uspi->s_nsect \
* uspi->s_nrpos) / uspi->s_npsect)
#define ufs_blkoff(loc)		((loc) & uspi->s_qbmask)
#define ufs_fragoff(loc)	((loc) & uspi->s_qfmask)
#define ufs_lblktosize(blk)	((blk) << uspi->s_bshift)
#define ufs_lblkno(loc)		((loc) >> uspi->s_bshift)
#define ufs_numfrags(loc)	((loc) >> uspi->s_fshift)
#define ufs_blkroundup(size)	(((size) + uspi->s_qbmask) & uspi->s_bmask)
#define ufs_fragroundup(size)	(((size) + uspi->s_qfmask) & uspi->s_fmask)
#define ufs_fragstoblks(frags)	((frags) >> uspi->s_fpbshift)
#define ufs_blkstofrags(blks)	((blks) << uspi->s_fpbshift)
#define ufs_fragnum(fsb)	((fsb) & uspi->s_fpbmask)
#define ufs_blknum(fsb)		((fsb) & ~uspi->s_fpbmask)
#define	UFS_MAXNAMLEN 255
#define UFS_MAXMNTLEN 512
#define UFS_MAXCSBUFS 31
#define UFS_LINK_MAX 32000
#define UFS_DIR_PAD			4
#define UFS_DIR_ROUND			(UFS_DIR_PAD - 1)
#define UFS_DIR_REC_LEN(name_len)	(((name_len) + 1 + 8 + UFS_DIR_ROUND) & ~UFS_DIR_ROUND)
struct ufs_timeval {
__s32	tv_sec;
__s32	tv_usec;
};
#define DT_UNKNOWN	0
#define DT_FIFO		1
#define DT_CHR		2
#define DT_DIR		4
#define DT_BLK		6
#define DT_REG		8
#define DT_LNK		10
#define DT_SOCK		12
#define DT_WHT		14
struct ufs_dir_entry {
__u32  d_ino;
__u16  d_reclen;
union {
__u16	d_namlen;
struct {
__u8	d_type;
__u8	d_namlen;
} d_44;
} d_u;
__u8	d_name[UFS_MAXNAMLEN + 1];
};
struct ufs_csum {
__u32	cs_ndir;
__u32	cs_nbfree;
__u32	cs_nifree;
__u32	cs_nffree;
};
struct ufs_super_block {
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
__s8	fs_fsmnt[UFS_MAXMNTLEN];
__u32	fs_cgrotor;
__u32	fs_csp[UFS_MAXCSBUFS];
__u32	fs_maxcluster;
__u32	fs_cpc;
__u16	fs_opostbl[16][8];
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
#define UFS_OPTTIME	0
#define UFS_OPTSPACE	1
#define UFS_42POSTBLFMT		-1
#define UFS_DYNAMICPOSTBLFMT	1
#define fs_cs(indx) \
u.ufs_sb.s_csp[(indx) >> uspi->s_csshift][(indx) & ~uspi->s_csmask]
#define	CG_MAGIC	0x090255
#define ufs_cg_chkmagic(ucg)	(SWAB32((ucg)->cg_magic) == CG_MAGIC)
struct	ufs_cylinder_group {
__u32	cg_link;
__u32	cg_magic;
__u32	cg_time;
__u32	cg_cgx;
__u16	cg_ncyl;
__u16	cg_niblk;
__u32	cg_ndblk;
struct	ufs_csum cg_cs;
__u32	cg_rotor;
__u32	cg_frotor;
__u32	cg_irotor;
__u32	cg_frsum[UFS_MAXFRAG];
__u32	cg_btotoff;
__u32	cg_boff;
__u32	cg_iusedoff;
__u32	cg_freeoff;
__u32	cg_nextfreeoff;
union {
struct {
__u32	cg_clustersumoff;
__u32	cg_clusteroff;
__u32	cg_nclusterblks;
__u32	cg_sparecon[13];
} cg_44;
__u32	cg_sparecon[16];
} cg_u;
__u8	cg_space[1];
};
struct ufs_inode {
__u16	ui_mode;
__u16	ui_nlink;
union {
struct {
__u16	ui_suid;
__u16	ui_sgid;
} oldids;
__u32	ui_inumber;
__u32	ui_author;
} ui_u1;
__u64	ui_size;
struct ufs_timeval ui_atime;
struct ufs_timeval ui_mtime;
struct ufs_timeval ui_ctime;
union {
struct {
__u32	ui_db[UFS_NDADDR];
__u32	ui_ib[UFS_NINDIR];
} ui_addr;
__u8	ui_symlink[4*(UFS_NDADDR+UFS_NINDIR)];
} ui_u2;
__u32	ui_flags;
__u32	ui_blocks;
__u32	ui_gen;
union {
struct {
__u32	ui_shadow;
__u32	ui_uid;
__u32	ui_gid;
__u32	ui_oeftflag;
} ui_sun;
struct {
__u32	ui_uid;
__u32	ui_gid;
__s32	ui_spare[2];
} ui_44;
struct {
__u32	ui_uid;
__u32	ui_gid;
__u16	ui_modeh;
__u16	ui_spare;
__u32	ui_trans;
} ui_hurd;
} ui_u3;
};
#define UFS_UF_SETTABLE   0x0000ffff
#define UFS_UF_NODUMP     0x00000001
#define UFS_UF_IMMUTABLE  0x00000002
#define UFS_UF_APPEND     0x00000004
#define UFS_UF_OPAQUE     0x00000008
#define UFS_UF_NOUNLINK   0x00000010
#define UFS_SF_SETTABLE   0xffff0000
#define UFS_SF_ARCHIVED   0x00010000
#define UFS_SF_IMMUTABLE  0x00020000
#define UFS_SF_APPEND     0x00040000
#define UFS_SF_NOUNLINK   0x00100000
#ifdef __KERNEL__
extern int ufs_permission (struct inode *, int);
extern void ufs_free_fragments (struct inode *, unsigned, unsigned);
extern void ufs_free_blocks (struct inode *, unsigned, unsigned);
extern unsigned ufs_new_fragments (struct inode *, u32 *, unsigned, unsigned, unsigned, int *);
extern struct ufs_cg_private_info * ufs_load_cylinder (struct super_block *, unsigned);
extern void ufs_put_cylinder (struct super_block *, unsigned);
extern struct inode_operations ufs_dir_inode_operations;
extern struct file_operations ufs_dir_operations;
extern int ufs_check_dir_entry (const char *, struct inode *, struct ufs_dir_entry *, struct buffer_head *, unsigned long);
extern struct inode_operations ufs_file_inode_operations;
extern struct file_operations ufs_file_operations;
extern void ufs_free_inode (struct inode *inode);
extern struct inode * ufs_new_inode (const struct inode *, int, int *);
extern int ufs_bmap (struct inode *, int);
extern void ufs_read_inode (struct inode *);
extern void ufs_put_inode (struct inode *);
extern void ufs_write_inode (struct inode *);
extern int ufs_sync_inode (struct inode *);
extern void ufs_write_inode (struct inode *);
extern void ufs_delete_inode (struct inode *);
extern struct buffer_head * ufs_getfrag (struct inode *, unsigned, int, int *);
extern struct buffer_head * ufs_bread (struct inode *, unsigned, int, int *);
extern struct dentry *ufs_lookup (struct inode *, struct dentry *);
extern int ufs_mkdir(struct inode *, struct dentry *, int);
extern int ufs_rmdir (struct inode *, struct dentry *);
extern int ufs_unlink (struct inode *, struct dentry *);
extern int ufs_create (struct inode *, struct dentry *, int);
extern int ufs_rename (struct inode *, struct dentry *, struct inode *, struct dentry *);
extern int ufs_mknod (struct inode *, struct dentry *, int, int);
extern int ufs_symlink (struct inode *, struct dentry *, const char *);
extern int ufs_link (struct dentry *, struct inode *, struct dentry *);
extern struct super_operations ufs_super_ops;
extern struct file_system_type ufs_fs_type;
extern void ufs_warning (struct super_block *, const char *, const char *, ...) __attribute__ ((format (printf, 3, 4)));
extern void ufs_error (struct super_block *, const char *, const char *, ...) __attribute__ ((format (printf, 3, 4)));
extern void ufs_panic (struct super_block *, const char *, const char *, ...) __attribute__ ((format (printf, 3, 4)));
extern int init_ufs_fs(void);
extern void ufs_write_super (struct super_block *);
extern struct inode_operations ufs_symlink_inode_operations;
extern void ufs_truncate (struct inode *);
#endif
#endif