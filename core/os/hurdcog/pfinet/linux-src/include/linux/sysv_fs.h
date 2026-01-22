#ifndef _LINUX_SYSV_FS_H
#define _LINUX_SYSV_FS_H
#ifdef __GNUC__
#define __packed2__  __attribute__ ((packed, aligned(2)))
#else
#error I want gcc!
#endif
#include <linux/stat.h>
#include <linux/sched.h>
#include <linux/sysv_fs_sb.h>
typedef u32 coh_ulong;
static inline coh_ulong to_coh_ulong (u32 x)
{
return ((x & 0xffff) << 16) | ((x & 0xffff0000) >> 16);
}
static inline u32 from_coh_ulong (coh_ulong x)
{
return ((x & 0xffff) << 16) | ((x & 0xffff0000) >> 16);
}
typedef u16 sysv_ino_t;
typedef u32 sysv_zone_t;
#define SYSV_BADBL_INO	1
#define SYSV_ROOT_INO	2
#define XENIX_NICINOD	100
#define XENIX_NICFREE	100
struct xenix_super_block {
u16		s_isize;
u32		s_fsize __packed2__;
u16		s_nfree;
u32		s_free[XENIX_NICFREE];
u16		s_ninode;
sysv_ino_t	s_inode[XENIX_NICINOD];
char		s_flock;
char		s_ilock;
char		s_fmod;
char		s_ronly;
u32		s_time __packed2__;
u32		s_tfree __packed2__;
u16		s_tinode;
s16		s_dinfo[4];
char		s_fname[6];
char		s_fpack[6];
char		s_clean;
char		s_fill[371];
s32		s_magic;
s32		s_type;
};
struct xenix_freelist_chunk {
u16	fl_nfree;
u32	fl_free[XENIX_NICFREE] __packed2__;
};
#define SYSV_NICINOD	100
#define SYSV_NICFREE	50
struct sysv4_super_block {
u16	s_isize;
u16	s_pad0;
u32	s_fsize;
u16	s_nfree;
u16	s_pad1;
u32	s_free[SYSV_NICFREE];
u16	s_ninode;
u16	s_pad2;
sysv_ino_t     s_inode[SYSV_NICINOD];
char	s_flock;
char	s_ilock;
char	s_fmod;
char	s_ronly;
u32	s_time;
s16	s_dinfo[4];
u32	s_tfree;
u16	s_tinode;
u16	s_pad3;
char	s_fname[6];
char	s_fpack[6];
s32	s_fill[12];
s32	s_state;
s32	s_magic;
s32	s_type;
};
struct sysv4_freelist_chunk {
u16 fl_nfree;
u32  fl_free[SYSV_NICFREE];
};
struct sysv2_super_block {
u16	s_isize;
u32	s_fsize __packed2__;
u16	s_nfree;
u32	s_free[SYSV_NICFREE];
u16	s_ninode;
sysv_ino_t     s_inode[SYSV_NICINOD];
char	s_flock;
char	s_ilock;
char	s_fmod;
char	s_ronly;
u32	s_time __packed2__;
s16	s_dinfo[4];
u32	s_tfree __packed2__;
u16	s_tinode;
char	s_fname[6];
char	s_fpack[6];
s32	s_fill[14];
s32	s_state;
s32	s_magic;
s32	s_type;
};
struct sysv2_freelist_chunk {
u16	fl_nfree;
u32	fl_free[SYSV_NICFREE] __packed2__;
};
#define COH_NICINOD	100
#define COH_NICFREE	64
struct coh_super_block {
u16		s_isize;
coh_ulong	s_fsize __packed2__;
u16 s_nfree;
coh_ulong	s_free[COH_NICFREE] __packed2__;
u16		s_ninode;
sysv_ino_t	s_inode[COH_NICINOD];
char		s_flock;
char		s_ilock;
char		s_fmod;
char		s_ronly;
coh_ulong	s_time __packed2__;
coh_ulong	s_tfree __packed2__;
u16		s_tinode;
u16		s_interleave_m;
u16		s_interleave_n;
char		s_fname[6];
char		s_fpack[6];
u32		s_unique;
};
struct coh_freelist_chunk {
u16 fl_nfree;
u32  fl_free[COH_NICFREE] __packed2__;
};
struct sysv_inode {
u16 i_mode;
u16 i_nlink;
u16 i_uid;
u16 i_gid;
u32 i_size;
union {
unsigned char i_addb[3*(10+1+1+1)+1];
dev_t i_rdev;
struct {
char p_addp[30];
s16 p_pnc;
s16 p_prx;
s16 p_pwx;
} i_p;
} i_a;
u32 i_atime;
u32 i_mtime;
u32 i_ctime;
};
#define COH_KLUDGE_SYMLINK_MODE	(S_IFREG | S_ISVTX)
#define COH_KLUDGE_NOT_SYMLINK	(S_IFREG | S_ISVTX | S_IRUSR)
extern inline mode_t from_coh_imode(unsigned short mode)
{
if (mode == COH_KLUDGE_SYMLINK_MODE)
return (S_IFLNK | 0777);
else
return mode;
}
extern inline unsigned short to_coh_imode(mode_t mode)
{
if (S_ISLNK(mode))
return COH_KLUDGE_SYMLINK_MODE;
else if (mode == COH_KLUDGE_SYMLINK_MODE)
return COH_KLUDGE_NOT_SYMLINK;
else
return mode;
}
#define XENIX_LINK_MAX	126
#define SYSV_LINK_MAX	126
#define COH_LINK_MAX	10000
#define SYSV_NAMELEN	14
struct sysv_dir_entry {
sysv_ino_t inode;
char name[SYSV_NAMELEN];
};
#define SYSV_DIRSIZE	sizeof(struct sysv_dir_entry)
#define FSTYPE_XENIX	1
#define FSTYPE_SYSV4	2
#define FSTYPE_SYSV2	3
#define FSTYPE_COH	4
#define SYSV_MAGIC_BASE		0x012FF7B3
#define XENIX_SUPER_MAGIC	(SYSV_MAGIC_BASE+FSTYPE_XENIX)
#define SYSV4_SUPER_MAGIC	(SYSV_MAGIC_BASE+FSTYPE_SYSV4)
#define SYSV2_SUPER_MAGIC	(SYSV_MAGIC_BASE+FSTYPE_SYSV2)
#define COH_SUPER_MAGIC		(SYSV_MAGIC_BASE+FSTYPE_COH)
#ifdef __KERNEL__
static inline struct buffer_head *
sv_get_hash_table (struct super_block *sb, kdev_t dev, unsigned int block)
{
return get_hash_table (dev, block + sb->sv_block_base, sb->sv_block_size);
}
static inline struct buffer_head *
sv_getblk (struct super_block *sb, kdev_t dev, unsigned int block)
{
return getblk (dev, block + sb->sv_block_base, sb->sv_block_size);
}
static inline struct buffer_head *
sv_bread (struct super_block *sb, kdev_t dev, unsigned int block)
{
return bread (dev, block + sb->sv_block_base, sb->sv_block_size);
}
extern struct dentry *sysv_lookup(struct inode * dir, struct dentry * dentry);
extern int sysv_create(struct inode * dir, struct dentry * dentry, int mode);
extern int sysv_mkdir(struct inode * dir, struct dentry * dentry, int mode);
extern int sysv_rmdir(struct inode * dir, struct dentry * dentry);
extern int sysv_unlink(struct inode * dir, struct dentry * dentry);
extern int sysv_symlink(struct inode * inode, struct dentry * dentry, const char * symname);
extern int sysv_link(struct dentry * old_dentry, struct inode * dir, struct dentry * dentry);
extern int sysv_mknod(struct inode * dir, struct dentry * dentry, int mode, int rdev);
extern int sysv_rename(struct inode * old_dir, struct dentry * old_dentry,
struct inode * new_dir, struct dentry * new_dentry);
extern struct inode * sysv_new_inode(const struct inode * dir);
extern void sysv_free_inode(struct inode * inode);
extern unsigned long sysv_count_free_inodes(struct super_block *sb);
extern int sysv_new_block(struct super_block * sb);
extern void sysv_free_block(struct super_block * sb, unsigned int block);
extern unsigned long sysv_count_free_blocks(struct super_block *sb);
extern int sysv_bmap(struct inode *,int);
extern struct buffer_head * sysv_getblk(struct inode *, unsigned int, int);
extern struct buffer_head * sysv_file_bread(struct inode *, int, int);
extern ssize_t sysv_file_read(struct file *, char *, size_t, loff_t *);
extern void sysv_truncate(struct inode *);
extern void sysv_put_super(struct super_block *);
extern struct super_block *sysv_read_super(struct super_block *,void *,int);
extern int init_sysv_fs(void);
extern void sysv_write_super(struct super_block *);
extern void sysv_read_inode(struct inode *);
extern int sysv_notify_change(struct dentry *, struct iattr *);
extern void sysv_write_inode(struct inode *);
extern int sysv_statfs(struct super_block *, struct statfs *, int);
extern int sysv_sync_inode(struct inode *);
extern int sysv_sync_file(struct file *, struct dentry *);
extern int sysv_mmap(struct file *, struct vm_area_struct *);
extern struct inode_operations sysv_file_inode_operations;
extern struct inode_operations sysv_file_inode_operations_with_bmap;
extern struct inode_operations sysv_dir_inode_operations;
extern struct inode_operations sysv_symlink_inode_operations;
#endif
#endif