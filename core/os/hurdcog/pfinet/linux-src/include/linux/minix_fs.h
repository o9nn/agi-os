#ifndef _LINUX_MINIX_FS_H
#define _LINUX_MINIX_FS_H
#define MINIX_ROOT_INO 1
#define MINIX_LINK_MAX	250
#define MINIX2_LINK_MAX	65530
#define MINIX_I_MAP_SLOTS	8
#define MINIX_Z_MAP_SLOTS	64
#define MINIX_SUPER_MAGIC	0x137F
#define MINIX_SUPER_MAGIC2	0x138F
#define MINIX2_SUPER_MAGIC	0x2468
#define MINIX2_SUPER_MAGIC2	0x2478
#define MINIX_VALID_FS		0x0001
#define MINIX_ERROR_FS		0x0002
#define MINIX_INODES_PER_BLOCK ((BLOCK_SIZE)/(sizeof (struct minix_inode)))
#define MINIX2_INODES_PER_BLOCK ((BLOCK_SIZE)/(sizeof (struct minix2_inode)))
#define MINIX_V1		0x0001
#define MINIX_V2		0x0002
#define INODE_VERSION(inode)	inode->i_sb->u.minix_sb.s_version
struct minix_inode {
__u16 i_mode;
__u16 i_uid;
__u32 i_size;
__u32 i_time;
__u8  i_gid;
__u8  i_nlinks;
__u16 i_zone[9];
};
struct minix2_inode {
__u16 i_mode;
__u16 i_nlinks;
__u16 i_uid;
__u16 i_gid;
__u32 i_size;
__u32 i_atime;
__u32 i_mtime;
__u32 i_ctime;
__u32 i_zone[10];
};
struct minix_super_block {
__u16 s_ninodes;
__u16 s_nzones;
__u16 s_imap_blocks;
__u16 s_zmap_blocks;
__u16 s_firstdatazone;
__u16 s_log_zone_size;
__u32 s_max_size;
__u16 s_magic;
__u16 s_state;
__u32 s_zones;
};
struct minix_dir_entry {
__u16 inode;
char name[0];
};
#ifdef __KERNEL__
extern struct dentry *minix_lookup(struct inode * dir, struct dentry *dentry);
extern int minix_create(struct inode * dir, struct dentry *dentry, int mode);
extern int minix_mkdir(struct inode * dir, struct dentry *dentry, int mode);
extern int minix_rmdir(struct inode * dir, struct dentry *dentry);
extern int minix_unlink(struct inode * dir, struct dentry *dentry);
extern int minix_symlink(struct inode * inode, struct dentry *dentry,
const char * symname);
extern int minix_link(struct dentry * old_dentry, struct inode * dir, struct dentry *dentry);
extern int minix_mknod(struct inode * dir, struct dentry *dentry, int mode, int rdev);
extern int minix_rename(struct inode * old_dir, struct dentry *old_dentry,
struct inode * new_dir, struct dentry *new_dentry);
extern struct inode * minix_new_inode(const struct inode * dir);
extern void minix_free_inode(struct inode * inode);
extern unsigned long minix_count_free_inodes(struct super_block *sb);
extern int minix_new_block(struct super_block * sb);
extern void minix_free_block(struct super_block * sb, int block);
extern unsigned long minix_count_free_blocks(struct super_block *sb);
extern int minix_bmap(struct inode *,int);
extern struct buffer_head * minix_getblk(struct inode *, int, int);
extern struct buffer_head * minix_bread(struct inode *, int, int);
extern void minix_truncate(struct inode *);
extern int init_minix_fs(void);
extern int minix_sync_inode(struct inode *);
extern int minix_sync_file(struct file *, struct dentry *);
extern struct inode_operations minix_file_inode_operations;
extern struct inode_operations minix_dir_inode_operations;
extern struct inode_operations minix_symlink_inode_operations;
extern struct dentry_operations minix_dentry_operations;
#endif
#endif