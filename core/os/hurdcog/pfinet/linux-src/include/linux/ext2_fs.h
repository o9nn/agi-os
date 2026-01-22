#ifndef _LINUX_EXT2_FS_H
#define _LINUX_EXT2_FS_H
#include <linux/types.h>
#undef EXT2FS_DEBUG
#define EXT2_PREALLOCATE
#define EXT2_DEFAULT_PREALLOC_BLOCKS	8
#define EXT2FS_DATE		"95/08/09"
#define EXT2FS_VERSION		"0.5b"
#ifdef EXT2FS_DEBUG
#	define ext2_debug(f, a...)	{ \
printk ("EXT2-fs DEBUG (%s, %d): %s:", \
__FILE__, __LINE__, __FUNCTION__); \
printk (f, ## a); \
}
#else
#	define ext2_debug(f, a...)
#endif
#define	EXT2_BAD_INO		 1
#define EXT2_ROOT_INO		 2
#define EXT2_ACL_IDX_INO	 3
#define EXT2_ACL_DATA_INO	 4
#define EXT2_BOOT_LOADER_INO	 5
#define EXT2_UNDEL_DIR_INO	 6
#define EXT2_GOOD_OLD_FIRST_INO	11
#define EXT2_SUPER_MAGIC	0xEF53
#define EXT2_LINK_MAX		32000
#define EXT2_MIN_BLOCK_SIZE		1024
#define	EXT2_MAX_BLOCK_SIZE		4096
#define EXT2_MIN_BLOCK_LOG_SIZE		  10
#ifdef __KERNEL__
# define EXT2_BLOCK_SIZE(s)		((s)->s_blocksize)
#else
# define EXT2_BLOCK_SIZE(s)		(EXT2_MIN_BLOCK_SIZE << (s)->s_log_block_size)
#endif
#define EXT2_ACLE_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_acl_entry))
#define	EXT2_ADDR_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (__u32))
#ifdef __KERNEL__
# define EXT2_BLOCK_SIZE_BITS(s)	((s)->s_blocksize_bits)
#else
# define EXT2_BLOCK_SIZE_BITS(s)	((s)->s_log_block_size + 10)
#endif
#ifdef __KERNEL__
#define	EXT2_ADDR_PER_BLOCK_BITS(s)	((s)->u.ext2_sb.s_addr_per_block_bits)
#define EXT2_INODE_SIZE(s)		((s)->u.ext2_sb.s_inode_size)
#define EXT2_FIRST_INO(s)		((s)->u.ext2_sb.s_first_ino)
#else
#define EXT2_INODE_SIZE(s)	(((s)->s_rev_level == EXT2_GOOD_OLD_REV) ? \
EXT2_GOOD_OLD_INODE_SIZE : \
(s)->s_inode_size)
#define EXT2_FIRST_INO(s)	(((s)->s_rev_level == EXT2_GOOD_OLD_REV) ? \
EXT2_GOOD_OLD_FIRST_INO : \
(s)->s_first_ino)
#endif
#define EXT2_MIN_FRAG_SIZE		1024
#define	EXT2_MAX_FRAG_SIZE		4096
#define EXT2_MIN_FRAG_LOG_SIZE		  10
#ifdef __KERNEL__
# define EXT2_FRAG_SIZE(s)		((s)->u.ext2_sb.s_frag_size)
# define EXT2_FRAGS_PER_BLOCK(s)	((s)->u.ext2_sb.s_frags_per_block)
#else
# define EXT2_FRAG_SIZE(s)		(EXT2_MIN_FRAG_SIZE << (s)->s_log_frag_size)
# define EXT2_FRAGS_PER_BLOCK(s)	(EXT2_BLOCK_SIZE(s) / EXT2_FRAG_SIZE(s))
#endif
struct ext2_acl_header
{
__u32	aclh_size;
__u32	aclh_file_count;
__u32	aclh_acle_count;
__u32	aclh_first_acle;
};
struct ext2_acl_entry
{
__u32	acle_size;
__u16	acle_perms;
__u16	acle_type;
__u16	acle_tag;
__u16	acle_pad1;
__u32	acle_next;
};
struct ext2_group_desc
{
__u32	bg_block_bitmap;
__u32	bg_inode_bitmap;
__u32	bg_inode_table;
__u16	bg_free_blocks_count;
__u16	bg_free_inodes_count;
__u16	bg_used_dirs_count;
__u16	bg_pad;
__u32	bg_reserved[3];
};
#ifdef __KERNEL__
# define EXT2_BLOCKS_PER_GROUP(s)	((s)->u.ext2_sb.s_blocks_per_group)
# define EXT2_DESC_PER_BLOCK(s)		((s)->u.ext2_sb.s_desc_per_block)
# define EXT2_INODES_PER_GROUP(s)	((s)->u.ext2_sb.s_inodes_per_group)
# define EXT2_DESC_PER_BLOCK_BITS(s)	((s)->u.ext2_sb.s_desc_per_block_bits)
#else
# define EXT2_BLOCKS_PER_GROUP(s)	((s)->s_blocks_per_group)
# define EXT2_DESC_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_group_desc))
# define EXT2_INODES_PER_GROUP(s)	((s)->s_inodes_per_group)
#endif
#define	EXT2_NDIR_BLOCKS		12
#define	EXT2_IND_BLOCK			EXT2_NDIR_BLOCKS
#define	EXT2_DIND_BLOCK			(EXT2_IND_BLOCK + 1)
#define	EXT2_TIND_BLOCK			(EXT2_DIND_BLOCK + 1)
#define	EXT2_N_BLOCKS			(EXT2_TIND_BLOCK + 1)
#define	EXT2_SECRM_FL			0x00000001
#define	EXT2_UNRM_FL			0x00000002
#define	EXT2_COMPR_FL			0x00000004
#define EXT2_SYNC_FL			0x00000008
#define EXT2_IMMUTABLE_FL		0x00000010
#define EXT2_APPEND_FL			0x00000020
#define EXT2_NODUMP_FL			0x00000040
#define EXT2_NOATIME_FL			0x00000080
#define EXT2_DIRTY_FL			0x00000100
#define EXT2_COMPRBLK_FL		0x00000200
#define EXT2_NOCOMP_FL			0x00000400
#define EXT2_ECOMPR_FL			0x00000800
#define EXT2_BTREE_FL			0x00001000
#define EXT2_RESERVED_FL		0x80000000
#define EXT2_FL_USER_VISIBLE		0x00001FFF
#define EXT2_FL_USER_MODIFIABLE		0x000000FF
#define	EXT2_IOC_GETFLAGS		_IOR('f', 1, long)
#define	EXT2_IOC_SETFLAGS		_IOW('f', 2, long)
#define	EXT2_IOC_GETVERSION		_IOR('v', 1, long)
#define	EXT2_IOC_SETVERSION		_IOW('v', 2, long)
struct ext2_inode {
__u16	i_mode;
__u16	i_uid;
__u32	i_size;
__u32	i_atime;
__u32	i_ctime;
__u32	i_mtime;
__u32	i_dtime;
__u16	i_gid;
__u16	i_links_count;
__u32	i_blocks;
__u32	i_flags;
union {
struct {
__u32  l_i_reserved1;
} linux1;
struct {
__u32  h_i_translator;
} hurd1;
struct {
__u32  m_i_reserved1;
} masix1;
} osd1;
__u32	i_block[EXT2_N_BLOCKS];
__u32	i_version;
__u32	i_file_acl;
__u32	i_dir_acl;
__u32	i_faddr;
union {
struct {
__u8	l_i_frag;
__u8	l_i_fsize;
__u16	i_pad1;
__u32	l_i_reserved2[2];
} linux2;
struct {
__u8	h_i_frag;
__u8	h_i_fsize;
__u16	h_i_mode_high;
__u16	h_i_uid_high;
__u16	h_i_gid_high;
__u32	h_i_author;
} hurd2;
struct {
__u8	m_i_frag;
__u8	m_i_fsize;
__u16	m_pad1;
__u32	m_i_reserved2[2];
} masix2;
} osd2;
};
#define i_size_high	i_dir_acl
#if defined(__KERNEL__) || defined(__linux__)
#define i_reserved1	osd1.linux1.l_i_reserved1
#define i_frag		osd2.linux2.l_i_frag
#define i_fsize		osd2.linux2.l_i_fsize
#define i_reserved2	osd2.linux2.l_i_reserved2
#endif
#ifdef	__hurd__
#define i_translator	osd1.hurd1.h_i_translator
#define i_frag		osd2.hurd2.h_i_frag;
#define i_fsize		osd2.hurd2.h_i_fsize;
#define i_uid_high	osd2.hurd2.h_i_uid_high
#define i_gid_high	osd2.hurd2.h_i_gid_high
#define i_author	osd2.hurd2.h_i_author
#endif
#ifdef	__masix__
#define i_reserved1	osd1.masix1.m_i_reserved1
#define i_frag		osd2.masix2.m_i_frag
#define i_fsize		osd2.masix2.m_i_fsize
#define i_reserved2	osd2.masix2.m_i_reserved2
#endif
#define	EXT2_VALID_FS			0x0001
#define	EXT2_ERROR_FS			0x0002
#define EXT2_MOUNT_CHECK_NORMAL		0x0001
#define EXT2_MOUNT_CHECK_STRICT		0x0002
#define EXT2_MOUNT_CHECK		(EXT2_MOUNT_CHECK_NORMAL | \
EXT2_MOUNT_CHECK_STRICT)
#define EXT2_MOUNT_GRPID		0x0004
#define EXT2_MOUNT_DEBUG		0x0008
#define EXT2_MOUNT_ERRORS_CONT		0x0010
#define EXT2_MOUNT_ERRORS_RO		0x0020
#define EXT2_MOUNT_ERRORS_PANIC		0x0040
#define EXT2_MOUNT_MINIX_DF		0x0080
#define clear_opt(o, opt)		o &= ~EXT2_MOUNT_##opt
#define set_opt(o, opt)			o |= EXT2_MOUNT_##opt
#define test_opt(sb, opt)		((sb)->u.ext2_sb.s_mount_opt & \
EXT2_MOUNT_##opt)
#define EXT2_DFL_MAX_MNT_COUNT		20
#define EXT2_DFL_CHECKINTERVAL		0
#define EXT2_ERRORS_CONTINUE		1
#define EXT2_ERRORS_RO			2
#define EXT2_ERRORS_PANIC		3
#define EXT2_ERRORS_DEFAULT		EXT2_ERRORS_CONTINUE
struct ext2_super_block {
__u32	s_inodes_count;
__u32	s_blocks_count;
__u32	s_r_blocks_count;
__u32	s_free_blocks_count;
__u32	s_free_inodes_count;
__u32	s_first_data_block;
__u32	s_log_block_size;
__s32	s_log_frag_size;
__u32	s_blocks_per_group;
__u32	s_frags_per_group;
__u32	s_inodes_per_group;
__u32	s_mtime;
__u32	s_wtime;
__u16	s_mnt_count;
__s16	s_max_mnt_count;
__u16	s_magic;
__u16	s_state;
__u16	s_errors;
__u16	s_minor_rev_level;
__u32	s_lastcheck;
__u32	s_checkinterval;
__u32	s_creator_os;
__u32	s_rev_level;
__u16	s_def_resuid;
__u16	s_def_resgid;
__u32	s_first_ino;
__u16   s_inode_size;
__u16	s_block_group_nr;
__u32	s_feature_compat;
__u32	s_feature_incompat;
__u32	s_feature_ro_compat;
__u8	s_uuid[16];
char	s_volume_name[16];
char	s_last_mounted[64];
__u32	s_algorithm_usage_bitmap;
__u8	s_prealloc_blocks;
__u8	s_prealloc_dir_blocks;
__u16	s_padding1;
__u32	s_reserved[204];
};
#ifdef __KERNEL__
#define EXT2_SB(sb)	(&((sb)->u.ext2_sb))
#else
#define EXT2_SB(sb)	(sb)
#endif
#define EXT2_OS_LINUX		0
#define EXT2_OS_HURD		1
#define EXT2_OS_MASIX		2
#define EXT2_OS_FREEBSD		3
#define EXT2_OS_LITES		4
#define EXT2_GOOD_OLD_REV	0
#define EXT2_DYNAMIC_REV	1
#define EXT2_CURRENT_REV	EXT2_GOOD_OLD_REV
#define EXT2_MAX_SUPP_REV	EXT2_DYNAMIC_REV
#define EXT2_GOOD_OLD_INODE_SIZE 128
#define EXT2_HAS_COMPAT_FEATURE(sb,mask)			\
( EXT2_SB(sb)->s_feature_compat & (mask) )
#define EXT2_HAS_RO_COMPAT_FEATURE(sb,mask)			\
( EXT2_SB(sb)->s_feature_ro_compat & (mask) )
#define EXT2_HAS_INCOMPAT_FEATURE(sb,mask)			\
( EXT2_SB(sb)->s_feature_incompat & (mask) )
#define EXT2_FEATURE_COMPAT_DIR_PREALLOC	0x0001
#define EXT2_FEATURE_RO_COMPAT_SPARSE_SUPER	0x0001
#define EXT2_FEATURE_RO_COMPAT_LARGE_FILE	0x0002
#define EXT2_FEATURE_RO_COMPAT_BTREE_DIR	0x0004
#define EXT2_FEATURE_INCOMPAT_COMPRESSION	0x0001
#define EXT2_FEATURE_INCOMPAT_FILETYPE		0x0002
#define EXT2_FEATURE_COMPAT_SUPP	0
#define EXT2_FEATURE_INCOMPAT_SUPP	EXT2_FEATURE_INCOMPAT_FILETYPE
#define EXT2_FEATURE_RO_COMPAT_SUPP	(EXT2_FEATURE_RO_COMPAT_SPARSE_SUPER| \
EXT2_FEATURE_RO_COMPAT_LARGE_FILE| \
EXT2_FEATURE_RO_COMPAT_BTREE_DIR)
#define	EXT2_DEF_RESUID		0
#define	EXT2_DEF_RESGID		0
#define EXT2_NAME_LEN 255
struct ext2_dir_entry {
__u32	inode;
__u16	rec_len;
__u16	name_len;
char	name[EXT2_NAME_LEN];
};
struct ext2_dir_entry_2 {
__u32	inode;
__u16	rec_len;
__u8	name_len;
__u8	file_type;
char	name[EXT2_NAME_LEN];
};
#define EXT2_FT_UNKNOWN		0
#define EXT2_FT_REG_FILE	1
#define EXT2_FT_DIR		2
#define EXT2_FT_CHRDEV		3
#define EXT2_FT_BLKDEV 		4
#define EXT2_FT_FIFO		5
#define EXT2_FT_SOCK		6
#define EXT2_FT_SYMLINK		7
#define EXT2_FT_MAX		8
#define EXT2_DIR_PAD		 	4
#define EXT2_DIR_ROUND 			(EXT2_DIR_PAD - 1)
#define EXT2_DIR_REC_LEN(name_len)	(((name_len) + 8 + EXT2_DIR_ROUND) & \
~EXT2_DIR_ROUND)
#ifdef __KERNEL__
extern long long ext2_max_sizes[];
# define NORET_TYPE
# define ATTRIB_NORET  __attribute__((noreturn))
# define NORET_AND     noreturn,
extern int ext2_permission (struct inode *, int);
extern int ext2_group_sparse(int group);
extern int ext2_new_block (const struct inode *, unsigned long,
__u32 *, __u32 *, int *);
extern void ext2_free_blocks (const struct inode *, unsigned long,
unsigned long);
extern unsigned long ext2_count_free_blocks (struct super_block *);
extern void ext2_check_blocks_bitmap (struct super_block *);
extern struct ext2_group_desc * ext2_get_group_desc(struct super_block * sb,
unsigned int block_group,
struct buffer_head ** bh);
extern unsigned long ext2_count_free (struct buffer_head *, unsigned);
extern int ext2_check_dir_entry (const char *, struct inode *,
struct ext2_dir_entry_2 *, struct buffer_head *,
unsigned long);
extern int ext2_read (struct inode *, struct file *, char *, int);
extern int ext2_write (struct inode *, struct file *, char *, int);
extern int ext2_sync_file (struct file *, struct dentry *);
extern struct inode * ext2_new_inode (const struct inode *, int, int *);
extern void ext2_free_inode (struct inode *);
extern unsigned long ext2_count_free_inodes (struct super_block *);
extern void ext2_check_inodes_bitmap (struct super_block *);
extern int ext2_bmap (struct inode *, int);
extern struct buffer_head * ext2_getblk (struct inode *, long, int, int *);
extern struct buffer_head * ext2_bread (struct inode *, int, int, int *);
extern int ext2_getcluster (struct inode * inode, long block);
extern void ext2_read_inode (struct inode *);
extern void ext2_write_inode (struct inode *);
extern void ext2_put_inode (struct inode *);
extern void ext2_delete_inode (struct inode *);
extern int ext2_sync_inode (struct inode *);
extern int ext2_notify_change(struct dentry *, struct iattr *);
extern void ext2_discard_prealloc (struct inode *);
extern int ext2_ioctl (struct inode *, struct file *, unsigned int,
unsigned long);
extern void ext2_release (struct inode *, struct file *);
extern struct dentry *ext2_lookup (struct inode *, struct dentry *);
extern int ext2_create (struct inode *,struct dentry *,int);
extern int ext2_mkdir (struct inode *,struct dentry *,int);
extern int ext2_rmdir (struct inode *,struct dentry *);
extern int ext2_unlink (struct inode *,struct dentry *);
extern int ext2_symlink (struct inode *,struct dentry *,const char *);
extern int ext2_link (struct dentry *, struct inode *, struct dentry *);
extern int ext2_mknod (struct inode *, struct dentry *, int, int);
extern int ext2_rename (struct inode *, struct dentry *,
struct inode *, struct dentry *);
extern void ext2_error (struct super_block *, const char *, const char *, ...)
__attribute__ ((format (printf, 3, 4)));
extern NORET_TYPE void ext2_panic (struct super_block *, const char *,
const char *, ...)
__attribute__ ((NORET_AND format (printf, 3, 4)));
extern void ext2_warning (struct super_block *, const char *, const char *, ...)
__attribute__ ((format (printf, 3, 4)));
extern void ext2_put_super (struct super_block *);
extern void ext2_write_super (struct super_block *);
extern int ext2_remount (struct super_block *, int *, char *);
extern struct super_block * ext2_read_super (struct super_block *,void *,int);
extern int init_ext2_fs(void);
extern int ext2_statfs (struct super_block *, struct statfs *, int);
extern void ext2_truncate (struct inode *);
extern struct inode_operations ext2_dir_inode_operations;
extern struct inode_operations ext2_file_inode_operations;
extern struct inode_operations ext2_symlink_inode_operations;
#endif
#endif