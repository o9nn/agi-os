#ifndef _LINUX_EXT2_FS_H
#define _LINUX_EXT2_FS_H
#undef EXT2FS_DEBUG
#define EXT2_PREALLOCATE
#define EXT2_DEFAULT_PREALLOC_BLOCKS 8
#define EXT2FS_DATE "95/08/09"
#define EXT2FS_VERSION "0.5b"
#ifdef EXT2FS_DEBUG
# define ext2_debug(f, a...) { \
printk ("EXT2-fs DEBUG (%s, %d): %s:", \
__FILE__, __LINE__, __FUNCTION__); \
printk (f, ## a); \
}
#else
# define ext2_debug(f, a...)
#endif
#define EXT2_BAD_INO 1
#define EXT2_ROOT_INO 2
#define EXT2_ACL_IDX_INO 3
#define EXT2_ACL_DATA_INO 4
#define EXT2_BOOT_LOADER_INO 5
#define EXT2_UNDEL_DIR_INO 6
#define EXT2_GOOD_OLD_FIRST_INO 11
#define EXT2_SUPER_MAGIC 0xEF53
#define EXT2_LINK_MAX 32000
#define EXT2_MIN_BLOCK_SIZE 1024
#define EXT2_MAX_BLOCK_SIZE 4096
#define EXT2_MIN_BLOCK_LOG_SIZE 10
#define EXT2_BLOCK_SIZE(s) (EXT2_MIN_BLOCK_SIZE << (s)->s_log_block_size)
#define EXT2_ACLE_PER_BLOCK(s) (EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_acl_entry))
#define EXT2_ADDR_PER_BLOCK(s) (EXT2_BLOCK_SIZE(s) / sizeof (__u32))
#define EXT2_BLOCK_SIZE_BITS(s) ((s)->s_log_block_size + 10)
#define EXT2_INODE_SIZE(s) (((s)->s_rev_level == htole32(EXT2_GOOD_OLD_REV)) ? \
EXT2_GOOD_OLD_INODE_SIZE : \
le16toh ((s)->s_inode_size))
#define EXT2_FIRST_INO(s) (((s)->s_rev_level == htole32(EXT2_GOOD_OLD_REV)) ? \
EXT2_GOOD_OLD_FIRST_INO : \
(le32toh ((s)->s_first_ino)))
#define EXT2_MIN_FRAG_SIZE 1024
#define EXT2_MAX_FRAG_SIZE 4096
#define EXT2_MIN_FRAG_LOG_SIZE 10
#define EXT2_FRAG_SIZE(s) (EXT2_MIN_FRAG_SIZE << le32toh((s)->s_log_frag_size))
#define EXT2_FRAGS_PER_BLOCK(s) (EXT2_BLOCK_SIZE(s) / EXT2_FRAG_SIZE(s))
struct ext2_acl_header
{
__u32 aclh_size;
__u32 aclh_file_count;
__u32 aclh_acle_count;
__u32 aclh_first_acle;
};
struct ext2_acl_entry
{
__u32 acle_size;
__u16 acle_perms;
__u16 acle_type;
__u16 acle_tag;
__u16 acle_pad1;
__u32 acle_next;
};
struct ext2_group_desc
{
__u32 bg_block_bitmap;
__u32 bg_inode_bitmap;
__u32 bg_inode_table;
__u16 bg_free_blocks_count;
__u16 bg_free_inodes_count;
__u16 bg_used_dirs_count;
__u16 bg_pad;
__u32 bg_reserved[3];
};
#define EXT2_BLOCKS_PER_GROUP(s) (le32toh ((s)->s_blocks_per_group))
#define EXT2_DESC_PER_BLOCK(s) (EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_group))
#define EXT2_INODES_PER_GROUP(s) (le32toh ((s)->s_inodes_per_group))
#define EXT2_NDIR_BLOCKS 12
#define EXT2_IND_BLOCK EXT2_NDIR_BLOCKS
#define EXT2_DIND_BLOCK (EXT2_IND_BLOCK + 1)
#define EXT2_TIND_BLOCK (EXT2_DIND_BLOCK + 1)
#define EXT2_N_BLOCKS (EXT2_TIND_BLOCK + 1)
#define EXT2_SECRM_FL 0x00000001
#define EXT2_UNRM_FL 0x00000002
#define EXT2_COMPR_FL 0x00000004
#define EXT2_SYNC_FL 0x00000008
#define EXT2_IMMUTABLE_FL 0x00000010
#define EXT2_APPEND_FL 0x00000020
#define EXT2_NODUMP_FL 0x00000040
#define EXT2_NOATIME_FL 0x00000080
#define EXT2_DIRTY_FL 0x00000100
#define EXT2_COMPRBLK_FL 0x00000200
#define EXT2_NOCOMP_FL 0x00000400
#define EXT2_ECOMPR_FL 0x00000800
#define EXT2_BTREE_FL 0x00001000
#define EXT2_IMAGIC_FL 0x00002000
#define EXT2_JOURNAL_DATA_FL 0x00004000
#define EXT2_NOTAIL_FL 0x00008000
#define EXT2_DIRSYNC_FL 0x00010000
#define EXT2_TOPDIR_FL 0x00020000
#define EXT2_RESERVED_FL 0x80000000
#define EXT2_FL_USER_VISIBLE 0x00001FFF
#define EXT2_FL_USER_MODIFIABLE 0x000000FF
#define EXT2_FL_INHERITED (EXT2_SECRM_FL | EXT2_UNRM_FL | EXT2_COMPR_FL |\
EXT2_SYNC_FL | EXT2_NODUMP_FL |\
EXT2_NOATIME_FL | EXT2_COMPRBLK_FL |\
EXT2_NOCOMP_FL | EXT2_JOURNAL_DATA_FL |\
EXT2_NOTAIL_FL | EXT2_DIRSYNC_FL)
#define EXT2_REG_FLMASK (~(EXT2_DIRSYNC_FL | EXT2_TOPDIR_FL))
#define EXT2_OTHER_FLMASK (EXT2_NODUMP_FL | EXT2_NOATIME_FL)
static __inline__ __u32 ext2_mask_flags(mode_t mode, __u32 flags)
{
if (S_ISDIR(mode))
return flags;
else if (S_ISREG(mode))
return flags & EXT2_REG_FLMASK;
else
return flags & EXT2_OTHER_FLMASK;
}
#define EXT2_IOC_GETFLAGS _IOR('f', 1, long)
#define EXT2_IOC_SETFLAGS _IOW('f', 2, long)
#define EXT2_IOC_GETVERSION _IOR('v', 1, long)
#define EXT2_IOC_SETVERSION _IOW('v', 2, long)
#define EXT2_IOC_GETRSVSZ _IOR('f', 5, long)
#define EXT2_IOC_SETRSVSZ _IOW('f', 6, long)
#define EXT2_IOC32_GETVERSION _IOR('v', 1, int)
#define EXT2_IOC32_SETVERSION _IOW('v', 2, int)
struct ext2_inode {
__u16 i_mode;
__u16 i_uid;
__u32 i_size;
__u32 i_atime;
__u32 i_ctime;
__u32 i_mtime;
__u32 i_dtime;
__u16 i_gid;
__u16 i_links_count;
__u32 i_blocks;
__u32 i_flags;
union {
struct {
__u32 l_i_reserved1;
} linux1;
struct {
__u32 h_i_translator;
} hurd1;
struct {
__u32 m_i_reserved1;
} masix1;
} osd1;
__u32 i_block[EXT2_N_BLOCKS];
__u32 i_generation;
__u32 i_file_acl;
__u32 i_dir_acl;
__u32 i_faddr;
union {
struct {
__u8 l_i_frag;
__u8 l_i_fsize;
__u16 i_pad1;
__u16 l_i_uid_high;
__u16 l_i_gid_high;
__u32 l_i_reserved2;
} linux2;
struct {
__u8 h_i_frag;
__u8 h_i_fsize;
__u16 h_i_mode_high;
__u16 h_i_uid_high;
__u16 h_i_gid_high;
__u32 h_i_author;
} hurd2;
struct {
__u8 m_i_frag;
__u8 m_i_fsize;
__u16 m_pad1;
__u32 m_i_reserved2[2];
} masix2;
} osd2;
};
#define i_size_high i_dir_acl
#define i_translator osd1.hurd1.h_i_translator
#define i_frag osd2.hurd2.h_i_frag
#define i_fsize osd2.hurd2.h_i_fsize
#define i_uid_high osd2.hurd2.h_i_uid_high
#define i_gid_high osd2.hurd2.h_i_gid_high
#define i_author osd2.hurd2.h_i_author
#define i_mode_high osd2.hurd2.h_i_mode_high
#define EXT2_VALID_FS 0x0001
#define EXT2_ERROR_FS 0x0002
#define EXT2_MOUNT_CHECK_NORMAL 0x000001
#define EXT2_MOUNT_CHECK_STRICT 0x000002
#define EXT2_MOUNT_CHECK (EXT2_MOUNT_CHECK_NORMAL | \
EXT2_MOUNT_CHECK_STRICT)
#define EXT2_MOUNT_OLDALLOC 0x000002
#define EXT2_MOUNT_GRPID 0x000004
#define EXT2_MOUNT_DEBUG 0x000008
#define EXT2_MOUNT_ERRORS_CONT 0x000010
#define EXT2_MOUNT_ERRORS_RO 0x000020
#define EXT2_MOUNT_ERRORS_PANIC 0x000040
#define EXT2_MOUNT_MINIX_DF 0x000080
#define EXT2_MOUNT_NOBH 0x000100
#define EXT2_MOUNT_NO_UID32 0x000200
#define EXT2_MOUNT_XATTR_USER 0x004000
#define EXT2_MOUNT_POSIX_ACL 0x008000
#define EXT2_MOUNT_XIP 0x010000
#define EXT2_MOUNT_USRQUOTA 0x020000
#define EXT2_MOUNT_GRPQUOTA 0x040000
#define EXT2_MOUNT_RESERVATION 0x080000
#define EXT2_MOUNT_DAX 0x100000
#define clear_opt(o, opt) o &= ~EXT2_MOUNT_##opt
#define set_opt(o, opt) o |= EXT2_MOUNT_##opt
#define test_opt(o, opt) o & EXT2_MOUNT_##opt
#define EXT2_DFL_MAX_MNT_COUNT 20
#define EXT2_DFL_CHECKINTERVAL 0
#define EXT2_ERRORS_CONTINUE 1
#define EXT2_ERRORS_RO 2
#define EXT2_ERRORS_PANIC 3
#define EXT2_ERRORS_DEFAULT EXT2_ERRORS_CONTINUE
struct ext2_super_block {
__u32 s_inodes_count;
__u32 s_blocks_count;
__u32 s_r_blocks_count;
__u32 s_free_blocks_count;
__u32 s_free_inodes_count;
__u32 s_first_data_block;
__u32 s_log_block_size;
__s32 s_log_frag_size;
__u32 s_blocks_per_group;
__u32 s_frags_per_group;
__u32 s_inodes_per_group;
__u32 s_mtime;
__u32 s_wtime;
__u16 s_mnt_count;
__s16 s_max_mnt_count;
__u16 s_magic;
__u16 s_state;
__u16 s_errors;
__u16 s_minor_rev_level;
__u32 s_lastcheck;
__u32 s_checkinterval;
__u32 s_creator_os;
__u32 s_rev_level;
__u16 s_def_resuid;
__u16 s_def_resgid;
__u32 s_first_ino;
__u16 s_inode_size;
__u16 s_block_group_nr;
__u32 s_feature_compat;
__u32 s_feature_incompat;
__u32 s_feature_ro_compat;
__u8 s_uuid[16];
char s_volume_name[16];
char s_last_mounted[64];
__u32 s_algorithm_usage_bitmap;
__u8 s_prealloc_blocks;
__u8 s_prealloc_dir_blocks;
__u16 s_padding1;
__u8 s_journal_uuid[16];
__u32 s_journal_inum;
__u32 s_journal_dev;
__u32 s_last_orphan;
__u32 s_hash_seed[4];
__u8 s_def_hash_version;
__u8 s_reserved_char_pad;
__u16 s_reserved_word_pad;
__u32 s_default_mount_opts;
__u32 s_first_meta_bg;
__u32 s_reserved[190];
};
#define EXT2_OS_LINUX 0
#define EXT2_OS_HURD 1
#define EXT2_OS_MASIX 2
#define EXT2_OS_FREEBSD 3
#define EXT2_OS_LITES 4
#define EXT2_GOOD_OLD_REV 0
#define EXT2_DYNAMIC_REV 1
#define EXT2_CURRENT_REV EXT2_GOOD_OLD_REV
#define EXT2_MAX_SUPP_REV EXT2_DYNAMIC_REV
#define EXT2_GOOD_OLD_INODE_SIZE 128
#define EXT2_HAS_COMPAT_FEATURE(sb,mask) \
( (sb)->s_feature_compat & htole32 (mask) )
#define EXT2_HAS_RO_COMPAT_FEATURE(sb,mask) \
( (sb)->s_feature_ro_compat & htole32 (mask) )
#define EXT2_HAS_INCOMPAT_FEATURE(sb,mask) \
( (sb)->s_feature_incompat & htole32 (mask) )
#define EXT2_SET_COMPAT_FEATURE(sb,mask) \
(sb)->s_feature_compat |= htole32 (mask)
#define EXT2_SET_RO_COMPAT_FEATURE(sb,mask) \
(sb)->s_feature_ro_compat |= htole32 (mask)
#define EXT2_SET_INCOMPAT_FEATURE(sb,mask) \
(sb)->s_feature_incompat |= htole32 (mask)
#define EXT2_CLEAR_COMPAT_FEATURE(sb,mask) \
(sb)->s_feature_compat &= ~htole32 (mask)
#define EXT2_CLEAR_RO_COMPAT_FEATURE(sb,mask) \
(sb)->s_feature_ro_compat &= ~htole32 (mask)
#define EXT2_CLEAR_INCOMPAT_FEATURE(sb,mask) \
(sb)->s_feature_incompat &= ~htole32 (mask)
#define EXT2_FEATURE_COMPAT_DIR_PREALLOC 0x0001
#define EXT2_FEATURE_COMPAT_IMAGIC_INODES 0x0002
#define EXT3_FEATURE_COMPAT_HAS_JOURNAL 0x0004
#define EXT2_FEATURE_COMPAT_EXT_ATTR 0x0008
#define EXT2_FEATURE_COMPAT_RESIZE_INO 0x0010
#define EXT2_FEATURE_COMPAT_DIR_INDEX 0x0020
#define EXT2_FEATURE_COMPAT_ANY 0xffffffff
#define EXT2_FEATURE_RO_COMPAT_SPARSE_SUPER 0x0001
#define EXT2_FEATURE_RO_COMPAT_LARGE_FILE 0x0002
#define EXT2_FEATURE_RO_COMPAT_BTREE_DIR 0x0004
#define EXT2_FEATURE_RO_COMPAT_ANY 0xffffffff
#define EXT2_FEATURE_INCOMPAT_COMPRESSION 0x0001
#define EXT2_FEATURE_INCOMPAT_FILETYPE 0x0002
#define EXT3_FEATURE_INCOMPAT_RECOVER 0x0004
#define EXT3_FEATURE_INCOMPAT_JOURNAL_DEV 0x0008
#define EXT2_FEATURE_INCOMPAT_META_BG 0x0010
#define EXT2_FEATURE_INCOMPAT_ANY 0xffffffff
#define EXT2_FEATURE_COMPAT_SUPP EXT2_FEATURE_COMPAT_EXT_ATTR
#define EXT2_FEATURE_INCOMPAT_SUPP EXT2_FEATURE_INCOMPAT_FILETYPE
#define EXT2_FEATURE_RO_COMPAT_SUPP (EXT2_FEATURE_RO_COMPAT_SPARSE_SUPER| \
EXT2_FEATURE_RO_COMPAT_LARGE_FILE| \
EXT2_FEATURE_RO_COMPAT_BTREE_DIR)
#define EXT2_FEATURE_RO_COMPAT_UNSUPPORTED ~EXT2_FEATURE_RO_COMPAT_SUPP
#define EXT2_FEATURE_INCOMPAT_UNSUPPORTED ~EXT2_FEATURE_INCOMPAT_SUPP
#define EXT2_DEF_RESUID 0
#define EXT2_DEF_RESGID 0
#define EXT2_DEFM_DEBUG 0x0001
#define EXT2_DEFM_BSDGROUPS 0x0002
#define EXT2_DEFM_XATTR_USER 0x0004
#define EXT2_DEFM_ACL 0x0008
#define EXT2_DEFM_UID16 0x0010
#define EXT3_DEFM_JMODE 0x0060
#define EXT3_DEFM_JMODE_DATA 0x0020
#define EXT3_DEFM_JMODE_ORDERED 0x0040
#define EXT3_DEFM_JMODE_WBACK 0x0060
#define EXT2_NAME_LEN 255
struct ext2_dir_entry {
__u32 inode;
__u16 rec_len;
__u16 name_len;
char name[EXT2_NAME_LEN];
};
struct ext2_dir_entry_2 {
__u32 inode;
__u16 rec_len;
__u8 name_len;
__u8 file_type;
char name[EXT2_NAME_LEN];
};
#define EXT2_FT_UNKNOWN 0
#define EXT2_FT_REG_FILE 1
#define EXT2_FT_DIR 2
#define EXT2_FT_CHRDEV 3
#define EXT2_FT_BLKDEV 4
#define EXT2_FT_FIFO 5
#define EXT2_FT_SOCK 6
#define EXT2_FT_SYMLINK 7
#define EXT2_FT_MAX 8
#define EXT2_DIR_PAD 4
#define EXT2_DIR_ROUND (EXT2_DIR_PAD - 1)
#define EXT2_DIR_REC_LEN(name_len) (((name_len) + 8 + EXT2_DIR_ROUND) & \
~EXT2_DIR_ROUND)
#define EXT2_MAX_REC_LEN ((1<<16)-1)
struct ext2_inode_info {
__u32 i_data[15];
__u32 i_flags;
__u32 i_faddr;
__u8 i_frag_no;
__u8 i_frag_size;
__u16 i_osync;
__u32 i_file_acl;
__u32 i_dir_acl;
__u32 i_dtime;
__u32 not_used_1;
__u32 i_block_group;
__u32 i_next_alloc_block;
__u32 i_next_alloc_goal;
__u32 i_prealloc_block;
__u32 i_prealloc_count;
int i_new_inode:1;
};
#endif