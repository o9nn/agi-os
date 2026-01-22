#ifndef _LINUX_MSDOS_FS_H
#define _LINUX_MSDOS_FS_H
#include <linux/fs.h>
#include <linux/stat.h>
#include <linux/fd.h>
#include <asm/byteorder.h>
#define MSDOS_ROOT_INO  1
#define SECTOR_SIZE     512
#define SECTOR_BITS	9
#define MSDOS_DPB	(MSDOS_DPS)
#define MSDOS_DPB_BITS	4
#define MSDOS_DPS	(SECTOR_SIZE/sizeof(struct msdos_dir_entry))
#define MSDOS_DPS_BITS	4
#define MSDOS_DIR_BITS	5
#define MSDOS_SUPER_MAGIC 0x4d44
#define FAT_CACHE    8
#define MSDOS_MAX_EXTRA	3
#define ATTR_RO      1
#define ATTR_HIDDEN  2
#define ATTR_SYS     4
#define ATTR_VOLUME  8
#define ATTR_DIR     16
#define ATTR_ARCH    32
#define ATTR_NONE    0
#define ATTR_UNUSED  (ATTR_VOLUME | ATTR_ARCH | ATTR_SYS | ATTR_HIDDEN)
#define ATTR_EXT     (ATTR_RO | ATTR_HIDDEN | ATTR_SYS | ATTR_VOLUME)
#define ATTR_DIR_READ_BOTH 512
#define ATTR_DIR_READ_SHORT 1024
#define CASE_LOWER_BASE 8
#define CASE_LOWER_EXT  16
#define SCAN_ANY     0
#define SCAN_HID     1
#define SCAN_NOTHID  2
#define SCAN_NOTANY  3
#define DELETED_FLAG 0xe5
#define IS_FREE(n) (!*(n) || *(const unsigned char *) (n) == DELETED_FLAG || \
*(const unsigned char *) (n) == FD_FILL_BYTE)
#define MSDOS_VALID_MODE (S_IFREG | S_IFDIR | S_IRWXU | S_IRWXG | S_IRWXO)
#define MSDOS_SB(s) (&((s)->u.msdos_sb))
#define MSDOS_I(i) (&((i)->u.msdos_i))
#define MSDOS_NAME 11
#define MSDOS_LONGNAME 256
#define MSDOS_SLOTS 21
#define MSDOS_DOT    ".          "
#define MSDOS_DOTDOT "..         "
#define MSDOS_FAT12 4078
#define EOF_FAT12 0xFF8
#define EOF_FAT16 0xFFF8
#define EOF_FAT32 0xFFFFFF8
#define EOF_FAT(s) (MSDOS_SB(s)->fat_bits == 32 ? EOF_FAT32 : \
MSDOS_SB(s)->fat_bits == 16 ? EOF_FAT16 : EOF_FAT12)
#define FAT_BINARY_FL		0x00000001
#define	VFAT_IOCTL_READDIR_BOTH		_IOR('r', 1, struct dirent [2])
#define	VFAT_IOCTL_READDIR_SHORT	_IOR('r', 2, struct dirent [2])
#define CF_LE_W(v) le16_to_cpu(v)
#define CF_LE_L(v) le32_to_cpu(v)
#define CT_LE_W(v) cpu_to_le16(v)
#define CT_LE_L(v) cpu_to_le32(v)
struct fat_boot_sector {
__s8	ignored[3];
__s8	system_id[8];
__u8	sector_size[2];
__u8	cluster_size;
__u16	reserved;
__u8	fats;
__u8	dir_entries[2];
__u8	sectors[2];
__u8	media;
__u16	fat_length;
__u16	secs_track;
__u16	heads;
__u32	hidden;
__u32	total_sect;
__u32	fat32_length;
__u16	flags;
__u8	version[2];
__u32	root_cluster;
__u16	info_sector;
__u16	backup_boot;
__u16	reserved2[6];
};
struct fat_boot_fsinfo {
__u32   reserved1;
__u32   signature;
__u32   free_clusters;
__u32   next_cluster;
__u32   reserved2[4];
};
struct msdos_dir_entry {
__s8	name[8],ext[3];
__u8	attr;
__u8    lcase;
__u8	ctime_ms;
__u16	ctime;
__u16	cdate;
__u16	adate;
__u16   starthi;
__u16	time,date,start;
__u32	size;
};
struct msdos_dir_slot {
__u8    id;
__u8    name0_4[10];
__u8    attr;
__u8    reserved;
__u8    alias_checksum;
__u8    name5_10[12];
__u16   start;
__u8    name11_12[4];
};
struct vfat_slot_info {
int is_long;
int long_slots;
int total_slots;
loff_t longname_offset;
loff_t shortname_offset;
int ino;
};
#define MSDOS_CAN_BMAP(mib) (!(((mib)->cluster_size & 1) || \
((mib)->data_start & 1)))
#define MSDOS_MKMODE(a,m) (m & (a & ATTR_RO ? S_IRUGO|S_IXUGO : S_IRWXUGO))
#define MSDOS_MKATTR(m) ((m & S_IWUGO) ? ATTR_NONE : ATTR_RO)
#ifdef __KERNEL__
struct fat_cache {
kdev_t device;
int start_cluster;
int file_cluster;
int disk_cluster;
struct fat_cache *next;
};
extern int fat_is_binary(char conversion,char *extension);
extern void lock_fat(struct super_block *sb);
extern void unlock_fat(struct super_block *sb);
extern int fat_add_cluster(struct inode *inode);
extern struct buffer_head *fat_add_cluster1(struct inode *inode);
extern int date_dos2unix(__u16 time, __u16 date);
extern void fat_fs_panic(struct super_block *s,const char *msg);
extern void fat_lock_creation(void);
extern void fat_unlock_creation(void);
extern void fat_date_unix2dos(int unix_date,__u16 *time, __u16 *date);
extern int fat__get_entry(struct inode *dir,loff_t *pos,struct buffer_head **bh,
struct msdos_dir_entry **de,int *ino);
static __inline__ int fat_get_entry(struct inode *dir,loff_t *pos,
struct buffer_head **bh,struct msdos_dir_entry **de,int *ino)
{
if (*bh && *de &&
(*de - (struct msdos_dir_entry *)(*bh)->b_data) < MSDOS_DPB-1) {
*pos += sizeof(struct msdos_dir_entry);
(*de)++;
(*ino)++;
return 0;
}
return fat__get_entry(dir,pos,bh,de,ino);
}
extern int fat_scan(struct inode *dir,const char *name,struct buffer_head **res_bh,
struct msdos_dir_entry **res_de,int *ino);
extern int fat_parent_ino(struct inode *dir,int locked);
extern int fat_subdirs(struct inode *dir);
void fat_clusters_flush(struct super_block *sb);
extern int fat_access(struct super_block *sb,int nr,int new_value);
extern int fat_smap(struct inode *inode,int sector);
extern int fat_free(struct inode *inode,int skip);
void fat_cache_inval_inode(struct inode *inode);
void fat_cache_inval_dev(kdev_t device);
extern void fat_cache_init(void);
void fat_cache_lookup(struct inode *inode,int cluster,int *f_clu,int *d_clu);
void fat_cache_add(struct inode *inode,int f_clu,int d_clu);
int fat_get_cluster(struct inode *inode,int cluster);
extern void fat_hash_init(void);
extern int fat_bmap(struct inode *inode,int block);
extern int fat_notify_change(struct dentry *, struct iattr *);
extern void fat_clear_inode(struct inode *inode);
extern void fat_delete_inode(struct inode *inode);
extern void fat_put_super(struct super_block *sb);
extern void fat_attach(struct inode *inode, int ino);
extern void fat_detach(struct inode *inode);
extern struct inode *fat_iget(struct super_block*,int);
extern struct inode *fat_build_inode(struct super_block*,struct msdos_dir_entry*,int,int*);
extern struct super_block *fat_read_super(struct super_block *s, void *data, int silent, struct inode_operations *dir_ops);
extern void msdos_put_super(struct super_block *sb);
extern int fat_statfs(struct super_block *sb,struct statfs *buf, int);
extern void fat_write_inode(struct inode *inode);
extern struct file_operations fat_dir_operations;
extern int fat_search_long(struct inode *dir, const char *name, int len,
int anycase, loff_t *spos, loff_t *lpos);
extern int fat_readdir(struct file *filp,
void *dirent, filldir_t);
extern int fat_dir_ioctl(struct inode * inode, struct file * filp,
unsigned int cmd, unsigned long arg);
int fat_add_entries(struct inode *dir,int slots, struct buffer_head **bh,
struct msdos_dir_entry **de, int *ino);
int fat_dir_empty(struct inode *dir);
extern struct inode_operations fat_file_inode_operations;
extern struct inode_operations fat_file_inode_operations_1024;
extern struct inode_operations fat_file_inode_operations_readpage;
extern ssize_t fat_file_read(struct file *, char *, size_t, loff_t *);
extern ssize_t fat_file_write(struct file *, const char *, size_t, loff_t *);
extern void fat_truncate(struct inode *inode);
extern int fat_mmap(struct file *, struct vm_area_struct *);
extern int fat_readpage(struct file *, struct page *);
extern int init_vfat_fs(void);
extern int init_msdos_fs(void);
extern struct file_system_type msdos_fs_type;
extern struct super_block *msdos_read_super(struct super_block *sb,void *data, int silent);
extern void msdos_read_inode(struct inode *inode);
extern struct dentry *msdos_lookup(struct inode *dir,struct dentry *);
extern int msdos_create(struct inode *dir,struct dentry *dentry,int mode);
extern int msdos_rmdir(struct inode *dir,struct dentry *dentry);
extern int msdos_mkdir(struct inode *dir,struct dentry *dentry,int mode);
extern int msdos_unlink(struct inode *dir,struct dentry *dentry);
extern int msdos_rename(struct inode *old_dir,struct dentry *old_dentry,
struct inode *new_dir,struct dentry *new_dentry);
extern int init_fat_nls(void);
extern struct fat_nls_table *fat_load_nls(int codepage);
extern unsigned char fat_uni2esc[];
extern unsigned char fat_esc2uni[];
extern int init_fat_fs(void);
extern void cleanup_fat_fs(void);
extern int fat_register_nls(struct fat_nls_table * fmt);
extern int fat_unregister_nls(struct fat_nls_table * fmt);
extern struct fat_nls_table *fat_find_nls(int codepage);
extern struct fat_nls_table *fat_load_nls(int codepage);
extern void fat_unload_nls(int codepage);
extern int init_fat_nls(void);
extern int vfat_create(struct inode *dir,struct dentry *dentry,int mode);
extern int vfat_unlink(struct inode *dir,struct dentry *dentry);
extern int vfat_mkdir(struct inode *dir,struct dentry *dentry,int mode);
extern int vfat_rmdir(struct inode *dir,struct dentry *dentry);
extern int vfat_rename(struct inode *old_dir,struct dentry *old_dentry,
struct inode *new_dir,struct dentry *new_dentry);
extern struct super_block *vfat_read_super(struct super_block *sb,void *data,
int silent);
extern void vfat_read_inode(struct inode *inode);
extern struct dentry *vfat_lookup(struct inode *dir,struct dentry *);
extern struct file_system_type vfat_fs_type;
#endif
#endif