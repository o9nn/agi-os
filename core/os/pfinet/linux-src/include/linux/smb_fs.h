#ifndef _LINUX_SMB_FS_H
#define _LINUX_SMB_FS_H
#include <linux/smb.h>
#define SMB_IOC_GETMOUNTUID _IOR('u', 1, __kernel_uid_t)
#define SMB_IOC_NEWCONN _IOW('u', 2, struct smb_conn_opt)
#ifdef __KERNEL__
#include <asm/unaligned.h>
#define WVAL(buf,pos) \
(le16_to_cpu(get_unaligned((__u16 *)((__u8 *)(buf) + (pos)))))
#define DVAL(buf,pos) \
(le32_to_cpu(get_unaligned((__u32 *)((__u8 *)(buf) + (pos)))))
#define WSET(buf,pos,val) \
put_unaligned(cpu_to_le16((__u16)(val)), (__u16 *)((__u8 *)(buf) + (pos)))
#define DSET(buf,pos,val) \
put_unaligned(cpu_to_le32((__u32)(val)), (__u32 *)((__u8 *)(buf) + (pos)))
#define smb_base(buf) ((__u8 *)(((__u8 *)(buf))+4))
#include <linux/vmalloc.h>
#ifdef DEBUG_SMB_MALLOC
extern int smb_malloced;
extern int smb_current_vmalloced;
static inline void *
smb_vmalloc(unsigned int size)
{
smb_malloced += 1;
smb_current_vmalloced += 1;
return vmalloc(size);
}
static inline void
smb_vfree(void *obj)
{
smb_current_vmalloced -= 1;
vfree(obj);
}
#else
#define smb_kmalloc(s,p) kmalloc(s,p)
#define smb_kfree_s(o,s) kfree_s(o,s)
#define smb_vmalloc(s) vmalloc(s)
#define smb_vfree(o) vfree(o)
#endif
#define SMB_F_CACHEVALID 0x01
#define SMB_F_LOCALWRITE 0x02
#define SMB_FIX_WIN95 0x0001
#define SMB_FIX_OLDATTR 0x0002
#define SMB_FIX_DIRATTR 0x0004
#define SMB_CAP_RAW_MODE 0x0001
#define SMB_CAP_MPX_MODE 0x0002
#define SMB_CAP_UNICODE 0x0004
#define SMB_CAP_LARGE_FILES 0x0008
#define SMB_CAP_NT_SMBS 0x0010
#define SMB_CAP_RPC_REMOTE_APIS 0x0020
#define SMB_CAP_STATUS32 0x0040
#define SMB_CAP_LEVEL_II_OPLOCKS 0x0080
#define SMB_CAP_LOCK_AND_READ 0x0100
#define SMB_CAP_NT_FIND 0x0200
#define SMB_CAP_DFS 0x1000
#define SMB_CAP_LARGE_READX 0x4000
int smb_mmap(struct file *, struct vm_area_struct *);
extern struct inode_operations smb_file_inode_operations;
extern struct inode_operations smb_dir_inode_operations;
void smb_renew_times(struct dentry *);
int smb_ioctl (struct inode *, struct file *, unsigned int, unsigned long);
struct super_block *smb_read_super(struct super_block *, void *, int);
void smb_get_inode_attr(struct inode *, struct smb_fattr *);
void smb_invalidate_inodes(struct smb_sb_info *);
int smb_revalidate_inode(struct dentry *);
int smb_notify_change(struct dentry *, struct iattr *);
unsigned long smb_invent_inos(unsigned long);
struct inode *smb_iget(struct super_block *, struct smb_fattr *);
extern int init_smb_fs(void);
__u32 smb_len(unsigned char *);
__u8 *smb_encode_smb_length(__u8 *, __u32);
__u8 *smb_setup_header(struct smb_sb_info *, __u8, __u16, __u16);
int smb_get_rsize(struct smb_sb_info *);
int smb_get_wsize(struct smb_sb_info *);
int smb_newconn(struct smb_sb_info *, struct smb_conn_opt *);
int smb_errno(struct smb_sb_info *);
int smb_close(struct inode *);
void smb_close_dentry(struct dentry *);
int smb_close_fileid(struct dentry *, __u16);
int smb_open(struct dentry *, int);
int smb_proc_read(struct dentry *, off_t, int, char *);
int smb_proc_write(struct dentry *, off_t, int, const char *);
int smb_proc_create(struct dentry *, __u16, time_t, __u16 *);
int smb_proc_mv(struct dentry *, struct dentry *);
int smb_proc_mkdir(struct dentry *);
int smb_proc_rmdir(struct dentry *);
int smb_proc_unlink(struct dentry *);
int smb_proc_readdir(struct dentry *, int, void *);
int smb_proc_getattr(struct dentry *, struct smb_fattr *);
int smb_proc_setattr(struct dentry *, struct smb_fattr *);
int smb_proc_settime(struct dentry *, struct smb_fattr *);
int smb_proc_dskattr(struct super_block *, struct statfs *);
int smb_proc_reconnect(struct smb_sb_info *);
int smb_proc_connect(struct smb_sb_info *);
int smb_proc_disconnect(struct smb_sb_info *);
int smb_proc_trunc(struct smb_sb_info *, __u16, __u32);
void smb_init_root_dirent(struct smb_sb_info *, struct smb_fattr *);
static inline int
smb_is_open(struct inode *i)
{
return (i->u.smbfs_i.open == SMB_SERVER(i)->generation);
}
int smb_round_length(int);
int smb_valid_socket(struct inode *);
void smb_close_socket(struct smb_sb_info *);
int smb_release(struct smb_sb_info *server);
int smb_connect(struct smb_sb_info *server);
int smb_request(struct smb_sb_info *server);
int smb_request_read_raw(struct smb_sb_info *, unsigned char *, int);
int smb_request_write_raw(struct smb_sb_info *, unsigned const char *, int);
int smb_catch_keepalive(struct smb_sb_info *server);
int smb_dont_catch_keepalive(struct smb_sb_info *server);
int smb_trans2_request(struct smb_sb_info *server, __u16 trans2_command,
int ldata, unsigned char *data,
int lparam, unsigned char *param,
int *lrdata, unsigned char **rdata,
int *lrparam, unsigned char **rparam);
struct cache_index {
unsigned short num_entries;
unsigned short space;
struct cache_block * block;
};
#define NINDEX (PAGE_SIZE-64)/sizeof(struct cache_index)
struct cache_head {
int valid;
int status;
int entries;
int pages;
int idx;
struct cache_index index[NINDEX];
};
struct cache_entry {
ino_t ino;
unsigned short namelen;
unsigned short offset;
};
struct cache_block {
union {
struct cache_entry table[1];
char names[PAGE_SIZE];
} cb_data;
};
struct cache_dirent {
ino_t ino;
unsigned long pos;
int len;
char * name;
};
struct cache_head * smb_get_dircache(struct dentry *);
void smb_init_dircache(struct cache_head *);
void smb_free_dircache(struct cache_head *);
int smb_refill_dircache(struct cache_head *, struct dentry *);
void smb_add_to_cache(struct cache_head *, struct cache_dirent *, off_t);
int smb_find_in_cache(struct cache_head *, off_t, struct cache_dirent *);
void smb_invalid_dir_cache(struct inode *);
#endif
#endif