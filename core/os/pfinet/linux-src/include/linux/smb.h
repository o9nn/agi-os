#ifndef _LINUX_SMB_H
#define _LINUX_SMB_H
#include <linux/types.h>
enum smb_protocol {
SMB_PROTOCOL_NONE,
SMB_PROTOCOL_CORE,
SMB_PROTOCOL_COREPLUS,
SMB_PROTOCOL_LANMAN1,
SMB_PROTOCOL_LANMAN2,
SMB_PROTOCOL_NT1
};
enum smb_case_hndl {
SMB_CASE_DEFAULT,
SMB_CASE_LOWER,
SMB_CASE_UPPER
};
struct smb_dskattr {
__u16 total;
__u16 allocblocks;
__u16 blocksize;
__u16 free;
};
struct smb_conn_opt {
unsigned int fd;
enum smb_protocol protocol;
enum smb_case_hndl case_handling;
__u32 max_xmit;
__u16 server_uid;
__u16 tid;
__u16 secmode;
__u16 maxmux;
__u16 maxvcs;
__u16 rawmode;
__u32 sesskey;
__u32 maxraw;
__u32 capabilities;
__s16 serverzone;
};
#ifdef __KERNEL__
#define SMB_MAXNAMELEN 255
#define SMB_MAXPATHLEN 1024
struct smb_fattr {
__u16 attr;
unsigned long f_ino;
umode_t f_mode;
nlink_t f_nlink;
uid_t f_uid;
gid_t f_gid;
kdev_t f_rdev;
off_t f_size;
time_t f_atime;
time_t f_mtime;
time_t f_ctime;
unsigned long f_blksize;
unsigned long f_blocks;
};
struct smb_dirent {
struct smb_fattr attr;
int f_pos;
int len;
__u8 name[SMB_MAXNAMELEN];
};
enum smb_conn_state {
CONN_VALID,
CONN_INVALID,
CONN_RETRIED
};
#define SMB_READDIR_CACHE_SIZE 64
#define SMB_SUPER_MAGIC 0x517B
#define SMB_SERVER(inode) (&(inode->i_sb->u.smbfs_sb))
#define SMB_INOP(inode) (&(inode->u.smbfs_i))
#define SMB_HEADER_LEN 37
#define SMB_DEF_MAX_XMIT 32768
#define SMB_INITIAL_PACKET_SIZE 4000
#define TRANS2_MAX_TRANSFER (4096-17)
#endif
#endif