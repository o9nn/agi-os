#ifndef _LINUX_SMB_FS_I
#define _LINUX_SMB_FS_I
#ifdef __KERNEL__
#include <linux/types.h>
struct smb_inode_info {
unsigned int open;
__u16 fileid;
__u16 attr;
__u16 access;
__u16 cache_valid;
unsigned long oldmtime;
unsigned long closed;
};
#endif
#endif