#ifndef _SYSV_FS_I
#define _SYSV_FS_I
struct sysv_inode_info {
u32 i_data[10+1+1+1];
};
#endif