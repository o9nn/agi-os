#ifndef _MSDOS_FS_I
#define _MSDOS_FS_I
#ifndef _LINUX_PIPE_FS_I_H
#include <linux/pipe_fs_i.h>
#endif
struct msdos_inode_info {
struct pipe_inode_info reserved;
int i_start;
int i_logstart;
int i_attrs;
int i_ctime_ms;
int i_binary;
int i_location;
struct inode *i_fat_inode;
struct list_head i_fat_hash;
off_t i_last_pos;
};
#endif