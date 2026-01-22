#ifndef _QNX4_FS_SB
#define _QNX4_FS_SB
#include <linux/qnxtypes.h>
struct qnx4_sb_info {
struct buffer_head *sb_buf;
struct qnx4_super_block *sb;
unsigned int Version;
struct qnx4_inode_entry *BitMap;
};
#endif