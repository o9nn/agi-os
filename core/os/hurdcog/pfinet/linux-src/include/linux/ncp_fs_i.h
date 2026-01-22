#ifndef _LINUX_NCP_FS_I
#define _LINUX_NCP_FS_I
#include <linux/ncp.h>
#ifdef __KERNEL__
enum ncp_inode_state {
NCP_INODE_VALID = 19,
NCP_INODE_LOOKED_UP,
NCP_INODE_CACHED,
NCP_INODE_INVALID
};
struct ncp_inode_info {
enum ncp_inode_state state;
int nused;
struct ncp_inode_info *dir;
struct ncp_inode_info *next, *prev;
struct inode *inode;
struct nw_file_info finfo;
};
#endif
#endif