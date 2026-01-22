#ifndef _NFS_FS_I
#define _NFS_FS_I
#include <linux/nfs.h>
#include <linux/pipe_fs_i.h>
struct nfs_inode_info {
struct pipe_inode_info	pipeinfo;
unsigned short		flags;
unsigned long		read_cache_jiffies;
unsigned long		read_cache_mtime;
unsigned long		attrtimeo;
struct nfs_wreq *	writeback;
};
#define NFS_INO_REVALIDATE	0x0001
#define NFS_IS_SNAPSHOT		0x0010
struct nfs_lock_info {
u32		state;
u32		flags;
};
#define NFS_LCK_GRANTED		0x0001
#endif