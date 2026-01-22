#ifndef _NFS_FS_SB
#define _NFS_FS_SB
#include <linux/nfs.h>
#include <linux/in.h>
struct nfs_server {
struct rpc_clnt *	client;
int			flags;
int			rsize;
int			wsize;
unsigned int		bsize;
unsigned int		acregmin;
unsigned int		acregmax;
unsigned int		acdirmin;
unsigned int		acdirmax;
char *			hostname;
};
struct nfs_sb_info {
struct nfs_server	s_server;
struct nfs_fh		s_root;
};
#endif