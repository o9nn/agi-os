#ifndef _LINUX_NFS_MOUNT_H
#define _LINUX_NFS_MOUNT_H
#define NFS_MOUNT_VERSION 3
struct nfs_mount_data {
int version;
int fd;
struct nfs_fh root;
int flags;
int rsize;
int wsize;
int timeo;
int retrans;
int acregmin;
int acregmax;
int acdirmin;
int acdirmax;
struct sockaddr_in addr;
char hostname[256];
int namlen;
unsigned int bsize;
};
#define NFS_MOUNT_SOFT 0x0001
#define NFS_MOUNT_INTR 0x0002
#define NFS_MOUNT_SECURE 0x0004
#define NFS_MOUNT_POSIX 0x0008
#define NFS_MOUNT_NOCTO 0x0010
#define NFS_MOUNT_NOAC 0x0020
#define NFS_MOUNT_TCP 0x0040
#define NFS_MOUNT_VER3 0x0080
#define NFS_MOUNT_KERBEROS 0x0100
#define NFS_MOUNT_NONLM 0x0200
#endif