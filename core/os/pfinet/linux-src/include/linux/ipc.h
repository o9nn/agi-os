#ifndef _LINUX_IPC_H
#define _LINUX_IPC_H
#include <linux/types.h>
#define IPC_PRIVATE ((__kernel_key_t) 0)
struct ipc_perm
{
__kernel_key_t	key;
__kernel_uid_t	uid;
__kernel_gid_t	gid;
__kernel_uid_t	cuid;
__kernel_gid_t	cgid;
__kernel_mode_t	mode;
unsigned short	seq;
};
#define IPC_CREAT  00001000
#define IPC_EXCL   00002000
#define IPC_NOWAIT 00004000
#define IPC_DIPC 00010000
#define IPC_OWN  00020000
#define IPC_RMID 0
#define IPC_SET  1
#define IPC_STAT 2
#define IPC_INFO 3
#ifdef __KERNEL__
#define IPC_UNUSED	((void *) -1)
#define IPC_NOID	((void *) -2)
#endif
#endif