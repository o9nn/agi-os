#ifndef _LINUX_IPC_H
#define _LINUX_IPC_H
#include <linux/types.h>
typedef int key_t;
#define IPC_PRIVATE ((key_t) 0)
struct ipc_perm
{
key_t key;
ushort uid;
ushort gid;
ushort cuid;
ushort cgid;
ushort mode;
ushort seq;
};
#define IPC_CREAT 00001000
#define IPC_EXCL 00002000
#define IPC_NOWAIT 00004000
#define IPC_RMID 0
#define IPC_SET 1
#define IPC_STAT 2
#define IPC_INFO 3
#ifdef __KERNEL__
#define IPC_UNUSED ((void *) -1)
#define IPC_NOID ((void *) -2)
struct ipc_kludge {
struct msgbuf *msgp;
long msgtyp;
};
#define SEMOP 1
#define SEMGET 2
#define SEMCTL 3
#define MSGSND 11
#define MSGRCV 12
#define MSGGET 13
#define MSGCTL 14
#define SHMAT 21
#define SHMDT 22
#define SHMGET 23
#define SHMCTL 24
#define IPCCALL(version,op) ((version)<<16 | (op))
#endif
#endif