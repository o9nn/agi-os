#ifndef _LINUX_MSG_H
#define _LINUX_MSG_H
#include <linux/ipc.h>
#define MSG_STAT 11
#define MSG_INFO 12
#define MSG_NOERROR 010000
#define MSG_EXCEPT 020000
struct msqid_ds {
struct ipc_perm msg_perm;
struct msg *msg_first;
struct msg *msg_last;
__kernel_time_t msg_stime;
__kernel_time_t msg_rtime;
__kernel_time_t msg_ctime;
struct wait_queue *wwait;
struct wait_queue *rwait;
unsigned short msg_cbytes;
unsigned short msg_qnum;
unsigned short msg_qbytes;
__kernel_ipc_pid_t msg_lspid;
__kernel_ipc_pid_t msg_lrpid;
};
struct msgbuf {
long mtype;
char mtext[1];
};
struct msginfo {
int msgpool;
int msgmap;
int msgmax;
int msgmnb;
int msgmni;
int msgssz;
int msgtql;
unsigned short msgseg;
};
#define MSGMNI 128
#define MSGMAX 4056
#define MSGMNB 16384
#define MSGQNUM 1024
#define MSGPOOL (MSGMNI*MSGMNB/1024)
#define MSGTQL MSGMNB
#define MSGMAP MSGMNB
#define MSGSSZ 16
#define __MSGSEG ((MSGPOOL*1024)/ MSGSSZ)
#define MSGSEG (__MSGSEG <= 0xffff ? __MSGSEG : 0xffff)
#ifdef __KERNEL__
struct msg {
struct msg *msg_next;
long msg_type;
char *msg_spot;
time_t msg_stime;
short msg_ts;
};
asmlinkage int sys_msgget (key_t key, int msgflg);
asmlinkage int sys_msgsnd (int msqid, struct msgbuf *msgp, size_t msgsz, int msgflg);
asmlinkage int sys_msgrcv (int msqid, struct msgbuf *msgp, size_t msgsz, long msgtyp,
int msgflg);
asmlinkage int sys_msgctl (int msqid, int cmd, struct msqid_ds *buf);
#endif
#endif