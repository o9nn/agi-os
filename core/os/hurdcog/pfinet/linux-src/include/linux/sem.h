#ifndef _LINUX_SEM_H
#define _LINUX_SEM_H
#include <linux/ipc.h>
#define SEM_UNDO        0x1000
#define GETPID  11
#define GETVAL  12
#define GETALL  13
#define GETNCNT 14
#define GETZCNT 15
#define SETVAL  16
#define SETALL  17
#define SEM_STAT 18
#define SEM_INFO 19
struct semid_ds {
struct ipc_perm	sem_perm;
__kernel_time_t	sem_otime;
__kernel_time_t	sem_ctime;
struct sem	*sem_base;
struct sem_queue *sem_pending;
struct sem_queue **sem_pending_last;
struct sem_undo	*undo;
unsigned short	sem_nsems;
};
struct sembuf {
unsigned short  sem_num;
short		sem_op;
short		sem_flg;
};
union semun {
int val;
struct semid_ds *buf;
unsigned short *array;
struct seminfo *__buf;
void *__pad;
};
struct  seminfo {
int semmap;
int semmni;
int semmns;
int semmnu;
int semmsl;
int semopm;
int semume;
int semusz;
int semvmx;
int semaem;
};
#define SEMMNI  128
#define SEMMSL  250
#define SEMMNS  (SEMMNI*SEMMSL)
#define SEMOPM  32
#define SEMVMX  32767
#define SEMUME  SEMOPM
#define SEMMNU  SEMMNS
#define SEMAEM  (SEMVMX >> 1)
#define SEMMAP  SEMMNS
#define SEMUSZ  20
#ifdef __KERNEL__
struct sem {
int	semval;
int	sempid;
};
struct sem_queue {
struct sem_queue *	next;
struct sem_queue **	prev;
struct wait_queue *	sleeper;
struct sem_undo *	undo;
int    			pid;
int    			status;
struct semid_ds *	sma;
struct sembuf *		sops;
int			nsops;
int			alter;
};
struct sem_undo {
struct sem_undo *	proc_next;
struct sem_undo *	id_next;
int			semid;
short *			semadj;
};
asmlinkage int sys_semget (key_t key, int nsems, int semflg);
asmlinkage int sys_semop (int semid, struct sembuf *sops, unsigned nsops);
asmlinkage int sys_semctl (int semid, int semnum, int cmd, union semun arg);
#endif
#endif