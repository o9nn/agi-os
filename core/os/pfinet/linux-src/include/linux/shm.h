#ifndef _LINUX_SHM_H_
#define _LINUX_SHM_H_
#include <linux/ipc.h>
#include <asm/shmparam.h>
struct shmid_ds {
struct ipc_perm		shm_perm;
int			shm_segsz;
__kernel_time_t		shm_atime;
__kernel_time_t		shm_dtime;
__kernel_time_t		shm_ctime;
__kernel_ipc_pid_t	shm_cpid;
__kernel_ipc_pid_t	shm_lpid;
unsigned short		shm_nattch;
unsigned short 		shm_unused;
void 			*shm_unused2;
void			*shm_unused3;
};
struct shmid_kernel
{
struct shmid_ds		u;
unsigned long		shm_npages;
unsigned long		*shm_pages;
struct vm_area_struct	*attaches;
};
#define SHM_R		0400
#define SHM_W		0200
#define	SHM_RDONLY	010000
#define	SHM_RND		020000
#define	SHM_REMAP	040000
#define SHM_LOCK 	11
#define SHM_UNLOCK 	12
#define SHM_STAT 	13
#define SHM_INFO 	14
struct	shminfo {
int shmmax;
int shmmin;
int shmmni;
int shmseg;
int shmall;
};
struct shm_info {
int used_ids;
unsigned long shm_tot;
unsigned long shm_rss;
unsigned long shm_swp;
unsigned long swap_attempts;
unsigned long swap_successes;
};
#ifdef __KERNEL__
#define	SHM_DEST	01000
#define SHM_LOCKED      02000
asmlinkage int sys_shmget (key_t key, int size, int flag);
asmlinkage int sys_shmat (int shmid, char *shmaddr, int shmflg, unsigned long *addr);
asmlinkage int sys_shmdt (char *shmaddr);
asmlinkage int sys_shmctl (int shmid, int cmd, struct shmid_ds *buf);
extern void shm_unuse(unsigned long entry, unsigned long page);
#endif
#endif