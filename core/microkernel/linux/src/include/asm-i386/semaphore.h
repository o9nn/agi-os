#ifndef _I386_SEMAPHORE_H
#define _I386_SEMAPHORE_H
#include <linux/linkage.h>
#include <asm/system.h>
struct semaphore {
int count;
int waking;
int lock ;
struct wait_queue * wait;
};
#define MUTEX ((struct semaphore) { 1, 0, 0, NULL })
#define MUTEX_LOCKED ((struct semaphore) { 0, 0, 0, NULL })
asmlinkage void down_failed(void );
asmlinkage void up_wakeup(void );
extern void __down(struct semaphore * sem);
extern void __up(struct semaphore * sem);
static inline void down(struct semaphore * sem)
{
int d0;
__asm__ __volatile__(
"# atomic down operation\n\t"
"movl $1f,%%eax\n\t"
#ifdef __SMP__
"lock ; "
#endif
"decl %1\n\t"
"js " SYMBOL_NAME_STR(down_failed) "\n"
"1:\n"
:"=&a" (d0), "=m" (sem->count)
:"c" (sem)
:"memory");
}
extern inline void get_buzz_lock(int *lock_ptr)
{
#ifdef __SMP__
while (xchg(lock_ptr,1) != 0) ;
#endif
}
extern inline void give_buzz_lock(int *lock_ptr)
{
#ifdef __SMP__
*lock_ptr = 0 ;
#endif
}
asmlinkage int down_failed_interruptible(void);
static inline int down_interruptible(struct semaphore * sem)
{
int	ret ;
__asm__ __volatile__(
"# atomic interruptible down operation\n\t"
"movl $2f,%%eax\n\t"
#ifdef __SMP__
"lock ; "
#endif
"decl %1\n\t"
"js " SYMBOL_NAME_STR(down_failed_interruptible) "\n\t"
"xorl %%eax,%%eax\n"
"2:\n"
:"=&a" (ret), "=m" (sem->count)
:"c" (sem)
:"memory");
return(ret) ;
}
static inline void up(struct semaphore * sem)
{
int d0;
__asm__ __volatile__(
"# atomic up operation\n\t"
"movl $1f,%%eax\n\t"
#ifdef __SMP__
"lock ; "
#endif
"incl %1\n\t"
"jle " SYMBOL_NAME_STR(up_wakeup)
"\n1:"
:"=&a" (d0), "=m" (sem->count)
:"c" (sem)
:"memory");
}
#endif