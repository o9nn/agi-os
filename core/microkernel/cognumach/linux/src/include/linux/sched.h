#ifndef _LINUX_SCHED_H
#define _LINUX_SCHED_H
#include <asm/param.h>
extern unsigned long event;
#include <linux/binfmts.h>
#include <linux/personality.h>
#include <linux/tasks.h>
#include <linux/kernel.h>
#include <asm/system.h>
#include <asm/semaphore.h>
#include <asm/page.h>
#include <linux/smp.h>
#include <linux/tty.h>
#include <linux/sem.h>
#define CSIGNAL 0x000000ff
#define CLONE_VM 0x00000100
#define CLONE_FS 0x00000200
#define CLONE_FILES 0x00000400
#define CLONE_SIGHAND 0x00000800
#define CLONE_PID 0x00001000
extern unsigned long avenrun[];
#define FSHIFT 11
#define FIXED_1 (1<<FSHIFT)
#define LOAD_FREQ (5*HZ)
#define EXP_1 1884
#define EXP_5 2014
#define EXP_15 2037
#define CALC_LOAD(load,exp,n) \
load *= exp; \
load += n*(FIXED_1-exp); \
load >>= FSHIFT;
#define CT_TO_SECS(x) ((x) / HZ)
#define CT_TO_USECS(x) (((x) % HZ) * 1000000/HZ)
extern int nr_running, nr_tasks;
extern int last_pid;
#define FIRST_TASK task[0]
#define LAST_TASK task[NR_TASKS-1]
#include <linux/head.h>
#include <linux/fs.h>
#include <linux/signal.h>
#include <linux/time.h>
#include <linux/param.h>
#include <linux/resource.h>
#include <linux/ptrace.h>
#include <linux/timer.h>
#include <asm/processor.h>
#define TASK_RUNNING 0
#define TASK_INTERRUPTIBLE 1
#define TASK_UNINTERRUPTIBLE 2
#define TASK_ZOMBIE 3
#define TASK_STOPPED 4
#define TASK_SWAPPING 5
#define SCHED_OTHER 0
#define SCHED_FIFO 1
#define SCHED_RR 2
struct sched_param {
int sched_priority;
};
#ifndef NULL
#define NULL ((void *) 0)
#endif
#ifdef __KERNEL__
extern void sched_init(void);
extern void show_state(void);
extern void trap_init(void);
asmlinkage void schedule(void);
struct files_struct {
int count;
fd_set close_on_exec;
fd_set open_fds;
struct file * fd[NR_OPEN];
};
#define INIT_FILES { \
1, \
{ { 0, } }, \
{ { 0, } }, \
{ NULL, } \
}
struct fs_struct {
int count;
unsigned short umask;
struct inode * root, * pwd;
};
#define INIT_FS { \
1, \
0022, \
NULL, NULL \
}
struct mm_struct {
int count;
pgd_t * pgd;
unsigned long context;
unsigned long start_code, end_code, start_data, end_data;
unsigned long start_brk, brk, start_stack, start_mmap;
unsigned long arg_start, arg_end, env_start, env_end;
unsigned long rss, total_vm, locked_vm;
unsigned long def_flags;
struct vm_area_struct * mmap;
struct vm_area_struct * mmap_avl;
struct semaphore mmap_sem;
};
#define INIT_MM { \
1, \
swapper_pg_dir, \
0, \
0, 0, 0, 0, \
0, 0, 0, 0, \
0, 0, 0, 0, \
0, 0, 0, \
0, \
&init_mmap, &init_mmap, MUTEX }
struct signal_struct {
int count;
struct sigaction action[32];
};
#define INIT_SIGNALS { \
1, \
{ {0,}, } }
struct task_struct {
volatile long state;
long counter;
long priority;
unsigned long signal;
unsigned long blocked;
unsigned long flags;
int errno;
long debugreg[8];
struct exec_domain *exec_domain;
struct linux_binfmt *binfmt;
struct task_struct *next_task, *prev_task;
struct task_struct *next_run, *prev_run;
unsigned long saved_kernel_stack;
unsigned long kernel_stack_page;
int exit_code, exit_signal;
unsigned long personality;
int dumpable:1;
int did_exec:1;
int pid;
int pgrp;
int tty_old_pgrp;
int session;
int leader;
int groups[NGROUPS];
struct task_struct *p_opptr, *p_pptr, *p_cptr, *p_ysptr, *p_osptr;
struct wait_queue *wait_chldexit;
unsigned short uid,euid,suid,fsuid;
unsigned short gid,egid,sgid,fsgid;
unsigned long timeout, policy, rt_priority;
unsigned long it_real_value, it_prof_value, it_virt_value;
unsigned long it_real_incr, it_prof_incr, it_virt_incr;
struct timer_list real_timer;
long utime, stime, cutime, cstime, start_time;
unsigned long min_flt, maj_flt, nswap, cmin_flt, cmaj_flt, cnswap;
int swappable:1;
unsigned long swap_address;
unsigned long old_maj_flt;
unsigned long dec_flt;
unsigned long swap_cnt;
struct rlimit rlim[RLIM_NLIMITS];
unsigned short used_math;
char comm[16];
int link_count;
struct tty_struct *tty;
struct sem_undo *semundo;
struct sem_queue *semsleeping;
struct desc_struct *ldt;
struct thread_struct tss;
struct fs_struct *fs;
struct files_struct *files;
struct mm_struct *mm;
struct signal_struct *sig;
#ifdef __SMP__
int processor;
int last_processor;
int lock_depth;
#endif
};
#define PF_ALIGNWARN 0x00000001
#define PF_PTRACED 0x00000010
#define PF_TRACESYS 0x00000020
#define PF_FORKNOEXEC 0x00000040
#define PF_SUPERPRIV 0x00000100
#define PF_DUMPCORE 0x00000200
#define PF_SIGNALED 0x00000400
#define PF_STARTING 0x00000002
#define PF_EXITING 0x00000004
#define PF_USEDFPU 0x00100000
#define PF_DTRACE 0x00200000
#define _STK_LIM (8*1024*1024)
#define DEF_PRIORITY (20*HZ/100)
#define INIT_TASK \
{ 0,DEF_PRIORITY,DEF_PRIORITY,0,0,0,0, \
{ 0, }, \
&default_exec_domain, \
NULL, \
&init_task,&init_task, &init_task, &init_task, \
0,(unsigned long) &init_kernel_stack, \
0,0,0,0,0, \
0,0,0,0,0, \
{NOGROUP,}, \
&init_task,&init_task,NULL,NULL,NULL,NULL, \
0,0,0,0,0,0,0,0, \
0,SCHED_OTHER,0,0,0,0,0,0,0, \
{ NULL, NULL, 0, 0, it_real_fn }, \
0,0,0,0,0, \
0,0,0,0,0,0, \
0,0,0,0,0, \
INIT_RLIMITS, \
0, \
"swapper", \
0,NULL, \
NULL, NULL, \
NULL, \
INIT_TSS, \
&init_fs, \
&init_files, \
&init_mm, \
&init_signals, \
}
extern struct mm_struct init_mm;
extern struct task_struct init_task;
extern struct task_struct *task[NR_TASKS];
extern struct task_struct *last_task_used_math;
extern struct task_struct *current_set[NR_CPUS];
#define current (0+current_set[smp_processor_id()])
extern unsigned long volatile jiffies;
extern unsigned long itimer_ticks;
extern unsigned long itimer_next;
extern struct timeval xtime;
extern int need_resched;
extern void do_timer(struct pt_regs *);
extern unsigned int * prof_buffer;
extern unsigned long prof_len;
extern unsigned long prof_shift;
extern int securelevel;
#define CURRENT_TIME (xtime.tv_sec)
extern void sleep_on(struct wait_queue ** p);
extern void interruptible_sleep_on(struct wait_queue ** p);
extern void wake_up(struct wait_queue ** p);
extern void wake_up_interruptible(struct wait_queue ** p);
extern void wake_up_process(struct task_struct * tsk);
extern void notify_parent(struct task_struct * tsk, int signal);
extern void force_sig(unsigned long sig,struct task_struct * p);
extern int send_sig(unsigned long sig,struct task_struct * p,int priv);
extern int in_group_p(gid_t grp);
extern int request_irq(unsigned int irq,
void (*handler)(int, void *, struct pt_regs *),
unsigned long flags,
const char *device,
void *dev_id);
extern void free_irq(unsigned int irq, void *dev_id);
extern inline int suser(void)
{
if (current->euid == 0) {
current->flags |= PF_SUPERPRIV;
return 1;
}
return 0;
}
extern void copy_thread(int, unsigned long, unsigned long, struct task_struct *, struct pt_regs *);
extern void flush_thread(void);
extern void exit_thread(void);
extern void exit_mm(struct task_struct *);
extern void exit_fs(struct task_struct *);
extern void exit_files(struct task_struct *);
extern void exit_sighand(struct task_struct *);
extern void release_thread(struct task_struct *);
extern int do_execve(char *, char **, char **, struct pt_regs *);
extern int do_fork(unsigned long, unsigned long, struct pt_regs *);
extern inline struct file *file_from_fd(const unsigned int fd)
{
if (fd >= NR_OPEN)
return NULL;
return current->files->fd[fd];
}
extern inline void __add_wait_queue(struct wait_queue ** p, struct wait_queue * wait)
{
struct wait_queue *head = *p;
struct wait_queue *next = WAIT_QUEUE_HEAD(p);
if (head)
next = head;
*p = wait;
wait->next = next;
}
extern inline void add_wait_queue(struct wait_queue ** p, struct wait_queue * wait)
{
unsigned long flags;
save_flags(flags);
cli();
__add_wait_queue(p, wait);
restore_flags(flags);
}
extern inline void __remove_wait_queue(struct wait_queue ** p, struct wait_queue * wait)
{
struct wait_queue * next = wait->next;
struct wait_queue * head = next;
for (;;) {
struct wait_queue * nextlist = head->next;
if (nextlist == wait)
break;
head = nextlist;
}
head->next = next;
}
extern inline void remove_wait_queue(struct wait_queue ** p, struct wait_queue * wait)
{
unsigned long flags;
save_flags(flags);
cli();
__remove_wait_queue(p, wait);
restore_flags(flags);
}
extern inline void select_wait(struct wait_queue ** wait_address, select_table * p)
{
struct select_table_entry * entry;
if (!p || !wait_address)
return;
if (p->nr >= __MAX_SELECT_TABLE_ENTRIES)
return;
entry = p->entry + p->nr;
entry->wait_address = wait_address;
entry->wait.task = current;
entry->wait.next = NULL;
add_wait_queue(wait_address,&entry->wait);
p->nr++;
}
#define REMOVE_LINKS(p) do { unsigned long flags; \
save_flags(flags) ; cli(); \
(p)->next_task->prev_task = (p)->prev_task; \
(p)->prev_task->next_task = (p)->next_task; \
restore_flags(flags); \
if ((p)->p_osptr) \
(p)->p_osptr->p_ysptr = (p)->p_ysptr; \
if ((p)->p_ysptr) \
(p)->p_ysptr->p_osptr = (p)->p_osptr; \
else \
(p)->p_pptr->p_cptr = (p)->p_osptr; \
} while (0)
#define SET_LINKS(p) do { unsigned long flags; \
save_flags(flags); cli(); \
(p)->next_task = &init_task; \
(p)->prev_task = init_task.prev_task; \
init_task.prev_task->next_task = (p); \
init_task.prev_task = (p); \
restore_flags(flags); \
(p)->p_ysptr = NULL; \
if (((p)->p_osptr = (p)->p_pptr->p_cptr) != NULL) \
(p)->p_osptr->p_ysptr = p; \
(p)->p_pptr->p_cptr = p; \
} while (0)
#define for_each_task(p) \
for (p = &init_task ; (p = p->next_task) != &init_task ; )
#endif
#endif