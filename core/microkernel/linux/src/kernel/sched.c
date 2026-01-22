#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/kernel.h>
#include <linux/kernel_stat.h>
#include <linux/fdreg.h>
#include <linux/errno.h>
#include <linux/time.h>
#include <linux/ptrace.h>
#include <linux/delay.h>
#include <linux/interrupt.h>
#include <linux/tqueue.h>
#include <linux/resource.h>
#include <linux/mm.h>
#include <linux/smp.h>
#include <asm/system.h>
#include <asm/io.h>
#include <asm/segment.h>
#include <asm/pgtable.h>
#include <asm/mmu_context.h>
#include <linux/timex.h>
int securelevel = 0;
long tick = (1000000 + HZ/2) / HZ;
volatile struct timeval xtime;
int tickadj = 500/HZ ? 500/HZ : 1;
DECLARE_TASK_QUEUE(tq_timer);
DECLARE_TASK_QUEUE(tq_immediate);
DECLARE_TASK_QUEUE(tq_scheduler);
int time_state = TIME_ERROR;
int time_status = STA_UNSYNC;
long time_offset = 0;
long time_constant = 2;
long time_tolerance = MAXFREQ;
long time_precision = 1;
long time_maxerror = NTP_PHASE_LIMIT;
long time_esterror = NTP_PHASE_LIMIT;
long time_phase = 0;
long time_freq = ((1000000 + HZ/2) % HZ - HZ/2) << SHIFT_USEC;
long time_adj = 0;
long time_reftime = 0;
long time_adjust = 0;
long time_adjust_step = 0;
int need_resched = 0;
unsigned long event = 0;
extern int _setitimer(int, struct itimerval *, struct itimerval *);
unsigned int * prof_buffer = NULL;
unsigned long prof_len = 0;
unsigned long prof_shift = 0;
#define _S(nr) (1<<((nr)-1))
extern void mem_use(void);
extern unsigned long get_wchan(struct task_struct *);
static unsigned long init_kernel_stack[1024] = { STACK_MAGIC, };
unsigned long init_user_stack[1024] = { STACK_MAGIC, };
static struct vm_area_struct init_mmap = INIT_MMAP;
static struct fs_struct init_fs = INIT_FS;
static struct files_struct init_files = INIT_FILES;
static struct signal_struct init_signals = INIT_SIGNALS;
struct mm_struct init_mm = INIT_MM;
struct task_struct init_task = INIT_TASK;
unsigned long volatile jiffies=0;
struct task_struct *current_set[NR_CPUS];
struct task_struct *last_task_used_math = NULL;
struct task_struct * task[NR_TASKS] = {&init_task, };
struct kernel_stat kstat = { 0 };
static inline void add_to_runqueue(struct task_struct * p)
{
#ifdef __SMP__
int cpu=smp_processor_id();
#endif
#if 1
if (p->next_run || p->prev_run) {
printk("task already on run-queue\n");
return;
}
#endif
if (p->policy != SCHED_OTHER || p->counter > current->counter + 3)
need_resched = 1;
nr_running++;
(p->prev_run = init_task.prev_run)->next_run = p;
p->next_run = &init_task;
init_task.prev_run = p;
#ifdef __SMP__
while(set_bit(31,&smp_process_available))
{
while(test_bit(31,&smp_process_available))
{
if(clear_bit(cpu,&smp_invalidate_needed))
{
local_flush_tlb();
set_bit(cpu,&cpu_callin_map[0]);
}
}
}
smp_process_available++;
clear_bit(31,&smp_process_available);
if ((0!=p->pid) && smp_threads_ready)
{
int i;
for (i=0;i<smp_num_cpus;i++)
{
if (0==current_set[cpu_logical_map[i]]->pid)
{
smp_message_pass(cpu_logical_map[i], MSG_RESCHEDULE, 0L, 0);
break;
}
}
}
#endif
}
static inline void del_from_runqueue(struct task_struct * p)
{
struct task_struct *next = p->next_run;
struct task_struct *prev = p->prev_run;
#if 1
if (!next || !prev) {
printk("task not on run-queue\n");
return;
}
#endif
if (p == &init_task) {
static int nr = 0;
if (nr < 5) {
nr++;
printk("idle task may not sleep\n");
}
return;
}
nr_running--;
next->prev_run = prev;
prev->next_run = next;
p->next_run = NULL;
p->prev_run = NULL;
}
static inline void move_last_runqueue(struct task_struct * p)
{
struct task_struct *next = p->next_run;
struct task_struct *prev = p->prev_run;
next->prev_run = prev;
prev->next_run = next;
p->next_run = &init_task;
prev = init_task.prev_run;
init_task.prev_run = p;
p->prev_run = prev;
prev->next_run = p;
}
inline void wake_up_process(struct task_struct * p)
{
unsigned long flags;
save_flags(flags);
cli();
p->state = TASK_RUNNING;
if (!p->next_run)
add_to_runqueue(p);
restore_flags(flags);
}
static void process_timeout(unsigned long __data)
{
struct task_struct * p = (struct task_struct *) __data;
p->timeout = 0;
wake_up_process(p);
}
static inline int goodness(struct task_struct * p, struct task_struct * prev, int this_cpu)
{
int weight;
#ifdef __SMP__
if (p->processor != NO_PROC_ID)
return -1000;
#ifdef PAST_2_0
if (p->processor_mask && !(p->processor_mask & (1<<this_cpu))
return -1000;
#endif
#endif
if (p->policy != SCHED_OTHER)
return 1000 + p->rt_priority;
weight = p->counter;
if (weight) {
#ifdef __SMP__
if (p->last_processor == this_cpu)
weight += PROC_CHANGE_PENALTY;
#endif
if (p == prev)
weight += 1;
}
return weight;
}
#if defined(__SMP__) && defined(__i386__)
volatile unsigned char smp_blocked_interrupt_pending = 0;
volatile unsigned char saved_active_kernel_processor = NO_PROC_ID;
void allow_interrupts(void)
{
if (smp_processor_id() == boot_cpu_id) return;
if (smp_blocked_interrupt_pending)
{
unsigned long saved_kernel_counter;
long timeout_counter;
saved_active_kernel_processor = active_kernel_processor;
saved_kernel_counter = kernel_counter;
kernel_counter = 0;
active_kernel_processor = boot_cpu_id;
timeout_counter = 6000000;
while (active_kernel_processor != saved_active_kernel_processor &&
--timeout_counter >= 0)
{
udelay(10);
barrier();
}
if (timeout_counter < 0)
panic("FORWARDED INTERRUPT TIMEOUT (AKP = %d, Saved AKP = %d)\n",
active_kernel_processor, saved_active_kernel_processor);
kernel_counter = saved_kernel_counter;
saved_active_kernel_processor = NO_PROC_ID;
}
}
#else
void allow_interrupts(void) {}
#endif
asmlinkage void schedule(void)
{
int c;
struct task_struct * p;
struct task_struct * prev, * next;
unsigned long timeout = 0;
int this_cpu=smp_processor_id();
allow_interrupts();
if (intr_count)
goto scheduling_in_interrupt;
if (bh_active & bh_mask) {
intr_count = 1;
do_bottom_half();
intr_count = 0;
}
run_task_queue(&tq_scheduler);
need_resched = 0;
prev = current;
cli();
if (!prev->counter && prev->policy == SCHED_RR) {
prev->counter = prev->priority;
move_last_runqueue(prev);
}
switch (prev->state) {
case TASK_INTERRUPTIBLE:
if (prev->signal & ~prev->blocked)
goto makerunnable;
timeout = prev->timeout;
if (timeout && (timeout <= jiffies)) {
prev->timeout = 0;
timeout = 0;
makerunnable:
prev->state = TASK_RUNNING;
break;
}
default:
del_from_runqueue(prev);
case TASK_RUNNING:
}
p = init_task.next_run;
sti();
#ifdef __SMP__
prev->processor = NO_PROC_ID;
#define idle_task (task[cpu_number_map[this_cpu]])
#else
#define idle_task (&init_task)
#endif
c = -1000;
next = idle_task;
while (p != &init_task) {
int weight = goodness(p, prev, this_cpu);
if (weight > c)
c = weight, next = p;
p = p->next_run;
}
if (!c) {
for_each_task(p)
p->counter = (p->counter >> 1) + p->priority;
}
#ifdef __SMP__
next->processor = this_cpu;
next->last_processor = this_cpu;
#endif
#ifdef __SMP_PROF__
if (0==next->pid)
set_bit(this_cpu,&smp_idle_map);
else
clear_bit(this_cpu,&smp_idle_map);
#endif
if (prev != next) {
struct timer_list timer;
kstat.context_swtch++;
if (timeout) {
init_timer(&timer);
timer.expires = timeout;
timer.data = (unsigned long) prev;
timer.function = process_timeout;
add_timer(&timer);
}
get_mmu_context(next);
switch_to(prev,next);
if (timeout)
del_timer(&timer);
}
return;
scheduling_in_interrupt:
printk("Aiee: scheduling in interrupt %p\n",
__builtin_return_address(0));
}
#ifndef __alpha__
asmlinkage int sys_pause(void)
{
current->state = TASK_INTERRUPTIBLE;
schedule();
return -ERESTARTNOHAND;
}
#endif
void wake_up(struct wait_queue **q)
{
struct wait_queue *next;
struct wait_queue *head;
if (!q || !(next = *q))
return;
head = WAIT_QUEUE_HEAD(q);
while (next != head) {
struct task_struct *p = next->task;
next = next->next;
if (p != NULL) {
if ((p->state == TASK_UNINTERRUPTIBLE) ||
(p->state == TASK_INTERRUPTIBLE))
wake_up_process(p);
}
if (!next)
goto bad;
}
return;
bad:
printk("wait_queue is bad (eip = %p)\n",
__builtin_return_address(0));
printk("        q = %p\n",q);
printk("       *q = %p\n",*q);
}
void wake_up_interruptible(struct wait_queue **q)
{
struct wait_queue *next;
struct wait_queue *head;
if (!q || !(next = *q))
return;
head = WAIT_QUEUE_HEAD(q);
while (next != head) {
struct task_struct *p = next->task;
next = next->next;
if (p != NULL) {
if (p->state == TASK_INTERRUPTIBLE)
wake_up_process(p);
}
if (!next)
goto bad;
}
return;
bad:
printk("wait_queue is bad (eip = %p)\n",
__builtin_return_address(0));
printk("        q = %p\n",q);
printk("       *q = %p\n",*q);
}
static inline int waking_non_zero(struct semaphore *sem)
{
int	ret ;
long	flags ;
get_buzz_lock(&sem->lock) ;
save_flags(flags) ;
cli() ;
if ((ret = (sem->waking > 0)))
sem->waking-- ;
restore_flags(flags) ;
give_buzz_lock(&sem->lock) ;
return(ret) ;
}
void __up(struct semaphore *sem)
{
atomic_inc(&sem->waking) ;
wake_up(&sem->wait);
}
int __do_down(struct semaphore * sem, int task_state)
{
struct task_struct *tsk = current;
struct wait_queue wait = { tsk, NULL };
int		  ret = 0 ;
tsk->state = task_state;
add_wait_queue(&sem->wait, &wait);
for (;;)
{
if (waking_non_zero(sem))
break ;
if (   task_state == TASK_INTERRUPTIBLE
&& (tsk->signal & ~tsk->blocked)
)
{
ret = -EINTR ;
atomic_inc(&sem->count) ;
break ;
}
schedule();
tsk->state = task_state;
}
tsk->state = TASK_RUNNING;
remove_wait_queue(&sem->wait, &wait);
return(ret) ;
}
void __down(struct semaphore * sem)
{
__do_down(sem,TASK_UNINTERRUPTIBLE) ;
}
int __down_interruptible(struct semaphore * sem)
{
return(__do_down(sem,TASK_INTERRUPTIBLE)) ;
}
static inline void __sleep_on(struct wait_queue **p, int state)
{
unsigned long flags;
struct wait_queue wait = { current, NULL };
if (!p)
return;
if (current == task[0])
panic("task[0] trying to sleep");
current->state = state;
save_flags(flags);
cli();
__add_wait_queue(p, &wait);
sti();
schedule();
cli();
__remove_wait_queue(p, &wait);
restore_flags(flags);
}
void interruptible_sleep_on(struct wait_queue **p)
{
__sleep_on(p,TASK_INTERRUPTIBLE);
}
void sleep_on(struct wait_queue **p)
{
__sleep_on(p,TASK_UNINTERRUPTIBLE);
}
#define TVN_BITS 6
#define TVR_BITS 8
#define TVN_SIZE (1 << TVN_BITS)
#define TVR_SIZE (1 << TVR_BITS)
#define TVN_MASK (TVN_SIZE - 1)
#define TVR_MASK (TVR_SIZE - 1)
#define SLOW_BUT_DEBUGGING_TIMERS 0
struct timer_vec {
int index;
struct timer_list *vec[TVN_SIZE];
};
struct timer_vec_root {
int index;
struct timer_list *vec[TVR_SIZE];
};
static struct timer_vec tv5 = { 0 };
static struct timer_vec tv4 = { 0 };
static struct timer_vec tv3 = { 0 };
static struct timer_vec tv2 = { 0 };
static struct timer_vec_root tv1 = { 0 };
static struct timer_vec * const tvecs[] = {
(struct timer_vec *)&tv1, &tv2, &tv3, &tv4, &tv5
};
#define NOOF_TVECS (sizeof(tvecs) / sizeof(tvecs[0]))
static unsigned long timer_jiffies = 0;
static inline void insert_timer(struct timer_list *timer,
struct timer_list **vec, int idx)
{
if ((timer->next = vec[idx]))
vec[idx]->prev = timer;
vec[idx] = timer;
timer->prev = (struct timer_list *)&vec[idx];
}
static inline void internal_add_timer(struct timer_list *timer)
{
unsigned long expires = timer->expires;
unsigned long idx = expires - timer_jiffies;
if (idx < TVR_SIZE) {
int i = expires & TVR_MASK;
insert_timer(timer, tv1.vec, i);
} else if (idx < 1 << (TVR_BITS + TVN_BITS)) {
int i = (expires >> TVR_BITS) & TVN_MASK;
insert_timer(timer, tv2.vec, i);
} else if (idx < 1 << (TVR_BITS + 2 * TVN_BITS)) {
int i = (expires >> (TVR_BITS + TVN_BITS)) & TVN_MASK;
insert_timer(timer, tv3.vec, i);
} else if (idx < 1 << (TVR_BITS + 3 * TVN_BITS)) {
int i = (expires >> (TVR_BITS + 2 * TVN_BITS)) & TVN_MASK;
insert_timer(timer, tv4.vec, i);
} else if (expires < timer_jiffies) {
insert_timer(timer, tv1.vec, tv1.index);
} else if (idx < 0xffffffffUL) {
int i = (expires >> (TVR_BITS + 3 * TVN_BITS)) & TVN_MASK;
insert_timer(timer, tv5.vec, i);
} else {
timer->next = timer->prev = timer;
}
}
void add_timer(struct timer_list *timer)
{
unsigned long flags;
save_flags(flags);
cli();
#if SLOW_BUT_DEBUGGING_TIMERS
if (timer->next || timer->prev) {
printk("add_timer() called with non-zero list from %p\n",
__builtin_return_address(0));
goto out;
}
#endif
internal_add_timer(timer);
#if SLOW_BUT_DEBUGGING_TIMERS
out:
#endif
restore_flags(flags);
}
static inline int detach_timer(struct timer_list *timer)
{
int ret = 0;
struct timer_list *next, *prev;
next = timer->next;
prev = timer->prev;
if (next) {
next->prev = prev;
}
if (prev) {
ret = 1;
prev->next = next;
}
return ret;
}
int del_timer(struct timer_list * timer)
{
int ret;
unsigned long flags;
save_flags(flags);
cli();
ret = detach_timer(timer);
timer->next = timer->prev = 0;
restore_flags(flags);
return ret;
}
static inline void cascade_timers(struct timer_vec *tv)
{
struct timer_list *timer;
timer = tv->vec[tv->index];
while (timer) {
struct timer_list *tmp = timer;
timer = timer->next;
internal_add_timer(tmp);
}
tv->vec[tv->index] = NULL;
tv->index = (tv->index + 1) & TVN_MASK;
}
static inline void run_timer_list(void)
{
cli();
while ((long)(jiffies - timer_jiffies) >= 0) {
struct timer_list *timer;
if (!tv1.index) {
int n = 1;
do {
cascade_timers(tvecs[n]);
} while (tvecs[n]->index == 1 && ++n < NOOF_TVECS);
}
while ((timer = tv1.vec[tv1.index])) {
void (*fn)(unsigned long) = timer->function;
unsigned long data = timer->data;
detach_timer(timer);
timer->next = timer->prev = NULL;
sti();
fn(data);
cli();
}
++timer_jiffies;
tv1.index = (tv1.index + 1) & TVR_MASK;
}
sti();
}
static inline void run_old_timers(void)
{
struct timer_struct *tp;
unsigned long mask;
for (mask = 1, tp = timer_table+0 ; mask ; tp++,mask += mask) {
if (mask > timer_active)
break;
if (!(mask & timer_active))
continue;
if (tp->expires > jiffies)
continue;
timer_active &= ~mask;
tp->fn();
sti();
}
}
void tqueue_bh(void)
{
run_task_queue(&tq_timer);
}
void immediate_bh(void)
{
run_task_queue(&tq_immediate);
}
unsigned long timer_active = 0;
struct timer_struct timer_table[32];
unsigned long avenrun[3] = { 0,0,0 };
static unsigned long count_active_tasks(void)
{
struct task_struct **p;
unsigned long nr = 0;
for(p = &LAST_TASK; p > &FIRST_TASK; --p)
if (*p && ((*p)->state == TASK_RUNNING ||
(*p)->state == TASK_UNINTERRUPTIBLE ||
(*p)->state == TASK_SWAPPING))
nr += FIXED_1;
#ifdef __SMP__
nr-=(smp_num_cpus-1)*FIXED_1;
#endif
return nr;
}
static inline void calc_load(unsigned long ticks)
{
unsigned long active_tasks;
static int count = LOAD_FREQ;
count -= ticks;
if (count < 0) {
count += LOAD_FREQ;
active_tasks = count_active_tasks();
CALC_LOAD(avenrun[0], EXP_1, active_tasks);
CALC_LOAD(avenrun[1], EXP_5, active_tasks);
CALC_LOAD(avenrun[2], EXP_15, active_tasks);
}
}
static void second_overflow(void)
{
long ltemp;
time_maxerror += time_tolerance >> SHIFT_USEC;
if ( time_maxerror > NTP_PHASE_LIMIT ) {
time_maxerror = NTP_PHASE_LIMIT;
time_state = TIME_ERROR;
time_status |= STA_UNSYNC;
}
switch (time_state) {
case TIME_OK:
if (time_status & STA_INS)
time_state = TIME_INS;
else if (time_status & STA_DEL)
time_state = TIME_DEL;
break;
case TIME_INS:
if (xtime.tv_sec % 86400 == 0) {
xtime.tv_sec--;
time_state = TIME_OOP;
printk(KERN_NOTICE "Clock: inserting leap second 23:59:60 UTC\n");
}
break;
case TIME_DEL:
if ((xtime.tv_sec + 1) % 86400 == 0) {
xtime.tv_sec++;
time_state = TIME_WAIT;
printk(KERN_NOTICE "Clock: deleting leap second 23:59:59 UTC\n");
}
break;
case TIME_OOP:
time_state = TIME_WAIT;
break;
case TIME_WAIT:
if (!(time_status & (STA_INS | STA_DEL)))
time_state = TIME_OK;
}
if (time_offset < 0) {
ltemp = -time_offset;
if (!(time_status & STA_FLL))
ltemp >>= SHIFT_KG + time_constant;
if (ltemp > (MAXPHASE / MINSEC) << SHIFT_UPDATE)
ltemp = (MAXPHASE / MINSEC) << SHIFT_UPDATE;
time_offset += ltemp;
time_adj = -ltemp << (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
} else {
ltemp = time_offset;
if (!(time_status & STA_FLL))
ltemp >>= SHIFT_KG + time_constant;
if (ltemp > (MAXPHASE / MINSEC) << SHIFT_UPDATE)
ltemp = (MAXPHASE / MINSEC) << SHIFT_UPDATE;
time_offset -= ltemp;
time_adj = ltemp << (SHIFT_SCALE - SHIFT_HZ - SHIFT_UPDATE);
}
pps_valid++;
if (pps_valid == PPS_VALID) {
pps_jitter = MAXTIME;
pps_stabil = MAXFREQ;
time_status &= ~(STA_PPSSIGNAL | STA_PPSJITTER |
STA_PPSWANDER | STA_PPSERROR);
}
ltemp = time_freq + pps_freq;
if (ltemp < 0)
time_adj -= -ltemp >> (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);
else
time_adj +=  ltemp >> (SHIFT_USEC + SHIFT_HZ - SHIFT_SCALE);
#if HZ == 100
if (time_adj < 0)
time_adj -= (-time_adj >> 2) + (-time_adj >> 5);
else
time_adj += (time_adj >> 2) + (time_adj >> 5);
#endif
}
static void update_wall_time_one_tick(void)
{
if ( (time_adjust_step = time_adjust) != 0 ) {
if (time_adjust > tickadj)
time_adjust_step = tickadj;
else if (time_adjust < -tickadj)
time_adjust_step = -tickadj;
time_adjust -= time_adjust_step;
}
xtime.tv_usec += tick + time_adjust_step;
time_phase += time_adj;
if (time_phase <= -FINEUSEC) {
long ltemp = -time_phase >> SHIFT_SCALE;
time_phase += ltemp << SHIFT_SCALE;
xtime.tv_usec -= ltemp;
}
else if (time_phase >= FINEUSEC) {
long ltemp = time_phase >> SHIFT_SCALE;
time_phase -= ltemp << SHIFT_SCALE;
xtime.tv_usec += ltemp;
}
}
static void update_wall_time(unsigned long ticks)
{
do {
ticks--;
update_wall_time_one_tick();
} while (ticks);
if (xtime.tv_usec >= 1000000) {
xtime.tv_usec -= 1000000;
xtime.tv_sec++;
second_overflow();
}
}
static inline void do_process_times(struct task_struct *p,
unsigned long user, unsigned long system)
{
long psecs;
p->utime += user;
p->stime += system;
psecs = (p->stime + p->utime) / HZ;
if (psecs > p->rlim[RLIMIT_CPU].rlim_cur) {
if (psecs * HZ == p->stime + p->utime)
send_sig(SIGXCPU, p, 1);
if (psecs > p->rlim[RLIMIT_CPU].rlim_max)
send_sig(SIGKILL, p, 1);
}
}
static inline void do_it_virt(struct task_struct * p, unsigned long ticks)
{
unsigned long it_virt = p->it_virt_value;
if (it_virt) {
if (it_virt <= ticks) {
it_virt = ticks + p->it_virt_incr;
send_sig(SIGVTALRM, p, 1);
}
p->it_virt_value = it_virt - ticks;
}
}
static inline void do_it_prof(struct task_struct * p, unsigned long ticks)
{
unsigned long it_prof = p->it_prof_value;
if (it_prof) {
if (it_prof <= ticks) {
it_prof = ticks + p->it_prof_incr;
send_sig(SIGPROF, p, 1);
}
p->it_prof_value = it_prof - ticks;
}
}
static __inline__ void update_one_process(struct task_struct *p,
unsigned long ticks, unsigned long user, unsigned long system)
{
do_process_times(p, user, system);
do_it_virt(p, user);
do_it_prof(p, ticks);
}
static void update_process_times(unsigned long ticks, unsigned long system)
{
#ifndef  __SMP__
struct task_struct * p = current;
unsigned long user = ticks - system;
if (p->pid) {
p->counter -= ticks;
if (p->counter < 0) {
p->counter = 0;
need_resched = 1;
}
if (p->priority < DEF_PRIORITY)
kstat.cpu_nice += user;
else
kstat.cpu_user += user;
kstat.cpu_system += system;
}
update_one_process(p, ticks, user, system);
#else
int cpu,j;
cpu = smp_processor_id();
for (j=0;j<smp_num_cpus;j++)
{
int i = cpu_logical_map[j];
struct task_struct *p;
#ifdef __SMP_PROF__
if (test_bit(i,&smp_idle_map))
smp_idle_count[i]++;
#endif
p = current_set[i];
if (p->pid) {
unsigned long utime = ticks;
unsigned long stime = 0;
if (cpu == i) {
utime = ticks-system;
stime = system;
} else if (smp_proc_in_lock[j]) {
utime = 0;
stime = ticks;
}
update_one_process(p, ticks, utime, stime);
if (p->priority < DEF_PRIORITY)
kstat.cpu_nice += utime;
else
kstat.cpu_user += utime;
kstat.cpu_system += stime;
p->counter -= ticks;
if (p->counter >= 0)
continue;
p->counter = 0;
} else {
if (!(0x7fffffff & smp_process_available))
continue;
}
if (i==cpu)
need_resched = 1;
else
smp_message_pass(i, MSG_RESCHEDULE, 0L, 0);
}
#endif
}
static unsigned long lost_ticks = 0;
static unsigned long lost_ticks_system = 0;
static inline void update_times(void)
{
unsigned long ticks;
ticks = xchg(&lost_ticks, 0);
if (ticks) {
unsigned long system;
system = xchg(&lost_ticks_system, 0);
calc_load(ticks);
update_wall_time(ticks);
update_process_times(ticks, system);
}
}
static void timer_bh(void)
{
update_times();
run_old_timers();
run_timer_list();
}
void do_timer(struct pt_regs * regs)
{
(*(unsigned long *)&jiffies)++;
lost_ticks++;
mark_bh(TIMER_BH);
if (!user_mode(regs)) {
lost_ticks_system++;
if (prof_buffer && current->pid) {
extern int _stext;
unsigned long ip = instruction_pointer(regs);
ip -= (unsigned long) &_stext;
ip >>= prof_shift;
if (ip < prof_len)
prof_buffer[ip]++;
}
}
if (tq_timer)
mark_bh(TQUEUE_BH);
}
#ifndef __alpha__
asmlinkage unsigned int sys_alarm(unsigned int seconds)
{
struct itimerval it_new, it_old;
unsigned int oldalarm;
it_new.it_interval.tv_sec = it_new.it_interval.tv_usec = 0;
it_new.it_value.tv_sec = seconds;
it_new.it_value.tv_usec = 0;
_setitimer(ITIMER_REAL, &it_new, &it_old);
oldalarm = it_old.it_value.tv_sec;
if (it_old.it_value.tv_usec)
oldalarm++;
return oldalarm;
}
asmlinkage int sys_getpid(void)
{
return current->pid;
}
asmlinkage int sys_getppid(void)
{
return current->p_opptr->pid;
}
asmlinkage int sys_getuid(void)
{
return current->uid;
}
asmlinkage int sys_geteuid(void)
{
return current->euid;
}
asmlinkage int sys_getgid(void)
{
return current->gid;
}
asmlinkage int sys_getegid(void)
{
return current->egid;
}
asmlinkage int sys_nice(int increment)
{
unsigned long newprio;
int increase = 0;
newprio = increment;
if (increment < 0) {
if (!suser())
return -EPERM;
newprio = -increment;
increase = 1;
}
if (newprio > 40)
newprio = 40;
newprio = (newprio * DEF_PRIORITY + 10) / 20;
increment = newprio;
if (increase)
increment = -increment;
newprio = current->priority - increment;
if ((signed) newprio < 1)
newprio = 1;
if (newprio > DEF_PRIORITY*2)
newprio = DEF_PRIORITY*2;
current->priority = newprio;
return 0;
}
#endif
static struct task_struct *find_process_by_pid(pid_t pid) {
struct task_struct *p, *q;
if (pid == 0)
p = current;
else {
p = 0;
for_each_task(q) {
if (q && q->pid == pid) {
p = q;
break;
}
}
}
return p;
}
static int setscheduler(pid_t pid, int policy,
struct sched_param *param)
{
int error;
struct sched_param lp;
struct task_struct *p;
if (!param || pid < 0)
return -EINVAL;
error = verify_area(VERIFY_READ, param, sizeof(struct sched_param));
if (error)
return error;
memcpy_fromfs(&lp, param, sizeof(struct sched_param));
p = find_process_by_pid(pid);
if (!p)
return -ESRCH;
if (policy < 0)
policy = p->policy;
else if (policy != SCHED_FIFO && policy != SCHED_RR &&
policy != SCHED_OTHER)
return -EINVAL;
if (lp.sched_priority < 0 || lp.sched_priority > 99)
return -EINVAL;
if ((policy == SCHED_OTHER) != (lp.sched_priority == 0))
return -EINVAL;
if ((policy == SCHED_FIFO || policy == SCHED_RR) && !suser())
return -EPERM;
if ((current->euid != p->euid) && (current->euid != p->uid) &&
!suser())
return -EPERM;
p->policy = policy;
p->rt_priority = lp.sched_priority;
cli();
if (p->next_run)
move_last_runqueue(p);
sti();
need_resched = 1;
return 0;
}
asmlinkage int sys_sched_setscheduler(pid_t pid, int policy,
struct sched_param *param)
{
return setscheduler(pid, policy, param);
}
asmlinkage int sys_sched_setparam(pid_t pid, struct sched_param *param)
{
return setscheduler(pid, -1, param);
}
asmlinkage int sys_sched_getscheduler(pid_t pid)
{
struct task_struct *p;
if (pid < 0)
return -EINVAL;
p = find_process_by_pid(pid);
if (!p)
return -ESRCH;
return p->policy;
}
asmlinkage int sys_sched_getparam(pid_t pid, struct sched_param *param)
{
int error;
struct task_struct *p;
struct sched_param lp;
if (!param || pid < 0)
return -EINVAL;
error = verify_area(VERIFY_WRITE, param, sizeof(struct sched_param));
if (error)
return error;
p = find_process_by_pid(pid);
if (!p)
return -ESRCH;
lp.sched_priority = p->rt_priority;
memcpy_tofs(param, &lp, sizeof(struct sched_param));
return 0;
}
asmlinkage int sys_sched_yield(void)
{
cli();
move_last_runqueue(current);
current->counter = 0;
need_resched = 1;
sti();
return 0;
}
asmlinkage int sys_sched_get_priority_max(int policy)
{
switch (policy) {
case SCHED_FIFO:
case SCHED_RR:
return 99;
case SCHED_OTHER:
return 0;
}
return -EINVAL;
}
asmlinkage int sys_sched_get_priority_min(int policy)
{
switch (policy) {
case SCHED_FIFO:
case SCHED_RR:
return 1;
case SCHED_OTHER:
return 0;
}
return -EINVAL;
}
asmlinkage int sys_sched_rr_get_interval(pid_t pid, struct timespec *interval)
{
int error;
struct timespec t;
error = verify_area(VERIFY_WRITE, interval, sizeof(struct timespec));
if (error)
return error;
t.tv_sec = 0;
t.tv_nsec = 150000;
memcpy_tofs(interval, &t, sizeof(struct timespec));
return 0;
}
static unsigned long timespectojiffies(struct timespec *value)
{
unsigned long sec = (unsigned) value->tv_sec;
long nsec = value->tv_nsec;
if (sec > (LONG_MAX / HZ))
return LONG_MAX;
nsec += 1000000000L / HZ - 1;
nsec /= 1000000000L / HZ;
return HZ * sec + nsec;
}
static void jiffiestotimespec(unsigned long jiffies, struct timespec *value)
{
value->tv_nsec = (jiffies % HZ) * (1000000000L / HZ);
value->tv_sec = jiffies / HZ;
return;
}
asmlinkage int sys_nanosleep(struct timespec *rqtp, struct timespec *rmtp)
{
int error;
struct timespec t;
unsigned long expire;
error = verify_area(VERIFY_READ, rqtp, sizeof(struct timespec));
if (error)
return error;
memcpy_fromfs(&t, rqtp, sizeof(struct timespec));
if (rmtp) {
error = verify_area(VERIFY_WRITE, rmtp,
sizeof(struct timespec));
if (error)
return error;
}
if (t.tv_nsec >= 1000000000L || t.tv_nsec < 0 || t.tv_sec < 0)
return -EINVAL;
if (t.tv_sec == 0 && t.tv_nsec <= 2000000L &&
current->policy != SCHED_OTHER) {
udelay((t.tv_nsec + 999) / 1000);
return 0;
}
expire = timespectojiffies(&t) + (t.tv_sec || t.tv_nsec) + jiffies;
current->timeout = expire;
current->state = TASK_INTERRUPTIBLE;
schedule();
if (expire > jiffies) {
if (rmtp) {
jiffiestotimespec(expire - jiffies -
(expire > jiffies + 1), &t);
memcpy_tofs(rmtp, &t, sizeof(struct timespec));
}
return -EINTR;
}
return 0;
}
static void show_task(int nr,struct task_struct * p)
{
unsigned long free;
static const char * stat_nam[] = { "R", "S", "D", "Z", "T", "W" };
printk("%-8s %3d ", p->comm, (p == current) ? -nr : nr);
if (((unsigned) p->state) < sizeof(stat_nam)/sizeof(char *))
printk(stat_nam[p->state]);
else
printk(" ");
#if ((~0UL) == 0xffffffff)
if (p == current)
printk(" current  ");
else
printk(" %08lX ", thread_saved_pc(&p->tss));
printk("%08lX ", get_wchan(p));
#else
if (p == current)
printk("   current task   ");
else
printk(" %016lx ", thread_saved_pc(&p->tss));
printk("%08lX ", get_wchan(p) & 0xffffffffL);
#endif
for (free = 1; free < PAGE_SIZE/sizeof(long) ; free++) {
if (((unsigned long *)p->kernel_stack_page)[free])
break;
}
printk("%5lu %5d %6d ", free*sizeof(long), p->pid, p->p_pptr->pid);
if (p->p_cptr)
printk("%5d ", p->p_cptr->pid);
else
printk("      ");
if (p->p_ysptr)
printk("%7d", p->p_ysptr->pid);
else
printk("       ");
if (p->p_osptr)
printk(" %5d\n", p->p_osptr->pid);
else
printk("\n");
}
void show_state(void)
{
int i;
#if ((~0UL) == 0xffffffff)
printk("\n"
"                                  free                        sibling\n");
printk("  task             PC     wchan   stack   pid father child younger older\n");
#else
printk("\n"
"                                           free                        sibling\n");
printk("  task                 PC         wchan    stack   pid father child younger older\n");
#endif
for (i=0 ; i<NR_TASKS ; i++)
if (task[i])
show_task(i,task[i]);
}
void sched_init(void)
{
int cpu=smp_processor_id();
#ifndef __SMP__
current_set[cpu]=&init_task;
#else
init_task.processor=cpu;
for(cpu = 0; cpu < NR_CPUS; cpu++)
current_set[cpu] = &init_task;
#endif
init_bh(TIMER_BH, timer_bh);
init_bh(TQUEUE_BH, tqueue_bh);
init_bh(IMMEDIATE_BH, immediate_bh);
}