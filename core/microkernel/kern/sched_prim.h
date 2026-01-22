#ifndef _KERN_SCHED_PRIM_H_
#define _KERN_SCHED_PRIM_H_
#include <mach/boolean.h>
#include <mach/message.h>
#include <kern/lock.h>
#include <kern/kern_types.h>
#define THREAD_AWAKENED 0
#define THREAD_TIMED_OUT 1
#define THREAD_INTERRUPTED 2
#define THREAD_RESTART 3
typedef void *event_t;
typedef void (*continuation_t)(void);
#define thread_no_continuation ((continuation_t) 0)
extern void sched_init(void);
extern void assert_wait(
event_t event,
boolean_t interruptible);
extern void clear_wait(
thread_t thread,
int result,
boolean_t interrupt_only);
extern void thread_sleep(
event_t event,
simple_lock_t lock,
boolean_t interruptible);
extern void thread_wakeup(void);
extern boolean_t thread_wakeup_prim(
event_t event,
boolean_t one_thread,
int result);
extern boolean_t thread_invoke(
thread_t old_thread,
continuation_t continuation,
thread_t new_thread);
extern void thread_block(
continuation_t continuation);
extern void thread_run(
continuation_t continuation,
thread_t new_thread);
extern void thread_set_timeout(
int t);
extern void thread_setrun(
thread_t thread,
boolean_t may_preempt);
extern void thread_dispatch(
thread_t thread);
extern void thread_continue(
thread_t old_thread);
extern void thread_go(
thread_t thread);
extern void thread_will_wait(
thread_t thread);
extern void thread_will_wait_with_timeout(
thread_t thread,
mach_msg_timeout_t msecs);
extern boolean_t thread_handoff(
thread_t old_thread,
continuation_t continuation,
thread_t new_thread);
extern void recompute_priorities(void *param);
extern void update_priority(
thread_t thread);
extern void compute_my_priority(
thread_t thread);
extern void thread_bind(
thread_t thread,
processor_t processor);
extern void compute_priority(
thread_t thread,
boolean_t resched);
extern void thread_timeout_setup(
thread_t thread);
#if NCPUS > 1
extern kern_return_t thread_migrate(
thread_t thread,
processor_t target_processor);
extern void thread_balance_load(void);
extern processor_t thread_select_best_processor(
thread_t thread);
extern void thread_update_cache_warmth(
thread_t thread);
#endif
#define thread_wakeup(x) \
thread_wakeup_prim((x), FALSE, THREAD_AWAKENED)
#define thread_wakeup_with_result(x, z) \
thread_wakeup_prim((x), FALSE, (z))
#define thread_wakeup_one(x) \
thread_wakeup_prim((x), TRUE, THREAD_AWAKENED)
extern void thread_bootstrap_return(void) __attribute__((noreturn));
extern void thread_exception_return(void) __attribute__((noreturn));
extern void __attribute__((__noreturn__)) thread_syscall_return(kern_return_t);
extern thread_t switch_context(
thread_t old_thread,
continuation_t continuation,
thread_t new_thread);
extern void stack_handoff(
thread_t old_thread,
thread_t new_thread);
extern kern_return_t stack_alloc(
thread_t thread,
void (*resume)(thread_t));
extern boolean_t stack_alloc_try(
thread_t thread,
void (*resume)(thread_t));
extern void stack_free(
thread_t thread);
#define convert_ipc_timeout_to_ticks(millis) \
(((millis) * hz + 999) / 1000)
void set_pri(thread_t th, int pri, boolean_t resched);
void do_thread_scan(void);
thread_t choose_pset_thread(processor_t myprocessor, processor_set_t pset);
#if DEBUG
#include <kern/sched.h>
void checkrq(run_queue_t rq, const char *msg);
void thread_check(thread_t th, run_queue_t rq);
#endif
extern void idle_thread(void) __attribute__((noreturn));
extern void sched_thread(void);
extern int stuck_count;
#endif