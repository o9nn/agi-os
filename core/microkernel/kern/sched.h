#ifndef _KERN_SCHED_H_
#define _KERN_SCHED_H_
#include <kern/queue.h>
#include <kern/lock.h>
#include <kern/kern_types.h>
#include <kern/macros.h>
#if MACH_FIXPRI
#include <mach/policy.h>
#endif
#if STAT_TIME
#define PRI_SHIFT 17
#else
#include <machine/sched_param.h>
#endif
#define NRQS 64
struct run_queue {
queue_head_t runq[NRQS];
decl_simple_lock_data(, lock)
int low;
int count;
};
typedef struct run_queue *run_queue_t;
#define RUN_QUEUE_NULL ((run_queue_t) 0)
#ifdef MACH_LDEBUG
#define runq_lock(rq) \
MACRO_BEGIN \
assert_splsched(); \
simple_lock_nocheck(&(rq)->lock); \
MACRO_END
#define runq_unlock(rq) \
MACRO_BEGIN \
assert_splsched(); \
simple_unlock_nocheck(&(rq)->lock); \
MACRO_END
#else
#define runq_lock(rq) simple_lock_nocheck(&(rq)->lock)
#define runq_unlock(rq) simple_unlock_nocheck(&(rq)->lock)
#endif
#if MACH_FIXPRI
#define csw_needed(thread, processor) ((thread)->state & TH_SUSP || \
((processor)->runq.count > 0) || \
((thread)->policy == POLICY_TIMESHARE && \
(processor)->first_quantum == FALSE && \
(processor)->processor_set->runq.count > 0 && \
(processor)->processor_set->runq.low <= \
(thread)->sched_pri) || \
((thread)->policy == POLICY_FIXEDPRI && \
(processor)->processor_set->runq.count > 0 && \
((((processor)->first_quantum == FALSE) && \
((processor)->processor_set->runq.low <= \
(thread)->sched_pri)) || \
((processor)->processor_set->runq.low < \
(thread)->sched_pri))))
#else
#define csw_needed(thread, processor) ((thread)->state & TH_SUSP || \
((processor)->runq.count > 0) || \
((processor)->first_quantum == FALSE && \
((processor)->processor_set->runq.count > 0 && \
(processor)->processor_set->runq.low <= \
((thread)->sched_pri))))
#endif
extern struct run_queue *rem_runq(thread_t);
extern struct thread *choose_thread(processor_t);
extern queue_head_t action_queue;
decl_simple_lock_data(extern,action_lock);
extern int min_quantum;
#define MIN_QUANTUM (hz / 33)
#define BASEPRI_SYSTEM 6
#define BASEPRI_USER 25
#define invalid_pri(pri) (((pri) < 0) || ((pri) >= NRQS))
struct shift {
int shift1;
int shift2;
};
typedef struct shift *shift_t, shift_data_t;
extern unsigned sched_tick;
#define SCHED_SCALE 128
#define SCHED_SHIFT 7
#define thread_timer_delta(thread) \
MACRO_BEGIN \
unsigned delta; \
\
delta = 0; \
TIMER_DELTA((thread)->system_timer, \
(thread)->system_timer_save, delta); \
TIMER_DELTA((thread)->user_timer, \
(thread)->user_timer_save, delta); \
(thread)->cpu_delta += delta; \
(thread)->sched_delta += delta * \
(thread)->processor_set->sched_load; \
MACRO_END
#endif