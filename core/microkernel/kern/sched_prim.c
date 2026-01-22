#include <kern/printf.h>
#include <kern/constants.h>
#include <mach/machine.h>
#include <machine/locore.h>
#include <machine/spl.h>
#include <machine/model_dep.h>
#include <kern/ast.h>
#include <kern/counters.h>
#include <kern/cpu_number.h>
#include <kern/debug.h>
#include <kern/lock.h>
#include <kern/mach_clock.h>
#include <kern/mach_factor.h>
#include <kern/macros.h>
#include <kern/processor.h>
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/smp.h>
#include <kern/syscall_subr.h>
#include <kern/thread.h>
#include <kern/thread_swap.h>
#include <kern/dtrace.h>
#include <vm/pmap.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#if MACH_FIXPRI
#include <mach/policy.h>
#endif
int min_quantum;
unsigned sched_tick;
thread_t sched_thread_id;
timer_elt_data_t recompute_priorities_timer;
#define NUMQUEUES SCHED_WAIT_HASH_SIZE
decl_simple_lock_data(static, wait_lock[NUMQUEUES])
queue_head_t wait_queue[NUMQUEUES];
#ifdef MACH_LDEBUG
#define waitq_lock(wl) \
MACRO_BEGIN \
assert_splsched(); \
simple_lock_nocheck(wl); \
MACRO_END
#define waitq_unlock(wl) \
MACRO_BEGIN \
assert_splsched(); \
simple_unlock_nocheck(wl); \
MACRO_END
#else
#define waitq_lock(wl) simple_lock_nocheck(wl)
#define waitq_unlock(wl) simple_unlock_nocheck(wl)
#endif
#define wait_hash(event) \
((((long)(event) < 0) ? ~(long)(event) : (long)(event)) % NUMQUEUES)
static void wait_queue_init(void)
{
int i;
for (i = 0; i < NUMQUEUES; i++) {
queue_init(&wait_queue[i]);
simple_lock_init(&wait_lock[i]);
}
}
void sched_init(void)
{
recompute_priorities_timer.fcn = recompute_priorities;
recompute_priorities_timer.param = NULL;
min_quantum = MIN_QUANTUM;
wait_queue_init();
pset_sys_bootstrap();
queue_init(&action_queue);
simple_lock_init(&action_lock);
sched_tick = 0;
ast_init();
}
static void thread_timeout(
void *_thread)
{
thread_t thread = _thread;
assert(thread->timer.set == TELT_UNSET);
clear_wait(thread, THREAD_TIMED_OUT, FALSE);
}
void thread_set_timeout(
int t)
{
thread_t thread = current_thread();
spl_t s;
s = splsched();
thread_lock(thread);
if ((thread->state & TH_WAIT) != 0) {
set_timeout(&thread->timer, t);
}
thread_unlock(thread);
splx(s);
}
void thread_timeout_setup(
thread_t thread)
{
thread->timer.fcn = thread_timeout;
thread->timer.param = thread;
thread->depress_timer.fcn = (void (*)(void*))thread_depress_timeout;
thread->depress_timer.param = thread;
}
void assert_wait(
event_t event,
boolean_t interruptible)
{
queue_t q;
int index;
thread_t thread;
decl_simple_lock_data( , *lock);
spl_t s;
thread = current_thread();
if (thread->wait_event != 0) {
panic("assert_wait: already asserted event %p\n",
thread->wait_event);
}
s = splsched();
if (event != 0) {
index = wait_hash(event);
q = &wait_queue[index];
lock = &wait_lock[index];
waitq_lock(lock);
thread_lock(thread);
enqueue_tail(q, &(thread->links));
thread->wait_event = event;
if (interruptible)
thread->state |= TH_WAIT;
else
thread->state |= TH_WAIT | TH_UNINT;
thread_unlock(thread);
waitq_unlock(lock);
}
else {
thread_lock(thread);
if (interruptible)
thread->state |= TH_WAIT;
else
thread->state |= TH_WAIT | TH_UNINT;
thread_unlock(thread);
}
splx(s);
}
void clear_wait(
thread_t thread,
int result,
boolean_t interrupt_only)
{
int index;
queue_t q;
decl_simple_lock_data( , *lock);
event_t event;
spl_t s;
s = splsched();
thread_lock(thread);
if (interrupt_only && (thread->state & TH_UNINT)) {
thread_unlock(thread);
splx(s);
return;
}
event = thread->wait_event;
if (event != 0) {
thread_unlock(thread);
index = wait_hash(event);
q = &wait_queue[index];
lock = &wait_lock[index];
waitq_lock(lock);
thread_lock(thread);
if (thread->wait_event == event) {
remqueue(q, (queue_entry_t)thread);
thread->wait_event = 0;
event = 0;
}
waitq_unlock(lock);
}
if (event == 0) {
int state = thread->state;
reset_timeout_check(&thread->timer);
switch (state & TH_SCHED_STATE) {
case TH_WAIT | TH_SUSP | TH_UNINT:
case TH_WAIT | TH_UNINT:
case TH_WAIT:
thread->state = (state &~ TH_WAIT) | TH_RUN;
thread->wait_result = result;
thread_setrun(thread, TRUE);
break;
case TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT:
case TH_RUN | TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT | TH_UNINT:
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
thread->state = state &~ TH_WAIT;
thread->wait_result = result;
break;
default:
break;
}
}
thread_unlock(thread);
splx(s);
}
#define state_panic(thread) \
panic ("thread %p has unexpected state %x (%s%s%s%s%s%s%s%s)", \
thread, thread->state, \
thread->state & TH_WAIT ? "TH_WAIT|" : "", \
thread->state & TH_SUSP ? "TH_SUSP|" : "", \
thread->state & TH_RUN ? "TH_RUN|" : "", \
thread->state & TH_UNINT ? "TH_UNINT|" : "", \
thread->state & TH_HALTED ? "TH_HALTED|" : "", \
thread->state & TH_IDLE ? "TH_IDLE|" : "", \
thread->state & TH_SWAPPED ? "TH_SWAPPED|" : "", \
thread->state & TH_SW_COMING_IN ? "TH_SW_COMING_IN|" : "")
boolean_t thread_wakeup_prim(
event_t event,
boolean_t one_thread,
int result)
{
queue_t q;
int index;
boolean_t woke = FALSE;
thread_t thread, next_th;
decl_simple_lock_data( , *lock);
spl_t s;
int state;
index = wait_hash(event);
q = &wait_queue[index];
s = splsched();
lock = &wait_lock[index];
waitq_lock(lock);
thread = (thread_t) queue_first(q);
while (!queue_end(q, (queue_entry_t)thread)) {
next_th = (thread_t) queue_next((queue_t) thread);
if (thread->wait_event == event) {
thread_lock(thread);
remqueue(q, (queue_entry_t) thread);
thread->wait_event = 0;
reset_timeout_check(&thread->timer);
state = thread->state;
switch (state & TH_SCHED_STATE) {
case TH_WAIT | TH_SUSP | TH_UNINT:
case TH_WAIT | TH_UNINT:
case TH_WAIT:
thread->state = (state &~ TH_WAIT) | TH_RUN;
thread->wait_result = result;
thread_setrun(thread, TRUE);
break;
case TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT:
case TH_RUN | TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT | TH_UNINT:
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
thread->state = state &~ TH_WAIT;
thread->wait_result = result;
break;
default:
state_panic(thread);
break;
}
thread_unlock(thread);
woke = TRUE;
if (one_thread)
break;
}
thread = next_th;
}
waitq_unlock(lock);
splx(s);
return (woke);
}
void thread_sleep(
event_t event,
simple_lock_t lock,
boolean_t interruptible)
{
assert_wait(event, interruptible);
simple_unlock(lock);
thread_block(thread_no_continuation);
}
void thread_bind(
thread_t thread,
processor_t processor)
{
spl_t s;
s = splsched();
thread_lock(thread);
thread->bound_processor = processor;
thread_unlock(thread);
(void) splx(s);
}
static thread_t thread_select(
processor_t myprocessor)
{
thread_t thread;
myprocessor->first_quantum = TRUE;
if (myprocessor->runq.count > 0) {
thread = choose_thread(myprocessor);
myprocessor->quantum = min_quantum;
}
else {
processor_set_t pset;
#if MACH_HOST
pset = myprocessor->processor_set;
#else
pset = &default_pset;
#endif
simple_lock(&pset->runq.lock);
#if DEBUG
checkrq(&pset->runq, "thread_select");
#endif
if (pset->runq.count == 0) {
thread = current_thread();
if ((thread->state == TH_RUN) &&
#if MACH_HOST
(thread->processor_set == pset) &&
#endif
((thread->bound_processor == PROCESSOR_NULL) ||
(thread->bound_processor == myprocessor))) {
simple_unlock(&pset->runq.lock);
thread_lock(thread);
if (thread->sched_stamp != sched_tick)
update_priority(thread);
thread_unlock(thread);
}
else {
thread = choose_pset_thread(myprocessor, pset);
}
}
else {
queue_t q;
q = pset->runq.runq + pset->runq.low;
if (queue_empty(q)) {
pset->runq.low++;
thread = choose_pset_thread(myprocessor, pset);
}
else {
thread = (thread_t) dequeue_head(q);
thread->runq = RUN_QUEUE_NULL;
pset->runq.count--;
#if MACH_FIXPRI
if ((pset->runq.count > 0) &&
(pset->policies & POLICY_FIXEDPRI)) {
while (queue_empty(q)) {
pset->runq.low++;
q++;
}
}
#endif
#if DEBUG
checkrq(&pset->runq, "thread_select: after");
#endif
simple_unlock(&pset->runq.lock);
}
}
#if MACH_FIXPRI
if (thread->policy == POLICY_TIMESHARE) {
#endif
myprocessor->quantum = pset->set_quantum;
#if MACH_FIXPRI
}
else {
myprocessor->quantum = thread->sched_data;
}
#endif
}
return thread;
}
boolean_t thread_invoke(
thread_t old_thread,
continuation_t continuation,
thread_t new_thread)
{
DTRACE_THREAD_SWITCH(old_thread, new_thread);
if (old_thread == new_thread) {
thread_lock(new_thread);
new_thread->state &= ~TH_UNINT;
thread_unlock(new_thread);
thread_wakeup(TH_EV_STATE(new_thread));
if (continuation != thread_no_continuation) {
(void) spl0();
call_continuation(continuation);
}
return TRUE;
}
thread_lock(new_thread);
if ((old_thread->stack_privilege != current_stack()) &&
(continuation != thread_no_continuation))
{
switch (new_thread->state & TH_SWAP_STATE) {
case TH_SWAPPED:
new_thread->state &= ~(TH_SWAPPED | TH_UNINT);
thread_unlock(new_thread);
thread_wakeup(TH_EV_STATE(new_thread));
#if NCPUS > 1
new_thread->last_processor = current_processor();
#endif
ast_context(new_thread, cpu_number());
timer_switch(&new_thread->system_timer);
stack_handoff(old_thread, new_thread);
thread_lock(old_thread);
old_thread->swap_func = continuation;
switch (old_thread->state) {
case TH_RUN | TH_SUSP:
case TH_RUN | TH_SUSP | TH_HALTED:
case TH_RUN | TH_WAIT | TH_SUSP:
old_thread->state = (old_thread->state & ~TH_RUN)
| TH_SWAPPED;
if (old_thread->wake_active) {
old_thread->wake_active = FALSE;
thread_unlock(old_thread);
thread_wakeup(TH_EV_WAKE_ACTIVE(old_thread));
goto after_old_thread;
}
break;
case TH_RUN | TH_SUSP | TH_UNINT:
case TH_RUN | TH_UNINT:
case TH_RUN:
old_thread->state |= TH_SWAPPED;
thread_setrun(old_thread, FALSE);
break;
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
case TH_RUN | TH_WAIT | TH_UNINT:
case TH_RUN | TH_WAIT:
old_thread->state = (old_thread->state & ~TH_RUN)
| TH_SWAPPED;
break;
case TH_RUN | TH_IDLE:
old_thread->state = TH_RUN | TH_IDLE | TH_SWAPPED;
break;
default:
state_panic(old_thread);
}
thread_unlock(old_thread);
after_old_thread:
counter(c_thread_invoke_hits++);
(void) spl0();
call_continuation(new_thread->swap_func);
return TRUE;
case TH_SW_COMING_IN:
thread_swapin(new_thread);
thread_unlock(new_thread);
counter(c_thread_invoke_misses++);
return FALSE;
case 0:
break;
}
}
else {
if (new_thread->state & TH_SWAPPED) {
if ((new_thread->state & TH_SW_COMING_IN) ||
!stack_alloc_try(new_thread, thread_continue))
{
thread_swapin(new_thread);
thread_unlock(new_thread);
counter(c_thread_invoke_misses++);
return FALSE;
}
}
}
new_thread->state &= ~(TH_SWAPPED | TH_UNINT);
thread_unlock(new_thread);
thread_wakeup(TH_EV_STATE(new_thread));
#if NCPUS > 1
new_thread->last_processor = current_processor();
#endif
ast_context(new_thread, cpu_number());
timer_switch(&new_thread->system_timer);
counter(c_thread_invoke_csw++);
old_thread = switch_context(old_thread, continuation, new_thread);
thread_dispatch(old_thread);
return TRUE;
}
void thread_continue(
thread_t old_thread)
{
continuation_t continuation = current_thread()->swap_func;
if (old_thread != THREAD_NULL)
thread_dispatch(old_thread);
(void) spl0();
(*continuation)();
}
void thread_block(
continuation_t continuation)
{
thread_t thread = current_thread();
processor_t myprocessor = cpu_to_processor(cpu_number());
thread_t new_thread;
spl_t s;
check_simple_locks();
s = splsched();
#if FAST_TAS
{
extern void recover_ras();
if (csw_needed(thread, myprocessor))
recover_ras(thread);
}
#endif
ast_off(cpu_number(), AST_BLOCK);
do
new_thread = thread_select(myprocessor);
while (!thread_invoke(thread, continuation, new_thread));
splx(s);
}
void thread_run(
continuation_t continuation,
thread_t new_thread)
{
thread_t thread = current_thread();
processor_t myprocessor = cpu_to_processor(cpu_number());
spl_t s;
check_simple_locks();
s = splsched();
while (!thread_invoke(thread, continuation, new_thread))
new_thread = thread_select(myprocessor);
splx(s);
}
void thread_dispatch(
thread_t thread)
{
thread_lock(thread);
if (thread->swap_func != thread_no_continuation) {
assert((thread->state & TH_SWAP_STATE) == 0);
thread->state |= TH_SWAPPED;
stack_free(thread);
}
switch (thread->state &~ TH_SWAP_STATE) {
case TH_RUN | TH_SUSP:
case TH_RUN | TH_SUSP | TH_HALTED:
case TH_RUN | TH_WAIT | TH_SUSP:
thread->state &= ~TH_RUN;
if (thread->wake_active) {
thread->wake_active = FALSE;
thread_unlock(thread);
thread_wakeup(TH_EV_WAKE_ACTIVE(thread));
return;
}
break;
case TH_RUN | TH_SUSP | TH_UNINT:
case TH_RUN | TH_UNINT:
case TH_RUN:
thread_setrun(thread, FALSE);
break;
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
case TH_RUN | TH_WAIT | TH_UNINT:
case TH_RUN | TH_WAIT:
thread->state &= ~TH_RUN;
break;
case TH_RUN | TH_IDLE:
break;
default:
state_panic(thread);
}
thread_unlock(thread);
}
shift_data_t wait_shift[32] = {
{1,1},{1,3},{1,-3},{2,-7},{3,5},{3,-5},{4,-8},{5,7},
{5,-7},{6,-10},{7,10},{7,-9},{8,-11},{9,12},{9,-11},{10,-13},
{11,14},{11,-13},{12,-15},{13,17},{13,-15},{14,-17},{15,19},{16,18},
{16,-19},{17,22},{18,20},{18,-20},{19,26},{20,22},{20,-22},{21,-27}};
#ifdef PRI_SHIFT_2
#if PRI_SHIFT_2 > 0
#define do_priority_computation(th, pri) \
MACRO_BEGIN \
(pri) = (th)->priority \
+ ((th)->sched_usage >> (PRI_SHIFT + SCHED_SHIFT)) \
+ ((th)->sched_usage >> (PRI_SHIFT_2 + SCHED_SHIFT)); \
if ((pri) > NRQS - 1) (pri) = NRQS - 1; \
MACRO_END
#else
#define do_priority_computation(th, pri) \
MACRO_BEGIN \
(pri) = (th)->priority \
+ ((th)->sched_usage >> (PRI_SHIFT + SCHED_SHIFT)) \
- ((th)->sched_usage >> (SCHED_SHIFT - PRI_SHIFT_2)); \
if ((pri) > NRQS - 1) (pri) = NRQS - 1; \
MACRO_END
#endif
#else
#define do_priority_computation(th, pri) \
MACRO_BEGIN \
(pri) = (th)->priority \
+ ((th)->sched_usage >> (PRI_SHIFT + SCHED_SHIFT)); \
if ((pri) > NRQS - 1) (pri) = NRQS - 1; \
MACRO_END
#endif
void compute_priority(
thread_t thread,
boolean_t resched)
{
int pri;
#if MACH_FIXPRI
if (thread->policy == POLICY_TIMESHARE) {
#endif
do_priority_computation(thread, pri);
if (thread->depress_priority < 0)
set_pri(thread, pri, resched);
else
thread->depress_priority = pri;
#if MACH_FIXPRI
}
else {
set_pri(thread, thread->priority, resched);
}
#endif
}
void compute_my_priority(
thread_t thread)
{
int temp_pri;
do_priority_computation(thread,temp_pri);
thread->sched_pri = temp_pri;
}
void recompute_priorities(void *param)
{
sched_tick++;
set_timeout(&recompute_priorities_timer, hz);
#if NCPUS > 1
if ((sched_tick % 4) == 0) {
thread_balance_load();
}
#endif
if (sched_thread_id != THREAD_NULL) {
clear_wait(sched_thread_id, THREAD_AWAKENED, FALSE);
}
}
void update_priority(
thread_t thread)
{
unsigned int ticks;
shift_t shiftp;
int temp_pri;
ticks = sched_tick - thread->sched_stamp;
assert(ticks != 0);
thread->sched_stamp += ticks;
thread_timer_delta(thread);
if (ticks > SCHED_CPU_USAGE_RESET_TICKS) {
thread->cpu_usage = 0;
thread->sched_usage = 0;
}
else {
thread->cpu_usage += thread->cpu_delta;
thread->sched_usage += thread->sched_delta;
shiftp = &wait_shift[ticks];
if (shiftp->shift2 > 0) {
thread->cpu_usage =
(thread->cpu_usage >> shiftp->shift1) +
(thread->cpu_usage >> shiftp->shift2);
thread->sched_usage =
(thread->sched_usage >> shiftp->shift1) +
(thread->sched_usage >> shiftp->shift2);
}
else {
thread->cpu_usage =
(thread->cpu_usage >> shiftp->shift1) -
(thread->cpu_usage >> -(shiftp->shift2));
thread->sched_usage =
(thread->sched_usage >> shiftp->shift1) -
(thread->sched_usage >> -(shiftp->shift2));
}
}
thread->cpu_delta = 0;
thread->sched_delta = 0;
if (
#if MACH_FIXPRI
(thread->policy == POLICY_TIMESHARE) &&
#endif
(thread->depress_priority < 0)) {
do_priority_computation(thread, temp_pri);
thread->sched_pri = temp_pri;
}
}
#if DEBUG
#define run_queue_enqueue(rq, th) \
MACRO_BEGIN \
unsigned int whichq; \
\
whichq = (th)->sched_pri; \
if (whichq >= NRQS) { \
printf("thread_setrun: pri too high (%d)\n", (th)->sched_pri); \
whichq = NRQS - 1; \
} \
\
runq_lock(rq); \
checkrq((rq), "thread_setrun: before adding thread"); \
enqueue_tail(&(rq)->runq[whichq], &((th)->links)); \
\
if (whichq < (rq)->low || (rq)->count == 0) \
(rq)->low = whichq; \
\
(rq)->count++; \
(th)->runq = (rq); \
thread_check((th), (rq)); \
checkrq((rq), "thread_setrun: after adding thread"); \
runq_unlock(rq); \
MACRO_END
#else
#define run_queue_enqueue(rq, th) \
MACRO_BEGIN \
unsigned int whichq; \
\
whichq = (th)->sched_pri; \
if (whichq >= NRQS) { \
printf("thread_setrun: pri too high (%d)\n", (th)->sched_pri); \
whichq = NRQS - 1; \
} \
\
runq_lock(rq); \
enqueue_tail(&(rq)->runq[whichq], &((th)->links)); \
\
if (whichq < (rq)->low || (rq)->count == 0) \
(rq)->low = whichq; \
\
(rq)->count++; \
(th)->runq = (rq); \
runq_unlock(rq); \
MACRO_END
#endif
void thread_setrun(
thread_t th,
boolean_t may_preempt)
{
processor_t processor;
run_queue_t rq;
#if NCPUS > 1
processor_set_t pset;
#endif
if (th->sched_stamp != sched_tick) {
update_priority(th);
}
assert(th->runq == RUN_QUEUE_NULL);
#if NCPUS > 1
if ((processor = th->bound_processor) == PROCESSOR_NULL) {
pset = th->processor_set;
processor = thread_select_best_processor(th);
#if HW_FOOTPRINT
if (processor != PROCESSOR_NULL && processor->state == PROCESSOR_IDLE) {
processor_lock(processor);
pset_idle_lock();
if ((processor->state == PROCESSOR_IDLE)
#if MACH_HOST
&& (processor->processor_set == pset)
#endif
) {
queue_remove(&pset->idle_queue, processor,
processor_t, processor_queue);
pset->idle_count--;
processor->next_thread = th;
processor->state = PROCESSOR_DISPATCHING;
if (processor == th->last_processor) {
thread_update_cache_warmth(th);
} else {
th->cache_warmth = 0;
th->last_processor = processor;
}
pset_idle_unlock();
processor_unlock(processor);
if (processor != current_processor())
cause_ast_check(processor);
return;
}
pset_idle_unlock();
processor_unlock(processor);
}
#endif
if (pset->idle_count > 0) {
pset_idle_lock();
if (pset->idle_count > 0) {
processor = (processor_t) queue_first(&pset->idle_queue);
queue_remove(&(pset->idle_queue), processor, processor_t,
processor_queue);
pset->idle_count--;
processor->next_thread = th;
processor->state = PROCESSOR_DISPATCHING;
pset_idle_unlock();
if (processor != current_processor())
cause_ast_check(processor);
return;
}
pset_idle_unlock();
}
rq = &(pset->runq);
run_queue_enqueue(rq,th);
if (may_preempt &&
#if MACH_HOST
(pset == current_processor()->processor_set) &&
#endif
(current_thread()->sched_pri > th->sched_pri)) {
current_processor()->first_quantum = FALSE;
ast_on(cpu_number(), AST_BLOCK);
}
}
else {
if (processor->state == PROCESSOR_IDLE) {
processor_lock(processor);
pset = processor->processor_set;
pset_idle_lock();
if (processor->state == PROCESSOR_IDLE) {
queue_remove(&pset->idle_queue, processor,
processor_t, processor_queue);
pset->idle_count--;
processor->next_thread = th;
processor->state = PROCESSOR_DISPATCHING;
pset_idle_unlock();
processor_unlock(processor);
if (processor != current_processor())
cause_ast_check(processor);
return;
}
pset_idle_unlock();
processor_unlock(processor);
}
rq = &(processor->runq);
run_queue_enqueue(rq,th);
if (processor == current_processor()) {
ast_on(cpu_number(), AST_BLOCK);
}
else if ((processor->state != PROCESSOR_OFF_LINE)) {
cause_ast_check(processor);
}
}
#else
if (default_pset.idle_count > 0) {
processor = (processor_t) queue_first(&default_pset.idle_queue);
queue_remove(&default_pset.idle_queue, processor,
processor_t, processor_queue);
default_pset.idle_count--;
processor->next_thread = th;
processor->state = PROCESSOR_DISPATCHING;
return;
}
if (th->bound_processor == PROCESSOR_NULL) {
rq = &(default_pset.runq);
}
else {
rq = &(master_processor->runq);
ast_on(cpu_number(), AST_BLOCK);
}
run_queue_enqueue(rq,th);
if (may_preempt && (current_thread()->sched_pri > th->sched_pri)) {
current_processor()->first_quantum = FALSE;
ast_on(cpu_number(), AST_BLOCK);
}
#endif
}
void set_pri(
thread_t th,
int pri,
boolean_t resched)
{
struct run_queue *rq;
rq = rem_runq(th);
th->sched_pri = pri;
if (rq != RUN_QUEUE_NULL) {
if (resched)
thread_setrun(th, TRUE);
else
run_queue_enqueue(rq, th);
}
}
struct run_queue *rem_runq(
thread_t th)
{
struct run_queue *rq;
rq = th->runq;
if (rq != RUN_QUEUE_NULL) {
runq_lock(rq);
#if DEBUG
checkrq(rq, "rem_runq: at entry");
#endif
if (rq == th->runq) {
#if DEBUG
checkrq(rq, "rem_runq: before removing thread");
thread_check(th, rq);
#endif
remqueue(&rq->runq[0], (queue_entry_t) th);
rq->count--;
#if DEBUG
checkrq(rq, "rem_runq: after removing thread");
#endif
th->runq = RUN_QUEUE_NULL;
runq_unlock(rq);
}
else {
runq_unlock(rq);
rq = RUN_QUEUE_NULL;
}
}
return rq;
}
thread_t choose_thread(
processor_t myprocessor)
{
thread_t th;
queue_t q;
run_queue_t runq;
int i;
processor_set_t pset;
runq = &myprocessor->runq;
simple_lock(&runq->lock);
if (runq->count > 0) {
q = runq->runq + runq->low;
for (i = runq->low; i < NRQS ; i++, q++) {
if (!queue_empty(q)) {
th = (thread_t) dequeue_head(q);
th->runq = RUN_QUEUE_NULL;
runq->count--;
runq->low = i;
simple_unlock(&runq->lock);
return th;
}
}
panic("choose_thread");
}
simple_unlock(&runq->lock);
pset = myprocessor->processor_set;
simple_lock(&pset->runq.lock);
return choose_pset_thread(myprocessor,pset);
}
thread_t choose_pset_thread(
processor_t myprocessor,
processor_set_t pset)
{
run_queue_t runq;
thread_t th;
queue_t q;
int i;
runq = &pset->runq;
if (runq->count > 0) {
q = runq->runq + runq->low;
for (i = runq->low; i < NRQS ; i++, q++) {
if (!queue_empty(q)) {
th = (thread_t) dequeue_head(q);
th->runq = RUN_QUEUE_NULL;
runq->count--;
#if MACH_FIXPRI
if ((runq->count > 0) &&
(pset->policies & POLICY_FIXEDPRI)) {
while (queue_empty(q)) {
q++;
i++;
}
}
#endif
runq->low = i;
#if DEBUG
checkrq(runq, "choose_pset_thread");
#endif
simple_unlock(&runq->lock);
return th;
}
}
panic("choose_pset_thread");
}
simple_unlock(&runq->lock);
pset_idle_lock();
if (myprocessor->state == PROCESSOR_RUNNING) {
myprocessor->state = PROCESSOR_IDLE;
if (myprocessor == master_processor) {
queue_enter(&(pset->idle_queue), myprocessor,
processor_t, processor_queue);
}
else {
queue_enter_first(&(pset->idle_queue), myprocessor,
processor_t, processor_queue);
}
pset->idle_count++;
}
pset_idle_unlock();
return myprocessor->idle_thread;
}
int no_dispatch_count = 0;
static void __attribute__((noreturn)) idle_thread_continue(void)
{
processor_t myprocessor;
volatile thread_t *threadp;
volatile int *gcount;
volatile int *lcount;
thread_t new_thread;
int state;
int mycpu;
spl_t s;
mycpu = cpu_number();
myprocessor = current_processor();
threadp = (volatile thread_t *) &myprocessor->next_thread;
lcount = (volatile int *) &myprocessor->runq.count;
while (TRUE) {
#ifdef MARK_CPU_IDLE
MARK_CPU_IDLE(mycpu);
#endif
#if MACH_HOST
gcount = (volatile int *)
&myprocessor->processor_set->runq.count;
#else
gcount = (volatile int *) &default_pset.runq.count;
#endif
while ((*threadp == (volatile thread_t)THREAD_NULL) &&
(*gcount == 0) && (*lcount == 0)) {
if (need_ast[mycpu] &~ AST_SCHEDULING) {
(void) splsched();
need_ast[mycpu] &= ~AST_SCHEDULING;
ast_taken();
}
#if POWER_SAVE
machine_idle(mycpu);
#endif
}
#ifdef MARK_CPU_ACTIVE
MARK_CPU_ACTIVE(mycpu);
#endif
s = splsched();
retry:
state = myprocessor->state;
if (state == PROCESSOR_DISPATCHING) {
new_thread = (thread_t) *threadp;
*threadp = (volatile thread_t) THREAD_NULL;
myprocessor->state = PROCESSOR_RUNNING;
#if MACH_FIXPRI
if (new_thread->policy == POLICY_TIMESHARE) {
#endif
#if MACH_HOST
myprocessor->quantum = new_thread->
processor_set->set_quantum;
#else
myprocessor->quantum =
default_pset.set_quantum;
#endif
#if MACH_FIXPRI
}
else {
myprocessor->quantum = new_thread->sched_data;
}
#endif
myprocessor->first_quantum = TRUE;
counter(c_idle_thread_handoff++);
thread_run(idle_thread_continue, new_thread);
}
else if (state == PROCESSOR_IDLE) {
processor_set_t pset;
pset = myprocessor->processor_set;
pset_idle_lock();
if (myprocessor->state != PROCESSOR_IDLE) {
pset_idle_unlock();
goto retry;
}
no_dispatch_count++;
pset->idle_count--;
queue_remove(&pset->idle_queue, myprocessor,
processor_t, processor_queue);
myprocessor->state = PROCESSOR_RUNNING;
pset_idle_unlock();
counter(c_idle_thread_block++);
thread_block(idle_thread_continue);
}
else if ((state == PROCESSOR_ASSIGN) ||
(state == PROCESSOR_SHUTDOWN)) {
if ((new_thread = (thread_t)*threadp)!= THREAD_NULL) {
*threadp = (volatile thread_t) THREAD_NULL;
thread_lock(new_thread);
thread_setrun(new_thread, FALSE);
thread_unlock(new_thread);
}
counter(c_idle_thread_block++);
thread_block(idle_thread_continue);
}
else {
printf(" Bad processor state %d (Cpu %d)\n",
cpu_state(mycpu), mycpu);
panic("idle_thread");
}
(void) splx(s);
}
}
void idle_thread(void)
{
thread_t self = current_thread();
spl_t s;
stack_privilege(self);
s = splsched();
self->priority = NRQS-1;
self->sched_pri = NRQS-1;
thread_lock(self);
self->state |= TH_IDLE;
thread_unlock(self);
current_processor()->idle_thread = self;
(void) splx(s);
counter(c_idle_thread_block++);
thread_block(idle_thread_continue);
idle_thread_continue();
}
static void sched_thread_continue(void)
{
while (TRUE) {
(void) compute_mach_factor();
if (sched_tick & 1)
do_thread_scan();
assert_wait((event_t) 0, FALSE);
counter(c_sched_thread_block++);
thread_block(sched_thread_continue);
}
}
void sched_thread(void)
{
sched_thread_id = current_thread();
assert_wait((event_t) 0, FALSE);
counter(c_sched_thread_block++);
thread_block(sched_thread_continue);
sched_thread_continue();
}
#define MAX_STUCK_THREADS 16
boolean_t do_thread_scan_debug = FALSE;
thread_t stuck_threads[MAX_STUCK_THREADS];
int stuck_count = 0;
static boolean_t
do_runq_scan(
run_queue_t runq)
{
spl_t s;
queue_t q;
thread_t thread;
int count;
s = splsched();
simple_lock(&runq->lock);
if ((count = runq->count) > 0) {
q = runq->runq + runq->low;
while (count > 0) {
thread = (thread_t) queue_first(q);
while (!queue_end(q, (queue_entry_t) thread)) {
thread_t next = (thread_t) queue_next(&thread->links);
if ((thread->state & TH_SCHED_STATE) == TH_RUN &&
sched_tick - thread->sched_stamp > 1) {
if (stuck_count == MAX_STUCK_THREADS) {
simple_unlock(&runq->lock);
splx(s);
return TRUE;
}
remqueue(q, (queue_entry_t) thread);
runq->count--;
thread->runq = RUN_QUEUE_NULL;
stuck_threads[stuck_count++] = thread;
if (do_thread_scan_debug)
printf("do_runq_scan: adding thread %p\n", thread);
}
count--;
thread = next;
}
q++;
}
}
simple_unlock(&runq->lock);
splx(s);
return FALSE;
}
void do_thread_scan(void)
{
spl_t s;
boolean_t restart_needed = 0;
thread_t thread;
int i;
#if MACH_HOST
processor_set_t pset;
#endif
do {
#if MACH_HOST
simple_lock(&all_psets_lock);
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
if ((restart_needed = do_runq_scan(&pset->runq)))
break;
}
simple_unlock(&all_psets_lock);
#else
restart_needed = do_runq_scan(&default_pset.runq);
#endif
if (!restart_needed) {
for (i = 0; i < smp_get_numcpus(); i++) {
if ((restart_needed = do_runq_scan(&cpu_to_processor(i)->runq)))
break;
}
}
while (stuck_count > 0) {
thread = stuck_threads[--stuck_count];
stuck_threads[stuck_count] = THREAD_NULL;
s = splsched();
thread_lock(thread);
if ((thread->state & TH_SCHED_STATE) == TH_RUN) {
update_priority(thread);
thread_setrun(thread, TRUE);
}
thread_unlock(thread);
splx(s);
}
} while (restart_needed);
}
#if DEBUG
void checkrq(
run_queue_t rq,
const char *msg)
{
queue_t q1;
int i, j;
queue_entry_t e;
int low;
low = -1;
j = 0;
q1 = rq->runq;
for (i = 0; i < NRQS; i++) {
if (q1->next == q1) {
if (q1->prev != q1)
panic("checkrq: empty at %s", msg);
}
else {
if (low == -1)
low = i;
for (e = q1->next; e != q1; e = e->next) {
j++;
if (e->next->prev != e)
panic("checkrq-2 at %s", msg);
if (e->prev->next != e)
panic("checkrq-3 at %s", msg);
}
}
q1++;
}
if (j != rq->count)
panic("checkrq: count wrong at %s", msg);
if (rq->count != 0 && low < rq->low)
panic("checkrq: low wrong at %s", msg);
}
void thread_check(
thread_t th,
run_queue_t rq)
{
unsigned int whichq;
whichq = th->sched_pri;
if (whichq >= NRQS) {
printf("thread_check: priority too high\n");
whichq = NRQS-1;
}
if ((th->links.next == &rq->runq[whichq]) &&
(rq->runq[whichq].prev != (queue_entry_t)th))
panic("thread_check");
}
#endif
#if NCPUS > 1
kern_return_t thread_migrate(
thread_t thread,
processor_t target_processor)
{
processor_t current_processor;
run_queue_t old_rq, new_rq;
spl_t s;
if (thread == THREAD_NULL || target_processor == PROCESSOR_NULL)
return KERN_INVALID_ARGUMENT;
s = splsched();
thread_lock(thread);
if ((thread->state & TH_RUN) == 0 || thread->state & TH_IDLE) {
thread_unlock(thread);
splx(s);
return KERN_FAILURE;
}
current_processor = thread->last_processor;
if (current_processor == target_processor) {
thread_unlock(thread);
splx(s);
return KERN_SUCCESS;
}
old_rq = rem_runq(thread);
if (old_rq == RUN_QUEUE_NULL) {
thread_unlock(thread);
splx(s);
return KERN_FAILURE;
}
thread->migration_count++;
thread->cache_warmth = 0;
if (current_processor != PROCESSOR_NULL) {
current_processor->migration_out++;
}
target_processor->migration_in++;
thread->last_processor = target_processor;
new_rq = &target_processor->runq;
run_queue_enqueue(new_rq, thread);
thread_unlock(thread);
if (target_processor->state == PROCESSOR_IDLE) {
cause_ast_check(target_processor);
}
splx(s);
return KERN_SUCCESS;
}
processor_t thread_select_best_processor(
thread_t thread)
{
processor_set_t pset;
processor_t best_processor, processor;
unsigned int min_load, current_load;
unsigned int cache_bonus;
if (thread->bound_processor != PROCESSOR_NULL)
return thread->bound_processor;
pset = thread->processor_set;
best_processor = PROCESSOR_NULL;
min_load = ~0U;
if (thread->last_processor != PROCESSOR_NULL &&
thread->last_processor->processor_set == pset) {
processor = thread->last_processor;
current_load = processor->runq.count;
cache_bonus = thread->cache_warmth > 10 ? 2 : 1;
if (current_load <= min_load + cache_bonus) {
best_processor = processor;
min_load = current_load;
}
}
queue_iterate(&pset->processors, processor, processor_t, processors) {
if (processor == thread->last_processor)
continue;
current_load = processor->runq.count;
if (current_load < min_load) {
best_processor = processor;
min_load = current_load;
}
}
return best_processor ? best_processor :
(processor_t)queue_first(&pset->processors);
}
void thread_update_cache_warmth(
thread_t thread)
{
if (thread->cache_warmth < 255)
thread->cache_warmth++;
}
void thread_balance_load(void)
{
processor_set_t pset;
processor_t src_processor, dst_processor;
processor_t busiest, idlest;
thread_t thread;
unsigned int max_load, min_load;
unsigned int load_diff;
spl_t s;
s = splsched();
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
if (pset->processor_count < 2)
continue;
busiest = idlest = PROCESSOR_NULL;
max_load = 0;
min_load = ~0U;
queue_iterate(&pset->processors, src_processor, processor_t, processors) {
unsigned int load = src_processor->runq.count;
if (load > max_load) {
max_load = load;
busiest = src_processor;
}
if (load < min_load) {
min_load = load;
idlest = src_processor;
}
}
if (busiest == PROCESSOR_NULL || idlest == PROCESSOR_NULL)
continue;
load_diff = max_load - min_load;
if (load_diff < 2)
continue;
runq_lock(&busiest->runq);
if (busiest->runq.count > 0) {
int i;
for (i = busiest->runq.low; i < NRQS; i++) {
queue_t q = &busiest->runq.runq[i];
if (!queue_empty(q)) {
thread = (thread_t)queue_first(q);
if (thread->bound_processor == PROCESSOR_NULL) {
runq_unlock(&busiest->runq);
thread_migrate(thread, idlest);
goto next_pset;
}
}
}
}
runq_unlock(&busiest->runq);
next_pset:
continue;
}
splx(s);
}
#endif