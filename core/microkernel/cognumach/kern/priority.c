#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/machine.h>
#include <kern/host.h>
#include <kern/mach_clock.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <kern/priority.h>
#include <kern/processor.h>
#include <kern/timer.h>
#include <machine/spl.h>
#ifdef PRI_SHIFT_2
#if PRI_SHIFT_2 > 0
#define USAGE_THRESHOLD (((1 << PRI_SHIFT) + (1 << PRI_SHIFT_2)) << (2 + SCHED_SHIFT))
#else
#define USAGE_THRESHOLD (((1 << PRI_SHIFT) - (1 << -(PRI_SHIFT_2))) << (2 + SCHED_SHIFT))
#endif
#else
#define USAGE_THRESHOLD (1 << (PRI_SHIFT + 2 + SCHED_SHIFT))
#endif
void thread_quantum_update(
int mycpu,
thread_t thread,
int nticks,
int state)
{
int quantum;
processor_t myprocessor;
#if NCPUS > 1
processor_set_t pset;
#endif
spl_t s;
myprocessor = cpu_to_processor(mycpu);
#if NCPUS > 1
pset = myprocessor->processor_set;
if (pset == 0) {
return;
}
#endif
#if NCPUS > 1
pset->set_quantum = pset->machine_quantum[
((pset->runq.count > pset->processor_count) ?
pset->processor_count : pset->runq.count)];
if (myprocessor->runq.count != 0)
quantum = min_quantum;
else
quantum = pset->set_quantum;
#else
quantum = min_quantum;
default_pset.set_quantum = quantum;
#endif
if (state != CPU_STATE_IDLE) {
myprocessor->quantum -= nticks;
#if NCPUS > 1
if ((quantum != myprocessor->last_quantum) &&
(pset->processor_count > 1)) {
myprocessor->last_quantum = quantum;
s = simple_lock_irq(&pset->quantum_adj_lock);
quantum = min_quantum + (pset->quantum_adj_index *
(quantum - min_quantum)) /
(pset->processor_count - 1);
if (++(pset->quantum_adj_index) >=
pset->processor_count)
pset->quantum_adj_index = 0;
simple_unlock_irq(s, &pset->quantum_adj_lock);
}
#endif
if (myprocessor->quantum <= 0) {
s = splsched();
thread_lock(thread);
if (thread->sched_stamp != sched_tick) {
update_priority(thread);
}
else {
if (
#if MACH_FIXPRI
(thread->policy == POLICY_TIMESHARE) &&
#endif
(thread->depress_priority < 0)) {
thread_timer_delta(thread);
thread->sched_usage +=
thread->sched_delta;
thread->sched_delta = 0;
compute_my_priority(thread);
}
}
thread_unlock(thread);
(void) splx(s);
myprocessor->first_quantum = FALSE;
#if MACH_FIXPRI
if (thread->policy == POLICY_TIMESHARE) {
#endif
myprocessor->quantum += quantum;
#if MACH_FIXPRI
}
else {
myprocessor->quantum += thread->sched_data;
}
#endif
}
else {
s = splsched();
thread_lock(thread);
if (thread->sched_stamp != sched_tick) {
update_priority(thread);
}
else {
if (
#if MACH_FIXPRI
(thread->policy == POLICY_TIMESHARE) &&
#endif
(thread->depress_priority < 0)) {
thread_timer_delta(thread);
if (thread->sched_delta >= USAGE_THRESHOLD) {
thread->sched_usage +=
thread->sched_delta;
thread->sched_delta = 0;
compute_my_priority(thread);
}
}
}
thread_unlock(thread);
(void) splx(s);
}
ast_check();
}
}