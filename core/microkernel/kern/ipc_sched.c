#include <mach/message.h>
#include <kern/counters.h>
#include "cpu_number.h"
#include <kern/debug.h>
#include <kern/lock.h>
#include <kern/mach_clock.h>
#include <kern/thread.h>
#include <kern/sched_prim.h>
#include <kern/processor.h>
#include <kern/thread_swap.h>
#include <kern/ipc_sched.h>
#include <machine/spl.h>
#include <machine/pmap.h>
void
thread_go(
thread_t thread)
{
int	state;
spl_t	s;
s = splsched();
thread_lock(thread);
reset_timeout_check(&thread->timer);
state = thread->state;
switch (state & TH_SCHED_STATE) {
case TH_WAIT | TH_SUSP | TH_UNINT:
case TH_WAIT	   | TH_UNINT:
case TH_WAIT:
thread->state = (state &~ TH_WAIT) | TH_RUN;
thread->wait_result = THREAD_AWAKENED;
thread_setrun(thread, TRUE);
break;
case	  TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT:
case TH_RUN | TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT	    | TH_UNINT:
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
thread->state = state & ~TH_WAIT;
thread->wait_result = THREAD_AWAKENED;
break;
default:
break;
}
thread_unlock(thread);
splx(s);
}
void
thread_will_wait(
thread_t thread)
{
spl_t	s;
s = splsched();
thread_lock(thread);
assert(thread->wait_result = -1);
thread->state |= TH_WAIT;
thread_unlock(thread);
splx(s);
}
void
thread_will_wait_with_timeout(
thread_t thread,
mach_msg_timeout_t msecs)
{
natural_t ticks = convert_ipc_timeout_to_ticks(msecs);
spl_t	s;
s = splsched();
thread_lock(thread);
assert(thread->wait_result = -1);
thread->state |= TH_WAIT;
set_timeout(&thread->timer, ticks);
thread_unlock(thread);
splx(s);
}
#if	MACH_HOST
#define check_processor_set(thread)	\
(current_processor()->processor_set == (thread)->processor_set)
#else
#define	check_processor_set(thread)	TRUE
#endif
#if	NCPUS > 1
#define	check_bound_processor(thread) \
((thread)->bound_processor == PROCESSOR_NULL || \
(thread)->bound_processor == current_processor())
#else
#define	check_bound_processor(thread)	TRUE
#endif
boolean_t
thread_handoff(
thread_t old,
continuation_t continuation,
thread_t new)
{
spl_t	s;
assert(current_thread() == old);
s = splsched();
thread_lock(new);
if ((old->stack_privilege == current_stack()) ||
(new->state != (TH_WAIT|TH_SWAPPED)) ||
!check_processor_set(new) ||
!check_bound_processor(new)) {
thread_unlock(new);
(void) splx(s);
counter(c_thread_handoff_misses++);
return FALSE;
}
reset_timeout_check(&new->timer);
new->state = TH_RUN;
thread_unlock(new);
#if	NCPUS > 1
new->last_processor = current_processor();
#endif
ast_context(new, cpu_number());
timer_switch(&new->system_timer);
stack_handoff(old, new);
thread_lock(old);
old->swap_func = continuation;
assert(old->wait_result = -1);
if (old->state == TH_RUN) {
old->state = TH_WAIT|TH_SWAPPED;
}
else if (old->state == (TH_RUN|TH_SUSP)) {
old->state = TH_WAIT|TH_SUSP|TH_SWAPPED;
if (old->wake_active) {
old->wake_active = FALSE;
thread_unlock(old);
thread_wakeup(TH_EV_WAKE_ACTIVE(old));
goto after_old_thread;
}
} else
panic("thread_handoff");
thread_unlock(old);
after_old_thread:
(void) splx(s);
counter(c_thread_handoff_hits++);
return TRUE;
}