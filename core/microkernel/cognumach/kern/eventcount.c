#include <kern/printf.h>
#include <string.h>
#include <mach/machine.h>
#include <kern/ast.h>
#include <kern/debug.h>
#include "cpu_number.h"
#include <kern/lock.h>
#include <kern/processor.h>
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <machine/spl.h>
#include <kern/eventcount.h>
#define	MAX_EVCS	10
evc_t	all_eventcounters[MAX_EVCS];
void
evc_init(evc_t	ev)
{
int i;
memset(ev, 0, sizeof(*ev));
for (i = 0; i < MAX_EVCS; i++)
if (all_eventcounters[i] == 0) break;
if (i == MAX_EVCS) {
printf("Too many eventcounters\n");
return;
}
all_eventcounters[i] = ev;
ev->ev_id = i;
ev->sanity = ev;
ev->waiting_thread = THREAD_NULL;
simple_lock_init(&ev->lock);
}
void
evc_destroy(evc_t	ev)
{
evc_signal(ev);
ev->sanity = 0;
if (all_eventcounters[ev->ev_id] == ev)
all_eventcounters[ev->ev_id] = 0;
ev->ev_id = -1;
}
void evc_notify_abort(const thread_t thread)
{
int i;
evc_t ev;
int s = splsched();
for (i = 0; i < MAX_EVCS; i++)  {
ev = all_eventcounters[i];
if (ev)  {
simple_lock(&ev->lock);
if (ev->waiting_thread == thread)
{
ev->waiting_thread = 0;
ev->count++;
}
simple_unlock(&ev->lock);
}
}
splx(s);
}
static void  __attribute__((noreturn))
evc_continue(void)
{
thread_syscall_return(KERN_SUCCESS);
}
kern_return_t evc_wait(natural_t ev_id)
{
spl_t		s;
kern_return_t	ret;
evc_t		ev;
if ((ev_id >= MAX_EVCS) ||
((ev = all_eventcounters[ev_id]) == 0) ||
(ev->ev_id != ev_id) || (ev->sanity != ev))
return KERN_INVALID_ARGUMENT;
s = splsched();
simple_lock(&ev->lock);
if (ev->count > 0) {
ev->count--;
ret = KERN_SUCCESS;
} else {
if (ev->waiting_thread == THREAD_NULL) {
ev->count--;
ev->waiting_thread = current_thread();
assert_wait((event_t) 0, TRUE);
simple_unlock(&ev->lock);
thread_block(evc_continue);
return KERN_SUCCESS;
}
ret = KERN_NO_SPACE;
}
simple_unlock(&ev->lock);
splx(s);
return ret;
}
kern_return_t evc_wait_clear(natural_t ev_id)
{
spl_t		s;
evc_t		ev;
if ((ev_id >= MAX_EVCS) ||
((ev = all_eventcounters[ev_id]) == 0) ||
(ev->ev_id != ev_id) || (ev->sanity != ev))
return KERN_INVALID_ARGUMENT;
s = splsched();
simple_lock(&ev->lock);
if (ev->waiting_thread == THREAD_NULL) {
ev->count = -1;
ev->waiting_thread = current_thread();
assert_wait((event_t) 0, TRUE);
simple_unlock(&ev->lock);
thread_block(evc_continue);
}
simple_unlock(&ev->lock);
splx(s);
return KERN_NO_SPACE;
}
void
evc_signal(evc_t ev)
{
volatile thread_t thread;
int state;
spl_t    s;
if (ev->sanity != ev)
return;
s = splsched();
simple_lock(&ev->lock);
ev->count++;
if (thread = ev->waiting_thread, thread != THREAD_NULL)
{
ev->waiting_thread = 0;
#if (NCPUS > 1)
retry:
while((thread->state & TH_RUN) || thread->lock.lock_data)
cpu_pause();
#endif
thread_lock(thread);
switch ((state = thread->state) & TH_SCHED_STATE)
{
case  TH_WAIT | TH_SUSP | TH_UNINT:
case  TH_WAIT           | TH_UNINT:
case  TH_WAIT:
thread->state = (state &~ TH_WAIT) | TH_RUN;
#if NCPUS > 1
thread_setrun(thread, TRUE);
#else
simpler_thread_setrun(thread, TRUE);
#endif
thread_unlock(thread);
break;
case TH_RUN | TH_WAIT:
#if (NCPUS > 1)
thread_unlock(thread);
goto retry;
#else
#endif
case          TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT | TH_SUSP:
case TH_RUN | TH_WAIT           | TH_UNINT:
case TH_RUN | TH_WAIT | TH_SUSP | TH_UNINT:
thread->state = state &~ TH_WAIT;
thread_unlock(thread);
break;
default:
panic("evc_signal.3");
thread_unlock(thread);
break;
}
}
simple_unlock(&ev->lock);
splx(s);
}
#if	NCPUS <= 1
void
simpler_thread_setrun(
thread_t	th,
boolean_t	may_preempt)
{
struct run_queue	*rq;
int			whichq;
if (default_pset.idle_count > 0) {
processor_t	processor;
processor = (processor_t) queue_first(&default_pset.idle_queue);
queue_remove(&default_pset.idle_queue, processor,
processor_t, processor_queue);
default_pset.idle_count--;
processor->next_thread = th;
processor->state = PROCESSOR_DISPATCHING;
return;
}
rq = &(master_processor->runq);
ast_on(cpu_number(), AST_BLOCK);
whichq = (th)->sched_pri;
runq_lock(rq);
enqueue_head(&(rq)->runq[whichq], &((th)->links));
if (whichq < (rq)->low || (rq)->count == 0)
(rq)->low = whichq;
(rq)->count++;
#ifdef MIGRATING_THREADS
(th)->shuttle.runq = (rq);
#else
(th)->runq = (rq);
#endif
runq_unlock(rq);
current_processor()->first_quantum = FALSE;
}
#endif