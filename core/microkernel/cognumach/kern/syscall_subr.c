#include <mach/boolean.h>
#include <mach/thread_switch.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <kern/counters.h>
#include <kern/ipc_kobject.h>
#include <kern/mach_clock.h>
#include <kern/printf.h>
#include <kern/processor.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/syscall_subr.h>
#include <kern/ipc_sched.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <machine/spl.h>
#if MACH_FIXPRI
#include <mach/policy.h>
#endif
static void swtch_continue(void)
{
processor_t myprocessor;
myprocessor = current_processor();
thread_syscall_return(myprocessor->runq.count > 0 ||
myprocessor->processor_set->runq.count > 0);
}
boolean_t swtch(void)
{
processor_t myprocessor;
#if NCPUS > 1
myprocessor = current_processor();
if (myprocessor->runq.count == 0 &&
myprocessor->processor_set->runq.count == 0)
return(FALSE);
#endif
counter(c_swtch_block++);
thread_block(swtch_continue);
myprocessor = current_processor();
return(myprocessor->runq.count > 0 ||
myprocessor->processor_set->runq.count > 0);
}
static void swtch_pri_continue(void)
{
thread_t thread = current_thread();
processor_t myprocessor;
if (thread->depress_priority >= 0)
(void) thread_depress_abort(thread);
myprocessor = current_processor();
thread_syscall_return(myprocessor->runq.count > 0 ||
myprocessor->processor_set->runq.count > 0);
}
boolean_t swtch_pri(int pri)
{
thread_t thread = current_thread();
processor_t myprocessor;
#if NCPUS > 1
myprocessor = current_processor();
if (myprocessor->runq.count == 0 &&
myprocessor->processor_set->runq.count == 0)
return(FALSE);
#endif
thread_depress_priority(thread, min_quantum);
counter(c_swtch_pri_block++);
thread_block(swtch_pri_continue);
if (thread->depress_priority >= 0)
(void) thread_depress_abort(thread);
myprocessor = current_processor();
return(myprocessor->runq.count > 0 ||
myprocessor->processor_set->runq.count > 0);
}
static void thread_switch_continue(void)
{
thread_t cur_thread = current_thread();
if (cur_thread->depress_priority >= 0)
(void) thread_depress_abort(cur_thread);
thread_syscall_return(KERN_SUCCESS);
}
kern_return_t thread_switch(
mach_port_name_t thread_name,
int option,
mach_msg_timeout_t option_time)
{
thread_t cur_thread = current_thread();
processor_t myprocessor;
ipc_port_t port;
switch (option) {
case SWITCH_OPTION_NONE:
break;
case SWITCH_OPTION_DEPRESS:
thread_depress_priority(cur_thread, option_time);
break;
case SWITCH_OPTION_WAIT:
thread_will_wait_with_timeout(cur_thread, option_time);
break;
default:
return(KERN_INVALID_ARGUMENT);
}
#ifndef MIGRATING_THREADS
if ((thread_name != 0) &&
(ipc_port_translate_send(cur_thread->task->itk_space,
thread_name, &port) == KERN_SUCCESS)) {
if (ip_active(port) && (ip_kotype(port) == IKOT_THREAD)) {
thread_t thread;
spl_t s;
thread = (thread_t) port->ip_kobject;
s = splsched();
thread_lock(thread);
if ((thread->processor_set == cur_thread->processor_set)
&& (rem_runq(thread) != RUN_QUEUE_NULL)) {
thread_unlock(thread);
(void) splx(s);
ip_unlock(port);
#if MACH_FIXPRI
if (thread->policy == POLICY_FIXEDPRI) {
myprocessor = current_processor();
myprocessor->quantum = thread->sched_data;
myprocessor->first_quantum = TRUE;
}
#endif
counter(c_thread_switch_handoff++);
thread_run(thread_switch_continue, thread);
if (cur_thread->depress_priority >= 0)
(void) thread_depress_abort(cur_thread);
return(KERN_SUCCESS);
}
thread_unlock(thread);
(void) splx(s);
}
ip_unlock(port);
}
#endif
#if NCPUS > 1
myprocessor = current_processor();
if (myprocessor->processor_set->runq.count > 0 ||
myprocessor->runq.count > 0)
#endif
{
counter(c_thread_switch_block++);
thread_block(thread_switch_continue);
}
if (cur_thread->depress_priority >= 0)
(void) thread_depress_abort(cur_thread);
return(KERN_SUCCESS);
}
void
thread_depress_priority(
thread_t thread,
mach_msg_timeout_t depress_time)
{
unsigned int ticks;
spl_t s;
ticks = convert_ipc_timeout_to_ticks(depress_time);
s = splsched();
thread_lock(thread);
reset_timeout_check(&thread->depress_timer);
thread->depress_priority = thread->priority;
thread->priority = NRQS-1;
thread->sched_pri = NRQS-1;
if (ticks != 0)
set_timeout(&thread->depress_timer, ticks);
thread_unlock(thread);
(void) splx(s);
}
void
thread_depress_timeout(thread_t thread)
{
spl_t s;
s = splsched();
thread_lock(thread);
if (thread->depress_priority >= 0) {
thread->priority = thread->depress_priority;
thread->depress_priority = -1;
compute_priority(thread, FALSE);
}
thread_unlock(thread);
(void) splx(s);
}
kern_return_t
thread_depress_abort(thread_t thread)
{
spl_t s;
if (thread == THREAD_NULL)
return(KERN_INVALID_ARGUMENT);
s = splsched();
thread_lock(thread);
if (thread->depress_priority >= 0) {
reset_timeout_check(&thread->depress_timer);
thread->priority = thread->depress_priority;
thread->depress_priority = -1;
compute_priority(thread, FALSE);
}
thread_unlock(thread);
(void) splx(s);
return(KERN_SUCCESS);
}
#ifdef MACH_KDB
void
mach_print(const char *s)
{
printf("%s", s);
}
#endif