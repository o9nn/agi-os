#include <string.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/mach_types.h>
#include <mach/machine.h>
#include <mach/host_info.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/ipc_host.h>
#include <kern/host.h>
#include <kern/machine.h>
#include <kern/mach_host.server.h>
#include <kern/lock.h>
#include <kern/processor.h>
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/printf.h>
#include <machine/spl.h>
#include <machine/model_dep.h>
#include <machine/pcb.h>
#include <sys/reboot.h>
struct machine_info machine_info;
struct machine_slot machine_slot[NCPUS];
queue_head_t action_queue;
def_simple_lock_data(,action_lock);
void cpu_up(int cpu)
{
struct machine_slot *ms;
processor_t processor;
spl_t s;
processor = cpu_to_processor(cpu);
pset_lock(&default_pset);
#if MACH_HOST
pset_lock(slave_pset);
#endif
s = splsched();
processor_lock(processor);
#if NCPUS > 1
init_ast_check(processor);
#endif
ms = &machine_slot[cpu];
ms->running = TRUE;
machine_info.avail_cpus++;
#if MACH_HOST
if (cpu != 0)
pset_add_processor(slave_pset, processor);
else
#endif
pset_add_processor(&default_pset, processor);
processor->state = PROCESSOR_RUNNING;
processor_unlock(processor);
splx(s);
#if MACH_HOST
pset_unlock(slave_pset);
#endif
pset_unlock(&default_pset);
}
kern_return_t
host_reboot(const host_t host, int options)
{
if (host == HOST_NULL)
return (KERN_INVALID_HOST);
if (options & RB_DEBUGGER) {
Debugger("Debugger");
} else {
#ifdef parisc
halt_all_cpus(options);
#else
halt_all_cpus(!(options & RB_HALT));
#endif
}
return (KERN_SUCCESS);
}
#if NCPUS > 1
static void cpu_down(int cpu)
{
struct machine_slot *ms;
processor_t processor;
spl_t s;
s = splsched();
processor = cpu_to_processor(cpu);
processor_lock(processor);
ms = &machine_slot[cpu];
ms->running = FALSE;
machine_info.avail_cpus--;
processor->processor_set_next = PROCESSOR_SET_NULL;
processor->state = PROCESSOR_OFF_LINE;
processor_unlock(processor);
splx(s);
}
static void
processor_request_action(
processor_t processor,
processor_set_t new_pset)
{
processor_set_t pset;
pset = processor->processor_set;
pset_idle_lock();
while (*(volatile int *)&processor->state == PROCESSOR_DISPATCHING)
cpu_pause();
simple_lock(&action_lock);
switch (processor->state) {
case PROCESSOR_IDLE:
queue_remove(&pset->idle_queue, processor, processor_t,
processor_queue);
pset->idle_count--;
case PROCESSOR_RUNNING:
queue_enter(&action_queue, processor, processor_t,
processor_queue);
case PROCESSOR_ASSIGN:
if (new_pset == PROCESSOR_SET_NULL) {
processor->state = PROCESSOR_SHUTDOWN;
}
else {
assert(processor->state != PROCESSOR_ASSIGN);
processor->state = PROCESSOR_ASSIGN;
processor->processor_set_next = new_pset;
}
break;
default:
printf("state: %d\n", processor->state);
panic("processor_request_action: bad state");
}
simple_unlock(&action_lock);
pset_idle_unlock();
thread_wakeup((event_t)&action_queue);
}
#if MACH_HOST
kern_return_t
processor_assign(
processor_t processor,
processor_set_t new_pset,
boolean_t wait)
{
spl_t s;
if (processor == PROCESSOR_NULL || new_pset == PROCESSOR_SET_NULL ||
processor == master_processor) {
return(KERN_INVALID_ARGUMENT);
}
pset_reference(new_pset);
Retry:
s = splsched();
processor_lock(processor);
if(processor->state == PROCESSOR_OFF_LINE ||
processor->state == PROCESSOR_SHUTDOWN) {
processor_unlock(processor);
(void) splx(s);
pset_deallocate(new_pset);
return(KERN_FAILURE);
}
if (processor->state == PROCESSOR_ASSIGN) {
assert_wait((event_t) processor, TRUE);
processor_unlock(processor);
splx(s);
thread_block(thread_no_continuation);
goto Retry;
}
if (processor->processor_set == new_pset) {
processor_unlock(processor);
(void) splx(s);
pset_deallocate(new_pset);
return(KERN_SUCCESS);
}
processor_request_action(processor, new_pset);
if (wait) {
while (processor->state == PROCESSOR_ASSIGN ||
processor->state == PROCESSOR_SHUTDOWN) {
assert_wait((event_t)processor, TRUE);
processor_unlock(processor);
splx(s);
thread_block(thread_no_continuation);
s = splsched();
processor_lock(processor);
}
}
processor_unlock(processor);
splx(s);
return(KERN_SUCCESS);
}
#else
kern_return_t
processor_assign(
processor_t processor,
processor_set_t new_pset,
boolean_t wait)
{
return KERN_FAILURE;
}
#endif
kern_return_t
processor_shutdown(processor_t processor)
{
spl_t s;
if (processor == PROCESSOR_NULL)
return KERN_INVALID_ARGUMENT;
s = splsched();
processor_lock(processor);
if(processor->state == PROCESSOR_OFF_LINE ||
processor->state == PROCESSOR_SHUTDOWN) {
processor_unlock(processor);
splx(s);
return(KERN_SUCCESS);
}
processor_request_action(processor, PROCESSOR_SET_NULL);
processor_unlock(processor);
splx(s);
return(KERN_SUCCESS);
}
static void processor_doaction(processor_t processor)
{
thread_t this_thread;
spl_t s;
processor_set_t pset;
#if MACH_HOST
processor_set_t new_pset;
thread_t thread;
thread_t prev_thread = THREAD_NULL;
boolean_t have_pset_ref = FALSE;
#endif
this_thread = current_thread();
thread_bind(this_thread, processor);
thread_block(thread_no_continuation);
pset = processor->processor_set;
#if MACH_HOST
pset_lock(pset);
if (pset->processor_count == 1) {
queue_iterate(&pset->threads, thread, thread_t, pset_threads) {
thread_hold(thread);
}
pset->empty = TRUE;
pset->ref_count++;
have_pset_ref = TRUE;
Restart_thread:
prev_thread = THREAD_NULL;
queue_iterate(&pset->threads, thread, thread_t, pset_threads) {
thread_reference(thread);
pset_unlock(pset);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
thread_freeze(thread);
if (thread->processor_set != pset) {
thread_unfreeze(thread);
thread_deallocate(thread);
pset_lock(pset);
goto Restart_thread;
}
(void) thread_dowait(thread, TRUE);
prev_thread = thread;
pset_lock(pset);
thread_unfreeze(prev_thread);
}
}
pset_unlock(pset);
new_pset = processor->processor_set_next;
Restart_pset:
if (new_pset) {
if ((integer_t) pset < (integer_t) new_pset) {
pset_lock(pset);
pset_lock(new_pset);
}
else {
pset_lock(new_pset);
pset_lock(pset);
}
if (!(new_pset->active)) {
pset_unlock(new_pset);
pset_unlock(pset);
pset_deallocate(new_pset);
new_pset = &default_pset;
pset_reference(new_pset);
goto Restart_pset;
}
while (new_pset->empty && new_pset->processor_count > 0) {
pset_unlock(new_pset);
pset_unlock(pset);
while (*(volatile boolean_t *)&new_pset->empty &&
*(volatile int *)&new_pset->processor_count > 0)
;
goto Restart_pset;
}
s = splsched();
processor_lock(processor);
assert(processor->processor_set_next == new_pset);
if (processor->state == PROCESSOR_SHUTDOWN) {
processor->processor_set_next = PROCESSOR_SET_NULL;
pset_unlock(new_pset);
goto shutdown;
}
pset_remove_processor(pset, processor);
pset_unlock(pset);
pset_add_processor(new_pset, processor);
if (new_pset->empty) {
queue_iterate(&new_pset->threads, thread, thread_t,
pset_threads) {
thread_release(thread);
}
new_pset->empty = FALSE;
}
processor->processor_set_next = PROCESSOR_SET_NULL;
processor->state = PROCESSOR_RUNNING;
thread_wakeup((event_t)processor);
processor_unlock(processor);
splx(s);
pset_unlock(new_pset);
pset_deallocate(new_pset);
if (have_pset_ref)
pset_deallocate(pset);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
thread_bind(this_thread, PROCESSOR_NULL);
thread_block(thread_no_continuation);
return;
}
#endif
if (processor->state != PROCESSOR_SHUTDOWN) {
printf("state: %d\n", processor->state);
panic("action_thread -- bad processor state");
}
s = splsched();
processor_lock(processor);
#if MACH_HOST
shutdown:
#endif
pset_remove_processor(pset, processor);
processor_unlock(processor);
pset_unlock(pset);
splx(s);
#if MACH_HOST
if (new_pset != PROCESSOR_SET_NULL)
pset_deallocate(new_pset);
if (have_pset_ref)
pset_deallocate(pset);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
#endif
thread_bind(this_thread, PROCESSOR_NULL);
switch_to_shutdown_context(this_thread,
processor_doshutdown,
processor);
}
void __attribute__((noreturn)) action_thread_continue(void)
{
processor_t processor;
spl_t s;
while (TRUE) {
s = splsched();
simple_lock(&action_lock);
while ( !queue_empty(&action_queue)) {
processor = (processor_t) queue_first(&action_queue);
queue_remove(&action_queue, processor, processor_t,
processor_queue);
simple_unlock(&action_lock);
(void) splx(s);
processor_doaction(processor);
s = splsched();
simple_lock(&action_lock);
}
assert_wait((event_t) &action_queue, FALSE);
simple_unlock(&action_lock);
(void) splx(s);
counter(c_action_thread_block++);
thread_block(action_thread_continue);
}
}
void __attribute__((noreturn)) action_thread(void)
{
action_thread_continue();
}
void processor_doshutdown(processor_t processor)
{
int cpu = processor->slot_num;
timer_switch(&kernel_timer[cpu]);
PMAP_DEACTIVATE_KERNEL(cpu);
#ifndef MIGRATING_THREADS
percpu_array[cpu].active_thread = THREAD_NULL;
#endif
cpu_down(cpu);
thread_wakeup((event_t)processor);
halt_cpu();
}
#else
kern_return_t
processor_assign(
processor_t processor,
processor_set_t new_pset,
boolean_t wait)
{
return(KERN_FAILURE);
}
#endif