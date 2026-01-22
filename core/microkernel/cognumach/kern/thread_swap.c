#include <ipc/ipc_kmsg.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/thread.h>
#include <kern/lock.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <mach/vm_param.h>
#include <kern/sched_prim.h>
#include <kern/processor.h>
#include <kern/thread_swap.h>
#include <machine/spl.h>
queue_head_t swapin_queue;
def_simple_lock_data(static, swapper_lock_data)
#define swapper_lock() simple_lock(&swapper_lock_data)
#define swapper_unlock() simple_unlock(&swapper_lock_data)
void swapper_init(void)
{
queue_init(&swapin_queue);
simple_lock_init(&swapper_lock_data);
}
void thread_swapin(thread_t thread)
{
switch (thread->state & TH_SWAP_STATE) {
case TH_SWAPPED:
thread->state = (thread->state & ~TH_SWAP_STATE)
| TH_SW_COMING_IN;
swapper_lock();
enqueue_tail(&swapin_queue, &(thread->links));
swapper_unlock();
thread_wakeup((event_t) &swapin_queue);
break;
case TH_SW_COMING_IN:
break;
default:
panic("thread_swapin");
}
}
kern_return_t thread_doswapin(thread_t thread)
{
kern_return_t kr;
spl_t s;
kr = stack_alloc(thread, thread_continue);
if (kr != KERN_SUCCESS)
return kr;
s = splsched();
thread_lock(thread);
thread->state &= ~(TH_SWAPPED | TH_SW_COMING_IN);
if (thread->state & TH_RUN)
thread_setrun(thread, TRUE);
thread_unlock(thread);
(void) splx(s);
return KERN_SUCCESS;
}
static void __attribute__((noreturn)) swapin_thread_continue(void)
{
for (;;) {
thread_t thread;
spl_t s;
s = splsched();
swapper_lock();
while ((thread = (thread_t) dequeue_head(&swapin_queue))
!= THREAD_NULL) {
kern_return_t kr;
swapper_unlock();
(void) splx(s);
kr = thread_doswapin(thread);
s = splsched();
swapper_lock();
if (kr != KERN_SUCCESS) {
enqueue_head(&swapin_queue,
(queue_entry_t) thread);
break;
}
}
assert_wait((event_t) &swapin_queue, FALSE);
swapper_unlock();
(void) splx(s);
counter(c_swapin_thread_block++);
thread_block(swapin_thread_continue);
}
}
void swapin_thread(void)
{
current_thread()->vm_privilege = 1;
stack_privilege(current_thread());
swapin_thread_continue();
}