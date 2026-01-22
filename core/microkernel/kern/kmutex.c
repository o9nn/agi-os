#include <kern/kmutex.h>
#include <kern/atomic.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
void kmutex_init (struct kmutex *mtxp)
{
mtxp->state = KMUTEX_AVAIL;
simple_lock_init (&mtxp->lock);
}
kern_return_t kmutex_lock (struct kmutex *mtxp, boolean_t interruptible)
{
check_simple_locks ();
if (atomic_cas_acq (&mtxp->state, KMUTEX_AVAIL, KMUTEX_LOCKED))
return (KERN_SUCCESS);
simple_lock (&mtxp->lock);
if (atomic_swap_acq (&mtxp->state, KMUTEX_CONTENDED) == KMUTEX_AVAIL)
{
simple_unlock (&mtxp->lock);
return (KERN_SUCCESS);
}
thread_sleep ((event_t)mtxp, (simple_lock_t)&mtxp->lock, interruptible);
return (current_thread()->wait_result == THREAD_AWAKENED ?
KERN_SUCCESS : KERN_INTERRUPTED);
}
kern_return_t kmutex_trylock (struct kmutex *mtxp)
{
return (atomic_cas_acq (&mtxp->state, KMUTEX_AVAIL, KMUTEX_LOCKED) ?
KERN_SUCCESS : KERN_FAILURE);
}
void kmutex_unlock (struct kmutex *mtxp)
{
if (atomic_cas_rel (&mtxp->state, KMUTEX_LOCKED, KMUTEX_AVAIL))
return;
simple_lock (&mtxp->lock);
if (!thread_wakeup_one ((event_t)mtxp))
mtxp->state = KMUTEX_AVAIL;
simple_unlock (&mtxp->lock);
}