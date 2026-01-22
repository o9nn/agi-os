#ifndef _KERN_EVENTCOUNT_H_
#define _KERN_EVENTCOUNT_H_ 1
#include <kern/lock.h>
typedef struct evc {
int count;
thread_t waiting_thread;
natural_t ev_id;
struct evc *sanity;
decl_simple_lock_data(, lock)
} *evc_t;
extern void evc_init(evc_t ev),
evc_destroy(evc_t ev),
evc_signal(evc_t ev),
evc_notify_abort(thread_t thread);
extern kern_return_t evc_wait(natural_t ev_id);
extern kern_return_t evc_wait_clear(natural_t ev_id);
#if NCPUS <= 1
void simpler_thread_setrun(
thread_t th,
boolean_t may_preempt);
#endif
#endif