#ifndef _KERN_PRIORITY_H_
#define _KERN_PRIORITY_H_
extern void thread_quantum_update(
int mycpu,
thread_t thread,
int nticks,
int state);
#endif