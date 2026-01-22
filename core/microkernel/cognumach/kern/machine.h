#ifndef _MACHINE_H_
#define _MACHINE_H_
#include <mach/std_types.h>
extern void cpu_up (int);
extern kern_return_t processor_assign (processor_t, processor_set_t, boolean_t);
extern kern_return_t processor_shutdown (processor_t);
extern void action_thread_continue (void) __attribute__((noreturn));
extern void action_thread(void) __attribute__((noreturn));
#endif