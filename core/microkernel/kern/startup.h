#ifndef _KERN_STARTUP_H_
#define _KERN_STARTUP_H_
#include <kern/thread.h>
extern void setup_main(void);
void cpu_launch_first_thread(thread_t th);
void start_kernel_threads(void);
#endif