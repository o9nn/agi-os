#ifndef _KERN_KERN_TYPES_H_
#define _KERN_KERN_TYPES_H_
#include <mach/port.h>
typedef struct task * task_t;
#define TASK_NULL ((task_t) 0)
typedef mach_port_t * task_array_t;
typedef struct thread * thread_t;
#define THREAD_NULL ((thread_t) 0)
typedef mach_port_t * thread_array_t;
typedef struct processor * processor_t;
#define PROCESSOR_NULL ((processor_t) 0)
typedef struct processor_set * processor_set_t;
#define PROCESSOR_SET_NULL ((processor_set_t) 0)
#endif