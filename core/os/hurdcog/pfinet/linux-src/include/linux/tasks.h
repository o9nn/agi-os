#ifndef _LINUX_TASKS_H
#define _LINUX_TASKS_H
#ifdef __SMP__
#define NR_CPUS	32
#else
#define NR_CPUS 1
#endif
#define NR_TASKS	512
#define MAX_TASKS_PER_USER (NR_TASKS/2)
#define MIN_TASKS_LEFT_FOR_ROOT 4
#define PID_MAX 0x8000
#endif