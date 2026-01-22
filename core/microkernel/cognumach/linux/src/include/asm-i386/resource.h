#ifndef _I386_RESOURCE_H
#define _I386_RESOURCE_H
#define RLIMIT_CPU	0
#define RLIMIT_FSIZE	1
#define RLIMIT_DATA	2
#define RLIMIT_STACK	3
#define RLIMIT_CORE	4
#define RLIMIT_RSS	5
#define RLIMIT_NPROC	6
#define RLIMIT_NOFILE	7
#define RLIMIT_MEMLOCK	8
#define RLIMIT_AS	9
#define RLIM_NLIMITS	10
#ifdef __KERNEL__
#define INIT_RLIMITS					\
{							\
{ LONG_MAX, LONG_MAX },				\
{ LONG_MAX, LONG_MAX },				\
{ LONG_MAX, LONG_MAX },				\
{ _STK_LIM, _STK_LIM },				\
{        0, LONG_MAX },				\
{ LONG_MAX, LONG_MAX },				\
{ MAX_TASKS_PER_USER, MAX_TASKS_PER_USER },	\
{ NR_OPEN, NR_OPEN },				\
{ LONG_MAX, LONG_MAX },				\
{ LONG_MAX, LONG_MAX },				\
}
#endif
#endif