#ifndef	_SCHED_H
#define	_SCHED_H	1
#include <features.h>
#include <bits/types.h>
#define __need_size_t
#define __need_NULL
#include <stddef.h>
#include <bits/types/time_t.h>
#include <bits/types/struct_timespec.h>
#ifndef __USE_XOPEN2K
# include <time.h>
#endif
#ifndef __pid_t_defined
typedef __pid_t pid_t;
# define __pid_t_defined
#endif
#include <bits/sched.h>
#include <bits/cpu-set.h>
#define sched_priority    sched_priority
#define __sched_priority  sched_priority
__BEGIN_DECLS
extern int sched_setparam (__pid_t __pid, const struct sched_param *__param)
__THROW;
extern int sched_getparam (__pid_t __pid, struct sched_param *__param) __THROW;
extern int sched_setscheduler (__pid_t __pid, int __policy,
const struct sched_param *__param) __THROW;
extern int sched_getscheduler (__pid_t __pid) __THROW;
extern int sched_yield (void) __THROW;
extern int sched_get_priority_max (int __algorithm) __THROW;
extern int sched_get_priority_min (int __algorithm) __THROW;
#ifndef __USE_TIME_BITS64
extern int sched_rr_get_interval (__pid_t __pid, struct timespec *__t) __THROW;
#else
# ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (sched_rr_get_interval,
(__pid_t __pid, struct timespec *__t),
__sched_rr_get_interval64);
# else
#  define sched_rr_get_interval __sched_rr_get_interval64
# endif
#endif
#ifdef __USE_GNU
# define CPU_SETSIZE __CPU_SETSIZE
# define CPU_SET(cpu, cpusetp)	 __CPU_SET_S (cpu, sizeof (cpu_set_t), cpusetp)
# define CPU_CLR(cpu, cpusetp)	 __CPU_CLR_S (cpu, sizeof (cpu_set_t), cpusetp)
# define CPU_ISSET(cpu, cpusetp) __CPU_ISSET_S (cpu, sizeof (cpu_set_t), \
cpusetp)
# define CPU_ZERO(cpusetp)	 __CPU_ZERO_S (sizeof (cpu_set_t), cpusetp)
# define CPU_COUNT(cpusetp)	 __CPU_COUNT_S (sizeof (cpu_set_t), cpusetp)
# define CPU_SET_S(cpu, setsize, cpusetp)   __CPU_SET_S (cpu, setsize, cpusetp)
# define CPU_CLR_S(cpu, setsize, cpusetp)   __CPU_CLR_S (cpu, setsize, cpusetp)
# define CPU_ISSET_S(cpu, setsize, cpusetp) __CPU_ISSET_S (cpu, setsize, \
cpusetp)
# define CPU_ZERO_S(setsize, cpusetp)	    __CPU_ZERO_S (setsize, cpusetp)
# define CPU_COUNT_S(setsize, cpusetp)	    __CPU_COUNT_S (setsize, cpusetp)
# define CPU_EQUAL(cpusetp1, cpusetp2) \
__CPU_EQUAL_S (sizeof (cpu_set_t), cpusetp1, cpusetp2)
# define CPU_EQUAL_S(setsize, cpusetp1, cpusetp2) \
__CPU_EQUAL_S (setsize, cpusetp1, cpusetp2)
# define CPU_AND(destset, srcset1, srcset2) \
__CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, &)
# define CPU_OR(destset, srcset1, srcset2) \
__CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, |)
# define CPU_XOR(destset, srcset1, srcset2) \
__CPU_OP_S (sizeof (cpu_set_t), destset, srcset1, srcset2, ^)
# define CPU_AND_S(setsize, destset, srcset1, srcset2) \
__CPU_OP_S (setsize, destset, srcset1, srcset2, &)
# define CPU_OR_S(setsize, destset, srcset1, srcset2) \
__CPU_OP_S (setsize, destset, srcset1, srcset2, |)
# define CPU_XOR_S(setsize, destset, srcset1, srcset2) \
__CPU_OP_S (setsize, destset, srcset1, srcset2, ^)
# define CPU_ALLOC_SIZE(count) __CPU_ALLOC_SIZE (count)
# define CPU_ALLOC(count) __CPU_ALLOC (count)
# define CPU_FREE(cpuset) __CPU_FREE (cpuset)
extern int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
const cpu_set_t *__cpuset) __THROW;
extern int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
cpu_set_t *__cpuset) __THROW;
#endif
__END_DECLS
#endif