#ifndef _BITS_PTHREADTYPES_COMMON_H
# define _BITS_PTHREADTYPES_COMMON_H	1
#include <bits/thread-shared-types.h>
typedef unsigned long int pthread_t;
typedef union
{
char __size[__SIZEOF_PTHREAD_MUTEXATTR_T];
int __align;
} pthread_mutexattr_t;
typedef union
{
char __size[__SIZEOF_PTHREAD_CONDATTR_T];
int __align;
} pthread_condattr_t;
typedef unsigned int pthread_key_t;
typedef int __ONCE_ALIGNMENT pthread_once_t;
union pthread_attr_t
{
char __size[__SIZEOF_PTHREAD_ATTR_T];
long int __align;
};
#ifndef __have_pthread_attr_t
typedef union pthread_attr_t pthread_attr_t;
# define __have_pthread_attr_t 1
#endif
typedef union
{
struct __pthread_mutex_s __data;
char __size[__SIZEOF_PTHREAD_MUTEX_T];
long int __align;
} pthread_mutex_t;
typedef union
{
struct __pthread_cond_s __data;
char __size[__SIZEOF_PTHREAD_COND_T];
__extension__ long long int __align;
} pthread_cond_t;
#if defined __USE_UNIX98 || defined __USE_XOPEN2K
typedef union
{
struct __pthread_rwlock_arch_t __data;
char __size[__SIZEOF_PTHREAD_RWLOCK_T];
long int __align;
} pthread_rwlock_t;
typedef union
{
char __size[__SIZEOF_PTHREAD_RWLOCKATTR_T];
long int __align;
} pthread_rwlockattr_t;
#endif
#ifdef __USE_XOPEN2K
typedef volatile int pthread_spinlock_t;
typedef union
{
char __size[__SIZEOF_PTHREAD_BARRIER_T];
long int __align;
} pthread_barrier_t;
typedef union
{
char __size[__SIZEOF_PTHREAD_BARRIERATTR_T];
int __align;
} pthread_barrierattr_t;
#endif
#endif