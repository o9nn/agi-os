#ifndef _RWLOCK_INTERNAL_H
#define _RWLOCK_INTERNAL_H
struct __pthread_rwlock_arch_t
{
unsigned int __readers;
unsigned int __writers;
unsigned int __wrphase_futex;
unsigned int __writers_futex;
unsigned int __pad3;
unsigned int __pad4;
#ifdef __x86_64__
int __cur_writer;
int __shared;
signed char __rwelision;
# ifdef  __ILP32__
unsigned char __pad1[3];
#  define __PTHREAD_RWLOCK_ELISION_EXTRA 0, { 0, 0, 0 }
# else
unsigned char __pad1[7];
#  define __PTHREAD_RWLOCK_ELISION_EXTRA 0, { 0, 0, 0, 0, 0, 0, 0 }
# endif
unsigned long int __pad2;
unsigned int __flags;
#else
unsigned char __flags;
unsigned char __shared;
signed char __rwelision;
unsigned char __pad2;
int __cur_writer;
#endif
};
#ifdef __x86_64__
# define __PTHREAD_RWLOCK_INITIALIZER(__flags) \
0, 0, 0, 0, 0, 0, 0, 0, __PTHREAD_RWLOCK_ELISION_EXTRA, 0, __flags
#else
# define __PTHREAD_RWLOCK_INITIALIZER(__flags) \
0, 0, 0, 0, 0, 0, __flags, 0, 0, 0, 0
#endif
#endif