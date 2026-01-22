#ifndef _STRUCT_TIMESPEC
#define _STRUCT_TIMESPEC 1
#include <bits/types.h>
#include <bits/endian.h>
#include <bits/types/time_t.h>
struct timespec
{
#ifdef __USE_TIME_BITS64
__time64_t tv_sec;
#else
__time_t tv_sec;
#endif
#if __WORDSIZE == 64 \
|| (defined __SYSCALL_WORDSIZE && __SYSCALL_WORDSIZE == 64) \
|| (__TIMESIZE == 32 && !defined __USE_TIME_BITS64)
__syscall_slong_t tv_nsec;
#else
# if __BYTE_ORDER == __BIG_ENDIAN
int: 32;
long int tv_nsec;
# else
long int tv_nsec;
int: 32;
# endif
#endif
};
#endif