#ifndef __timeval_defined
#define __timeval_defined 1
#include <bits/types.h>
struct timeval
{
#ifdef __USE_TIME_BITS64
__time64_t tv_sec;
__suseconds64_t tv_usec;
#else
__time_t tv_sec;
__suseconds_t tv_usec;
#endif
};
#endif