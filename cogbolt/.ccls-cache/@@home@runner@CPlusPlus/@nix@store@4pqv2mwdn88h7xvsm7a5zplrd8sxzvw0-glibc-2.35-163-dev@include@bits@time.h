#ifndef _BITS_TIME_H
#define _BITS_TIME_H	1
#include <bits/types.h>
#define CLOCKS_PER_SEC  ((__clock_t) 1000000)
#if (!defined __STRICT_ANSI__ || defined __USE_POSIX) \
&& !defined __USE_XOPEN2K
extern long int __sysconf (int);
# define CLK_TCK ((__clock_t) __sysconf (2))
#endif
#ifdef __USE_POSIX199309
# define CLOCK_REALTIME			0
# define CLOCK_MONOTONIC		1
# define CLOCK_PROCESS_CPUTIME_ID	2
# define CLOCK_THREAD_CPUTIME_ID	3
# define CLOCK_MONOTONIC_RAW		4
# define CLOCK_REALTIME_COARSE		5
# define CLOCK_MONOTONIC_COARSE		6
# define CLOCK_BOOTTIME			7
# define CLOCK_REALTIME_ALARM		8
# define CLOCK_BOOTTIME_ALARM		9
# define CLOCK_TAI			11
# define TIMER_ABSTIME			1
#endif
#ifdef __USE_GNU
# include <bits/timex.h>
__BEGIN_DECLS
extern int clock_adjtime (__clockid_t __clock_id, struct timex *__utx) __THROW;
#ifdef __USE_TIME_BITS64
# if defined(__REDIRECT_NTH)
extern int __REDIRECT_NTH (clock_adjtime, (__clockid_t __clock_id,
struct timex *__utx),
__clock_adjtime64);
# else
# define clock_adjtime __clock_adjtime64
# endif
#endif
__END_DECLS
#endif
#endif