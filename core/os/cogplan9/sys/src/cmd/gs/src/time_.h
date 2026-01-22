#ifndef time__INCLUDED
#  define time__INCLUDED
#include "std.h"
#include "gconfig_.h"
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#  if defined(Plan9) || defined(M_UNIX) || defined(_IBMR2) || defined(_SEQUENT_) || defined(__GNUC__) || defined(__INTEL_COMPILER)
#    include <time.h>
#  endif
#else
#  include <time.h>
#  if !defined(__DECC) && !defined(__MWERKS__)
struct timeval {
long tv_sec, tv_usec;
};
#  endif
struct timezone {
int tz_minuteswest, tz_dsttime;
};
#endif
#if defined(ultrix) && defined(mips)
#endif
#ifdef SVR4_0
#  define gettimeofday_no_timezone 1
#else
#  define gettimeofday_no_timezone 0
#endif
#ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
#  define use_times_for_usertime 1
#  ifndef CLK_TCK
#    define CLK_TCK 100
#  endif
#else
#  define use_times_for_usertime 0
#endif
#endif