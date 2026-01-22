#ifndef	_TIME_H
#define _TIME_H	1
#include <features.h>
#define __need_size_t
#define __need_NULL
#include <stddef.h>
#include <bits/time.h>
#include <bits/types/clock_t.h>
#include <bits/types/time_t.h>
#include <bits/types/struct_tm.h>
#if defined __USE_POSIX199309 || defined __USE_ISOC11
# include <bits/types/struct_timespec.h>
#endif
#ifdef __USE_POSIX199309
# include <bits/types/clockid_t.h>
# include <bits/types/timer_t.h>
# include <bits/types/struct_itimerspec.h>
struct sigevent;
#endif
#ifdef __USE_XOPEN2K
# ifndef __pid_t_defined
typedef __pid_t pid_t;
#  define __pid_t_defined
# endif
#endif
#ifdef __USE_XOPEN2K8
# include <bits/types/locale_t.h>
#endif
#ifdef __USE_ISOC11
# define TIME_UTC 1
#endif
__BEGIN_DECLS
extern clock_t clock (void) __THROW;
#ifndef __USE_TIME_BITS64
extern time_t time (time_t *__timer) __THROW;
extern double difftime (time_t __time1, time_t __time0)
__THROW __attribute__ ((__const__));
extern time_t mktime (struct tm *__tp) __THROW;
#else
# ifdef __REDIRECT_NTH
extern time_t __REDIRECT_NTH (time, (time_t *__timer), __time64);
extern double __REDIRECT_NTH (difftime, (time_t __time1, time_t __time0),
__difftime64) __attribute__ ((__const__));
extern time_t __REDIRECT_NTH (mktime, (struct tm *__tp), __mktime64);
# else
#  define time __time64
#  define difftime __difftime64
#  define mktime __mktime64
# endif
#endif
extern size_t strftime (char *__restrict __s, size_t __maxsize,
const char *__restrict __format,
const struct tm *__restrict __tp)
__THROW __nonnull((1, 3, 4));
#ifdef __USE_XOPEN
extern char *strptime (const char *__restrict __s,
const char *__restrict __fmt, struct tm *__tp)
__THROW;
#endif
#ifdef __USE_XOPEN2K8
extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
const char *__restrict __format,
const struct tm *__restrict __tp,
locale_t __loc) __THROW;
#endif
#ifdef __USE_GNU
extern char *strptime_l (const char *__restrict __s,
const char *__restrict __fmt, struct tm *__tp,
locale_t __loc) __THROW;
#endif
#ifndef __USE_TIME_BITS64
extern struct tm *gmtime (const time_t *__timer) __THROW;
extern struct tm *localtime (const time_t *__timer) __THROW;
#else
# ifdef __REDIRECT_NTH
extern struct tm*__REDIRECT_NTH (gmtime, (const time_t *__timer), __gmtime64);
extern struct tm *__REDIRECT_NTH (localtime, (const time_t *__timer),
__localtime64);
# else
#  define gmtime __gmtime64
#  define localtime __localtime64
# endif
#endif
#if defined __USE_POSIX || __GLIBC_USE (ISOC2X)
# ifndef __USE_TIME_BITS64
extern struct tm *gmtime_r (const time_t *__restrict __timer,
struct tm *__restrict __tp) __THROW;
extern struct tm *localtime_r (const time_t *__restrict __timer,
struct tm *__restrict __tp) __THROW;
# else
#  ifdef __REDIRECT_NTH
extern struct tm*__REDIRECT_NTH (gmtime_r, (const time_t *__restrict __timer,
struct tm *__restrict __tp),
__gmtime64_r);
extern struct tm*__REDIRECT_NTH (localtime_r, (const time_t *__restrict __t,
struct tm *__restrict __tp),
__localtime64_r);
#  else
#   define gmtime_r __gmtime64_r
#   define localtime_r __localtime_r
#  endif
# endif
#endif
extern char *asctime (const struct tm *__tp) __THROW;
#ifndef __USE_TIME_BITS64
extern char *ctime (const time_t *__timer) __THROW;
#else
# ifdef __REDIRECT_NTH
extern char *__REDIRECT_NTH (ctime, (const time_t *__timer), __ctime64);
# else
#  define ctime __ctime64
# endif
#endif
#ifdef __USE_POSIX
extern char *asctime_r (const struct tm *__restrict __tp,
char *__restrict __buf) __THROW;
#ifndef __USE_TIME_BITS64
extern char *ctime_r (const time_t *__restrict __timer,
char *__restrict __buf) __THROW;
#else
# ifdef __REDIRECT_NTH
extern char *__REDIRECT_NTH (ctime_r, (const time_t *__restrict __timer,
char *__restrict __buf), __ctime64_r);
# else
#  define ctime_r __ctime64_r
# endif
#endif
#endif
extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;
#ifdef	__USE_POSIX
extern char *tzname[2];
extern void tzset (void) __THROW;
#endif
#if defined __USE_MISC || defined __USE_XOPEN
extern int daylight;
extern long int timezone;
#endif
#define __isleap(year)	\
((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))
#if defined __USE_MISC || __GLIBC_USE (ISOC2X)
# ifndef __USE_TIME_BITS64
extern time_t timegm (struct tm *__tp) __THROW;
# else
#  ifdef __REDIRECT_NTH
extern time_t __REDIRECT_NTH (timegm, (struct tm *__tp), __timegm64);
#  else
#   define timegm __timegm64
#  endif
# endif
#endif
#ifdef __USE_MISC
#ifndef __USE_TIME_BITS64
extern time_t timelocal (struct tm *__tp) __THROW;
#else
# ifdef __REDIRECT_NTH
extern time_t __REDIRECT_NTH (timelocal, (struct tm *__tp), __mktime64);
# endif
#endif
extern int dysize (int __year) __THROW  __attribute__ ((__const__));
#endif
#ifdef __USE_POSIX199309
# ifndef __USE_TIME_BITS64
extern int nanosleep (const struct timespec *__requested_time,
struct timespec *__remaining);
extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __THROW;
extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp)
__THROW __nonnull((2));
extern int clock_settime (clockid_t __clock_id, const struct timespec *__tp)
__THROW __nonnull((2));
# else
#  ifdef __REDIRECT
extern int __REDIRECT (nanosleep, (const struct timespec *__requested_time,
struct timespec *__remaining),
__nanosleep64);
extern int __REDIRECT_NTH (clock_getres, (clockid_t __clock_id,
struct timespec *__res),
__clock_getres64);
extern int __REDIRECT_NTH (clock_gettime, (clockid_t __clock_id, struct
timespec *__tp), __clock_gettime64)
__nonnull((2));
extern int __REDIRECT_NTH (clock_settime, (clockid_t __clock_id, const struct
timespec *__tp), __clock_settime64)
__nonnull((2));
#  else
#   define nanosleep __nanosleep64
#   define clock_getres __clock_getres64
#   define clock_gettime __clock_gettime64
#   define clock_settime __clock_settime64
#  endif
# endif
# ifdef __USE_XOPEN2K
#  ifndef __USE_TIME_BITS64
extern int clock_nanosleep (clockid_t __clock_id, int __flags,
const struct timespec *__req,
struct timespec *__rem);
#  else
#   ifdef __REDIRECT
extern int __REDIRECT (clock_nanosleep, (clockid_t __clock_id, int __flags,
const struct timespec *__req,
struct timespec *__rem),
__clock_nanosleep_time64);
#   else
#    define clock_nanosleep __clock_nanosleep_time64
#   endif
#  endif
extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __THROW;
# endif
extern int timer_create (clockid_t __clock_id,
struct sigevent *__restrict __evp,
timer_t *__restrict __timerid) __THROW;
extern int timer_delete (timer_t __timerid) __THROW;
# ifndef __USE_TIME_BITS64
extern int timer_settime (timer_t __timerid, int __flags,
const struct itimerspec *__restrict __value,
struct itimerspec *__restrict __ovalue) __THROW;
extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
__THROW;
# else
#  ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (timer_settime, (timer_t __timerid, int __flags,
const struct itimerspec *__restrict __value,
struct itimerspec *__restrict __ovalue),
__timer_settime64);
extern int __REDIRECT_NTH (timer_gettime, (timer_t __timerid,
struct itimerspec *__value),
__timer_gettime64);
#  else
#   define timer_settime __timer_settime64
#   define timer_gettime __timer_gettime64
#  endif
# endif
extern int timer_getoverrun (timer_t __timerid) __THROW;
#endif
#ifdef __USE_ISOC11
# ifndef __USE_TIME_BITS64
extern int timespec_get (struct timespec *__ts, int __base)
__THROW __nonnull ((1));
# else
#  ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (timespec_get, (struct timespec *__ts, int __base),
__timespec_get64) __nonnull ((1));
#  else
#   define timespec_get __timespec_get64
#  endif
# endif
#endif
#if __GLIBC_USE (ISOC2X)
# ifndef __USE_TIME_BITS64
extern int timespec_getres (struct timespec *__ts, int __base)
__THROW;
# else
#  ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (timespec_getres, (struct timespec *__ts,
int __base),
__timespec_getres64);
#  else
#   define timespec_getres __timespec_getres64
#  endif
# endif
#endif
#ifdef __USE_XOPEN_EXTENDED
extern int getdate_err;
extern struct tm *getdate (const char *__string);
#endif
#ifdef __USE_GNU
extern int getdate_r (const char *__restrict __string,
struct tm *__restrict __resbufp);
#endif
__END_DECLS
#endif