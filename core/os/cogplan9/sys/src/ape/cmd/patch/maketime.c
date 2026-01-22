#if has_conf_h
# include <conf.h>
#else
# if HAVE_CONFIG_H
#  include <config.h>
# else
#  ifndef __STDC__
#   define const
#  endif
# endif
# include <sys/types.h>
# if HAVE_LIMITS_H
#  include <limits.h>
# endif
# ifndef LONG_MIN
# define LONG_MIN (-1-2147483647L)
# endif
# if STDC_HEADERS
#  include <stdlib.h>
# endif
# include <time.h>
# ifdef __STDC__
#  define P(x) x
# else
#  define P(x) ()
# endif
#endif
#include <partime.h>
#include <maketime.h>
char const maketId[] =
"$Id: maketime.c,v 5.15 1997/06/17 16:54:36 eggert Exp $";
static int isleap P ((int));
static int month_days P ((struct tm const *));
static time_t maketime P ((struct partime const *, time_t));
#define TM_YEAR_ORIGIN 1900
static int
isleap (y)
int y;
{
return (y & 3) == 0 && (y % 100 != 0 || y % 400 == 0);
}
static int const month_yday[] =
{
0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
};
static int
month_days (tm)
struct tm const *tm;
{
int m = tm->tm_mon;
return (month_yday[m + 1] - month_yday[m]
+ (m == 1 && isleap (tm->tm_year + TM_YEAR_ORIGIN)));
}
struct tm *
time2tm (unixtime, localzone)
time_t unixtime;
int localzone;
{
struct tm *tm;
#ifdef TZ_is_unset
static char const *TZ;
if (!TZ && !(TZ = getenv ("TZ")))
TZ_is_unset ("The TZ environment variable is not set; please set it to your timezone");
#endif
if (localzone || !(tm = gmtime (&unixtime)))
tm = localtime (&unixtime);
return tm;
}
time_t
difftm (a, b)
struct tm const *a;
struct tm const *b;
{
int ay = a->tm_year + (TM_YEAR_ORIGIN - 1);
int by = b->tm_year + (TM_YEAR_ORIGIN - 1);
int ac = ay / 100 - (ay % 100 < 0);
int bc = by / 100 - (by % 100 < 0);
int difference_in_day_of_year = a->tm_yday - b->tm_yday;
int intervening_leap_days = (((ay >> 2) - (by >> 2))
- (ac - bc)
+ ((ac >> 2) - (bc >> 2)));
time_t difference_in_years = ay - by;
time_t difference_in_days
= (difference_in_years * 365
+ (intervening_leap_days + difference_in_day_of_year));
return (((((difference_in_days * 24
+ (a->tm_hour - b->tm_hour))
* 60)
+ (a->tm_min - b->tm_min))
* 60)
+ (a->tm_sec - b->tm_sec));
}
void
adjzone (t, seconds)
register struct tm *t;
long seconds;
{
int leap_second = t->tm_sec == 60;
long sec = seconds + (t->tm_sec - leap_second);
if (sec < 0)
{
if ((t->tm_min -= (59 - sec) / 60) < 0)
{
if ((t->tm_hour -= (59 - t->tm_min) / 60) < 0)
{
t->tm_hour += 24;
if (TM_DEFINED (t->tm_wday) && --t->tm_wday < 0)
t->tm_wday = 6;
if (--t->tm_mday <= 0)
{
if (--t->tm_mon < 0)
{
--t->tm_year;
t->tm_mon = 11;
}
t->tm_mday = month_days (t);
}
}
t->tm_min += 24 * 60;
}
sec += 24L * 60 * 60;
}
else if (60 <= (t->tm_min += sec / 60))
if (24 <= (t->tm_hour += t->tm_min / 60))
{
t->tm_hour -= 24;
if (TM_DEFINED (t->tm_wday) && ++t->tm_wday == 7)
t->tm_wday = 0;
if (month_days (t) < ++t->tm_mday)
{
if (11 < ++t->tm_mon)
{
++t->tm_year;
t->tm_mon = 0;
}
t->tm_mday = 1;
}
}
t->tm_min %= 60;
t->tm_sec = (int) (sec % 60) + leap_second;
}
time_t
tm2time (tm, localzone)
struct tm *tm;
int localzone;
{
static time_t t_cache[2];
static struct tm tm_cache[2];
time_t d, gt;
struct tm const *gtm;
int remaining_tries = 8;
if (12 <= (unsigned) tm->tm_mon)
return -1;
tm->tm_yday = month_yday[tm->tm_mon] + tm->tm_mday
- (tm->tm_mon < 2 || !isleap (tm->tm_year + TM_YEAR_ORIGIN));
gt = t_cache[localzone];
gtm = gt ? &tm_cache[localzone] : time2tm (gt, localzone);
while ((d = difftm (tm, gtm)) != 0)
{
if (--remaining_tries == 0)
return -1;
gt += d;
gtm = time2tm (gt, localzone);
}
#define TM_DIFFER(a,b) \
( \
((a)->tm_year ^ (b)->tm_year) | \
((a)->tm_mon ^ (b)->tm_mon) | \
((a)->tm_mday ^ (b)->tm_mday) | \
((a)->tm_hour ^ (b)->tm_hour) | \
((a)->tm_min ^ (b)->tm_min) | \
((a)->tm_sec ^ (b)->tm_sec) \
)
if (TM_DIFFER (tm, gtm))
{
int yd = tm->tm_year - gtm->tm_year;
gt += yd + (yd ? 0 : tm->tm_mon - gtm->tm_mon);
gtm = time2tm (gt, localzone);
if (TM_DIFFER (tm, gtm))
return -1;
}
t_cache[localzone] = gt;
tm_cache[localzone] = *gtm;
tm->tm_wday = gtm->tm_wday;
return gt;
}
static time_t
maketime (pt, default_time)
struct partime const *pt;
time_t default_time;
{
int localzone, wday;
struct tm tm;
struct tm *tm0 = 0;
time_t r;
tm0 = 0;
localzone = pt->zone == TM_LOCAL_ZONE;
tm = pt->tm;
if (TM_DEFINED (pt->ymodulus) || !TM_DEFINED (tm.tm_year))
{
tm0 = time2tm (default_time, localzone);
if (!localzone)
adjzone (tm0, pt->zone);
}
if (TM_DEFINED (pt->ymodulus))
tm.tm_year +=
(tm0->tm_year + TM_YEAR_ORIGIN) / pt->ymodulus * pt->ymodulus;
else if (!TM_DEFINED (tm.tm_year))
{
tm.tm_year = tm0->tm_year + TM_YEAR_ORIGIN;
if (!TM_DEFINED (tm.tm_mon))
{
tm.tm_mon = tm0->tm_mon;
if (!TM_DEFINED (tm.tm_mday))
tm.tm_mday = tm0->tm_mday;
}
}
tm.tm_year -= TM_YEAR_ORIGIN;
if (!TM_DEFINED (tm.tm_mon))
tm.tm_mon = 0;
if (!TM_DEFINED (tm.tm_mday))
tm.tm_mday = 1;
if (!TM_DEFINED (tm.tm_hour))
tm.tm_hour = 0;
if (!TM_DEFINED (tm.tm_min))
tm.tm_min = 0;
if (!TM_DEFINED (tm.tm_sec))
tm.tm_sec = 0;
if (!localzone)
adjzone (&tm, -pt->zone);
wday = tm.tm_wday;
r = tm2time (&tm, localzone);
if (r != -1 && TM_DEFINED (wday) && wday != tm.tm_wday)
return -1;
return r;
}
time_t
str2time (source, default_time, default_zone)
char const *source;
time_t default_time;
long default_zone;
{
struct partime pt;
if (*partime (source, &pt))
return -1;
if (pt.zone == TM_UNDEFINED_ZONE)
pt.zone = default_zone;
return maketime (&pt, default_time);
}
#if TEST
#include <stdio.h>
int
main (argc, argv)
int argc;
char **argv;
{
time_t default_time = time ((time_t *) 0);
long default_zone = argv[1] ? atol (argv[1]) : 0;
char buf[1000];
while (fgets (buf, sizeof (buf), stdin))
{
time_t t = str2time (buf, default_time, default_zone);
printf ("%s", asctime (gmtime (&t)));
}
return 0;
}
#endif