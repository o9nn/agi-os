#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include "timefmt.h"
#define SECOND 1
#define MINUTE 60
#define HOUR (60*MINUTE)
#define DAY (24*HOUR)
#define WEEK (7*DAY)
#define MONTH (31*DAY)
#define YEAR (365*DAY)
static unsigned
int_len (unsigned n)
{
unsigned len = 1;
while (n >= 10)
{
n /= 10;
len++;
}
return len;
}
static unsigned
tv_div (struct timeval *tv1, struct timeval *tv2)
{
return
tv2->tv_sec
? tv1->tv_sec / tv2->tv_sec
: (tv1->tv_usec / tv2->tv_usec
+ (tv1->tv_sec ? tv1->tv_sec * 1000000 / tv2->tv_usec : 0));
}
static inline int
tv_is_zero (struct timeval *tv)
{
return tv->tv_sec == 0 && tv->tv_usec == 0;
}
static inline int
tv_is_ge (struct timeval *tv1, struct timeval *tv2)
{
return
tv1->tv_sec > tv2->tv_sec
|| (tv1->tv_sec == tv2->tv_sec && tv1->tv_usec >= tv2->tv_usec);
}
size_t
fmt_named_interval (struct timeval *tv, size_t width,
char *buf, size_t buf_len)
{
struct tscale
{
struct timeval thresh;
struct timeval unit;
struct timeval frac_thresh;
char *sfxs[5];
}
time_scales[] =
{
{{2*YEAR, 0}, {YEAR, 0}, {MONTH, 0},{" years", "years", "yrs", "y", 0 }},
{{3*MONTH, 0}, {MONTH, 0}, {WEEK, 0}, {" months","months","mo", 0 }},
{{2*WEEK, 0}, {WEEK, 0}, {DAY, 0}, {" weeks", "weeks", "wks", "w", 0 }},
{{2*DAY, 0}, {DAY, 0}, {HOUR, 0}, {" days", "days", "dys", "d", 0 }},
{{2*HOUR, 0}, {HOUR, 0}, {MINUTE, 0},{" hours","hours", "hrs", "h", 0 }},
{{2*MINUTE, 0},{MINUTE, 0},{1, 0}, {" minutes","min", "mi", "m", 0 }},
{{1, 100000}, {1, 0}, {0, 100000},{" seconds", "sec", "s", 0 }},
{{1, 0}, {1, 0}, {0, 0}, {" second", "sec", "s", 0 }},
{{0, 1100}, {0, 1000}, {0, 100}, {" milliseconds", "ms", 0 }},
{{0, 1000}, {0, 1000}, {0, 0}, {" millisecond", "ms", 0 }},
{{0, 2}, {0, 1}, {0, 0}, {" microseconds", "us", 0 }},
{{0, 1}, {0, 1}, {0, 0}, {" microsecond", "us", 0 }},
{{0, 0} }
};
struct tscale *ts;
if (width <= 0 || width >= buf_len)
width = buf_len - 1;
for (ts = time_scales; !tv_is_zero (&ts->thresh); ts++)
if (tv_is_ge (tv, &ts->thresh))
{
char **sfx;
struct timeval *u = &ts->unit;
unsigned num = tv_div (tv, u);
unsigned frac = 0;
unsigned num_len = int_len (num);
if (num < 10
&& !tv_is_zero (&ts->frac_thresh)
&& tv_is_ge (tv, &ts->frac_thresh))
{
struct timeval tv10 =
{ tv->tv_sec * 10 + tv->tv_usec / 100000,
(tv->tv_usec % 100000) * 10 };
frac = tv_div (&tv10, u) - num * 10;
if (frac)
num_len += 2;
}
for (sfx = ts->sfxs; sfx[1]; sfx++)
if (num_len + strlen (*sfx) <= width)
break;
if (!sfx[1] && frac)
{
num_len -= 2;
frac = 0;
for (sfx = ts->sfxs; sfx[1]; sfx++)
if (num_len + strlen (*sfx) <= width)
break;
}
if (!sfx[1])
sfx--;
if (frac)
return snprintf (buf, buf_len, "%d.%d%s", num, frac, *sfx);
else
return snprintf (buf, buf_len, "%d%s", num, *sfx);
}
return sprintf (buf, "0");
}
static size_t
add_field (int *secs, int unit, int *leading_zeros,
size_t min_width, char *suffix,
size_t width, char *buf)
{
int units = *secs / unit;
if (units || (width >= min_width && *leading_zeros))
{
*secs -= units * unit;
*leading_zeros = 1;
return
sprintf (buf,
(width == min_width ? "%d%s"
: width == min_width + 1 ? "%2d%s"
: "%02d%s"),
units, suffix);
}
else
return 0;
}
size_t
fmt_seconds (struct timeval *tv, int leading_zeros, int frac_places,
size_t width, char *buf, size_t buf_len)
{
char *p = buf;
int secs = tv->tv_sec;
if (width <= 0 || width >= buf_len)
width = buf_len - 1;
if (tv->tv_sec > DAY)
return fmt_named_interval (tv, width, buf, buf_len);
if (frac_places > 0)
width -= frac_places + 1;
if ((secs > 10*HOUR && width < 8)
|| (secs > HOUR && width < 7)
|| (secs > 10*MINUTE && width < 5)
|| (secs > MINUTE && width < 4)
|| (secs > 10 && width < 2)
|| width < 1)
return fmt_named_interval (tv, width, buf, buf_len);
p += add_field (&secs, HOUR, &leading_zeros, 7, ":", width, p);
p += add_field (&secs, MINUTE, &leading_zeros, 4, ":", width, p);
p += add_field (&secs, SECOND, &leading_zeros, 1, "", width, p);
if (frac_places < 0 && (p - buf) < (int) width - 2)
frac_places = width - (p - buf) - 1;
if (frac_places > 0)
{
int frac = tv->tv_usec, i;
for (i = 6; i > frac_places; i--)
frac /= 10;
return (p - buf) + sprintf (p, ".%0*d", frac_places, frac);
}
else
return (p - buf);
}
size_t
fmt_minutes (struct timeval *tv, int leading_zeros,
size_t width, char *buf, size_t buf_len)
{
char *p = buf;
int secs = tv->tv_sec;
if (width <= 0 || width >= buf_len)
width = buf_len - 1;
if (secs > DAY)
return fmt_named_interval (tv, width, buf, buf_len);
if ((secs > 10*HOUR && width < 5)
|| (secs > HOUR && width < 4)
|| (secs > 10*MINUTE && width < 2)
|| width < 1)
return fmt_named_interval (tv, width, buf, buf_len);
p += add_field (&secs, HOUR, &leading_zeros, 4, ":", width, p);
p += add_field (&secs, MINUTE, &leading_zeros, 1, "", width, p);
return p - buf;
}
size_t
fmt_past_time (struct timeval *tv, struct timeval *now,
size_t width, char *buf, size_t buf_len)
{
static char *time_fmts[] = { "%-r", "%-l:%M%p", "%-l%p", 0 };
static char *week_fmts[] = { "%A", "%a", 0 };
static char *month_fmts[] = { "%A %-d", "%a %-d", "%a%-d", 0 };
static char *date_fmts[] =
{ "%A, %-d %B", "%a, %-d %b", "%-d %B", "%-d %b", "%-d%b", 0 };
static char *year_fmts[] =
{ "%A, %-d %B %Y", "%a, %-d %b %Y", "%a, %-d %b %y", "%-d %b %y", "%-d%b%y", 0 };
struct tm tm;
int used = 0;
long diff = now ? (now->tv_sec - tv->tv_sec) : tv->tv_sec;
if (diff < 0)
diff = -diff;
memcpy (&tm, localtime ((time_t *) &tv->tv_sec), sizeof tm);
if (width <= 0 || width >= buf_len)
width = buf_len - 1;
if (diff < DAY)
{
char **fmt;
for (fmt = time_fmts; *fmt && !used; fmt++)
used = strftime (buf, width + 1, *fmt, &tm);
if (! used)
used = strftime (buf, buf_len, fmt[-1], &tm);
}
else
{
static char *seps[] = { ", ", " ", "", 0 };
char **fmt, **dfmt, **dfmts, **sep;
if (diff < WEEK)
dfmts = week_fmts;
else if (diff < MONTH)
dfmts = month_fmts;
else if (diff < YEAR)
dfmts = date_fmts;
else
dfmts = year_fmts;
for (fmt = time_fmts; *fmt && !used; fmt++)
for (sep = seps; *sep && !used; sep++)
for (dfmt = dfmts; *dfmt && !used; dfmt++)
{
char whole_fmt[strlen (*dfmt) + strlen (*sep) + strlen (*fmt) + 1];
char *end = whole_fmt;
end = stpcpy (end, *dfmt);
end = stpcpy (end, *sep);
stpcpy (end, *fmt);
used = strftime (buf, width + 1, whole_fmt, &tm);
}
if (! used)
for (dfmt = dfmts; *dfmt && !used; dfmt++)
used = strftime (buf, width + 1, *dfmt, &tm);
if (! used)
used = strftime (buf, buf_len, dfmt[-1], &tm);
}
return used;
}