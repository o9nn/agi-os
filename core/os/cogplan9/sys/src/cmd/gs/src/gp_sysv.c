#include <errno.h>
#include "stdio_.h"
#include "time_.h"
#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <sys/param.h>
int
rename(const char *a, const char *b)
{
if (access(a, 0) == -1)
return (-1);
unlink(b);
if (link(a, b) == -1)
return (-1);
if (unlink(a) == -1) {
unlink(b);
return (-1);
}
return (0);
}
#ifndef HZ
#  define	HZ	100
#endif
int
gettimeofday(struct timeval *tvp, struct timezone *tzp)
{
struct tms tms;
static long offset = 0;
long ticks;
if (!offset) {
time(&offset);
offset -= (times(&tms) / HZ);
}
ticks = times(&tms);
tvp->tv_sec = ticks / HZ + offset;
tvp->tv_usec = (ticks % HZ) * (1000 * 1000 / HZ);
return 0;
}