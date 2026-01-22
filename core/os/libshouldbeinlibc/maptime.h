#ifndef __MAPTIME_H__
#define __MAPTIME_H__
#include <mach/time_value.h>
#include <sys/time.h>
#include <errno.h>
#include <features.h>
#ifdef MAPTIME_DEFINE_EI
#define MAPTIME_EI
#else
#define MAPTIME_EI __extern_inline
#endif
error_t maptime_map (int use_mach_dev, char *dev_name,
volatile struct mapped_time_value **mtime);
extern void maptime_read (volatile struct mapped_time_value *mtime, struct timeval *tv);
#if defined(__USE_EXTERN_INLINES) || defined(MAPTIME_DEFINE_EI)
MAPTIME_EI void
maptime_read (volatile struct mapped_time_value *mtime, struct timeval *tv)
{
#ifdef HAVE_STRUCT_MAPPED_TIME_VALUE_TIME_VALUE_SECONDS
if (mtime->time_value.seconds != 0)
{
do
{
tv->tv_sec = mtime->time_value.seconds;
__sync_synchronize ();
tv->tv_usec = mtime->time_value.nanoseconds / 1000;
__sync_synchronize ();
}
while (tv->tv_sec != mtime->check_seconds64);
return;
}
#endif
do
{
tv->tv_sec = mtime->seconds;
__sync_synchronize ();
tv->tv_usec = mtime->microseconds;
__sync_synchronize ();
}
while (tv->tv_sec != mtime->check_seconds);
}
#endif
#endif