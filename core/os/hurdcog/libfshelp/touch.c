#include "fshelp.h"
void
fshelp_touch (struct stat *st, unsigned what,
volatile struct mapped_time_value *maptime)
{
struct timeval tv;
maptime_read (maptime, &tv);
if (what & TOUCH_ATIME)
{
st->st_atim.tv_sec = tv.tv_sec;
st->st_atim.tv_nsec = tv.tv_usec * 1000;
}
if (what & TOUCH_CTIME)
{
st->st_ctim.tv_sec = tv.tv_sec;
st->st_ctim.tv_nsec = tv.tv_usec * 1000;
}
if (what & TOUCH_MTIME)
{
st->st_mtim.tv_sec = tv.tv_sec;
st->st_mtim.tv_nsec = tv.tv_usec * 1000;
}
}