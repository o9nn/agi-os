#ifndef _TIMER_H_
#define _TIMER_H_
#include <errno.h>
#include <maptime.h>
error_t timer_init (void);
struct timer_list
{
struct timer_list *next, **prev;
long long expires;
int (*fnc) (void *);
void *fnc_data;
};
void timer_clear (struct timer_list *timer);
void timer_add (struct timer_list *timer);
int timer_remove (struct timer_list *timer);
void timer_change (struct timer_list *timer, long long expires);
static inline long long
fetch_jiffies (void)
{
extern volatile struct mapped_time_value *timer_mapped_time;
extern long long timer_root_jiffies;
struct timeval tv;
long long j;
maptime_read (timer_mapped_time, &tv);
#define HZ 100
j = (long long) tv.tv_sec * HZ + ((long long) tv.tv_usec * HZ) / 1000000;
return j - timer_root_jiffies;
}
#endif