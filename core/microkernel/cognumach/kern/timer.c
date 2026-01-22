#include <mach/kern_return.h>
#include <mach/port.h>
#include <kern/queue.h>
#include <kern/thread.h>
#include <mach/time_value.h>
#include <kern/timer.h>
#include <kern/cpu_number.h>
#include <kern/constants.h>
#include <kern/assert.h>
#include <kern/macros.h>
timer_t current_timer[NCPUS];
timer_data_t kernel_timer[NCPUS];
void init_timers(void)
{
int i;
timer_t this_timer;
this_timer = &kernel_timer[0];
for ( i=0 ; i<NCPUS ; i++, this_timer++) {
timer_init(this_timer);
current_timer[i] = (timer_t) 0;
}
start_timer(&kernel_timer[cpu_number()]);
}
void timer_init(timer_t this_timer)
{
this_timer->low_bits = 0;
this_timer->high_bits = 0;
this_timer->tstamp = 0;
this_timer->high_bits_check = 0;
}
#ifdef TICKLESS_TIMER
static inline void
timer_update_optimized(timer_t timer, int elapsed)
{
timer->low_bits += elapsed;
if (__builtin_expect(timer->low_bits & TIMER_LOW_FULL, 0)) {
timer_normalize(timer);
}
}
#endif
#if STAT_TIME
#else
#ifdef MACHINE_TIMER_ROUTINES
#else
void
start_timer(timer_t timer)
{
timer->tstamp = get_timestamp();
current_timer[cpu_number()] = timer;
}
void
time_trap_uentry(unsigned ts)
{
int elapsed;
int mycpu;
timer_t mytimer;
mycpu = cpu_number();
mytimer = current_timer[mycpu];
elapsed = ts - mytimer->tstamp;
#ifdef TIMER_MAX
if (elapsed < 0) elapsed += TIMER_MAX;
#endif
#ifdef TICKLESS_TIMER
timer_update_optimized(mytimer, elapsed);
#else
mytimer->low_bits += elapsed;
if (mytimer->low_bits & TIMER_LOW_FULL) {
timer_normalize(mytimer);
}
#endif
mytimer->tstamp = 0;
mytimer = &(current_thread()->system_timer);
current_timer[mycpu] = mytimer;
mytimer->tstamp = ts;
}
void
time_trap_uexit(int ts)
{
int elapsed;
int mycpu;
timer_t mytimer;
mycpu = cpu_number();
mytimer = current_timer[mycpu];
elapsed = ts - mytimer->tstamp;
#ifdef TIMER_MAX
if (elapsed < 0) elapsed += TIMER_MAX;
#endif
mytimer->low_bits += elapsed;
mytimer->tstamp = 0;
if (mytimer->low_bits & TIMER_LOW_FULL) {
timer_normalize(mytimer);
}
mytimer = &(current_thread()->user_timer);
current_timer[mycpu] = mytimer;
mytimer->tstamp = ts;
}
timer_t
time_int_entry(
unsigned ts,
timer_t new_timer)
{
int elapsed;
int mycpu;
timer_t mytimer;
mycpu = cpu_number();
mytimer = current_timer[mycpu];
elapsed = ts - mytimer->tstamp;
#ifdef TIMER_MAX
if (elapsed < 0) elapsed += TIMER_MAX;
#endif
mytimer->low_bits += elapsed;
mytimer->tstamp = 0;
new_timer->tstamp = ts;
current_timer[mycpu] = new_timer;
return(mytimer);
}
void
time_int_exit(
unsigned ts,
timer_t old_timer)
{
int elapsed;
int mycpu;
timer_t mytimer;
mycpu = cpu_number();
mytimer = current_timer[mycpu];
elapsed = ts - mytimer->tstamp;
#ifdef TIMER_MAX
if (elapsed < 0) elapsed += TIMER_MAX;
#endif
mytimer->low_bits += elapsed;
mytimer->tstamp = 0;
if (mytimer->low_bits & TIMER_LOW_FULL) {
timer_normalize(mytimer);
}
if (old_timer->low_bits & TIMER_LOW_FULL) {
timer_normalize(old_timer);
}
old_timer->tstamp = ts;
current_timer[mycpu] = old_timer;
}
void
timer_switch(timer_t new_timer)
{
int elapsed;
int mycpu;
timer_t mytimer;
unsigned ts;
mycpu = cpu_number();
mytimer = current_timer[mycpu];
ts = get_timestamp();
elapsed = ts - mytimer->tstamp;
#ifdef TIMER_MAX
if (elapsed < 0) elapsed += TIMER_MAX;
#endif
mytimer->low_bits += elapsed;
mytimer->tstamp = 0;
if (mytimer->low_bits & TIMER_LOW_FULL) {
timer_normalize(mytimer);
}
current_timer[mycpu] = new_timer;
new_timer->tstamp = ts;
}
#endif
#endif
void timer_normalize(timer_t timer)
{
unsigned int high_increment;
high_increment = timer->low_bits/TIMER_HIGH_UNIT;
timer->high_bits_check += high_increment;
__sync_synchronize();
timer->low_bits %= TIMER_HIGH_UNIT;
__sync_synchronize();
timer->high_bits += high_increment;
}
static void timer_grab(
timer_t timer,
timer_save_t save)
{
#if MACH_ASSERT
unsigned int passes=0;
#endif
do {
(save)->high = (timer)->high_bits;
__sync_synchronize ();
(save)->low = (timer)->low_bits;
__sync_synchronize ();
#if MACH_ASSERT
passes++;
assert((passes < 10000) ? (1) : ((timer->high_bits_check = save->high), 0));
#endif
} while ( (save)->high != (timer)->high_bits_check);
}
#define TIMER_TO_TIME_VALUE64(tv, timer) \
MACRO_BEGIN \
(tv)->seconds = (timer)->high + (timer)->low / MICROSECONDS_PER_SECOND; \
(tv)->nanoseconds = (timer)->low % MICROSECONDS_PER_SECOND * 1000; \
MACRO_END
void
timer_read(
timer_t timer,
time_value64_t *tv)
{
timer_save_data_t temp;
timer_grab(timer,&temp);
#ifdef TIMER_ADJUST
TIMER_ADJUST(&temp);
#endif
TIMER_TO_TIME_VALUE64(tv, &temp);
}
void thread_read_times(
thread_t thread,
time_value64_t *user_time_p,
time_value64_t *system_time_p)
{
timer_read(&thread->user_timer, user_time_p);
timer_read(&thread->system_timer, system_time_p);
}
#if MACH_DEBUG
static void db_timer_grab(
timer_t timer,
timer_save_t save)
{
(save)->high = (timer)->high_bits;
(save)->low = (timer)->low_bits;
}
static void
nonblocking_timer_read(
timer_t timer,
time_value64_t *tv)
{
timer_save_data_t temp;
db_timer_grab(timer, &temp);
#ifdef TIMER_ADJUST
TIMER_ADJUST(&temp);
#endif
TIMER_TO_TIME_VALUE64(tv, &temp);
}
void db_thread_read_times(
thread_t thread,
time_value64_t *user_time_p,
time_value64_t *system_time_p)
{
nonblocking_timer_read(&thread->user_timer, user_time_p);
nonblocking_timer_read(&thread->system_timer, system_time_p);
}
#endif
unsigned
timer_delta(
timer_t timer,
timer_save_t save)
{
timer_save_data_t new_save;
unsigned result;
timer_grab(timer,&new_save);
result = (new_save.high - save->high) * TIMER_HIGH_UNIT +
new_save.low - save->low;
save->high = new_save.high;
save->low = new_save.low;
return(result);
}