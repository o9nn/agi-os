#ifndef _KERN_TIMER_H_
#define _KERN_TIMER_H_
#include <kern/macros.h>
#if STAT_TIME
#undef TIMER_MAX
#define TIMER_RATE 1000000
#define TIMER_HIGH_UNIT TIMER_RATE
#undef TIMER_ADJUST
#undef MACHINE_TIMER_ROUTINES
#else
#include <machine/timer.h>
#endif
struct timer {
unsigned low_bits;
unsigned high_bits;
unsigned high_bits_check;
unsigned tstamp;
};
typedef struct timer timer_data_t;
typedef struct timer *timer_t;
#define TIMER_LOW_FULL 0x80000000U
extern timer_t current_timer[NCPUS];
extern timer_data_t kernel_timer[NCPUS];
struct timer_save {
unsigned low;
unsigned high;
};
typedef struct timer_save timer_save_data_t, *timer_save_t;
#if STAT_TIME
#define start_timer(timer)
#define timer_switch(timer)
#else
extern void start_timer(timer_t);
extern void timer_switch(timer_t);
#endif
extern void timer_read(timer_t, time_value64_t *);
extern void thread_read_times(thread_t, time_value64_t *, time_value64_t *);
extern unsigned timer_delta(timer_t, timer_save_t);
extern void timer_normalize(timer_t);
extern void timer_init(timer_t);
#if STAT_TIME
#define timer_bump(timer, usec) \
MACRO_BEGIN \
(timer)->low_bits += usec; \
if ((timer)->low_bits & TIMER_LOW_FULL) { \
timer_normalize(timer); \
} \
MACRO_END
#else
extern void time_trap_uentry(unsigned);
extern void time_trap_uexit(int);
extern timer_t time_int_entry(unsigned, timer_t);
extern void time_int_exit(unsigned, timer_t);
#endif
#define TIMER_DELTA(timer, save, result) \
MACRO_BEGIN \
unsigned temp; \
\
temp = (timer).low_bits; \
if ((save).high != (timer).high_bits_check) { \
result += timer_delta(&(timer), &(save)); \
} \
else { \
result += temp - (save).low; \
(save).low = temp; \
} \
MACRO_END
extern void init_timers(void);
#if MACH_DEBUG
void db_thread_read_times(
thread_t thread,
time_value64_t *user_time_p,
time_value64_t *system_time_p);
#endif
#endif