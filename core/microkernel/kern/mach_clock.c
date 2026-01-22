#include <string.h>
#include <mach/boolean.h>
#include <mach/machine.h>
#include <mach/time_value.h>
#include <mach/vm_param.h>
#include <mach/vm_prot.h>
#include <kern/counters.h>
#include "cpu_number.h"
#include <kern/debug.h>
#include <kern/host.h>
#include <kern/lock.h>
#include <kern/mach_clock.h>
#include <kern/mach_host.server.h>
#include <kern/processor.h>
#include <kern/queue.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <kern/timer.h>
#include <kern/priority.h>
#include <vm/vm_kern.h>
#include <machine/mach_param.h>
#include <machine/spl.h>
#include <machine/model_dep.h>
#if MACH_PCSAMPLE
#include <kern/pc_sample.h>
#endif
#include <kern/constants.h>
#define MICROSECONDS_IN_ONE_SECOND MICROSECONDS_PER_SECOND
int hz = HZ;
int tick = (MICROSECONDS_IN_ONE_SECOND / HZ);
time_value64_t time = { 0, 0 };
time_value64_t uptime = { 0, 0 };
unsigned long elapsed_ticks = 0;
int timedelta = 0;
int tickdelta = 0;
#if HZ > 500
unsigned tickadj = 1;
#else
unsigned tickadj = 500 / HZ;
#endif
unsigned bigadj = MICROSECONDS_PER_SECOND;
uint32_t last_hpc_read = 0;
volatile mapped_time_value_t *mtime = 0;
#define update_mapped_time(time) \
MACRO_BEGIN \
if (mtime != 0) { \
mtime->check_seconds = (time)->seconds; \
mtime->check_seconds64 = (time)->seconds; \
__sync_synchronize(); \
mtime->microseconds = (time)->nanoseconds / 1000; \
mtime->time_value.nanoseconds = (time)->nanoseconds; \
__sync_synchronize(); \
mtime->seconds = (time)->seconds; \
mtime->time_value.seconds = (time)->seconds; \
} \
MACRO_END
#define update_mapped_uptime(uptime) \
MACRO_BEGIN \
if (mtime != 0) { \
mtime->check_upseconds64 = (uptime)->seconds; \
__sync_synchronize(); \
mtime->uptime_value.nanoseconds = (uptime)->nanoseconds;\
__sync_synchronize(); \
mtime->uptime_value.seconds = (uptime)->seconds; \
} \
MACRO_END
#define read_mapped_time(time) \
MACRO_BEGIN \
do { \
(time)->seconds = mtime->time_value.seconds; \
__sync_synchronize(); \
(time)->nanoseconds = mtime->time_value.nanoseconds; \
__sync_synchronize(); \
} while ((time)->seconds != mtime->check_seconds64); \
time_value64_add_hpc(time); \
MACRO_END
#define read_mapped_uptime(uptime) \
MACRO_BEGIN \
do { \
(uptime)->seconds = mtime->uptime_value.seconds; \
__sync_synchronize(); \
(uptime)->nanoseconds = mtime->uptime_value.nanoseconds;\
__sync_synchronize(); \
} while ((uptime)->seconds != mtime->check_upseconds64); \
time_value64_add_hpc(uptime); \
MACRO_END
def_simple_lock_irq_data(static, timer_lock)
timer_elt_data_t timer_head;
#ifdef TICKLESS_TIMER
static boolean_t
tickless_have_pending_timers(void)
{
timer_elt_t telt;
boolean_t have_pending;
spl_t s;
s = simple_lock_irq(&timer_lock);
telt = (timer_elt_t)queue_first(&timer_head.chain);
have_pending = (telt->ticks <= elapsed_ticks);
simple_unlock_irq(s, &timer_lock);
return have_pending;
}
static unsigned long
tickless_next_timer_deadline(void)
{
timer_elt_t telt;
unsigned long next_deadline = 0;
spl_t s;
s = simple_lock_irq(&timer_lock);
telt = (timer_elt_t)queue_first(&timer_head.chain);
if (telt->ticks > elapsed_ticks) {
next_deadline = telt->ticks - elapsed_ticks;
if (next_deadline > 100) {
next_deadline = 100;
}
}
simple_unlock_irq(s, &timer_lock);
return next_deadline;
}
static boolean_t
tickless_can_skip_tick(void)
{
if (tickless_have_pending_timers()) {
return FALSE;
}
return (tickless_next_timer_deadline() > 1);
}
#endif
void clock_interrupt(
int usec,
boolean_t usermode,
boolean_t basepri,
vm_offset_t pc)
{
int my_cpu = cpu_number();
thread_t thread = current_thread();
counter(c_clock_ticks++);
counter(c_threads_total += c_threads_current);
counter(c_stacks_total += c_stacks_current);
#if STAT_TIME
if (usermode) {
timer_bump(&thread->user_timer, usec);
}
else {
if (thread)
timer_bump(&thread->system_timer, usec);
}
#endif
{
int state;
if (usermode)
state = CPU_STATE_USER;
else if (!cpu_idle(my_cpu))
state = CPU_STATE_SYSTEM;
else
state = CPU_STATE_IDLE;
machine_slot[my_cpu].cpu_ticks[state]++;
thread_quantum_update(my_cpu, thread, 1, state);
}
#if MACH_PCSAMPLE
#ifndef MACH_KERNSAMPLE
if (usermode)
#endif
{
if (thread)
take_pc_sample_macro(thread, SAMPLED_PC_PERIODIC, usermode, pc);
}
#endif
if (my_cpu == master_cpu) {
spl_t s;
timer_elt_t telt;
boolean_t needsoft = FALSE;
#ifdef TICKLESS_TIMER
boolean_t should_skip_tick = FALSE;
#endif
s = simple_lock_irq(&timer_lock);
#ifdef TICKLESS_TIMER
should_skip_tick = tickless_can_skip_tick();
if (!should_skip_tick) {
elapsed_ticks++;
telt = (timer_elt_t)queue_first(&timer_head.chain);
if (telt->ticks <= elapsed_ticks)
needsoft = TRUE;
}
#else
elapsed_ticks++;
telt = (timer_elt_t)queue_first(&timer_head.chain);
if (telt->ticks <= elapsed_ticks)
needsoft = TRUE;
#endif
simple_unlock_irq(s, &timer_lock);
if (timedelta == 0) {
time_value64_add_nanos(&time, usec * 1000);
time_value64_add_nanos(&uptime, usec * 1000);
}
else {
int delta;
if (timedelta < 0) {
if (usec > tickdelta) {
delta = usec - tickdelta;
timedelta += tickdelta;
} else {
delta = 1;
timedelta += usec - 1;
}
}
else {
delta = usec + tickdelta;
timedelta -= tickdelta;
}
time_value64_add_nanos(&time, delta * 1000);
time_value64_add_nanos(&uptime, delta * 1000);
}
update_mapped_time(&time);
update_mapped_uptime(&uptime);
if (needsoft) {
if (basepri) {
(void) splsoftclock();
softclock();
}
else {
setsoftclock();
}
}
}
last_hpc_read = hpclock_read_counter();
}
void softclock(void)
{
spl_t s;
timer_elt_t telt;
void (*fcn)( void * param );
void *param;
#ifdef TICKLESS_TIMER
int processed = 0;
const int max_batch = 16;
#endif
while (TRUE) {
s = simple_lock_irq(&timer_lock);
telt = (timer_elt_t) queue_first(&timer_head.chain);
if (telt->ticks > elapsed_ticks) {
simple_unlock_irq(s, &timer_lock);
break;
}
fcn = telt->fcn;
param = telt->param;
remqueue(&timer_head.chain, (queue_entry_t)telt);
telt->set = TELT_UNSET;
simple_unlock_irq(s, &timer_lock);
assert(fcn != 0);
(*fcn)(param);
#ifdef TICKLESS_TIMER
if (++processed >= max_batch) {
s = simple_lock_irq(&timer_lock);
telt = (timer_elt_t) queue_first(&timer_head.chain);
if (telt->ticks <= elapsed_ticks) {
simple_unlock_irq(s, &timer_lock);
setsoftclock();
}
else {
simple_unlock_irq(s, &timer_lock);
}
break;
}
#endif
}
}
void set_timeout(
timer_elt_t telt,
unsigned int interval)
{
spl_t s;
timer_elt_t next;
s = simple_lock_irq(&timer_lock);
interval += elapsed_ticks;
for (next = (timer_elt_t)queue_first(&timer_head.chain);
;
next = (timer_elt_t)queue_next((queue_entry_t)next)) {
if (next->ticks > interval)
break;
}
telt->ticks = interval;
insque((queue_entry_t) telt, ((queue_entry_t)next)->prev);
telt->set = TELT_SET;
simple_unlock_irq(s, &timer_lock);
}
boolean_t reset_timeout(timer_elt_t telt)
{
spl_t s;
s = simple_lock_irq(&timer_lock);
if (telt->set) {
remqueue(&timer_head.chain, (queue_entry_t)telt);
telt->set = TELT_UNSET;
simple_unlock_irq(s, &timer_lock);
return TRUE;
}
else {
simple_unlock_irq(s, &timer_lock);
return FALSE;
}
}
void init_timeout(void)
{
simple_lock_init_irq(&timer_lock);
queue_init(&timer_head.chain);
timer_head.ticks = ~0;
elapsed_ticks = 0;
#ifdef TICKLESS_TIMER
printf("Tickless timer optimization enabled\n");
#endif
}
struct time_value64 clock_boottime_offset;
static void
clock_boottime_update(const struct time_value64 *new_time)
{
struct time_value64 delta = time;
time_value64_sub(&delta, new_time);
time_value64_add(&clock_boottime_offset, &delta);
}
static void
time_value64_add_hpc(time_value64_t *value)
{
uint32_t now = hpclock_read_counter();
int64_t ns = (now - last_hpc_read) * hpclock_get_counter_period_nsec();
if (ns >= tick * 1000)
ns = (tick * 1000) - 1;
time_value64_add_nanos(value, ns);
}
void
record_time_stamp(time_value64_t *stamp)
{
read_mapped_time(stamp);
time_value64_add(stamp, &clock_boottime_offset);
}
void
read_time_stamp (const time_value64_t *stamp, time_value64_t *result)
{
*result = *stamp;
time_value64_sub(result, &clock_boottime_offset);
}
kern_return_t
host_get_time(const host_t host, time_value_t *current_time)
{
if (host == HOST_NULL)
return(KERN_INVALID_HOST);
time_value64_t current_time64;
read_mapped_time(&current_time64);
TIME_VALUE64_TO_TIME_VALUE(&current_time64, current_time);
return (KERN_SUCCESS);
}
kern_return_t
host_get_time64(const host_t host, time_value64_t *current_time)
{
if (host == HOST_NULL)
return(KERN_INVALID_HOST);
read_mapped_time(current_time);
return (KERN_SUCCESS);
}
kern_return_t
host_set_time(const host_t host, time_value_t new_time)
{
time_value64_t new_time64;
TIME_VALUE_TO_TIME_VALUE64(&new_time, &new_time64);
return host_set_time64(host, new_time64);
}
kern_return_t
host_set_time64(const host_t host, time_value64_t new_time)
{
spl_t s;
if (host == HOST_NULL)
return(KERN_INVALID_HOST);
#if NCPUS > 1
thread_bind(current_thread(), master_processor);
if (current_processor() != master_processor)
thread_block(thread_no_continuation);
#endif
s = splhigh();
clock_boottime_update(&new_time);
time = new_time;
update_mapped_time(&time);
resettodr();
splx(s);
#if NCPUS > 1
thread_bind(current_thread(), PROCESSOR_NULL);
#endif
return(KERN_SUCCESS);
}
kern_return_t
host_adjust_time(
const host_t host,
time_value_t new_adjustment,
time_value_t *old_adjustment )
{
time_value64_t old_adjustment64;
time_value64_t new_adjustment64;
kern_return_t ret;
TIME_VALUE_TO_TIME_VALUE64(&new_adjustment, &new_adjustment64);
ret = host_adjust_time64(host, new_adjustment64, &old_adjustment64);
if (ret == KERN_SUCCESS) {
TIME_VALUE64_TO_TIME_VALUE(&old_adjustment64, old_adjustment);
}
return ret;
}
kern_return_t
host_adjust_time64(
const host_t host,
time_value64_t new_adjustment,
time_value64_t *old_adjustment )
{
time_value64_t oadj;
uint64_t ndelta_microseconds;
spl_t s;
if (host == HOST_NULL)
return (KERN_INVALID_HOST);
ndelta_microseconds = new_adjustment.seconds * MICROSECONDS_IN_ONE_SECOND
+ new_adjustment.nanoseconds / 1000;
#if NCPUS > 1
thread_bind(current_thread(), master_processor);
if (current_processor() != master_processor)
thread_block(thread_no_continuation);
#endif
s = splclock();
oadj.seconds = timedelta / MICROSECONDS_IN_ONE_SECOND;
oadj.nanoseconds = (timedelta % MICROSECONDS_IN_ONE_SECOND) * 1000;
if (timedelta == 0) {
if (ndelta_microseconds > bigadj)
tickdelta = 10 * tickadj;
else
tickdelta = tickadj;
}
if (ndelta_microseconds % tickdelta)
ndelta_microseconds = ndelta_microseconds / tickdelta * tickdelta;
timedelta = ndelta_microseconds;
splx(s);
#if NCPUS > 1
thread_bind(current_thread(), PROCESSOR_NULL);
#endif
*old_adjustment = oadj;
return (KERN_SUCCESS);
}
kern_return_t
host_get_uptime64(const host_t host, time_value64_t *uptime)
{
if (host == HOST_NULL)
return (KERN_INVALID_HOST);
read_mapped_uptime(uptime);
return (KERN_SUCCESS);
}
void mapable_time_init(void)
{
if (kmem_alloc_wired(kernel_map, (vm_offset_t *) &mtime, PAGE_SIZE)
!= KERN_SUCCESS)
panic("mapable_time_init");
memset((void *) mtime, 0, PAGE_SIZE);
update_mapped_time(&time);
update_mapped_uptime(&uptime);
}
int timeopen(dev_t dev, int flag, io_req_t ior)
{
return(0);
}
void timeclose(dev_t dev, int flag)
{
return;
}
#define NTIMERS 20
timer_elt_data_t timeout_timers[NTIMERS];
void timeout(
void (*fcn)(void *param),
void * param,
int interval)
{
spl_t s;
timer_elt_t elt;
s = simple_lock_irq(&timer_lock);
for (elt = &timeout_timers[0]; elt < &timeout_timers[NTIMERS]; elt++)
if (elt->set == TELT_UNSET)
break;
if (elt == &timeout_timers[NTIMERS])
panic("timeout");
elt->fcn = fcn;
elt->param = param;
elt->set = TELT_ALLOC;
simple_unlock_irq(s, &timer_lock);
set_timeout(elt, (unsigned int)interval);
}
boolean_t untimeout(void (*fcn)( void * param ), const void *param)
{
spl_t s;
timer_elt_t elt;
s = simple_lock_irq(&timer_lock);
queue_iterate(&timer_head.chain, elt, timer_elt_t, chain) {
if ((fcn == elt->fcn) && (param == elt->param)) {
remqueue(&timer_head.chain, (queue_entry_t)elt);
elt->set = TELT_UNSET;
simple_unlock_irq(s, &timer_lock);
return (TRUE);
}
}
simple_unlock_irq(s, &timer_lock);
return (FALSE);
}