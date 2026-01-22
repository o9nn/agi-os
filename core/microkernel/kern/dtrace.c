#include <kern/dtrace.h>
#if MACH_DTRACE
#include <mach/time_value.h>
#include <kern/thread.h>
#include <kern/task.h>
#include <kern/cpu_number.h>
#include <kern/kalloc.h>
#include <kern/sched_prim.h>
#include <kern/printf.h>
#include <kern/mach_clock.h>
#include <string.h>
static dtrace_state_t dtrace_state;
#define DTRACE_MAX_PROBES 512
void
dtrace_init(void)
{
memset(&dtrace_state, 0, sizeof(dtrace_state));
dtrace_state.max_probes = DTRACE_MAX_PROBES;
dtrace_state.probes = (dtrace_probe_t *)kalloc(
dtrace_state.max_probes * sizeof(dtrace_probe_t));
if (dtrace_state.probes == NULL) {
printf("dtrace_init: failed to allocate probe table\n");
return;
}
memset(dtrace_state.probes, 0,
dtrace_state.max_probes * sizeof(dtrace_probe_t));
simple_lock_init_irq(&dtrace_state.probe_lock);
simple_lock_init_irq(&dtrace_state.buffer.lock);
dtrace_state.buffer.head = 0;
dtrace_state.buffer.tail = 0;
dtrace_state.buffer.count = 0;
dtrace_state.buffer.overruns = 0;
dtrace_state.metrics.max_probes = DTRACE_MAX_PROBES;
dtrace_state.enabled = TRUE;
dtrace_probe_register(DTRACE_PROBE_THREAD_SWITCH, "thread_switch",
"thread_invoke", NULL);
dtrace_probe_register(DTRACE_PROBE_IPC_SEND, "ipc_send",
"mach_msg_trap", NULL);
dtrace_probe_register(DTRACE_PROBE_VM_FAULT, "vm_fault",
"vm_fault", NULL);
for (uint32_t i = 1; i <= 3; i++) {
dtrace_probe_enable(i);
}
printf("DTrace instrumentation framework initialized (%d probes max)\n",
DTRACE_MAX_PROBES);
printf("Default probes registered: thread_switch, ipc_send, vm_fault\n");
}
void
dtrace_shutdown(void)
{
if (dtrace_state.probes) {
kfree((vm_offset_t)dtrace_state.probes,
dtrace_state.max_probes * sizeof(dtrace_probe_t));
dtrace_state.probes = NULL;
}
dtrace_state.enabled = FALSE;
dtrace_state.probe_count = 0;
printf("DTrace instrumentation framework shutdown\n");
}
uint64_t
dtrace_gethrtime(void)
{
time_value64_t tv;
record_time_stamp(&tv);
return ((uint64_t)tv.seconds * 1000000000ULL) +
((uint64_t)tv.nanoseconds);
}
uint32_t
dtrace_probe_register(dtrace_probe_type_t type, const char *name,
const char *function, dtrace_handler_t handler)
{
spl_t s;
uint32_t probe_id = 0;
dtrace_probe_t *probe;
if (!dtrace_state.probes || !name) {
return 0;
}
s = simple_lock_irq(&dtrace_state.probe_lock);
for (uint32_t i = 1; i < dtrace_state.max_probes; i++) {
if (dtrace_state.probes[i].id == 0) {
probe_id = i;
break;
}
}
if (probe_id == 0) {
simple_unlock_irq(s, &dtrace_state.probe_lock);
return 0;
}
probe = &dtrace_state.probes[probe_id];
probe->id = probe_id;
probe->type = type;
probe->name = name;
probe->function = function;
probe->enabled = FALSE;
probe->fire_count = 0;
probe->total_time = 0;
probe->arg_count = 6;
probe->handler = (void *)handler;
dtrace_state.probe_count++;
dtrace_state.metrics.active_probes = dtrace_state.probe_count;
simple_unlock_irq(s, &dtrace_state.probe_lock);
return probe_id;
}
boolean_t
dtrace_probe_enable(uint32_t probe_id)
{
spl_t s;
boolean_t result = FALSE;
if (probe_id >= dtrace_state.max_probes) {
return FALSE;
}
s = simple_lock_irq(&dtrace_state.probe_lock);
if (dtrace_state.probes[probe_id].id == probe_id) {
dtrace_state.probes[probe_id].enabled = TRUE;
result = TRUE;
}
simple_unlock_irq(s, &dtrace_state.probe_lock);
return result;
}
boolean_t
dtrace_probe_disable(uint32_t probe_id)
{
spl_t s;
boolean_t result = FALSE;
if (probe_id >= dtrace_state.max_probes) {
return FALSE;
}
s = simple_lock_irq(&dtrace_state.probe_lock);
if (dtrace_state.probes[probe_id].id == probe_id) {
dtrace_state.probes[probe_id].enabled = FALSE;
result = TRUE;
}
simple_unlock_irq(s, &dtrace_state.probe_lock);
return result;
}
boolean_t
dtrace_probe_remove(uint32_t probe_id)
{
spl_t s;
boolean_t result = FALSE;
if (probe_id >= dtrace_state.max_probes) {
return FALSE;
}
s = simple_lock_irq(&dtrace_state.probe_lock);
if (dtrace_state.probes[probe_id].id == probe_id) {
memset(&dtrace_state.probes[probe_id], 0, sizeof(dtrace_probe_t));
dtrace_state.probe_count--;
dtrace_state.metrics.active_probes = dtrace_state.probe_count;
result = TRUE;
}
simple_unlock_irq(s, &dtrace_state.probe_lock);
return result;
}
void
dtrace_probe_fire(dtrace_probe_type_t type, const char *name,
uint64_t arg0, uint64_t arg1, uint64_t arg2,
uint64_t arg3, uint64_t arg4, uint64_t arg5)
{
spl_t s;
dtrace_event_t *event;
dtrace_probe_t *probe = NULL;
uint64_t start_time, end_time;
thread_t thread;
task_t task;
if (!dtrace_state.enabled || !dtrace_state.probes) {
return;
}
start_time = dtrace_gethrtime();
for (uint32_t i = 1; i < dtrace_state.max_probes; i++) {
if (dtrace_state.probes[i].id == i &&
dtrace_state.probes[i].type == type &&
dtrace_state.probes[i].enabled &&
dtrace_state.probes[i].name &&
strcmp(dtrace_state.probes[i].name, name) == 0) {
probe = &dtrace_state.probes[i];
break;
}
}
if (!probe) {
return;
}
probe->fire_count++;
dtrace_state.metrics.total_probes_fired++;
thread = current_thread();
task = (thread != THREAD_NULL) ? thread->task : TASK_NULL;
s = simple_lock_irq(&dtrace_state.buffer.lock);
if (dtrace_state.buffer.count < DTRACE_BUFFER_SIZE) {
event = &dtrace_state.buffer.events[dtrace_state.buffer.head];
event->probe_id = probe->id;
event->timestamp = start_time;
event->cpu_id = cpu_number();
event->thread_id = (thread != THREAD_NULL) ? (uint32_t)(uintptr_t)thread : 0;
event->task_id = (task != TASK_NULL) ? (uint32_t)(uintptr_t)task : 0;
event->args[0] = arg0;
event->args[1] = arg1;
event->args[2] = arg2;
event->args[3] = arg3;
event->args[4] = arg4;
event->args[5] = arg5;
dtrace_state.buffer.head = (dtrace_state.buffer.head + 1) % DTRACE_BUFFER_SIZE;
dtrace_state.buffer.count++;
dtrace_state.metrics.total_events_captured++;
} else {
dtrace_state.buffer.overruns++;
dtrace_state.metrics.buffer_overruns++;
}
simple_unlock_irq(s, &dtrace_state.buffer.lock);
if (probe->handler) {
dtrace_handler_t handler = (dtrace_handler_t)probe->handler;
handler(probe, arg0, arg1, arg2, arg3, arg4, arg5);
}
end_time = dtrace_gethrtime();
probe->total_time += (end_time - start_time);
dtrace_state.metrics.probe_overhead_ns += (end_time - start_time);
}
uint32_t
dtrace_buffer_read(dtrace_event_t *events, uint32_t max_events)
{
spl_t s;
uint32_t count = 0;
if (!events || max_events == 0) {
return 0;
}
s = simple_lock_irq(&dtrace_state.buffer.lock);
while (count < max_events && dtrace_state.buffer.count > 0) {
events[count] = dtrace_state.buffer.events[dtrace_state.buffer.tail];
dtrace_state.buffer.tail = (dtrace_state.buffer.tail + 1) % DTRACE_BUFFER_SIZE;
dtrace_state.buffer.count--;
count++;
}
simple_unlock_irq(s, &dtrace_state.buffer.lock);
return count;
}
void
dtrace_buffer_clear(void)
{
spl_t s;
s = simple_lock_irq(&dtrace_state.buffer.lock);
dtrace_state.buffer.head = 0;
dtrace_state.buffer.tail = 0;
dtrace_state.buffer.count = 0;
simple_unlock_irq(s, &dtrace_state.buffer.lock);
}
void
dtrace_get_metrics(dtrace_metrics_t *metrics)
{
if (metrics) {
*metrics = dtrace_state.metrics;
}
}
uint32_t
dtrace_get_probe_count(void)
{
return dtrace_state.probe_count;
}
boolean_t
dtrace_get_probe_info(uint32_t index, dtrace_probe_t *probe_info)
{
spl_t s;
boolean_t result = FALSE;
if (!probe_info || index >= dtrace_state.max_probes) {
return FALSE;
}
s = simple_lock_irq(&dtrace_state.probe_lock);
if (dtrace_state.probes[index].id != 0) {
*probe_info = dtrace_state.probes[index];
probe_info->handler = NULL;
result = TRUE;
}
simple_unlock_irq(s, &dtrace_state.probe_lock);
return result;
}
void
dtrace_enable(void)
{
dtrace_state.enabled = TRUE;
}
void
dtrace_disable(void)
{
dtrace_state.enabled = FALSE;
}
boolean_t
dtrace_is_enabled(void)
{
return dtrace_state.enabled;
}
#endif