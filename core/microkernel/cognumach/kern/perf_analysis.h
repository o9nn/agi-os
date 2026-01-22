#ifndef _KERN_PERF_ANALYSIS_H_
#define _KERN_PERF_ANALYSIS_H_
#include <kern/kern_types.h>
#include <kern/lock.h>
#include <mach/kern_return.h>
#include <mach/time_value.h>
typedef enum {
PERF_EVENT_IPC_SEND = 0,
PERF_EVENT_IPC_RECEIVE,
PERF_EVENT_VM_ALLOC,
PERF_EVENT_VM_FREE,
PERF_EVENT_TASK_CREATE,
PERF_EVENT_TASK_TERMINATE,
PERF_EVENT_THREAD_CREATE,
PERF_EVENT_THREAD_TERMINATE,
PERF_EVENT_CONTEXT_SWITCH,
PERF_EVENT_INTERRUPT,
PERF_EVENT_SYSCALL,
PERF_EVENT_PAGE_FAULT,
PERF_EVENT_MAX
} perf_event_type_t;
struct perf_sample {
uint64_t timestamp;
perf_event_type_t event;
uint32_t cpu_id;
uint32_t task_id;
uint32_t thread_id;
uint64_t data1;
uint64_t data2;
uint32_t duration_us;
};
struct perf_event_stats {
uint64_t count;
uint64_t total_time_us;
uint64_t min_time_us;
uint64_t max_time_us;
uint64_t avg_time_us;
uint64_t last_timestamp;
};
struct perf_monitor {
boolean_t enabled;
boolean_t sampling_enabled;
uint32_t sample_rate;
uint32_t buffer_size;
uint32_t buffer_head;
uint32_t buffer_tail;
uint32_t samples_dropped;
simple_lock_data_t lock;
struct perf_sample *sample_buffer;
struct perf_event_stats event_stats[PERF_EVENT_MAX];
uint32_t latency_threshold_us;
uint32_t throughput_threshold;
uint32_t error_rate_threshold;
uint64_t baseline_timestamp;
struct perf_event_stats baseline_stats[PERF_EVENT_MAX];
boolean_t regression_detected;
};
struct perf_analysis_control {
boolean_t profiling_enabled;
boolean_t trace_enabled;
uint32_t trace_mask;
uint32_t profile_interval_ms;
void (*pre_event_hook)(perf_event_type_t event, void *data);
void (*post_event_hook)(perf_event_type_t event, void *data, uint32_t duration);
};
extern struct perf_monitor global_perf_monitor;
extern struct perf_analysis_control perf_control;
void perf_analysis_init(void);
kern_return_t perf_monitor_enable(boolean_t enable);
kern_return_t perf_monitor_configure(uint32_t sample_rate, uint32_t buffer_size);
void perf_record_event(perf_event_type_t event, uint32_t task_id,
uint32_t thread_id, uint64_t data1, uint64_t data2);
uint64_t perf_event_start(perf_event_type_t event);
void perf_event_end(perf_event_type_t event, uint64_t start_time,
uint32_t task_id, uint32_t thread_id,
uint64_t data1, uint64_t data2);
kern_return_t perf_get_event_stats(perf_event_type_t event,
struct perf_event_stats *stats);
kern_return_t perf_get_system_stats(struct perf_event_stats *summary,
uint32_t *total_events);
kern_return_t perf_set_baseline(void);
boolean_t perf_check_regression(perf_event_type_t event, uint32_t threshold_percent);
void perf_reset_stats(void);
kern_return_t perf_read_samples(struct perf_sample *buffer, uint32_t max_samples,
uint32_t *samples_read);
boolean_t perf_monitor_check_thresholds(void);
void perf_monitor_set_thresholds(uint32_t latency_us, uint32_t throughput,
uint32_t error_rate);
#define PERF_EVENT_RECORD(event, task, thread, d1, d2) \
do { \
if (global_perf_monitor.enabled) { \
perf_record_event(event, task, thread, d1, d2); \
} \
} while (0)
#define PERF_EVENT_TIME_START(event) \
(global_perf_monitor.enabled ? perf_event_start(event) : 0)
#define PERF_EVENT_TIME_END(event, start, task, thread, d1, d2) \
do { \
if (global_perf_monitor.enabled && (start) != 0) { \
perf_event_end(event, start, task, thread, d1, d2); \
} \
} while (0)
#define PERF_TIME_BLOCK(event, task, thread, d1, d2, code_block) \
do { \
uint64_t _start = PERF_EVENT_TIME_START(event); \
code_block; \
PERF_EVENT_TIME_END(event, _start, task, thread, d1, d2); \
} while (0)
#endif