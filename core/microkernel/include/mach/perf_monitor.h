#ifndef _MACH_PERF_MONITOR_H_
#define _MACH_PERF_MONITOR_H_
#include <mach/kern_return.h>
#include <mach/mach_types.h>
#define PERF_MONITOR_ENABLE 1
#define PERF_MONITOR_DISABLE 2
#define PERF_MONITOR_CONFIGURE 3
#define PERF_MONITOR_GET_STATS 4
#define PERF_MONITOR_READ_SAMPLES 5
#define PERF_MONITOR_SET_BASELINE 6
#define PERF_MONITOR_CHECK_REGRESSION 7
#define PERF_MONITOR_RESET_STATS 8
#define PERF_MONITOR_SET_THRESHOLDS 9
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
struct perf_sample_user {
uint64_t timestamp;
uint32_t event;
uint32_t cpu_id;
uint32_t task_id;
uint32_t thread_id;
uint64_t data1;
uint64_t data2;
uint32_t duration_us;
};
struct perf_event_stats_user {
uint64_t count;
uint64_t total_time_us;
uint64_t min_time_us;
uint64_t max_time_us;
uint64_t avg_time_us;
uint64_t last_timestamp;
};
struct perf_monitor_config {
uint32_t sample_rate;
uint32_t buffer_size;
uint32_t latency_threshold_us;
uint32_t throughput_threshold;
uint32_t error_rate_threshold;
};
struct perf_system_summary {
uint32_t total_events;
uint64_t monitoring_time_us;
uint32_t samples_dropped;
boolean_t regression_detected;
struct perf_event_stats_user overall_stats;
};
#endif