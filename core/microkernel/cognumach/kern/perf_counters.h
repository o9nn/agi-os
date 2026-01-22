#ifndef _KERN_PERF_COUNTERS_H_
#define _KERN_PERF_COUNTERS_H_
#include <kern/kern_types.h>
#include <kern/perf_analysis.h>
typedef enum {
PERF_COUNTER_INSTRUCTIONS = 0,
PERF_COUNTER_CACHE_MISSES,
PERF_COUNTER_BRANCH_MISPREDICTS,
PERF_COUNTER_TLB_MISSES,
PERF_COUNTER_MEMORY_BANDWIDTH,
PERF_COUNTER_LOCK_CONTENTION,
PERF_COUNTER_INTERRUPT_OVERHEAD,
PERF_COUNTER_SCHEDULE_LATENCY,
PERF_COUNTER_IPC_LATENCY,
PERF_COUNTER_VM_PRESSURE,
PERF_COUNTER_MAX
} perf_counter_type_t;
typedef struct perf_counter_data {
uint64_t value;
uint64_t max_value;
uint64_t min_value;
uint64_t total;
uint64_t samples;
uint64_t last_update;
} perf_counter_data_t;
typedef struct perf_system_snapshot {
uint64_t timestamp;
uint32_t active_tasks;
uint32_t active_threads;
uint64_t total_memory_used;
uint64_t cpu_utilization;
perf_counter_data_t counters[PERF_COUNTER_MAX];
struct {
uint64_t user_time;
uint64_t kernel_time;
uint64_t idle_time;
uint64_t interrupt_time;
} cpu_time;
} perf_system_snapshot_t;
typedef struct perf_trend_data {
perf_counter_type_t counter_type;
uint64_t trend_period;
double growth_rate;
double volatility;
boolean_t anomaly_detected;
uint64_t prediction_next;
} perf_trend_data_t;
void perf_counters_init(void);
void perf_counter_update(perf_counter_type_t type, uint64_t value);
boolean_t perf_counter_get(perf_counter_type_t type, perf_counter_data_t *data);
boolean_t perf_take_system_snapshot(perf_system_snapshot_t *snapshot);
boolean_t perf_analyze_trends(perf_counter_type_t type, perf_trend_data_t *trend);
boolean_t perf_detect_anomalies(perf_system_snapshot_t *snapshots,
uint32_t count,
perf_counter_type_t **anomalous_counters,
uint32_t *anomaly_count);
const char *perf_counter_name(perf_counter_type_t type);
void perf_counters_reset(void);
boolean_t perf_counter_enable(perf_counter_type_t type, boolean_t enable);
uint32_t perf_get_system_health_score(void);
#endif