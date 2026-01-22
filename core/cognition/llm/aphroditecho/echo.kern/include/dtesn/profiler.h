#ifndef DTESN_PROFILER_H
#define DTESN_PROFILER_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef enum dtesn_profile_type {
DTESN_PROFILE_MEMORY_ALLOC = 0,
DTESN_PROFILE_MEMORY_FREE,
DTESN_PROFILE_MEMBRANE_EVOLUTION,
DTESN_PROFILE_BSERIES_COMPUTE,
DTESN_PROFILE_ESN_UPDATE,
DTESN_PROFILE_SYSCALL,
DTESN_PROFILE_SCHEDULER,
DTESN_PROFILE_TYPE_COUNT
} dtesn_profile_type_t;
typedef enum dtesn_hw_counter_type {
DTESN_HW_CPU_CYCLES = 0,
DTESN_HW_INSTRUCTIONS,
DTESN_HW_CACHE_MISSES,
DTESN_HW_BRANCH_MISSES,
DTESN_HW_PAGE_FAULTS,
DTESN_HW_CONTEXT_SWITCHES,
DTESN_HW_COUNTER_TYPE_COUNT
} dtesn_hw_counter_type_t;
typedef struct dtesn_profile_measurement {
uint64_t start_time_ns;
uint64_t end_time_ns;
uint64_t duration_ns;
dtesn_profile_type_t type;
uint32_t membrane_level;
uint64_t hw_counters[DTESN_HW_COUNTER_TYPE_COUNT];
} dtesn_profile_measurement_t;
typedef struct dtesn_profile_stats {
uint64_t total_count;
uint64_t total_time_ns;
uint64_t min_time_ns;
uint64_t max_time_ns;
uint64_t avg_time_ns;
uint64_t violations;
uint64_t last_measurement_ns;
} dtesn_profile_stats_t;
typedef struct dtesn_profile_context {
dtesn_profile_measurement_t measurement;
bool active;
uint32_t thread_id;
} dtesn_profile_context_t;
typedef struct dtesn_profiler {
bool initialized;
bool enabled;
uint64_t overhead_ns;
dtesn_profile_stats_t stats[DTESN_PROFILE_TYPE_COUNT];
dtesn_profile_context_t *contexts;
uint32_t max_contexts;
uint64_t dropped_measurements;
} dtesn_profiler_t;
#define DTESN_PROFILE_MEMORY_ALLOC_TARGET_NS   10000
#define DTESN_PROFILE_MEMORY_FREE_TARGET_NS     5000
#define DTESN_PROFILE_MEMBRANE_EVOLUTION_TARGET_NS  10000
#define DTESN_PROFILE_BSERIES_COMPUTE_TARGET_NS 100000
#define DTESN_PROFILE_ESN_UPDATE_TARGET_NS     1000000
#define DTESN_PROFILE_SYSCALL_TARGET_NS         5000
#define DTESN_PROFILE_OVERHEAD_THRESHOLD        2.0
#define DTESN_PROFILE_DEFAULT_MAX_CONTEXTS      32
int dtesn_profile_init(uint32_t max_contexts);
dtesn_profile_context_t *dtesn_profile_start(dtesn_profile_type_t type,
uint32_t membrane_level);
int dtesn_profile_end(dtesn_profile_context_t *context);
int dtesn_profile_report(char *buffer, size_t buffer_size);
int dtesn_hw_counters(uint64_t counters[DTESN_HW_COUNTER_TYPE_COUNT]);
int dtesn_profile_enable(bool enabled);
int dtesn_profile_reset(void);
uint64_t dtesn_profile_overhead(void);
int dtesn_profile_cleanup(void);
#define DTESN_PROFILE_BLOCK(type, level) \
dtesn_profile_context_t *__prof_ctx = dtesn_profile_start(type, level); \
do
#define DTESN_PROFILE_END() \
while (0); \
if (__prof_ctx) dtesn_profile_end(__prof_ctx)
#define DTESN_PROFILE_FUNCTION(type, level) \
dtesn_profile_context_t *__prof_ctx = dtesn_profile_start(type, level)
#define DTESN_PROFILE_FUNCTION_END() \
if (__prof_ctx) dtesn_profile_end(__prof_ctx)
#define DTESN_PROFILE_QUICK(type) \
dtesn_profile_context_t *__prof_ctx = dtesn_profile_start(type, 0)
#ifdef __cplusplus
}
#endif
#endif