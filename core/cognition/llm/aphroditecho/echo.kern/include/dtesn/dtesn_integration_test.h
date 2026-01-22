#ifndef DTESN_INTEGRATION_TEST_H
#define DTESN_INTEGRATION_TEST_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <time.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_INTEGRATION_MAX_COMPONENTS 8
#define DTESN_INTEGRATION_MAX_TESTS 256
#define DTESN_INTEGRATION_MAX_METRICS 64
#define DTESN_INTEGRATION_TIMEOUT_MS 5000
#define DTESN_PERF_MEMBRANE_EVOLUTION_US 10
#define DTESN_PERF_BSERIES_COMPUTATION_US 100
#define DTESN_PERF_ESN_STATE_UPDATE_US 1000
#define DTESN_PERF_CONTEXT_SWITCH_US 5
#define DTESN_PERF_SCHEDULING_LATENCY_US 10
#define DTESN_COVERAGE_TARGET_PCT 95
#define DTESN_INTEGRATION_POINTS 8
#define DTESN_OEIS_MAX_DEPTH 12
extern const int dtesn_oeis_a000081_sequence[DTESN_OEIS_MAX_DEPTH];
typedef enum {
DTESN_COMPONENT_MEMORY = 0,
DTESN_COMPONENT_PSYSTEM,
DTESN_COMPONENT_BSERIES,
DTESN_COMPONENT_ESN,
DTESN_COMPONENT_SCHEDULER,
DTESN_COMPONENT_SYSCALLS,
DTESN_COMPONENT_HAL,
DTESN_COMPONENT_PROFILER,
DTESN_COMPONENT_COUNT
} dtesn_component_id_t;
typedef enum {
DTESN_TEST_PASS = 0,
DTESN_TEST_FAIL,
DTESN_TEST_TIMEOUT,
DTESN_TEST_SKIP,
DTESN_TEST_ERROR
} dtesn_test_result_t;
typedef enum {
DTESN_METRIC_LATENCY = 0,
DTESN_METRIC_THROUGHPUT,
DTESN_METRIC_JITTER,
DTESN_METRIC_CPU_USAGE,
DTESN_METRIC_MEMORY_USAGE,
DTESN_METRIC_COUNT
} dtesn_metric_type_t;
typedef struct {
uint32_t test_id;
const char *test_name;
dtesn_component_id_t primary_component;
dtesn_component_id_t secondary_component;
uint64_t start_time_ns;
uint64_t end_time_ns;
dtesn_test_result_t result;
char error_message[256];
} dtesn_test_context_t;
typedef struct {
dtesn_metric_type_t type;
uint64_t value_ns;
uint64_t threshold_ns;
bool meets_requirement;
const char *description;
} dtesn_performance_metric_t;
typedef struct {
uint32_t total_tests;
uint32_t tests_passed;
uint32_t tests_failed;
uint32_t tests_skipped;
uint64_t total_runtime_ns;
dtesn_performance_metric_t metrics[DTESN_INTEGRATION_MAX_METRICS];
uint32_t metric_count;
bool oeis_compliance_validated;
double code_coverage_pct;
} dtesn_integration_state_t;
typedef struct {
dtesn_integration_state_t state;
uint64_t timestamp_ns;
char version[32];
char build_info[256];
bool regression_detected;
char report_filename[256];
} dtesn_integration_report_t;
int dtesn_integration_test_init(const void *config);
void dtesn_integration_test_cleanup(void);
dtesn_test_result_t dtesn_test_cross_component(
dtesn_component_id_t comp1,
dtesn_component_id_t comp2,
const void *test_data,
size_t test_size);
int dtesn_test_performance_regression(const char *baseline_file, double regression_threshold_pct);
bool dtesn_validate_oeis_compliance(int max_depth);
int dtesn_test_report_generate(dtesn_integration_report_t *report, const char *output_file);
dtesn_test_result_t dtesn_test_memory_integration(void);
dtesn_test_result_t dtesn_test_psystem_integration(void);
dtesn_test_result_t dtesn_test_esn_integration(void);
dtesn_test_result_t dtesn_test_scheduler_integration(void);
dtesn_test_result_t dtesn_test_realistic_workload(int workload_type, uint32_t duration_ms);
dtesn_test_result_t dtesn_test_stress_integration(int max_load_pct, uint32_t duration_ms);
const char *dtesn_get_component_name(dtesn_component_id_t component);
dtesn_performance_metric_t dtesn_measure_performance(
dtesn_component_id_t component,
dtesn_metric_type_t metric_type,
uint32_t duration_ms);
const dtesn_integration_state_t *dtesn_get_integration_state(void);
void dtesn_reset_integration_state(void);
static inline uint64_t dtesn_get_time_ns(void) {
struct timespec ts;
clock_gettime(CLOCK_MONOTONIC, &ts);
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
static inline double dtesn_ns_to_us(uint64_t ns) {
return (double)ns / 1000.0;
}
static inline double dtesn_ns_to_ms(uint64_t ns) {
return (double)ns / 1000000.0;
}
#ifdef __cplusplus
}
#endif
#endif