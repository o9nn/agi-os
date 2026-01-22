#include <kern/instrumentation_integration.h>
#include <kern/dtrace.h>
#include <kern/dynamic_probes.h>
#include <kern/perf_counters.h>
#include <kern/perf_analysis.h>
#include <mach/lttng.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <machine/spl.h>
#include <string.h>
static struct {
boolean_t initialized;
boolean_t dtrace_enabled;
boolean_t dynamic_probes_enabled;
boolean_t performance_counters_enabled;
boolean_t lttng_enabled;
simple_lock_data_t lock;
uint64_t total_overhead_ns;
uint32_t active_probes;
} instrumentation_state;
void
instrumentation_integration_init(void)
{
simple_lock_init(&instrumentation_state.lock);
instrumentation_state.initialized = TRUE;
instrumentation_state.dtrace_enabled = TRUE;
instrumentation_state.dynamic_probes_enabled = TRUE;
instrumentation_state.performance_counters_enabled = TRUE;
instrumentation_state.lttng_enabled = TRUE;
instrumentation_state.total_overhead_ns = 0;
instrumentation_state.active_probes = 0;
printf("Unified kernel instrumentation framework initialized\n");
}
uint32_t
instrumentation_get_overhead_percent(void)
{
if (!instrumentation_state.initialized) {
return 0;
}
uint64_t uptime_ns = dtrace_gethrtime();
if (uptime_ns == 0) {
return 0;
}
uint64_t overhead_percent = (instrumentation_state.total_overhead_ns * 100) / uptime_ns;
return (uint32_t)(overhead_percent > 100 ? 100 : overhead_percent);
}
uint32_t
instrumentation_create_comprehensive_probe(const char *name,
const char *function,
dtrace_probe_type_t type)
{
uint32_t probe_id = 0;
spl_t s;
if (!instrumentation_state.initialized || !name || !function) {
return 0;
}
s = splhigh();
simple_lock(&instrumentation_state.lock);
if (instrumentation_state.dtrace_enabled) {
probe_id = dtrace_probe_register(type, name, function, NULL);
}
if (instrumentation_state.dynamic_probes_enabled && probe_id > 0) {
dynamic_probe_config_t config = {
.module_name = "kernel",
.function_name = function,
.probe_name = name,
.type = type,
.address = 0,
.enabled = TRUE
};
dynamic_probe_create(&config);
}
if (instrumentation_state.performance_counters_enabled) {
perf_counter_update(PERF_COUNTER_LOCK_CONTENTION, 1);
}
if (instrumentation_state.lttng_enabled) {
mach_trace_event(MACH_TRACE_SCHED, MACH_TRACE_LEVEL_INFO, 0, "probe_created: %s", name);
}
if (probe_id > 0) {
instrumentation_state.active_probes++;
}
simple_unlock(&instrumentation_state.lock);
splx(s);
return probe_id;
}
uint32_t
instrumentation_system_health_check(void)
{
uint32_t health_score = 100;
if (!instrumentation_state.initialized) {
return 0;
}
uint32_t overhead = instrumentation_get_overhead_percent();
if (overhead > 5) {
health_score -= (overhead - 5) * 10;
}
uint32_t perf_health = perf_get_system_health_score();
if (perf_health < 100) {
health_score = (health_score + perf_health) / 2;
}
if (instrumentation_state.active_probes == 0) {
health_score -= 20;
}
return health_score > 0 ? health_score : 0;
}
void
instrumentation_generate_report(void)
{
if (!instrumentation_state.initialized) {
printf("Instrumentation not initialized\n");
return;
}
printf("\n=== Kernel Instrumentation Framework Report ===\n");
printf("Framework Status: %s\n",
instrumentation_state.initialized ? "ACTIVE" : "INACTIVE");
printf("\nComponent Status:\n");
printf("  DTrace Framework:      %s\n",
instrumentation_state.dtrace_enabled ? "ENABLED" : "DISABLED");
printf("  Dynamic Probes:        %s\n",
instrumentation_state.dynamic_probes_enabled ? "ENABLED" : "DISABLED");
printf("  Performance Counters:  %s\n",
instrumentation_state.performance_counters_enabled ? "ENABLED" : "DISABLED");
printf("  LTTng Tracing:         %s\n",
instrumentation_state.lttng_enabled ? "ENABLED" : "DISABLED");
printf("\nPerformance Metrics:\n");
printf("  Active Probes:         %u\n", instrumentation_state.active_probes);
printf("  Total Overhead:        %llu ns\n", instrumentation_state.total_overhead_ns);
printf("  Overhead Percentage:   %u%%\n", instrumentation_get_overhead_percent());
printf("  System Health Score:   %u/100\n", instrumentation_system_health_check());
printf("\n--- DTrace Statistics ---\n");
printf("  Total probes: %u\n", instrumentation_state.active_probes);
printf("\n--- Performance Analysis ---\n");
printf("  Overhead: %u%%\n", instrumentation_get_overhead_percent());
printf("\n--- LTTng Statistics ---\n");
mach_trace_print_stats();
printf("=== End Report ===\n\n");
}
boolean_t
instrumentation_set_component_state(instrumentation_component_t component,
boolean_t enabled)
{
spl_t s;
if (!instrumentation_state.initialized) {
return FALSE;
}
s = splhigh();
simple_lock(&instrumentation_state.lock);
switch (component) {
case INSTRUMENTATION_DTRACE:
instrumentation_state.dtrace_enabled = enabled;
break;
case INSTRUMENTATION_DYNAMIC_PROBES:
instrumentation_state.dynamic_probes_enabled = enabled;
break;
case INSTRUMENTATION_PERFORMANCE_COUNTERS:
instrumentation_state.performance_counters_enabled = enabled;
break;
case INSTRUMENTATION_LTTNG:
instrumentation_state.lttng_enabled = enabled;
mach_trace_enable(enabled);
break;
default:
simple_unlock(&instrumentation_state.lock);
splx(s);
return FALSE;
}
simple_unlock(&instrumentation_state.lock);
splx(s);
printf("Instrumentation component %d %s\n",
component, enabled ? "enabled" : "disabled");
return TRUE;
}
boolean_t
instrumentation_check_regression(void)
{
boolean_t regression_detected = FALSE;
if (!instrumentation_state.initialized) {
return FALSE;
}
uint32_t overhead = instrumentation_get_overhead_percent();
if (overhead > 5) {
printf("REGRESSION: Instrumentation overhead %u%% exceeds 5%% limit\n", overhead);
regression_detected = TRUE;
}
for (perf_event_type_t event = 0; event < PERF_EVENT_MAX; event++) {
if (perf_check_regression(event, 20)) {
printf("REGRESSION: Performance event %d shows >20%% degradation\n", event);
regression_detected = TRUE;
}
}
uint32_t health = instrumentation_system_health_check();
if (health < 80) {
printf("REGRESSION: System health score %u below 80 threshold\n", health);
regression_detected = TRUE;
}
return regression_detected;
}
boolean_t
instrumentation_real_time_monitor_active(void)
{
return instrumentation_state.initialized &&
instrumentation_state.performance_counters_enabled &&
instrumentation_state.active_probes > 0;
}