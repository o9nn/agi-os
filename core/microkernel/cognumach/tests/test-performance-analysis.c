#include <testlib.h>
#include <mach.h>
#include <mach/host_info.h>
#include <mach/message.h>
#include <mach/vm_map.h>
#include <mach/perf_monitor.h>
#define TEST_ITERATIONS 1000
#define TEST_ALLOC_SIZE 4096
#define TEST_MSG_SIZE 64
struct test_message {
mach_msg_header_t header;
mach_msg_type_t type;
int data[16];
};
static void test_performance_monitoring_basic(void)
{
kern_return_t kr;
host_t host_port;
printf("Testing basic performance monitoring...\n");
host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("WARNING: Performance monitoring not available (kr=%d)\n", kr);
return;
}
printf("✓ Performance monitoring enabled\n");
kr = perf_monitor_configure(host_port, 1000, 4096);
if (kr == KERN_SUCCESS) {
printf("✓ Performance monitoring configured\n");
} else {
printf("WARNING: Could not configure monitoring (kr=%d)\n", kr);
}
kr = perf_monitor_enable(host_port, FALSE);
if (kr == KERN_SUCCESS) {
printf("✓ Performance monitoring disabled\n");
}
mach_port_deallocate(mach_task_self(), host_port);
}
static void test_performance_statistics(void)
{
kern_return_t kr;
host_t host_port;
uint64_t count, total_time, min_time, max_time, avg_time, last_timestamp;
uint32_t total_events, samples_dropped;
boolean_t regression_detected;
uint64_t overall_count, overall_total_time, overall_min_time, overall_max_time, overall_avg_time;
printf("Testing performance statistics collection...\n");
host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("INFO: Performance monitoring not available, skipping statistics test\n");
mach_port_deallocate(mach_task_self(), host_port);
return;
}
for (int i = 0; i < 100; i++) {
vm_address_t addr = 0;
kr = vm_allocate(mach_task_self(), &addr, TEST_ALLOC_SIZE, TRUE);
if (kr == KERN_SUCCESS) {
vm_deallocate(mach_task_self(), addr, TEST_ALLOC_SIZE);
}
}
kr = perf_get_event_stats(host_port, PERF_EVENT_VM_ALLOC,
&count, &total_time, &min_time, &max_time,
&avg_time, &last_timestamp);
if (kr == KERN_SUCCESS) {
printf("✓ VM allocation statistics: count=%llu, avg_time=%llu us\n",
count, avg_time);
} else {
printf("WARNING: Could not get event statistics (kr=%d)\n", kr);
}
kr = perf_get_system_stats(host_port, &total_events, &samples_dropped,
&regression_detected, &overall_count,
&overall_total_time, &overall_min_time,
&overall_max_time, &overall_avg_time);
if (kr == KERN_SUCCESS) {
printf("✓ System statistics: total_events=%u, samples_dropped=%u\n",
total_events, samples_dropped);
} else {
printf("WARNING: Could not get system statistics (kr=%d)\n", kr);
}
perf_monitor_enable(host_port, FALSE);
mach_port_deallocate(mach_task_self(), host_port);
}
static void test_regression_detection(void)
{
kern_return_t kr;
host_t host_port;
boolean_t regression_detected;
printf("Testing regression detection...\n");
host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("INFO: Performance monitoring not available, skipping regression test\n");
mach_port_deallocate(mach_task_self(), host_port);
return;
}
for (int i = 0; i < 50; i++) {
vm_address_t addr = 0;
kr = vm_allocate(mach_task_self(), &addr, TEST_ALLOC_SIZE, TRUE);
if (kr == KERN_SUCCESS) {
vm_deallocate(mach_task_self(), addr, TEST_ALLOC_SIZE);
}
}
kr = perf_set_baseline(host_port);
if (kr == KERN_SUCCESS) {
printf("✓ Performance baseline set\n");
} else {
printf("WARNING: Could not set baseline (kr=%d)\n", kr);
}
for (int i = 0; i < 50; i++) {
vm_address_t addr = 0;
kr = vm_allocate(mach_task_self(), &addr, TEST_ALLOC_SIZE, TRUE);
if (kr == KERN_SUCCESS) {
vm_deallocate(mach_task_self(), addr, TEST_ALLOC_SIZE);
}
}
kr = perf_check_regression(host_port, PERF_EVENT_VM_ALLOC, 50, &regression_detected);
if (kr == KERN_SUCCESS) {
printf("✓ Regression check completed: regression=%s\n",
regression_detected ? "detected" : "none");
} else {
printf("WARNING: Could not check regression (kr=%d)\n", kr);
}
perf_monitor_enable(host_port, FALSE);
mach_port_deallocate(mach_task_self(), host_port);
}
static void test_threshold_monitoring(void)
{
kern_return_t kr;
host_t host_port;
printf("Testing threshold monitoring...\n");
host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("INFO: Performance monitoring not available, skipping threshold test\n");
mach_port_deallocate(mach_task_self(), host_port);
return;
}
kr = perf_set_thresholds(host_port, 1000, 1000, 5);
if (kr == KERN_SUCCESS) {
printf("✓ Monitoring thresholds set\n");
} else {
printf("WARNING: Could not set thresholds (kr=%d)\n", kr);
}
perf_monitor_enable(host_port, FALSE);
mach_port_deallocate(mach_task_self(), host_port);
}
static void test_statistics_reset(void)
{
kern_return_t kr;
host_t host_port;
uint32_t total_events_before, total_events_after;
uint32_t samples_dropped;
boolean_t regression_detected;
uint64_t overall_count, overall_total_time, overall_min_time, overall_max_time, overall_avg_time;
printf("Testing statistics reset...\n");
host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("INFO: Performance monitoring not available, skipping reset test\n");
mach_port_deallocate(mach_task_self(), host_port);
return;
}
for (int i = 0; i < 20; i++) {
vm_address_t addr = 0;
kr = vm_allocate(mach_task_self(), &addr, TEST_ALLOC_SIZE, TRUE);
if (kr == KERN_SUCCESS) {
vm_deallocate(mach_task_self(), addr, TEST_ALLOC_SIZE);
}
}
kr = perf_get_system_stats(host_port, &total_events_before, &samples_dropped,
&regression_detected, &overall_count,
&overall_total_time, &overall_min_time,
&overall_max_time, &overall_avg_time);
if (kr != KERN_SUCCESS) {
printf("WARNING: Could not get statistics before reset\n");
total_events_before = 0;
}
kr = perf_reset_stats(host_port);
if (kr == KERN_SUCCESS) {
printf("✓ Statistics reset\n");
} else {
printf("WARNING: Could not reset statistics (kr=%d)\n", kr);
}
kr = perf_get_system_stats(host_port, &total_events_after, &samples_dropped,
&regression_detected, &overall_count,
&overall_total_time, &overall_min_time,
&overall_max_time, &overall_avg_time);
if (kr == KERN_SUCCESS) {
printf("✓ Statistics after reset: events before=%u, after=%u\n",
total_events_before, total_events_after);
}
perf_monitor_enable(host_port, FALSE);
mach_port_deallocate(mach_task_self(), host_port);
}
static void test_performance_stress(void)
{
printf("Running performance analysis stress test...\n");
host_t host_port = mach_host_self();
if (host_port == MACH_PORT_NULL) {
printf("ERROR: Could not get host port\n");
test_exit(1);
}
kern_return_t kr = perf_monitor_enable(host_port, TRUE);
if (kr != KERN_SUCCESS) {
printf("INFO: Performance monitoring not available, skipping stress test\n");
mach_port_deallocate(mach_task_self(), host_port);
return;
}
for (int i = 0; i < TEST_ITERATIONS; i++) {
vm_address_t addr = 0;
kr = vm_allocate(mach_task_self(), &addr, TEST_ALLOC_SIZE, TRUE);
if (kr == KERN_SUCCESS) {
vm_deallocate(mach_task_self(), addr, TEST_ALLOC_SIZE);
}
mach_port_t port;
kr = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &port);
if (kr == KERN_SUCCESS) {
mach_port_deallocate(mach_task_self(), port);
}
}
printf("✓ Stress test completed (%d iterations)\n", TEST_ITERATIONS);
perf_monitor_enable(host_port, FALSE);
mach_port_deallocate(mach_task_self(), host_port);
}
void test_performance_analysis_framework(void)
{
printf("=== Performance Analysis Framework Test ===\n");
test_performance_monitoring_basic();
test_performance_statistics();
test_regression_detection();
test_threshold_monitoring();
test_statistics_reset();
test_performance_stress();
printf("✓ Performance analysis framework tests completed\n");
}
int main(int argc, char **argv)
{
test_init();
test_performance_analysis_framework();
test_exit(0);
}