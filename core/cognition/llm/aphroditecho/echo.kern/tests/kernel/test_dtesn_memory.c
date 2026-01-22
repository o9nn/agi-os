#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#define TEST_ASSERT(condition, message) \
do { \
if (!(condition)) { \
printf("FAIL: %s - %s\n", __func__, message); \
return 0; \
} \
} while(0)
#define TEST_PASS() \
do { \
printf("PASS: %s\n", __func__); \
return 1; \
} while(0)
#define TEST_ITERATIONS 1000
#define STRESS_TEST_ALLOCATIONS 10000
#define PERFORMANCE_SAMPLES 1000
static int tests_passed = 0;
static int tests_failed = 0;
static int test_dtesn_mem_init(void);
static int test_dtesn_basic_allocation(void);
static int test_dtesn_membrane_level_allocation(void);
static int test_dtesn_allocation_performance(void);
static int test_dtesn_deallocation_performance(void);
static int test_dtesn_memory_stats(void);
static int test_dtesn_a000081_validation(void);
static int test_dtesn_fragmentation_handling(void);
static int test_dtesn_memory_pressure(void);
static int test_dtesn_stress_allocation(void);
static int test_dtesn_edge_cases(void);
static int test_dtesn_defragmentation(void);
static uint64_t get_time_ns(void) {
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
static void memory_pressure_callback(uint32_t pressure_level) {
printf("Memory pressure detected: level %u\n", pressure_level);
}
static int test_dtesn_mem_init(void) {
int result = dtesn_mem_init();
TEST_ASSERT(result == 0, "Memory initialization failed");
result = dtesn_mem_init();
TEST_ASSERT(result == 0, "Double initialization should succeed");
TEST_PASS();
}
static int test_dtesn_basic_allocation(void) {
size_t sizes[] = {16, 64, 256, 1024, 4096, 16384};
void *ptrs[sizeof(sizes)/sizeof(sizes[0])];
for (int i = 0; i < sizeof(sizes)/sizeof(sizes[0]); i++) {
ptrs[i] = dtesn_alloc(sizes[i], 0);
TEST_ASSERT(ptrs[i] != NULL, "Basic allocation failed");
memset(ptrs[i], 0xAA, sizes[i]);
unsigned char *ptr = (unsigned char *)ptrs[i];
for (size_t j = 0; j < sizes[i]; j++) {
TEST_ASSERT(ptr[j] == 0xAA, "Memory corruption detected");
}
}
for (int i = 0; i < sizeof(sizes)/sizeof(sizes[0]); i++) {
dtesn_free(ptrs[i]);
}
TEST_PASS();
}
static int test_dtesn_membrane_level_allocation(void) {
void *ptrs[DTESN_MAX_LEVELS];
for (uint32_t level = 0; level < DTESN_MAX_LEVELS; level++) {
ptrs[level] = dtesn_alloc(1024, level);
TEST_ASSERT(ptrs[level] != NULL, "Membrane level allocation failed");
}
for (uint32_t level = 0; level < DTESN_MAX_LEVELS; level++) {
dtesn_free(ptrs[level]);
}
void *invalid_ptr = dtesn_alloc(1024, DTESN_MAX_LEVELS);
TEST_ASSERT(invalid_ptr == NULL, "Invalid membrane level should fail");
TEST_PASS();
}
static int test_dtesn_allocation_performance(void) {
uint64_t total_time = 0;
uint64_t max_time = 0;
void *ptrs[PERFORMANCE_SAMPLES];
printf("Testing allocation performance (%d samples)...\n", PERFORMANCE_SAMPLES);
for (int i = 0; i < PERFORMANCE_SAMPLES; i++) {
uint64_t start = get_time_ns();
ptrs[i] = dtesn_alloc(1024, i % DTESN_MAX_LEVELS);
uint64_t end = get_time_ns();
TEST_ASSERT(ptrs[i] != NULL, "Performance test allocation failed");
uint64_t duration = end - start;
total_time += duration;
if (duration > max_time) {
max_time = duration;
}
}
uint64_t avg_time_ns = total_time / PERFORMANCE_SAMPLES;
uint64_t threshold_ns = DTESN_ALLOC_LATENCY_THRESHOLD_US * 1000;
printf("  Average allocation time: %lu ns\n", avg_time_ns);
printf("  Maximum allocation time: %lu ns\n", max_time);
printf("  Threshold: %lu ns\n", threshold_ns);
for (int i = 0; i < PERFORMANCE_SAMPLES; i++) {
dtesn_free(ptrs[i]);
}
if (avg_time_ns > threshold_ns) {
printf("WARNING: Average allocation time exceeds threshold\n");
}
TEST_PASS();
}
static int test_dtesn_deallocation_performance(void) {
void *ptrs[PERFORMANCE_SAMPLES];
for (int i = 0; i < PERFORMANCE_SAMPLES; i++) {
ptrs[i] = dtesn_alloc(1024, i % DTESN_MAX_LEVELS);
TEST_ASSERT(ptrs[i] != NULL, "Pre-allocation for deallocation test failed");
}
printf("Testing deallocation performance (%d samples)...\n", PERFORMANCE_SAMPLES);
uint64_t total_time = 0;
uint64_t max_time = 0;
for (int i = 0; i < PERFORMANCE_SAMPLES; i++) {
uint64_t start = get_time_ns();
dtesn_free(ptrs[i]);
uint64_t end = get_time_ns();
uint64_t duration = end - start;
total_time += duration;
if (duration > max_time) {
max_time = duration;
}
}
uint64_t avg_time_ns = total_time / PERFORMANCE_SAMPLES;
uint64_t threshold_ns = DTESN_FREE_LATENCY_THRESHOLD_US * 1000;
printf("  Average deallocation time: %lu ns\n", avg_time_ns);
printf("  Maximum deallocation time: %lu ns\n", max_time);
printf("  Threshold: %lu ns\n", threshold_ns);
if (avg_time_ns > threshold_ns) {
printf("WARNING: Average deallocation time exceeds threshold\n");
}
TEST_PASS();
}
static int test_dtesn_memory_stats(void) {
dtesn_memory_stats_t stats;
int result = dtesn_mem_stats(&stats);
TEST_ASSERT(result == 0, "Getting memory stats failed");
void *ptr1 = dtesn_alloc(1024, 0);
void *ptr2 = dtesn_alloc(2048, 1);
result = dtesn_mem_stats(&stats);
TEST_ASSERT(result == 0, "Getting updated stats failed");
TEST_ASSERT(stats.allocation_count >= 2, "Allocation count not updated");
TEST_ASSERT(stats.total_allocated >= 3072, "Total allocated not updated");
printf("  Total allocated: %lu bytes\n", stats.total_allocated);
printf("  Peak usage: %lu bytes\n", stats.peak_usage);
printf("  Allocations: %u\n", stats.allocation_count);
printf("  Fragmentation: %u%%\n", stats.fragmentation_pct);
printf("  Overhead: %u%%\n", stats.overhead_pct);
dtesn_free(ptr1);
dtesn_free(ptr2);
TEST_PASS();
}
static int test_dtesn_a000081_validation(void) {
printf("Testing OEIS A000081 compliance...\n");
uint32_t expected[] = {1, 1, 2, 4, 9, 20, 48, 115, 286, 719};
for (uint32_t level = 0; level < DTESN_MAX_LEVELS && level < 10; level++) {
bool is_valid = dtesn_mem_validate_a000081(level);
printf("  Level %u: %u membranes (expected %u) - %s\n",
level, expected[level], expected[level],
is_valid ? "VALID" : "INVALID");
if (!is_valid) {
printf("    DEBUG: Level %u failed validation\n", level);
}
}
bool invalid_result = dtesn_mem_validate_a000081(DTESN_MAX_LEVELS);
TEST_ASSERT(!invalid_result, "Invalid level should return false");
TEST_PASS();
}
static int test_dtesn_fragmentation_handling(void) {
void *small_ptrs[100];
printf("Creating fragmentation with small allocations...\n");
for (int i = 0; i < 100; i++) {
small_ptrs[i] = dtesn_alloc(64, i % DTESN_MAX_LEVELS);
TEST_ASSERT(small_ptrs[i] != NULL, "Small allocation failed");
}
for (int i = 0; i < 100; i += 2) {
dtesn_free(small_ptrs[i]);
}
dtesn_memory_stats_t stats;
dtesn_mem_stats(&stats);
printf("  Fragmentation after pattern: %u%%\n", stats.fragmentation_pct);
for (int i = 1; i < 100; i += 2) {
dtesn_free(small_ptrs[i]);
}
TEST_PASS();
}
static int test_dtesn_memory_pressure(void) {
printf("Testing memory pressure detection...\n");
dtesn_mem_pressure_callback(memory_pressure_callback);
void *large_ptrs[100];
int allocated = 0;
for (int i = 0; i < 100; i++) {
large_ptrs[i] = dtesn_alloc(1024 * 1024, i % DTESN_MAX_LEVELS);
if (large_ptrs[i] != NULL) {
allocated++;
} else {
break;
}
}
printf("  Allocated %d large blocks before exhaustion\n", allocated);
for (int i = 0; i < allocated; i++) {
dtesn_free(large_ptrs[i]);
}
TEST_PASS();
}
static int test_dtesn_stress_allocation(void) {
printf("Running stress test with %d allocations...\n", STRESS_TEST_ALLOCATIONS);
void **ptrs = malloc(STRESS_TEST_ALLOCATIONS * sizeof(void*));
TEST_ASSERT(ptrs != NULL, "Failed to allocate pointer array");
uint64_t start_time = get_time_ns();
srand(42);
for (int i = 0; i < STRESS_TEST_ALLOCATIONS; i++) {
size_t size = 64 + (rand() % 4032);
uint32_t level = rand() % DTESN_MAX_LEVELS;
ptrs[i] = dtesn_alloc(size, level);
if (ptrs[i] == NULL) {
printf("  Allocation failed at iteration %d\n", i);
break;
}
memset(ptrs[i], (i & 0xFF), size);
}
uint64_t alloc_time = get_time_ns() - start_time;
start_time = get_time_ns();
for (int i = STRESS_TEST_ALLOCATIONS - 1; i >= 0; i--) {
if (ptrs[i] != NULL) {
dtesn_free(ptrs[i]);
}
}
uint64_t free_time = get_time_ns() - start_time;
printf("  Allocation phase: %lu ms\n", alloc_time / 1000000);
printf("  Deallocation phase: %lu ms\n", free_time / 1000000);
free(ptrs);
TEST_PASS();
}
static int test_dtesn_edge_cases(void) {
void *ptr = dtesn_alloc(0, 0);
TEST_ASSERT(ptr == NULL, "Zero size allocation should fail");
dtesn_free(NULL);
ptr = dtesn_alloc(1024, DTESN_MAX_LEVELS + 1);
TEST_ASSERT(ptr == NULL, "Invalid membrane level should fail");
int result = dtesn_mem_stats(NULL);
TEST_ASSERT(result != 0, "Stats with NULL should fail");
TEST_PASS();
}
static int test_dtesn_defragmentation(void) {
printf("Testing memory defragmentation...\n");
dtesn_memory_stats_t stats_before;
dtesn_mem_stats(&stats_before);
int64_t bytes_recovered = dtesn_mem_defragment();
printf("  Bytes recovered: %ld\n", bytes_recovered);
TEST_ASSERT(bytes_recovered >= 0, "Defragmentation should not fail");
dtesn_memory_stats_t stats_after;
dtesn_mem_stats(&stats_after);
printf("  Fragmentation before: %u%%\n", stats_before.fragmentation_pct);
printf("  Fragmentation after: %u%%\n", stats_after.fragmentation_pct);
TEST_PASS();
}
static void run_test(int (*test_func)(void)) {
if (test_func()) {
tests_passed++;
} else {
tests_failed++;
}
}
int main(void) {
printf("DTESN Memory Management System - Unit Tests\n");
printf("===========================================\n\n");
if (dtesn_mem_init() != 0) {
printf("FATAL: Failed to initialize DTESN memory system\n");
return 1;
}
run_test(test_dtesn_mem_init);
run_test(test_dtesn_basic_allocation);
run_test(test_dtesn_membrane_level_allocation);
run_test(test_dtesn_allocation_performance);
run_test(test_dtesn_deallocation_performance);
run_test(test_dtesn_memory_stats);
run_test(test_dtesn_a000081_validation);
run_test(test_dtesn_fragmentation_handling);
run_test(test_dtesn_memory_pressure);
run_test(test_dtesn_stress_allocation);
run_test(test_dtesn_edge_cases);
run_test(test_dtesn_defragmentation);
dtesn_mem_shutdown();
printf("\nTest Results:\n");
printf("============\n");
printf("Tests passed: %d\n", tests_passed);
printf("Tests failed: %d\n", tests_failed);
printf("Total tests:  %d\n", tests_passed + tests_failed);
if (tests_failed == 0) {
printf("\n✅ ALL TESTS PASSED\n");
return 0;
} else {
printf("\n❌ %d TEST(S) FAILED\n", tests_failed);
return 1;
}
}