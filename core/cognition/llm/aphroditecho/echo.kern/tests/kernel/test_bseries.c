#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/bseries.h"
#include "include/dtesn/memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <math.h>
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
#define TEST_PERFORMANCE_THRESHOLD_NS(actual, threshold_us, label) \
do { \
uint64_t threshold_ns = (threshold_us) * 1000; \
if ((actual) > threshold_ns) { \
printf("WARNING: %s performance: %lu ns (threshold: %lu ns)\n", \
label, actual, threshold_ns); \
} \
} while(0)
#define TEST_TOLERANCE 1e-10
#define TEST_ITERATIONS 100
#define STRESS_TEST_TREES 50
#define PERFORMANCE_SAMPLES 1000
#define VECTOR_TEST_SIZE 100
static struct {
int tests_run;
int tests_passed;
int tests_failed;
uint64_t total_test_time_ns;
} g_test_stats = {0};
static uint64_t get_test_timestamp_ns(void)
{
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
static int test_bseries_init(void)
{
g_test_stats.tests_run++;
int result = dtesn_bseries_init();
TEST_ASSERT(result == 0, "B-Series system initialization failed");
result = dtesn_bseries_init();
TEST_ASSERT(result == 0, "Double initialization should succeed");
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_bseries_system_lifecycle(void)
{
g_test_stats.tests_run++;
dtesn_bseries_system_t *system = dtesn_bseries_system_create("test_system", 5);
TEST_ASSERT(system != NULL, "System creation failed");
TEST_ASSERT(strcmp(system->system_name, "test_system") == 0, "System name mismatch");
TEST_ASSERT(system->max_order == 5, "Max order mismatch");
TEST_ASSERT(system->orders != NULL, "Orders array not allocated");
dtesn_bseries_system_t *invalid_system = dtesn_bseries_system_create(NULL, 5);
TEST_ASSERT(invalid_system == NULL, "Should fail with NULL name");
invalid_system = dtesn_bseries_system_create("test", 0);
TEST_ASSERT(invalid_system == NULL, "Should fail with zero order");
invalid_system = dtesn_bseries_system_create("test", DTESN_BSERIES_MAX_ORDER + 1);
TEST_ASSERT(invalid_system == NULL, "Should fail with excessive order");
dtesn_bseries_system_destroy(system);
dtesn_bseries_system_destroy(NULL);
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_tree_initialization(void)
{
g_test_stats.tests_run++;
dtesn_bseries_tree_t tree;
uint64_t start_time = get_test_timestamp_ns();
int result = bseries_tree_init(&tree, 1, DTESN_BSERIES_SINGLE_NODE);
uint64_t elapsed_time = get_test_timestamp_ns() - start_time;
TEST_ASSERT(result == 0, "Single node tree initialization failed");
TEST_ASSERT(tree.order == 1, "Order mismatch");
TEST_ASSERT(tree.tree_type == DTESN_BSERIES_SINGLE_NODE, "Type mismatch");
TEST_ASSERT(tree.nodes != NULL, "Nodes not allocated");
TEST_ASSERT(tree.node_count == 1, "Node count mismatch");
TEST_PERFORMANCE_THRESHOLD_NS(elapsed_time, DTESN_BSERIES_TREE_THRESHOLD_US, "tree_init");
if (tree.nodes) {
dtesn_free(tree.nodes);
}
start_time = get_test_timestamp_ns();
result = bseries_tree_init(&tree, 3, DTESN_BSERIES_LINEAR_CHAIN);
elapsed_time = get_test_timestamp_ns() - start_time;
TEST_ASSERT(result == 0, "Linear chain tree initialization failed");
TEST_ASSERT(tree.order == 3, "Order mismatch for linear chain");
TEST_ASSERT(tree.tree_type == DTESN_BSERIES_LINEAR_CHAIN, "Type mismatch for linear chain");
TEST_ASSERT(tree.max_depth == 2, "Max depth should be 2 for 3-node chain");
TEST_PERFORMANCE_THRESHOLD_NS(elapsed_time, DTESN_BSERIES_TREE_THRESHOLD_US, "tree_init_chain");
if (tree.nodes) {
for (uint32_t i = 0; i < tree.node_count; i++) {
if (tree.nodes[i].children_ids) {
dtesn_free(tree.nodes[i].children_ids);
}
}
dtesn_free(tree.nodes);
}
result = bseries_tree_init(NULL, 1, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with NULL tree");
result = bseries_tree_init(&tree, 0, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with zero order");
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_coefficient_computation(void)
{
g_test_stats.tests_run++;
dtesn_bseries_tree_t tree;
double coefficient;
uint64_t start_time, elapsed_time;
int result = bseries_tree_init(&tree, 1, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result == 0, "Tree initialization failed");
start_time = get_test_timestamp_ns();
result = bseries_compute_coefficient(&tree, &coefficient);
elapsed_time = get_test_timestamp_ns() - start_time;
TEST_ASSERT(result == 0, "Coefficient computation failed");
TEST_ASSERT(fabs(coefficient - 1.0) < TEST_TOLERANCE, "Single node coefficient should be 1.0");
TEST_PERFORMANCE_THRESHOLD_NS(elapsed_time, DTESN_BSERIES_COEFF_THRESHOLD_US, "coeff_compute");
if (tree.nodes) {
dtesn_free(tree.nodes);
}
result = bseries_tree_init(&tree, 2, DTESN_BSERIES_LINEAR_CHAIN);
TEST_ASSERT(result == 0, "Linear chain initialization failed");
start_time = get_test_timestamp_ns();
result = bseries_compute_coefficient(&tree, &coefficient);
elapsed_time = get_test_timestamp_ns() - start_time;
TEST_ASSERT(result == 0, "Linear chain coefficient computation failed");
TEST_ASSERT(fabs(coefficient - 0.5) < TEST_TOLERANCE, "Linear chain coefficient should be 1/2");
TEST_PERFORMANCE_THRESHOLD_NS(elapsed_time, DTESN_BSERIES_COEFF_THRESHOLD_US, "coeff_compute_chain");
if (tree.nodes) {
for (uint32_t i = 0; i < tree.node_count; i++) {
if (tree.nodes[i].children_ids) {
dtesn_free(tree.nodes[i].children_ids);
}
}
dtesn_free(tree.nodes);
}
result = bseries_compute_coefficient(NULL, &coefficient);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with NULL tree");
result = bseries_compute_coefficient(&tree, NULL);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with NULL coefficient");
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_tree_classification(void)
{
g_test_stats.tests_run++;
dtesn_bseries_tree_t tree;
dtesn_bseries_tree_type_t classified_type;
int result = bseries_tree_init(&tree, 1, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result == 0, "Single node tree initialization failed");
result = bseries_tree_classify(&tree, &classified_type);
TEST_ASSERT(result == 0, "Single node classification failed");
TEST_ASSERT(classified_type == DTESN_BSERIES_SINGLE_NODE, "Single node misclassified");
if (tree.nodes) {
dtesn_free(tree.nodes);
}
result = bseries_tree_init(&tree, 3, DTESN_BSERIES_LINEAR_CHAIN);
TEST_ASSERT(result == 0, "Linear chain tree initialization failed");
result = bseries_tree_classify(&tree, &classified_type);
TEST_ASSERT(result == 0, "Linear chain classification failed");
TEST_ASSERT(classified_type == DTESN_BSERIES_LINEAR_CHAIN, "Linear chain misclassified");
if (tree.nodes) {
for (uint32_t i = 0; i < tree.node_count; i++) {
if (tree.nodes[i].children_ids) {
dtesn_free(tree.nodes[i].children_ids);
}
}
dtesn_free(tree.nodes);
}
result = bseries_tree_init(&tree, 3, DTESN_BSERIES_STAR_GRAPH);
TEST_ASSERT(result == 0, "Star graph tree initialization failed");
result = bseries_tree_classify(&tree, &classified_type);
TEST_ASSERT(result == 0, "Star graph classification failed");
TEST_ASSERT(classified_type == DTESN_BSERIES_STAR_GRAPH, "Star graph misclassified");
if (tree.nodes) {
for (uint32_t i = 0; i < tree.node_count; i++) {
if (tree.nodes[i].children_ids) {
dtesn_free(tree.nodes[i].children_ids);
}
}
dtesn_free(tree.nodes);
}
result = bseries_tree_classify(NULL, &classified_type);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with NULL tree");
result = bseries_tree_classify(&tree, NULL);
TEST_ASSERT(result == DTESN_BSERIES_EINVAL, "Should fail with NULL type");
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_stability_validation(void)
{
g_test_stats.tests_run++;
dtesn_bseries_tree_t tree;
double coefficient;
int result = bseries_tree_init(&tree, 1, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result == 0, "Tree initialization failed");
result = bseries_compute_coefficient(&tree, &coefficient);
TEST_ASSERT(result == 0, "Coefficient computation failed");
bool is_stable = bseries_validate_stability(&tree, TEST_TOLERANCE);
TEST_ASSERT(is_stable, "Single node should be stable");
if (tree.nodes) {
dtesn_free(tree.nodes);
}
result = bseries_tree_init(&tree, 2, DTESN_BSERIES_LINEAR_CHAIN);
TEST_ASSERT(result == 0, "Linear chain initialization failed");
result = bseries_compute_coefficient(&tree, &coefficient);
TEST_ASSERT(result == 0, "Linear chain coefficient computation failed");
is_stable = bseries_validate_stability(&tree, TEST_TOLERANCE);
TEST_ASSERT(is_stable, "Linear chain should be stable");
if (tree.nodes) {
for (uint32_t i = 0; i < tree.node_count; i++) {
if (tree.nodes[i].children_ids) {
dtesn_free(tree.nodes[i].children_ids);
}
}
dtesn_free(tree.nodes);
}
is_stable = bseries_validate_stability(NULL, TEST_TOLERANCE);
TEST_ASSERT(!is_stable, "Should fail with NULL tree");
is_stable = bseries_validate_stability(&tree, -1.0);
TEST_ASSERT(!is_stable, "Should fail with negative tolerance");
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_vector_operations(void)
{
g_test_stats.tests_run++;
dtesn_bseries_system_t *system = dtesn_bseries_system_create("vector_test", 3);
TEST_ASSERT(system != NULL, "System creation failed");
int tree_count = dtesn_bseries_generate_order(system, 1);
TEST_ASSERT(tree_count >= 1, "Failed to generate order 1 trees");
tree_count = dtesn_bseries_generate_order(system, 2);
TEST_ASSERT(tree_count >= 1, "Failed to generate order 2 trees");
dtesn_bseries_vector_op_t vector_op;
vector_op.tree_count = 2;
vector_op.trees = malloc(sizeof(dtesn_bseries_tree_t*) * vector_op.tree_count);
vector_op.coefficients = malloc(sizeof(double) * vector_op.tree_count);
vector_op.computational_costs = malloc(sizeof(double) * vector_op.tree_count);
TEST_ASSERT(vector_op.trees != NULL, "Failed to allocate tree pointers");
TEST_ASSERT(vector_op.coefficients != NULL, "Failed to allocate coefficients");
TEST_ASSERT(vector_op.computational_costs != NULL, "Failed to allocate costs");
vector_op.trees[0] = dtesn_bseries_get_tree(system, 1, 0);
vector_op.trees[1] = dtesn_bseries_get_tree(system, 2, 0);
TEST_ASSERT(vector_op.trees[0] != NULL, "Failed to get order 1 tree");
TEST_ASSERT(vector_op.trees[1] != NULL, "Failed to get order 2 tree");
uint64_t start_time = get_test_timestamp_ns();
int result = bseries_vector_op(&vector_op);
uint64_t elapsed_time = get_test_timestamp_ns() - start_time;
TEST_ASSERT(result == 0, "Vector operation failed");
TEST_ASSERT(vector_op.success, "Vector operation not marked as successful");
TEST_ASSERT(fabs(vector_op.coefficients[0] - 1.0) < TEST_TOLERANCE, "Order 1 coefficient incorrect");
TEST_ASSERT(fabs(vector_op.coefficients[1] - 0.5) < TEST_TOLERANCE, "Order 2 coefficient incorrect");
printf("Vector operation time: %lu ns for %u trees\n", elapsed_time, vector_op.tree_count);
free(vector_op.trees);
free(vector_op.coefficients);
free(vector_op.computational_costs);
dtesn_bseries_system_destroy(system);
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_oeis_compliance(void)
{
g_test_stats.tests_run++;
dtesn_bseries_system_t *system = dtesn_bseries_system_create("oeis_test", 5);
TEST_ASSERT(system != NULL, "System creation failed");
int generated;
generated = dtesn_bseries_generate_order(system, 1);
TEST_ASSERT(generated == 1, "Order 1 should have 1 tree");
generated = dtesn_bseries_generate_order(system, 2);
TEST_ASSERT(generated == 1, "Order 2 should have 1 tree");
generated = dtesn_bseries_generate_order(system, 3);
TEST_ASSERT(generated == 2, "Order 3 should have 2 trees");
bool is_compliant = dtesn_bseries_validate_a000081(system);
TEST_ASSERT(is_compliant, "System should be OEIS A000081 compliant");
TEST_ASSERT(system->orders[1].expected_count == 1, "Order 1 expected count wrong");
TEST_ASSERT(system->orders[2].expected_count == 1, "Order 2 expected count wrong");
TEST_ASSERT(system->orders[3].expected_count == 2, "Order 3 expected count wrong");
TEST_ASSERT(system->orders[1].tree_count == 1, "Order 1 actual count wrong");
TEST_ASSERT(system->orders[2].tree_count == 1, "Order 2 actual count wrong");
TEST_ASSERT(system->orders[3].tree_count == 2, "Order 3 actual count wrong");
dtesn_bseries_system_destroy(system);
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_performance_statistics(void)
{
g_test_stats.tests_run++;
dtesn_bseries_system_t *system = dtesn_bseries_system_create("perf_test", 3);
TEST_ASSERT(system != NULL, "System creation failed");
dtesn_bseries_generate_order(system, 1);
dtesn_bseries_generate_order(system, 2);
dtesn_bseries_generate_order(system, 3);
dtesn_bseries_stats_t stats;
int result = dtesn_bseries_get_stats(system, &stats);
TEST_ASSERT(result == 0, "Failed to get performance statistics");
TEST_ASSERT(stats.vector_throughput_per_ms > 0, "Vector throughput should be positive");
TEST_ASSERT(stats.cache_hit_rate_pct <= 100, "Cache hit rate should be <= 100%");
printf("Performance Statistics:\n");
printf("  Tree computations: %lu\n", stats.total_tree_computations);
printf("  Coefficient computations: %lu\n", stats.total_coefficient_computations);
printf("  Vector throughput: %u trees/ms\n", stats.vector_throughput_per_ms);
printf("  Cache hit rate: %u%%\n", stats.cache_hit_rate_pct);
printf("  Tree threshold met: %s\n", stats.tree_threshold_met ? "Yes" : "No");
printf("  Coefficient threshold met: %s\n", stats.coeff_threshold_met ? "Yes" : "No");
printf("  Vector threshold met: %s\n", stats.vector_threshold_met ? "Yes" : "No");
dtesn_bseries_system_destroy(system);
g_test_stats.tests_passed++;
TEST_PASS();
}
static int test_tree_isomorphism(void)
{
g_test_stats.tests_run++;
dtesn_bseries_tree_t tree1, tree2, tree3;
int result1 = bseries_tree_init(&tree1, 1, DTESN_BSERIES_SINGLE_NODE);
int result2 = bseries_tree_init(&tree2, 1, DTESN_BSERIES_SINGLE_NODE);
TEST_ASSERT(result1 == 0 && result2 == 0, "Tree initialization failed");
bool is_isomorphic = dtesn_bseries_tree_isomorphic(&tree1, &tree2);
TEST_ASSERT(is_isomorphic, "Identical trees should be isomorphic");
int result3 = bseries_tree_init(&tree3, 2, DTESN_BSERIES_LINEAR_CHAIN);
TEST_ASSERT(result3 == 0, "Linear chain initialization failed");
is_isomorphic = dtesn_bseries_tree_isomorphic(&tree1, &tree3);
TEST_ASSERT(!is_isomorphic, "Different trees should not be isomorphic");
is_isomorphic = dtesn_bseries_tree_isomorphic(NULL, &tree2);
TEST_ASSERT(!is_isomorphic, "Should return false with NULL tree");
is_isomorphic = dtesn_bseries_tree_isomorphic(&tree1, NULL);
TEST_ASSERT(!is_isomorphic, "Should return false with NULL tree");
if (tree1.nodes) dtesn_free(tree1.nodes);
if (tree2.nodes) dtesn_free(tree2.nodes);
if (tree3.nodes) {
for (uint32_t i = 0; i < tree3.node_count; i++) {
if (tree3.nodes[i].children_ids) {
dtesn_free(tree3.nodes[i].children_ids);
}
}
dtesn_free(tree3.nodes);
}
g_test_stats.tests_passed++;
TEST_PASS();
}
static void run_test(const char *test_name, int (*test_func)(void))
{
printf("Running %s...\n", test_name);
uint64_t start_time = get_test_timestamp_ns();
int result = test_func();
uint64_t elapsed_time = get_test_timestamp_ns() - start_time;
g_test_stats.total_test_time_ns += elapsed_time;
if (!result) {
g_test_stats.tests_failed++;
printf("  Time: %lu ns\n", elapsed_time);
} else {
printf("  Time: %lu ns\n", elapsed_time);
}
printf("\n");
}
int main(void)
{
printf("DTESN B-Series Tree Computation Engine Unit Tests\n");
printf("================================================\n\n");
memset(&g_test_stats, 0, sizeof(g_test_stats));
run_test("B-Series Initialization", test_bseries_init);
run_test("System Lifecycle", test_bseries_system_lifecycle);
run_test("Tree Initialization", test_tree_initialization);
run_test("Coefficient Computation", test_coefficient_computation);
run_test("Tree Classification", test_tree_classification);
run_test("Stability Validation", test_stability_validation);
run_test("Vector Operations", test_vector_operations);
run_test("OEIS A000081 Compliance", test_oeis_compliance);
run_test("Performance Statistics", test_performance_statistics);
run_test("Tree Isomorphism", test_tree_isomorphism);
printf("Test Summary\n");
printf("============\n");
printf("Tests run: %d\n", g_test_stats.tests_run);
printf("Tests passed: %d\n", g_test_stats.tests_passed);
printf("Tests failed: %d\n", g_test_stats.tests_failed);
printf("Success rate: %.1f%%\n",
g_test_stats.tests_run > 0 ?
(100.0 * g_test_stats.tests_passed / g_test_stats.tests_run) : 0.0);
printf("Total test time: %.3f ms\n", g_test_stats.total_test_time_ns / 1000000.0);
dtesn_bseries_shutdown();
return (g_test_stats.tests_failed == 0) ? 0 : 1;
}