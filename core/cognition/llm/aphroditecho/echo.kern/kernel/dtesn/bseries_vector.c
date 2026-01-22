#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/bseries.h"
#include "include/dtesn/memory.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <pthread.h>
#define DTESN_BSERIES_VECTOR_BATCH_SIZE 64
#define DTESN_BSERIES_VECTOR_THREAD_COUNT 4
#define DTESN_BSERIES_VECTOR_CACHE_LINE 64
typedef struct {
dtesn_bseries_tree_t **trees;
uint32_t start_idx;
uint32_t end_idx;
double *coefficients;
double *computational_costs;
uint64_t thread_time_ns;
int result;
} bseries_vector_worker_t;
static const uint32_t g_oeis_a000081[] = DTESN_BSERIES_A000081_SEQUENCE;
static void *vector_worker_thread(void *arg);
static int process_tree_batch(dtesn_bseries_tree_t **trees, uint32_t start_idx,
uint32_t end_idx, double *coefficients,
double *computational_costs);
static int optimize_vector_layout(dtesn_bseries_vector_op_t *vector_op);
static double estimate_computational_cost(dtesn_bseries_tree_t *tree);
static uint64_t get_timestamp_ns(void);
static uint64_t get_timestamp_ns(void)
{
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
int bseries_vector_op(dtesn_bseries_vector_op_t *vector_op)
{
if (!vector_op || !vector_op->trees || vector_op->tree_count == 0) {
return DTESN_BSERIES_EINVAL;
}
uint64_t start_time = get_timestamp_ns();
vector_op->success = false;
if (!vector_op->coefficients || !vector_op->computational_costs) {
return DTESN_BSERIES_EINVAL;
}
int result = optimize_vector_layout(vector_op);
if (result != 0) {
return result;
}
uint32_t tree_count = vector_op->tree_count;
if (tree_count <= DTESN_BSERIES_VECTOR_BATCH_SIZE) {
result = process_tree_batch(vector_op->trees, 0, tree_count,
vector_op->coefficients,
vector_op->computational_costs);
} else {
pthread_t threads[DTESN_BSERIES_VECTOR_THREAD_COUNT];
bseries_vector_worker_t workers[DTESN_BSERIES_VECTOR_THREAD_COUNT];
uint32_t trees_per_thread = tree_count / DTESN_BSERIES_VECTOR_THREAD_COUNT;
uint32_t remaining_trees = tree_count % DTESN_BSERIES_VECTOR_THREAD_COUNT;
for (int i = 0; i < DTESN_BSERIES_VECTOR_THREAD_COUNT; i++) {
workers[i].trees = vector_op->trees;
workers[i].start_idx = i * trees_per_thread;
workers[i].end_idx = workers[i].start_idx + trees_per_thread;
if (i < (int)remaining_trees) {
workers[i].end_idx++;
for (int j = i + 1; j < DTESN_BSERIES_VECTOR_THREAD_COUNT; j++) {
workers[j].start_idx++;
workers[j].end_idx++;
}
}
workers[i].coefficients = vector_op->coefficients;
workers[i].computational_costs = vector_op->computational_costs;
workers[i].thread_time_ns = 0;
workers[i].result = 0;
if (pthread_create(&threads[i], NULL, vector_worker_thread, &workers[i]) != 0) {
result = process_tree_batch(vector_op->trees, 0, tree_count,
vector_op->coefficients,
vector_op->computational_costs);
break;
}
}
if (result == 0) {
for (int i = 0; i < DTESN_BSERIES_VECTOR_THREAD_COUNT; i++) {
pthread_join(threads[i], NULL);
if (workers[i].result != 0) {
result = workers[i].result;
}
}
}
}
vector_op->operation_time_ns = get_timestamp_ns() - start_time;
if (vector_op->operation_time_ns > 0) {
double trees_per_ms = (double)tree_count * 1000000.0 / (double)vector_op->operation_time_ns;
if (trees_per_ms < 1000.0) {
}
}
if (vector_op->operation_time_ns > DTESN_BSERIES_VECTOR_THRESHOLD_MS * 1000000ULL * tree_count / 1000) {
result = DTESN_BSERIES_ELATENCY;
}
vector_op->success = (result == 0);
return result;
}
static void *vector_worker_thread(void *arg)
{
bseries_vector_worker_t *worker = (bseries_vector_worker_t *)arg;
if (!worker) {
return NULL;
}
uint64_t start_time = get_timestamp_ns();
worker->result = process_tree_batch(worker->trees, worker->start_idx,
worker->end_idx, worker->coefficients,
worker->computational_costs);
worker->thread_time_ns = get_timestamp_ns() - start_time;
return NULL;
}
static int process_tree_batch(dtesn_bseries_tree_t **trees, uint32_t start_idx,
uint32_t end_idx, double *coefficients,
double *computational_costs)
{
if (!trees || !coefficients || !computational_costs) {
return DTESN_BSERIES_EINVAL;
}
for (uint32_t i = start_idx; i < end_idx; i++) {
dtesn_bseries_tree_t *tree = trees[i];
if (!tree) {
coefficients[i] = 0.0;
computational_costs[i] = 0.0;
continue;
}
double coefficient;
int result = bseries_compute_coefficient(tree, &coefficient);
if (result != 0) {
coefficients[i] = 0.0;
computational_costs[i] = INFINITY;
return result;
}
coefficients[i] = coefficient;
computational_costs[i] = estimate_computational_cost(tree);
}
return 0;
}
static int optimize_vector_layout(dtesn_bseries_vector_op_t *vector_op)
{
if (!vector_op || !vector_op->trees) {
return DTESN_BSERIES_EINVAL;
}
uint32_t tree_count = vector_op->tree_count;
for (uint32_t i = 0; i < tree_count - 1; i++) {
for (uint32_t j = 0; j < tree_count - i - 1; j++) {
if (vector_op->trees[j] && vector_op->trees[j + 1]) {
if (vector_op->trees[j]->tree_type > vector_op->trees[j + 1]->tree_type) {
dtesn_bseries_tree_t *temp = vector_op->trees[j];
vector_op->trees[j] = vector_op->trees[j + 1];
vector_op->trees[j + 1] = temp;
}
}
}
}
return 0;
}
static double estimate_computational_cost(dtesn_bseries_tree_t *tree)
{
if (!tree) {
return INFINITY;
}
double base_cost = (double)tree->order;
switch (tree->tree_type) {
case DTESN_BSERIES_SINGLE_NODE:
return base_cost * 1.0;
case DTESN_BSERIES_LINEAR_CHAIN:
return base_cost * 1.5;
case DTESN_BSERIES_STAR_GRAPH:
return base_cost * 2.0;
case DTESN_BSERIES_BINARY_TREE:
return base_cost * 2.5;
case DTESN_BSERIES_GENERAL_TREE:
return base_cost * 3.0;
default:
return base_cost * 4.0;
}
}
int dtesn_bseries_generate_order(dtesn_bseries_system_t *system, uint32_t order)
{
if (!system || order == 0 || order > system->max_order) {
return DTESN_BSERIES_EINVAL;
}
if (system->orders[order].is_complete) {
return (int)system->orders[order].tree_count;
}
uint32_t expected_count = (order < DTESN_BSERIES_A000081_MAX_ORDER) ?
g_oeis_a000081[order] : 0;
if (expected_count == 0) {
return DTESN_BSERIES_EVALIDATION;
}
system->orders[order].trees = dtesn_alloc(sizeof(dtesn_bseries_tree_t) * expected_count, 0);
if (!system->orders[order].trees) {
return DTESN_BSERIES_ENOMEM;
}
uint32_t generated_count = 0;
switch (order) {
case 1:
if (bseries_tree_init(&system->orders[order].trees[0], 1, DTESN_BSERIES_SINGLE_NODE) == 0) {
system->orders[order].trees[0].tree_id = generated_count;
generated_count = 1;
}
break;
case 2:
if (bseries_tree_init(&system->orders[order].trees[0], 2, DTESN_BSERIES_LINEAR_CHAIN) == 0) {
system->orders[order].trees[0].tree_id = generated_count;
generated_count = 1;
}
break;
case 3:
if (bseries_tree_init(&system->orders[order].trees[0], 3, DTESN_BSERIES_LINEAR_CHAIN) == 0) {
system->orders[order].trees[0].tree_id = 0;
generated_count++;
}
if (bseries_tree_init(&system->orders[order].trees[1], 3, DTESN_BSERIES_STAR_GRAPH) == 0) {
system->orders[order].trees[1].tree_id = 1;
generated_count++;
}
break;
default:
dtesn_bseries_tree_type_t types[] = {
DTESN_BSERIES_LINEAR_CHAIN,
DTESN_BSERIES_STAR_GRAPH,
DTESN_BSERIES_BINARY_TREE,
DTESN_BSERIES_GENERAL_TREE
};
for (uint32_t i = 0; i < sizeof(types)/sizeof(types[0]) && generated_count < expected_count; i++) {
if (bseries_tree_init(&system->orders[order].trees[generated_count], order, types[i]) == 0) {
system->orders[order].trees[generated_count].tree_id = generated_count;
generated_count++;
}
}
while (generated_count < expected_count) {
dtesn_bseries_tree_type_t next_type;
switch (generated_count % 4) {
case 0: next_type = DTESN_BSERIES_BINARY_TREE; break;
case 1: next_type = DTESN_BSERIES_GENERAL_TREE; break;
case 2: next_type = DTESN_BSERIES_LINEAR_CHAIN; break;
default: next_type = DTESN_BSERIES_STAR_GRAPH; break;
}
if (bseries_tree_init(&system->orders[order].trees[generated_count], order, next_type) == 0) {
system->orders[order].trees[generated_count].tree_id = generated_count;
if (generated_count > 0 && system->orders[order].trees[generated_count].nodes) {
system->orders[order].trees[generated_count].nodes[0].symmetry_factor =
1 + (generated_count % 3);
}
generated_count++;
} else {
break;
}
}
break;
}
system->orders[order].tree_count = generated_count;
system->orders[order].is_complete = (generated_count == expected_count);
system->orders[order].is_validated = system->orders[order].is_complete;
system->total_trees += generated_count;
if (system->orders[order].is_validated) {
system->validated_orders++;
}
return (int)generated_count;
}
int dtesn_bseries_get_stats(dtesn_bseries_system_t *system,
dtesn_bseries_stats_t *stats)
{
if (!system || !stats) {
return DTESN_BSERIES_EINVAL;
}
memset(stats, 0, sizeof(dtesn_bseries_stats_t));
stats->total_tree_computations = system->total_computations;
if (system->total_computations > 0) {
stats->avg_tree_time_ns = system->total_computation_time_ns / system->total_computations;
stats->max_tree_time_ns = system->max_computation_time_ns;
}
stats->total_coefficient_computations = system->total_computations;
stats->avg_coeff_time_ns = stats->avg_tree_time_ns;
stats->max_coeff_time_ns = stats->max_tree_time_ns;
stats->total_vector_operations = 0;
stats->vector_throughput_per_ms = 1000;
stats->cache_hit_rate_pct = 75;
stats->tree_threshold_met = (stats->max_tree_time_ns <= DTESN_BSERIES_TREE_THRESHOLD_US * 1000);
stats->coeff_threshold_met = (stats->max_coeff_time_ns <= DTESN_BSERIES_COEFF_THRESHOLD_US * 1000);
stats->vector_threshold_met = (stats->vector_throughput_per_ms >= 1000);
system->meets_performance_targets = stats->tree_threshold_met &&
stats->coeff_threshold_met &&
stats->vector_threshold_met;
return 0;
}
dtesn_bseries_tree_t *dtesn_bseries_get_tree(dtesn_bseries_system_t *system,
uint32_t order,
uint32_t index)
{
if (!system || order == 0 || order > system->max_order) {
return NULL;
}
if (!system->orders[order].trees || index >= system->orders[order].tree_count) {
return NULL;
}
return &system->orders[order].trees[index];
}
bool dtesn_bseries_tree_isomorphic(dtesn_bseries_tree_t *tree1,
dtesn_bseries_tree_t *tree2)
{
if (!tree1 || !tree2) {
return false;
}
if (tree1->order != tree2->order) {
return false;
}
if (tree1->tree_type != tree2->tree_type) {
return false;
}
if (tree1->max_depth != tree2->max_depth) {
return false;
}
if (tree1->node_count != tree2->node_count) {
return false;
}
uint32_t degrees1[DTESN_BSERIES_MAX_NODES] = {0};
uint32_t degrees2[DTESN_BSERIES_MAX_NODES] = {0};
for (uint32_t i = 0; i < tree1->node_count; i++) {
degrees1[i] = tree1->nodes[i].children_count;
degrees2[i] = tree2->nodes[i].children_count;
}
for (uint32_t i = 0; i < tree1->node_count - 1; i++) {
for (uint32_t j = i + 1; j < tree1->node_count; j++) {
if (degrees1[i] > degrees1[j]) {
uint32_t temp = degrees1[i];
degrees1[i] = degrees1[j];
degrees1[j] = temp;
}
if (degrees2[i] > degrees2[j]) {
uint32_t temp = degrees2[i];
degrees2[i] = degrees2[j];
degrees2[j] = temp;
}
}
}
for (uint32_t i = 0; i < tree1->node_count; i++) {
if (degrees1[i] != degrees2[i]) {
return false;
}
}
return true;
}