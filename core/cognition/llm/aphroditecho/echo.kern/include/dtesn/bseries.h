#ifndef DTESN_BSERIES_H
#define DTESN_BSERIES_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <math.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_BSERIES_MAX_ORDER         10
#define DTESN_BSERIES_MAX_TREES         1024
#define DTESN_BSERIES_MAX_NODES         64
#define DTESN_BSERIES_MAX_CHILDREN      8
#define DTESN_BSERIES_MAX_SYMBOL_LEN    128
#define DTESN_BSERIES_TREE_THRESHOLD_US       100
#define DTESN_BSERIES_COEFF_THRESHOLD_US      50
#define DTESN_BSERIES_VECTOR_THRESHOLD_MS     1
#define DTESN_BSERIES_A000081_MAX_ORDER 15
#define DTESN_BSERIES_A000081_SEQUENCE \
{ 0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973 }
typedef enum {
DTESN_BSERIES_SINGLE_NODE = 0,
DTESN_BSERIES_LINEAR_CHAIN = 1,
DTESN_BSERIES_STAR_GRAPH = 2,
DTESN_BSERIES_BINARY_TREE = 3,
DTESN_BSERIES_GENERAL_TREE = 4
} dtesn_bseries_tree_type_t;
typedef enum {
DTESN_BSERIES_DIFF_F = 0,
DTESN_BSERIES_DIFF_FP = 1,
DTESN_BSERIES_DIFF_FPP = 2,
DTESN_BSERIES_DIFF_FPPP = 3,
DTESN_BSERIES_DIFF_COMPLEX = 4
} dtesn_bseries_diff_type_t;
typedef struct dtesn_bseries_node {
uint32_t node_id;
uint32_t parent_id;
uint32_t *children_ids;
uint32_t children_count;
uint32_t subtree_size;
uint32_t depth;
double coefficient_contribution;
dtesn_bseries_diff_type_t diff_type;
uint32_t symmetry_factor;
} dtesn_bseries_node_t;
typedef struct dtesn_bseries_tree {
uint32_t tree_id;
uint32_t order;
dtesn_bseries_tree_type_t tree_type;
dtesn_bseries_node_t *nodes;
uint32_t node_count;
uint32_t root_id;
uint32_t max_depth;
double bseries_coefficient;
double gamma_factor;
uint32_t factorial_order;
char differential_expression[DTESN_BSERIES_MAX_SYMBOL_LEN];
dtesn_bseries_diff_type_t primary_diff_type;
double computational_cost;
bool is_validated;
uint64_t creation_time_ns;
uint64_t last_computed_ns;
} dtesn_bseries_tree_t;
typedef struct dtesn_bseries_order {
uint32_t order;
uint32_t tree_count;
uint32_t expected_count;
dtesn_bseries_tree_t *trees;
bool is_complete;
bool is_validated;
} dtesn_bseries_order_t;
typedef struct dtesn_bseries_system {
char system_name[64];
uint32_t max_order;
dtesn_bseries_order_t *orders;
uint32_t total_trees;
uint32_t validated_orders;
uint64_t total_computations;
uint64_t total_computation_time_ns;
uint64_t avg_computation_time_ns;
uint64_t max_computation_time_ns;
bool meets_performance_targets;
void *memory_pool;
size_t memory_pool_size;
size_t memory_used;
} dtesn_bseries_system_t;
typedef struct dtesn_bseries_vector_op {
dtesn_bseries_tree_t **trees;
uint32_t tree_count;
double *coefficients;
double *computational_costs;
uint64_t operation_time_ns;
bool success;
} dtesn_bseries_vector_op_t;
typedef struct dtesn_bseries_stats {
uint64_t total_tree_computations;
uint64_t total_coefficient_computations;
uint64_t total_vector_operations;
uint64_t avg_tree_time_ns;
uint64_t max_tree_time_ns;
uint64_t avg_coeff_time_ns;
uint64_t max_coeff_time_ns;
uint32_t vector_throughput_per_ms;
uint32_t cache_hit_rate_pct;
bool tree_threshold_met;
bool coeff_threshold_met;
bool vector_threshold_met;
} dtesn_bseries_stats_t;
int dtesn_bseries_init(void);
dtesn_bseries_system_t *dtesn_bseries_system_create(const char *system_name,
uint32_t max_order);
void dtesn_bseries_system_destroy(dtesn_bseries_system_t *system);
int bseries_tree_init(dtesn_bseries_tree_t *tree,
uint32_t order,
dtesn_bseries_tree_type_t tree_type);
int bseries_compute_coefficient(dtesn_bseries_tree_t *tree, double *coefficient);
int bseries_tree_classify(dtesn_bseries_tree_t *tree,
dtesn_bseries_tree_type_t *tree_type);
bool bseries_validate_stability(dtesn_bseries_tree_t *tree, double tolerance);
int bseries_vector_op(dtesn_bseries_vector_op_t *vector_op);
int dtesn_bseries_generate_order(dtesn_bseries_system_t *system, uint32_t order);
bool dtesn_bseries_validate_a000081(dtesn_bseries_system_t *system);
int dtesn_bseries_get_stats(dtesn_bseries_system_t *system,
dtesn_bseries_stats_t *stats);
dtesn_bseries_tree_t *dtesn_bseries_get_tree(dtesn_bseries_system_t *system,
uint32_t order,
uint32_t index);
bool dtesn_bseries_tree_isomorphic(dtesn_bseries_tree_t *tree1,
dtesn_bseries_tree_t *tree2);
void dtesn_bseries_shutdown(void);
#define DTESN_BSERIES_ENOMEM         -20
#define DTESN_BSERIES_EINVAL         -21
#define DTESN_BSERIES_ENOTFOUND      -22
#define DTESN_BSERIES_ELATENCY       -23
#define DTESN_BSERIES_EVALIDATION    -24
#define DTESN_BSERIES_ESTABILITY     -25
#define DTESN_BSERIES_ECOMPUTATION   -26
#define DTESN_BSERIES_EVECTOR        -27
#ifdef __cplusplus
}
#endif
#endif