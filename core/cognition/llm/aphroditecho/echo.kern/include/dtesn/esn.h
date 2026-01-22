#ifndef DTESN_ESN_H
#define DTESN_ESN_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_ESN_MAX_RESERVOIR_SIZE    10000
#define DTESN_ESN_MAX_INPUT_SIZE        1000
#define DTESN_ESN_MAX_OUTPUT_SIZE       1000
#define DTESN_ESN_MAX_CONNECTIONS       1000000
#define DTESN_ESN_DEFAULT_SPARSITY      0.1
#define DTESN_ESN_STATE_UPDATE_THRESHOLD_US     1000
#define DTESN_ESN_MATRIX_MULT_THRESHOLD_US      500
#define DTESN_ESN_MEMORY_BANDWIDTH_GBPS         10
#define DTESN_ESN_SPARSITY_EFFICIENCY_PCT       90
#define DTESN_ESN_A000081_MAX_DEPTH 12
#define DTESN_ESN_A000081_SEQUENCE \
{ 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766 }
typedef enum {
DTESN_ESN_STATE_UNINITIALIZED = 0,
DTESN_ESN_STATE_INITIALIZED = 1,
DTESN_ESN_STATE_RUNNING = 2,
DTESN_ESN_STATE_PAUSED = 3,
DTESN_ESN_STATE_ERROR = 4
} dtesn_esn_state_t;
typedef enum {
DTESN_ESN_ACCEL_NONE = 0,
DTESN_ESN_ACCEL_SIMD = 1,
DTESN_ESN_ACCEL_GPU = 2,
DTESN_ESN_ACCEL_FPGA = 3,
DTESN_ESN_ACCEL_NEUROMORPHIC = 4
} dtesn_esn_accel_type_t;
typedef enum {
DTESN_ESN_ACTIVATION_TANH = 0,
DTESN_ESN_ACTIVATION_SIGMOID = 1,
DTESN_ESN_ACTIVATION_RELU = 2,
DTESN_ESN_ACTIVATION_LINEAR = 3
} dtesn_esn_activation_t;
typedef struct dtesn_esn_sparse_matrix {
uint32_t rows;
uint32_t cols;
uint32_t nnz;
uint32_t *row_ptr;
uint32_t *col_idx;
float *values;
float sparsity;
bool is_symmetric;
} dtesn_esn_sparse_matrix_t;
typedef struct dtesn_esn_config {
uint32_t reservoir_size;
uint32_t input_size;
uint32_t output_size;
float spectral_radius;
float leak_rate;
float input_scaling;
float bias_scaling;
float noise_level;
float connectivity;
uint32_t input_connectivity;
bool use_bias;
dtesn_esn_activation_t activation;
dtesn_esn_accel_type_t accel_type;
bool use_sparse_matrices;
uint32_t thread_count;
bool oeis_compliance;
uint32_t tree_depth;
} dtesn_esn_config_t;
typedef struct dtesn_esn_reservoir {
uint32_t reservoir_id;
char name[64];
dtesn_esn_config_t config;
dtesn_esn_state_t state;
dtesn_esn_sparse_matrix_t *W_res;
dtesn_esn_sparse_matrix_t *W_in;
float *W_out;
float *bias;
float *x_current;
float *x_previous;
float *u_current;
float *y_current;
float current_spectral_radius;
uint64_t adaptation_step;
float adaptation_rate;
uint64_t total_updates;
uint64_t total_update_time_ns;
uint64_t avg_update_time_ns;
uint64_t max_update_time_ns;
float memory_bandwidth_gbps;
float sparsity_efficiency;
void *accel_context;
bool accel_available;
pthread_mutex_t state_lock;
pthread_cond_t update_cond;
void *memory_pool;
size_t memory_pool_size;
size_t memory_used;
bool is_validated;
uint64_t creation_time_ns;
uint64_t last_update_ns;
} dtesn_esn_reservoir_t;
typedef struct dtesn_esn_stats {
uint64_t total_state_updates;
uint64_t total_matrix_multiplies;
uint64_t total_sparse_operations;
uint64_t avg_state_update_time_ns;
uint64_t max_state_update_time_ns;
uint64_t avg_matrix_mult_time_ns;
uint64_t max_matrix_mult_time_ns;
float avg_memory_bandwidth_gbps;
float peak_memory_bandwidth_gbps;
float avg_sparsity_efficiency;
uint32_t hardware_accel_usage_pct;
uint32_t cache_hit_rate_pct;
bool state_threshold_met;
bool matrix_threshold_met;
bool bandwidth_threshold_met;
bool sparsity_threshold_met;
} dtesn_esn_stats_t;
typedef struct dtesn_esn_accel_context {
dtesn_esn_accel_type_t type;
void *device_context;
uint32_t device_id;
char device_name[64];
size_t device_memory_size;
bool is_available;
float performance_factor;
} dtesn_esn_accel_context_t;
int dtesn_esn_init(void);
dtesn_esn_reservoir_t *esn_reservoir_init(const dtesn_esn_config_t *config,
const char *name);
void dtesn_esn_reservoir_destroy(dtesn_esn_reservoir_t *reservoir);
int esn_state_update(dtesn_esn_reservoir_t *reservoir,
const float *input,
uint32_t input_size);
int dtesn_esn_compute_output(dtesn_esn_reservoir_t *reservoir,
float *output,
uint32_t output_size);
int esn_sparse_multiply(const dtesn_esn_sparse_matrix_t *matrix,
const float *input,
float *output);
dtesn_esn_sparse_matrix_t *dtesn_esn_sparse_matrix_create(uint32_t rows,
uint32_t cols,
float sparsity);
void dtesn_esn_sparse_matrix_destroy(dtesn_esn_sparse_matrix_t *matrix);
int esn_hardware_accel(dtesn_esn_reservoir_t *reservoir,
dtesn_esn_accel_type_t accel_type);
int dtesn_esn_detect_hardware(dtesn_esn_accel_context_t *contexts,
uint32_t max_contexts);
int esn_adaptive_scale(dtesn_esn_reservoir_t *reservoir,
float performance_metric,
float target_metric);
int dtesn_esn_auto_tune(dtesn_esn_reservoir_t *reservoir,
const float **training_data,
const float **target_data,
uint32_t num_samples);
bool dtesn_esn_validate_a000081(dtesn_esn_reservoir_t *reservoir);
int dtesn_esn_get_stats(dtesn_esn_reservoir_t *reservoir,
dtesn_esn_stats_t *stats);
int dtesn_esn_check_stability(dtesn_esn_reservoir_t *reservoir,
float *stability_metric);
int dtesn_esn_config_default(dtesn_esn_config_t *config);
int dtesn_esn_save_state(dtesn_esn_reservoir_t *reservoir,
void *buffer,
size_t buffer_size);
int dtesn_esn_load_state(dtesn_esn_reservoir_t *reservoir,
const void *buffer,
size_t buffer_size);
void dtesn_esn_shutdown(void);
#define DTESN_ESN_ENOMEM         -30
#define DTESN_ESN_EINVAL         -31
#define DTESN_ESN_ENOTFOUND      -32
#define DTESN_ESN_ELATENCY       -33
#define DTESN_ESN_EVALIDATION    -34
#define DTESN_ESN_ESTABILITY     -35
#define DTESN_ESN_ECOMPUTATION   -36
#define DTESN_ESN_ESPARSE        -37
#define DTESN_ESN_EHARDWARE      -38
#define DTESN_ESN_EADAPTATION    -39
#ifdef __cplusplus
}
#endif
#endif