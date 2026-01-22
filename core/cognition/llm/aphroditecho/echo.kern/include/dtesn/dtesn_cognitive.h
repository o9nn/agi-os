#ifndef DTESN_COGNITIVE_H
#define DTESN_COGNITIVE_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>
#include "esn.h"
#include "memory.h"
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_COGNITIVE_MAX_LEARNING_ITERATIONS     1000
#define DTESN_COGNITIVE_MAX_MEMORY_NODES           10000
#define DTESN_COGNITIVE_MAX_ATTENTION_CHANNELS        64
#define DTESN_COGNITIVE_MAX_MODALITIES                 8
#define DTESN_COGNITIVE_MAX_DISTRIBUTED_NODES         32
#define DTESN_COGNITIVE_LEARNING_CONVERGENCE_MAX    1000000
#define DTESN_COGNITIVE_MEMORY_CONSOLIDATION_US      100000
#define DTESN_COGNITIVE_ATTENTION_SWITCH_US           10000
#define DTESN_COGNITIVE_STATE_PERSISTENCE_US          50000
#define DTESN_COGNITIVE_A000081_MAX_DEPTH 12
#define DTESN_COGNITIVE_A000081_SEQUENCE \
{ 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766 }
typedef enum {
DTESN_COGNITIVE_LEARN_HEBBIAN = 0,
DTESN_COGNITIVE_LEARN_STDP = 1,
DTESN_COGNITIVE_LEARN_BCM = 2,
DTESN_COGNITIVE_LEARN_RLRL = 3,
DTESN_COGNITIVE_LEARN_ADAPTIVE = 4
} dtesn_cognitive_learn_type_t;
typedef enum {
DTESN_COGNITIVE_CONSOLIDATE_IMMEDIATE = 0,
DTESN_COGNITIVE_CONSOLIDATE_DELAYED = 1,
DTESN_COGNITIVE_CONSOLIDATE_REPLAY = 2,
DTESN_COGNITIVE_CONSOLIDATE_ADAPTIVE = 3
} dtesn_cognitive_consolidate_type_t;
typedef enum {
DTESN_COGNITIVE_ATTENTION_BOTTOM_UP = 0,
DTESN_COGNITIVE_ATTENTION_TOP_DOWN = 1,
DTESN_COGNITIVE_ATTENTION_COMPETITIVE = 2,
DTESN_COGNITIVE_ATTENTION_COOPERATIVE = 3
} dtesn_cognitive_attention_type_t;
typedef enum {
DTESN_COGNITIVE_FUSION_EARLY = 0,
DTESN_COGNITIVE_FUSION_LATE = 1,
DTESN_COGNITIVE_FUSION_HIERARCHICAL = 2,
DTESN_COGNITIVE_FUSION_ADAPTIVE = 3
} dtesn_cognitive_fusion_type_t;
typedef struct dtesn_cognitive_learn_params {
dtesn_cognitive_learn_type_t learn_type;
float learning_rate;
float adaptation_rate;
uint32_t max_iterations;
float convergence_threshold;
bool enable_plasticity;
bool enable_homeostasis;
uint32_t batch_size;
} dtesn_cognitive_learn_params_t;
typedef struct dtesn_cognitive_memory_node {
uint32_t node_id;
char label[64];
float *data;
uint32_t data_size;
float activation;
float decay_rate;
uint64_t timestamp_ns;
uint32_t access_count;
bool persistent;
struct dtesn_cognitive_memory_node *next;
} dtesn_cognitive_memory_node_t;
typedef struct dtesn_cognitive_attention_channel {
uint32_t channel_id;
dtesn_cognitive_attention_type_t type;
float weight;
float *focus_vector;
uint32_t focus_size;
uint64_t switch_time_ns;
bool active;
} dtesn_cognitive_attention_channel_t;
typedef struct dtesn_cognitive_modality_data {
uint32_t modality_id;
char name[32];
float *data;
uint32_t data_size;
float confidence;
uint64_t timestamp_ns;
bool valid;
} dtesn_cognitive_modality_data_t;
typedef struct dtesn_cognitive_distributed_node {
uint32_t node_id;
uint32_t ip_address;
uint16_t port;
float computational_load;
float network_latency_ms;
bool online;
uint64_t last_sync_ns;
} dtesn_cognitive_distributed_node_t;
typedef struct dtesn_cognitive_system {
uint32_t system_id;
char name[64];
dtesn_esn_reservoir_t *reservoir;
dtesn_cognitive_memory_node_t *memory_head;
uint32_t memory_node_count;
pthread_mutex_t memory_lock;
dtesn_cognitive_attention_channel_t *attention_channels;
uint32_t num_attention_channels;
uint32_t active_channel_id;
pthread_mutex_t attention_lock;
dtesn_cognitive_modality_data_t *modalities;
uint32_t num_modalities;
dtesn_cognitive_fusion_type_t fusion_type;
float *fused_representation;
uint32_t fused_size;
dtesn_cognitive_distributed_node_t *nodes;
uint32_t num_nodes;
pthread_mutex_t distributed_lock;
uint64_t total_learning_iterations;
uint64_t total_learning_time_ns;
uint64_t total_consolidations;
uint64_t total_consolidation_time_ns;
uint64_t total_attention_switches;
uint64_t total_attention_switch_time_ns;
uint64_t total_state_saves;
uint64_t total_state_save_time_ns;
bool initialized;
pthread_mutex_t system_lock;
} dtesn_cognitive_system_t;
int dtesn_cognitive_init(void);
int dtesn_cognitive_cleanup(void);
dtesn_cognitive_system_t *dtesn_cognitive_system_create(const char *name,
dtesn_esn_reservoir_t *reservoir);
int dtesn_cognitive_system_destroy(dtesn_cognitive_system_t *system);
int dtesn_adaptive_learn(dtesn_cognitive_system_t *system,
const float **input_data,
const float **target_data,
uint32_t num_samples,
const dtesn_cognitive_learn_params_t *params);
int dtesn_adaptive_learn_online(dtesn_cognitive_system_t *system,
const float *input,
const float *target,
const dtesn_cognitive_learn_params_t *params);
int dtesn_memory_consolidate(dtesn_cognitive_system_t *system,
dtesn_cognitive_consolidate_type_t consolidate_type);
int dtesn_memory_consolidate_selective(dtesn_cognitive_system_t *system,
float threshold,
dtesn_cognitive_consolidate_type_t consolidate_type);
int dtesn_attention_focus(dtesn_cognitive_system_t *system,
uint32_t channel_id,
const float *focus_vector,
uint32_t focus_size);
int dtesn_attention_distribute(dtesn_cognitive_system_t *system,
const float *weights,
uint32_t num_weights);
int dtesn_multimodal_fuse(dtesn_cognitive_system_t *system,
const dtesn_cognitive_modality_data_t *input_data,
uint32_t num_modalities,
dtesn_cognitive_fusion_type_t fusion_type,
float *output,
uint32_t output_size);
typedef struct dtesn_sensor_calibration dtesn_sensor_calibration_t;
typedef enum {
DTESN_NOISE_GAUSSIAN = 0,
DTESN_NOISE_UNIFORM = 1,
DTESN_NOISE_IMPULSE = 2,
DTESN_NOISE_ADAPTIVE = 3
} dtesn_noise_model_type_t;
dtesn_sensor_calibration_t *dtesn_sensor_calibration_create(uint32_t sensor_id,
dtesn_noise_model_type_t noise_model);
void dtesn_sensor_calibration_destroy(dtesn_sensor_calibration_t *calibration);
int dtesn_sensor_calibrate(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *modality);
int dtesn_sensor_filter_noise(dtesn_sensor_calibration_t *calibration,
const dtesn_cognitive_modality_data_t *input_modality,
dtesn_cognitive_modality_data_t *filtered_modality);
int dtesn_sensor_calibration_get_stats(const dtesn_sensor_calibration_t *calibration,
void *stats_buffer, size_t buffer_size);
int dtesn_distributed_sync(dtesn_cognitive_system_t *system,
uint32_t sync_timeout_ms);
int dtesn_distributed_add_node(dtesn_cognitive_system_t *system,
uint32_t node_id,
uint32_t ip_address,
uint16_t port);
bool dtesn_cognitive_validate_a000081(const dtesn_cognitive_system_t *system);
int dtesn_cognitive_get_performance_stats(const dtesn_cognitive_system_t *system,
void *stats_buffer,
size_t buffer_size);
int dtesn_cognitive_reset_stats(dtesn_cognitive_system_t *system);
#ifdef __cplusplus
}
#endif
#endif