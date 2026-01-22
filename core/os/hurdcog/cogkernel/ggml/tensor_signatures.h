#ifndef COGNITIVE_TENSOR_SIGNATURES_H
#define COGNITIVE_TENSOR_SIGNATURES_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
typedef enum {
MODALITY_VISUAL = 0,
MODALITY_AUDITORY = 1,
MODALITY_CONCEPTUAL = 2,
MODALITY_LINGUISTIC = 3,
MODALITY_MOTOR = 4,
MODALITY_EMOTIONAL = 5,
MODALITY_SPATIAL = 6,
MODALITY_TEMPORAL = 7,
MODALITY_META = 8,
MODALITY_COUNT = 9
} cognitive_modality_t;
typedef enum {
DEPTH_PRIMITIVE = 0,
DEPTH_PATTERN = 1,
DEPTH_ABSTRACTION = 2,
DEPTH_META = 3,
DEPTH_RECURSIVE = 4,
DEPTH_TRANSCENDENT = 5
} cognitive_depth_t;
typedef enum {
SALIENCE_MINIMAL = 0,
SALIENCE_LOW = 25,
SALIENCE_MEDIUM = 50,
SALIENCE_HIGH = 75,
SALIENCE_CRITICAL = 100
} cognitive_salience_t;
typedef enum {
AUTONOMY_PASSIVE = 0,
AUTONOMY_REACTIVE = 25,
AUTONOMY_ADAPTIVE = 50,
AUTONOMY_CREATIVE = 75,
AUTONOMY_TRANSCENDENT = 100
} cognitive_autonomy_t;
typedef struct {
cognitive_modality_t modality;
cognitive_depth_t depth;
uint32_t context;
cognitive_salience_t salience;
cognitive_autonomy_t autonomy_index;
} cognitive_tensor_shape_t;
typedef enum {
TENSOR_TYPE_SYMBOLIC,
TENSOR_TYPE_NEURAL,
TENSOR_TYPE_HYBRID,
TENSOR_TYPE_HYPERGRAPH,
TENSOR_TYPE_ATTENTION,
TENSOR_TYPE_META
} cognitive_tensor_type_t;
struct atomspace_handle;
struct ecan_context;
struct hurd_primitive;
typedef struct cognitive_tensor {
cognitive_tensor_shape_t shape;
cognitive_tensor_type_t type;
void* data;
size_t data_size;
uint64_t creation_time;
uint64_t last_access;
uint32_t access_count;
float confidence;
struct atomspace_handle* atomspace_ref;
struct ecan_context* attention_ref;
struct hurd_primitive* hurd_ref;
struct cognitive_tensor* (*clone)(struct cognitive_tensor* self);
int (*transform)(struct cognitive_tensor* self, void* params);
float (*similarity)(struct cognitive_tensor* self, struct cognitive_tensor* other);
void (*destroy)(struct cognitive_tensor* self);
} cognitive_tensor_t;
typedef enum {
OP_COGNITIVE_CONV,
OP_ATTENTION_POOL,
OP_SYMBOLIC_ACTIVATION,
OP_RECURSIVE_TRANSFORM,
OP_META_REFLECTION,
OP_HYPERGRAPH_MERGE,
OP_PATTERN_MATCH,
OP_INFERENCE,
OP_COUNT
} cognitive_operation_t;
typedef struct {
cognitive_operation_t operation;
cognitive_tensor_shape_t input_shape;
cognitive_tensor_shape_t output_shape;
uint32_t max_iterations;
float convergence_threshold;
bool enable_gpu_acceleration;
bool enable_parallel_processing;
float attention_threshold;
float salience_decay_rate;
uint32_t recursive_depth_limit;
} cognitive_kernel_config_t;
typedef struct {
cognitive_tensor_t* output_tensor;
float confidence_score;
uint64_t processing_time_ns;
uint32_t operations_performed;
bool convergence_achieved;
char* debug_info;
} cognitive_result_t;
cognitive_tensor_t* create_cognitive_tensor(
cognitive_tensor_shape_t shape,
cognitive_tensor_type_t type,
void* initial_data,
size_t data_size
);
cognitive_result_t cognitive_convolution(
cognitive_tensor_t* input,
cognitive_tensor_t* kernel,
cognitive_kernel_config_t config
);
cognitive_result_t attention_pooling(
cognitive_tensor_t* input,
cognitive_salience_t target_salience,
cognitive_kernel_config_t config
);
cognitive_result_t symbolic_activation(
cognitive_tensor_t* input,
cognitive_kernel_config_t config
);
cognitive_result_t recursive_transform(
cognitive_tensor_t* input,
uint32_t depth,
cognitive_kernel_config_t config
);
cognitive_result_t meta_cognitive_reflection(
cognitive_tensor_t* input,
cognitive_kernel_config_t config
);
int atomspace_integrate_result(cognitive_result_t result);
int ecan_integrate_result(cognitive_result_t result);
int hurd_integrate_result(cognitive_result_t result);
void destroy_cognitive_result(cognitive_result_t* result);
#define VALIDATE_TENSOR_SHAPE(shape) \
((shape).modality < MODALITY_COUNT && \
(shape).depth <= DEPTH_TRANSCENDENT && \
(shape).salience <= SALIENCE_CRITICAL && \
(shape).autonomy_index <= AUTONOMY_TRANSCENDENT)
#define TENSOR_SHAPE_SIGNATURE(shape) \
(((uint64_t)(shape).modality << 32) | \
((uint64_t)(shape).depth << 24) | \
((uint64_t)(shape).salience << 16) | \
((uint64_t)(shape).autonomy_index << 8) | \
((uint64_t)(shape).context & 0xFF))
#define PRIME_MODALITY_BASE 2
#define PRIME_DEPTH_BASE 3
#define PRIME_CONTEXT_BASE 5
#define PRIME_SALIENCE_BASE 7
#define PRIME_AUTONOMY_BASE 11
#define COGNITIVE_PRIME_SIGNATURE(shape) \
(pow(PRIME_MODALITY_BASE, (shape).modality) * \
pow(PRIME_DEPTH_BASE, (shape).depth) * \
pow(PRIME_CONTEXT_BASE, (shape).context % 100) * \
pow(PRIME_SALIENCE_BASE, (shape).salience / 10) * \
pow(PRIME_AUTONOMY_BASE, (shape).autonomy_index / 10))
#endif