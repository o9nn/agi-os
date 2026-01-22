#include "cognitive_kernels.h"
#include "tensor_signatures.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#endif
static size_t total_allocated = 0;
static size_t total_freed = 0;
static size_t peak_memory = 0;
static cognitive_performance_stats_t perf_stats = {0};
static bool initialized = false;
static cognitive_perf_config_t global_config = {0};
static uint64_t get_time_ns(void) {
#if defined(_POSIX_TIMERS) && _POSIX_TIMERS > 0
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
#endif
return (uint64_t)time(NULL) * 1000000000ULL;
}
int cognitive_kernels_init(cognitive_perf_config_t* config) {
if (initialized) {
return COGNITIVE_SUCCESS;
}
if (config) {
global_config = *config;
} else {
global_config.enable_gpu_acceleration = false;
global_config.enable_parallel_processing = true;
global_config.num_threads = 4;
global_config.memory_limit_bytes = 1024 * 1024 * 1024;
global_config.attention_threshold = 0.5f;
}
initialized = true;
return COGNITIVE_SUCCESS;
}
void cognitive_kernels_shutdown(void) {
initialized = false;
total_allocated = 0;
total_freed = 0;
peak_memory = 0;
}
void* cognitive_alloc(size_t size) {
void* ptr = malloc(size);
if (ptr) {
total_allocated += size;
if (total_allocated - total_freed > peak_memory) {
peak_memory = total_allocated - total_freed;
}
}
return ptr;
}
void cognitive_free(void* ptr) {
if (ptr) {
free(ptr);
}
}
void cognitive_memory_stats(size_t* allocated, size_t* freed, size_t* peak) {
if (allocated) *allocated = total_allocated;
if (freed) *freed = total_freed;
if (peak) *peak = peak_memory;
}
cognitive_tensor_t* create_cognitive_tensor(
cognitive_tensor_shape_t shape,
cognitive_tensor_type_t type,
void* initial_data,
size_t data_size
) {
if (!VALIDATE_TENSOR_SHAPE(shape)) {
return NULL;
}
cognitive_tensor_t* tensor = cognitive_alloc(sizeof(cognitive_tensor_t));
if (!tensor) {
return NULL;
}
tensor->shape = shape;
tensor->type = type;
tensor->data_size = data_size;
tensor->data = cognitive_alloc(data_size);
if (!tensor->data) {
cognitive_free(tensor);
return NULL;
}
if (initial_data) {
memcpy(tensor->data, initial_data, data_size);
} else {
memset(tensor->data, 0, data_size);
}
tensor->creation_time = get_time_ns();
tensor->last_access = tensor->creation_time;
tensor->access_count = 0;
tensor->confidence = 1.0f;
tensor->atomspace_ref = NULL;
tensor->attention_ref = NULL;
tensor->hurd_ref = NULL;
tensor->clone = cognitive_tensor_clone;
tensor->transform = NULL;
tensor->similarity = NULL;
tensor->destroy = cognitive_tensor_destroy;
return tensor;
}
cognitive_tensor_t* cognitive_tensor_clone(cognitive_tensor_t* tensor) {
if (!tensor) {
return NULL;
}
return create_cognitive_tensor(
tensor->shape,
tensor->type,
tensor->data,
tensor->data_size
);
}
void cognitive_tensor_destroy(cognitive_tensor_t* tensor) {
if (!tensor) {
return;
}
if (tensor->data) {
cognitive_free(tensor->data);
}
cognitive_free(tensor);
}
cognitive_result_t cognitive_convolution(
cognitive_tensor_t* input,
cognitive_tensor_t* kernel,
cognitive_kernel_config_t config
) {
cognitive_result_t result = {0};
uint64_t start_time = get_time_ns();
if (!input || !kernel) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
input->last_access = get_time_ns();
input->access_count++;
result.output_tensor = create_cognitive_tensor(
input->shape,
TENSOR_TYPE_HYBRID,
NULL,
input->data_size
);
if (!result.output_tensor) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
float* input_data = (float*)input->data;
float* kernel_data = (float*)kernel->data;
float* output_data = (float*)result.output_tensor->data;
size_t num_elements = input->data_size / sizeof(float);
size_t kernel_elements = kernel->data_size / sizeof(float);
for (size_t i = 0; i < num_elements; i++) {
float weighted_sum = 0.0f;
for (size_t k = 0; k < kernel_elements; k++) {
if (i + k < num_elements) {
weighted_sum += input_data[i + k] * kernel_data[k];
}
}
output_data[i] = weighted_sum * (input->shape.salience / 100.0f);
}
result.confidence_score = input->confidence * 0.95f;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = num_elements * kernel_elements;
result.convergence_achieved = true;
result.debug_info = NULL;
perf_stats.total_operations++;
perf_stats.total_time_ns += result.processing_time_ns;
if (peak_memory > perf_stats.peak_memory_bytes) {
perf_stats.peak_memory_bytes = peak_memory;
}
return result;
}
cognitive_result_t attention_pooling(
cognitive_tensor_t* input,
cognitive_salience_t target_salience,
cognitive_kernel_config_t config
) {
cognitive_result_t result = {0};
uint64_t start_time = get_time_ns();
if (!input) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
result.output_tensor = cognitive_tensor_clone(input);
if (!result.output_tensor) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
float* input_data = (float*)input->data;
float* output_data = (float*)result.output_tensor->data;
size_t num_elements = input->data_size / sizeof(float);
float attention_weight = (float)target_salience / (float)input->shape.salience;
for (size_t i = 0; i < num_elements; i++) {
output_data[i] = input_data[i] * attention_weight;
}
result.output_tensor->shape.salience = target_salience;
result.confidence_score = input->confidence * 0.9f;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = num_elements;
result.convergence_achieved = true;
result.debug_info = NULL;
perf_stats.total_operations++;
perf_stats.total_time_ns += result.processing_time_ns;
return result;
}
cognitive_result_t symbolic_activation(
cognitive_tensor_t* input,
cognitive_kernel_config_t config
) {
cognitive_result_t result = {0};
uint64_t start_time = get_time_ns();
if (!input) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
result.output_tensor = cognitive_tensor_clone(input);
if (!result.output_tensor) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
float* input_data = (float*)input->data;
float* output_data = (float*)result.output_tensor->data;
size_t num_elements = input->data_size / sizeof(float);
float threshold = config.attention_threshold;
for (size_t i = 0; i < num_elements; i++) {
output_data[i] = (input_data[i] > threshold) ? 1.0f : 0.0f;
}
result.confidence_score = 1.0f;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = num_elements;
result.convergence_achieved = true;
result.debug_info = NULL;
perf_stats.total_operations++;
perf_stats.total_time_ns += result.processing_time_ns;
return result;
}
cognitive_result_t recursive_transform(
cognitive_tensor_t* input,
uint32_t depth,
cognitive_kernel_config_t config
) {
cognitive_result_t result = {0};
uint64_t start_time = get_time_ns();
if (!input || depth > config.recursive_depth_limit) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
if (depth == 0) {
result.output_tensor = cognitive_tensor_clone(input);
result.confidence_score = input->confidence;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = 1;
result.convergence_achieved = true;
return result;
}
cognitive_tensor_t* transformed = cognitive_tensor_clone(input);
if (!transformed) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
float* data = (float*)transformed->data;
size_t num_elements = transformed->data_size / sizeof(float);
for (size_t i = 0; i < num_elements; i++) {
data[i] = data[i] * (1.0f - (float)depth / config.recursive_depth_limit);
}
cognitive_result_t recursive_result = recursive_transform(
transformed,
depth - 1,
config
);
cognitive_tensor_destroy(transformed);
if (recursive_result.output_tensor) {
result.output_tensor = recursive_result.output_tensor;
result.confidence_score = input->confidence * 0.95f;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = num_elements * depth;
result.convergence_achieved = true;
}
perf_stats.total_operations++;
perf_stats.total_time_ns += result.processing_time_ns;
return result;
}
cognitive_result_t meta_cognitive_reflection(
cognitive_tensor_t* input,
cognitive_kernel_config_t config
) {
cognitive_result_t result = {0};
uint64_t start_time = get_time_ns();
if (!input) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
cognitive_tensor_shape_t meta_shape = input->shape;
meta_shape.depth = DEPTH_META;
meta_shape.modality = MODALITY_META;
result.output_tensor = create_cognitive_tensor(
meta_shape,
TENSOR_TYPE_META,
input->data,
input->data_size
);
if (!result.output_tensor) {
result.confidence_score = 0.0f;
result.convergence_achieved = false;
return result;
}
float* data = (float*)result.output_tensor->data;
size_t num_elements = result.output_tensor->data_size / sizeof(float);
float sum = 0.0f;
float sum_sq = 0.0f;
for (size_t i = 0; i < num_elements; i++) {
sum += data[i];
sum_sq += data[i] * data[i];
}
float mean = sum / num_elements;
float variance = (sum_sq / num_elements) - (mean * mean);
for (size_t i = 0; i < num_elements; i++) {
data[i] = (data[i] - mean) / (sqrt(variance) + 1e-6f);
}
result.confidence_score = 0.8f;
result.processing_time_ns = get_time_ns() - start_time;
result.operations_performed = num_elements * 3;
result.convergence_achieved = true;
result.debug_info = NULL;
perf_stats.total_operations++;
perf_stats.total_time_ns += result.processing_time_ns;
return result;
}
void destroy_cognitive_result(cognitive_result_t* result) {
if (!result) {
return;
}
if (result->output_tensor) {
cognitive_tensor_destroy(result->output_tensor);
result->output_tensor = NULL;
}
if (result->debug_info) {
cognitive_free(result->debug_info);
result->debug_info = NULL;
}
}
void cognitive_get_performance_stats(cognitive_performance_stats_t* stats) {
if (!stats) {
return;
}
*stats = perf_stats;
if (perf_stats.total_operations > 0) {
stats->average_latency_ms =
(float)perf_stats.total_time_ns / perf_stats.total_operations / 1000000.0f;
if (perf_stats.total_time_ns > 0) {
stats->operations_per_second =
(float)perf_stats.total_operations /
((float)perf_stats.total_time_ns / 1000000000.0f);
}
}
}
void cognitive_reset_performance_stats(void) {
memset(&perf_stats, 0, sizeof(perf_stats));
}
const char* cognitive_error_string(cognitive_error_t error) {
switch (error) {
case COGNITIVE_SUCCESS:
return "Success";
case COGNITIVE_ERROR_INVALID_PARAM:
return "Invalid parameter";
case COGNITIVE_ERROR_OUT_OF_MEMORY:
return "Out of memory";
case COGNITIVE_ERROR_SHAPE_MISMATCH:
return "Tensor shape mismatch";
case COGNITIVE_ERROR_NOT_INITIALIZED:
return "Cognitive kernels not initialized";
case COGNITIVE_ERROR_ATOMSPACE_INTEGRATION:
return "AtomSpace integration error";
case COGNITIVE_ERROR_COMPUTATION_FAILED:
return "Computation failed";
default:
return "Unknown error";
}
}