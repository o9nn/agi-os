#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L
#include "include/dtesn/esn.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#ifdef __x86_64__
#include <immintrin.h>
#endif
static dtesn_esn_accel_context_t g_accel_contexts[16];
static uint32_t g_num_contexts = 0;
static bool g_hardware_detected = false;
static int dtesn_esn_detect_simd(void);
#ifdef DTESN_ESN_GPU_SUPPORT
static int dtesn_esn_detect_gpu(void);
#endif
#ifdef DTESN_ESN_FPGA_SUPPORT
static int dtesn_esn_detect_fpga(void);
#endif
#ifdef DTESN_ESN_NEUROMORPHIC_SUPPORT
static int dtesn_esn_detect_neuromorphic(void);
#endif
static int dtesn_esn_simd_sparse_multiply(const dtesn_esn_sparse_matrix_t *matrix,
const float *input, float *output);
static int dtesn_esn_simd_vector_ops(float *dst, const float *src1, const float *src2,
uint32_t size, int operation);
static int dtesn_esn_detect_simd(void) {
dtesn_esn_accel_context_t *ctx = &g_accel_contexts[g_num_contexts];
ctx->type = DTESN_ESN_ACCEL_SIMD;
ctx->device_id = 0;
strcpy(ctx->device_name, "CPU SIMD");
ctx->device_memory_size = 0;
ctx->performance_factor = 1.0f;
#ifdef __x86_64__
__builtin_cpu_init();
if (__builtin_cpu_supports("avx2")) {
ctx->is_available = true;
ctx->performance_factor = 4.0f;
strcpy(ctx->device_name, "CPU AVX2 SIMD");
} else if (__builtin_cpu_supports("sse4.2")) {
ctx->is_available = true;
ctx->performance_factor = 2.0f;
strcpy(ctx->device_name, "CPU SSE4.2 SIMD");
} else {
ctx->is_available = false;
return 0;
}
#else
ctx->is_available = true;
ctx->performance_factor = 2.0f;
strcpy(ctx->device_name, "CPU Generic SIMD");
#endif
g_num_contexts++;
return 1;
}
#ifdef DTESN_ESN_GPU_SUPPORT
static int dtesn_esn_detect_gpu(void) {
dtesn_esn_accel_context_t *ctx = &g_accel_contexts[g_num_contexts];
ctx->type = DTESN_ESN_ACCEL_GPU;
ctx->device_id = 0;
ctx->device_memory_size = 0;
ctx->is_available = false;
ctx->performance_factor = 10.0f;
return 0;
}
#endif
#ifdef DTESN_ESN_FPGA_SUPPORT
static int dtesn_esn_detect_fpga(void) {
dtesn_esn_accel_context_t *ctx = &g_accel_contexts[g_num_contexts];
ctx->type = DTESN_ESN_ACCEL_FPGA;
ctx->device_id = 0;
ctx->device_memory_size = 0;
ctx->is_available = false;
ctx->performance_factor = 20.0f;
return 0;
}
#endif
#ifdef DTESN_ESN_NEUROMORPHIC_SUPPORT
static int dtesn_esn_detect_neuromorphic(void) {
dtesn_esn_accel_context_t *ctx = &g_accel_contexts[g_num_contexts];
ctx->type = DTESN_ESN_ACCEL_NEUROMORPHIC;
ctx->device_id = 0;
ctx->device_memory_size = 0;
ctx->is_available = false;
ctx->performance_factor = 100.0f;
return 0;
}
#endif
static int dtesn_esn_simd_sparse_multiply(const dtesn_esn_sparse_matrix_t *matrix,
const float *input, float *output) {
if (!matrix || !input || !output) {
return DTESN_ESN_EINVAL;
}
#ifdef __x86_64__
if (__builtin_cpu_supports("avx2")) {
memset(output, 0, matrix->rows * sizeof(float));
for (uint32_t i = 0; i < matrix->rows; i++) {
uint32_t start = matrix->row_ptr[i];
uint32_t end = matrix->row_ptr[i + 1];
__m256 sum_vec = _mm256_setzero_ps();
uint32_t j = start;
for (; j + 7 < end; j += 8) {
__m256 val_vec = _mm256_loadu_ps(&matrix->values[j]);
__m256 input_vec = _mm256_set_ps(
input[matrix->col_idx[j + 7]], input[matrix->col_idx[j + 6]],
input[matrix->col_idx[j + 5]], input[matrix->col_idx[j + 4]],
input[matrix->col_idx[j + 3]], input[matrix->col_idx[j + 2]],
input[matrix->col_idx[j + 1]], input[matrix->col_idx[j + 0]]
);
sum_vec = _mm256_fmadd_ps(val_vec, input_vec, sum_vec);
}
float sum_array[8];
_mm256_storeu_ps(sum_array, sum_vec);
float sum = sum_array[0] + sum_array[1] + sum_array[2] + sum_array[3] +
sum_array[4] + sum_array[5] + sum_array[6] + sum_array[7];
for (; j < end; j++) {
sum += matrix->values[j] * input[matrix->col_idx[j]];
}
output[i] = sum;
}
return 0;
}
#endif
return esn_sparse_multiply(matrix, input, output);
}
static int dtesn_esn_simd_vector_ops(float *dst, const float *src1, const float *src2,
uint32_t size, int operation) {
if (!dst || !src1 || !src2) {
return DTESN_ESN_EINVAL;
}
#ifdef __x86_64__
if (__builtin_cpu_supports("avx2")) {
uint32_t i = 0;
for (; i + 7 < size; i += 8) {
__m256 vec1 = _mm256_loadu_ps(&src1[i]);
__m256 vec2 = _mm256_loadu_ps(&src2[i]);
__m256 result;
switch (operation) {
case 0:
result = _mm256_add_ps(vec1, vec2);
break;
case 1:
result = _mm256_sub_ps(vec1, vec2);
break;
case 2:
result = _mm256_mul_ps(vec1, vec2);
break;
case 3:
result = _mm256_fmadd_ps(vec1, vec2, _mm256_loadu_ps(&dst[i]));
break;
default:
return DTESN_ESN_EINVAL;
}
_mm256_storeu_ps(&dst[i], result);
}
for (; i < size; i++) {
switch (operation) {
case 0: dst[i] = src1[i] + src2[i]; break;
case 1: dst[i] = src1[i] - src2[i]; break;
case 2: dst[i] = src1[i] * src2[i]; break;
case 3: dst[i] = src1[i] * src2[i] + dst[i]; break;
}
}
return 0;
}
#endif
for (uint32_t i = 0; i < size; i++) {
switch (operation) {
case 0: dst[i] = src1[i] + src2[i]; break;
case 1: dst[i] = src1[i] - src2[i]; break;
case 2: dst[i] = src1[i] * src2[i]; break;
case 3: dst[i] = src1[i] * src2[i] + dst[i]; break;
default:
return DTESN_ESN_EINVAL;
}
}
return 0;
}
int dtesn_esn_detect_hardware(dtesn_esn_accel_context_t *contexts, uint32_t max_contexts) {
if (!contexts || max_contexts == 0) {
return DTESN_ESN_EINVAL;
}
if (g_hardware_detected) {
uint32_t count = (g_num_contexts < max_contexts) ? g_num_contexts : max_contexts;
memcpy(contexts, g_accel_contexts, count * sizeof(dtesn_esn_accel_context_t));
return count;
}
g_num_contexts = 0;
dtesn_esn_detect_simd();
#ifdef DTESN_ESN_GPU_SUPPORT
dtesn_esn_detect_gpu();
#endif
#ifdef DTESN_ESN_FPGA_SUPPORT
dtesn_esn_detect_fpga();
#endif
#ifdef DTESN_ESN_NEUROMORPHIC_SUPPORT
dtesn_esn_detect_neuromorphic();
#endif
g_hardware_detected = true;
uint32_t count = (g_num_contexts < max_contexts) ? g_num_contexts : max_contexts;
memcpy(contexts, g_accel_contexts, count * sizeof(dtesn_esn_accel_context_t));
return count;
}
int esn_hardware_accel(dtesn_esn_reservoir_t *reservoir, dtesn_esn_accel_type_t accel_type) {
if (!reservoir) {
return DTESN_ESN_EINVAL;
}
dtesn_esn_accel_context_t *ctx = NULL;
for (uint32_t i = 0; i < g_num_contexts; i++) {
if (g_accel_contexts[i].type == accel_type && g_accel_contexts[i].is_available) {
ctx = &g_accel_contexts[i];
break;
}
}
if (!ctx) {
return DTESN_ESN_EHARDWARE;
}
if (reservoir->accel_context) {
free(reservoir->accel_context);
}
reservoir->accel_context = malloc(sizeof(dtesn_esn_accel_context_t));
if (!reservoir->accel_context) {
return DTESN_ESN_ENOMEM;
}
memcpy(reservoir->accel_context, ctx, sizeof(dtesn_esn_accel_context_t));
reservoir->config.accel_type = accel_type;
reservoir->accel_available = true;
return 0;
}
int dtesn_esn_accel_sparse_multiply(dtesn_esn_reservoir_t *reservoir,
const dtesn_esn_sparse_matrix_t *matrix,
const float *input, float *output) {
if (!reservoir || !matrix || !input || !output) {
return DTESN_ESN_EINVAL;
}
if (!reservoir->accel_available || !reservoir->accel_context) {
return esn_sparse_multiply(matrix, input, output);
}
dtesn_esn_accel_context_t *ctx = (dtesn_esn_accel_context_t *)reservoir->accel_context;
switch (ctx->type) {
case DTESN_ESN_ACCEL_SIMD:
return dtesn_esn_simd_sparse_multiply(matrix, input, output);
case DTESN_ESN_ACCEL_GPU:
return DTESN_ESN_EHARDWARE;
case DTESN_ESN_ACCEL_FPGA:
return DTESN_ESN_EHARDWARE;
case DTESN_ESN_ACCEL_NEUROMORPHIC:
return DTESN_ESN_EHARDWARE;
default:
return esn_sparse_multiply(matrix, input, output);
}
}
int dtesn_esn_accel_vector_add(dtesn_esn_reservoir_t *reservoir,
float *dst, const float *src1, const float *src2, uint32_t size) {
if (!reservoir) {
return DTESN_ESN_EINVAL;
}
if (reservoir->accel_available && reservoir->accel_context) {
dtesn_esn_accel_context_t *ctx = (dtesn_esn_accel_context_t *)reservoir->accel_context;
if (ctx->type == DTESN_ESN_ACCEL_SIMD) {
return dtesn_esn_simd_vector_ops(dst, src1, src2, size, 0);
}
}
for (uint32_t i = 0; i < size; i++) {
dst[i] = src1[i] + src2[i];
}
return 0;
}
int dtesn_esn_accel_vector_multiply(dtesn_esn_reservoir_t *reservoir,
float *dst, const float *src1, const float *src2, uint32_t size) {
if (!reservoir) {
return DTESN_ESN_EINVAL;
}
if (reservoir->accel_available && reservoir->accel_context) {
dtesn_esn_accel_context_t *ctx = (dtesn_esn_accel_context_t *)reservoir->accel_context;
if (ctx->type == DTESN_ESN_ACCEL_SIMD) {
return dtesn_esn_simd_vector_ops(dst, src1, src2, size, 2);
}
}
for (uint32_t i = 0; i < size; i++) {
dst[i] = src1[i] * src2[i];
}
return 0;
}
int dtesn_esn_accel_activation(dtesn_esn_reservoir_t *reservoir,
float *dst, const float *src, uint32_t size,
dtesn_esn_activation_t activation) {
if (!reservoir || !dst || !src) {
return DTESN_ESN_EINVAL;
}
for (uint32_t i = 0; i < size; i++) {
switch (activation) {
case DTESN_ESN_ACTIVATION_TANH:
dst[i] = tanhf(src[i]);
break;
case DTESN_ESN_ACTIVATION_SIGMOID:
dst[i] = 1.0f / (1.0f + expf(-src[i]));
break;
case DTESN_ESN_ACTIVATION_RELU:
dst[i] = fmaxf(0.0f, src[i]);
break;
case DTESN_ESN_ACTIVATION_LINEAR:
default:
dst[i] = src[i];
break;
}
}
return 0;
}
int dtesn_esn_accel_get_performance_factor(dtesn_esn_reservoir_t *reservoir, float *factor) {
if (!reservoir || !factor) {
return DTESN_ESN_EINVAL;
}
if (reservoir->accel_available && reservoir->accel_context) {
dtesn_esn_accel_context_t *ctx = (dtesn_esn_accel_context_t *)reservoir->accel_context;
*factor = ctx->performance_factor;
} else {
*factor = 1.0f;
}
return 0;
}