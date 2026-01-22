#pragma once
#include "ggml.h"
#include "gguf.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#ifdef __ARM_FEATURE_SVE
#include <arm_sve.h>
#endif
#if defined(__ARM_NEON) && !defined(__CUDACC__) && !defined(__MUSACC__)
#include <arm_neon.h>
#endif
#if defined(__F16C__)
#include <immintrin.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifndef MIN
#    define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX
#    define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#define TENSOR_ALIGNMENT 32
#ifndef __cplusplus
#ifndef static_assert
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201100L)
#define static_assert(cond, msg) _Static_assert(cond, msg)
#else
#define static_assert(cond, msg) struct global_scope_noop_trick
#endif
#endif
#endif
static inline int ggml_up32(int n) {
return (n + 31) & ~31;
}
static inline int ggml_up(int n, int m) {
GGML_ASSERT((m & (m - 1)) == 0);
return (n + m - 1) & ~(m - 1);
}
GGML_ATTRIBUTE_FORMAT(2, 3)
GGML_API void ggml_log_internal        (enum ggml_log_level level, const char * format, ...);
GGML_API void ggml_log_callback_default(enum ggml_log_level level, const char * text, void * user_data);
#define GGML_LOG(...)       ggml_log_internal(GGML_LOG_LEVEL_NONE , __VA_ARGS__)
#define GGML_LOG_INFO(...)  ggml_log_internal(GGML_LOG_LEVEL_INFO , __VA_ARGS__)
#define GGML_LOG_WARN(...)  ggml_log_internal(GGML_LOG_LEVEL_WARN , __VA_ARGS__)
#define GGML_LOG_ERROR(...) ggml_log_internal(GGML_LOG_LEVEL_ERROR, __VA_ARGS__)
#define GGML_LOG_DEBUG(...) ggml_log_internal(GGML_LOG_LEVEL_DEBUG, __VA_ARGS__)
#define GGML_LOG_CONT(...)  ggml_log_internal(GGML_LOG_LEVEL_CONT , __VA_ARGS__)
#define GGML_DEBUG 0
#if (GGML_DEBUG >= 1)
#define GGML_PRINT_DEBUG(...) GGML_LOG_DEBUG(__VA_ARGS__)
#else
#define GGML_PRINT_DEBUG(...)
#endif
#if (GGML_DEBUG >= 5)
#define GGML_PRINT_DEBUG_5(...) GGML_LOG_DEBUG(__VA_ARGS__)
#else
#define GGML_PRINT_DEBUG_5(...)
#endif
#if (GGML_DEBUG >= 10)
#define GGML_PRINT_DEBUG_10(...) GGML_LOG_DEBUG(__VA_ARGS__)
#else
#define GGML_PRINT_DEBUG_10(...)
#endif
static void ggml_set_op_params(struct ggml_tensor * tensor, const void * params, size_t params_size) {
GGML_ASSERT(tensor != NULL);
assert(params_size <= GGML_MAX_OP_PARAMS);
memcpy(tensor->op_params, params, params_size);
}
static int32_t ggml_get_op_params_i32(const struct ggml_tensor * tensor, uint32_t i) {
assert(i < GGML_MAX_OP_PARAMS / sizeof(int32_t));
return ((const int32_t *)(tensor->op_params))[i];
}
static float ggml_get_op_params_f32(const struct ggml_tensor * tensor, uint32_t i) {
assert(i < GGML_MAX_OP_PARAMS / sizeof(float));
return ((const float *)(tensor->op_params))[i];
}
static void ggml_set_op_params_i32(struct ggml_tensor * tensor, uint32_t i, int32_t value) {
assert(i < GGML_MAX_OP_PARAMS / sizeof(int32_t));
((int32_t *)(tensor->op_params))[i] = value;
}
static void ggml_set_op_params_f32(struct ggml_tensor * tensor, uint32_t i, float value) {
assert(i < GGML_MAX_OP_PARAMS / sizeof(float));
((float *)(tensor->op_params))[i] = value;
}
struct ggml_map_custom1_op_params {
ggml_custom1_op_t  fun;
int                n_tasks;
void             * userdata;
};
struct ggml_map_custom2_op_params {
ggml_custom2_op_t   fun;
int                 n_tasks;
void              * userdata;
};
struct ggml_map_custom3_op_params {
ggml_custom3_op_t fun;
int               n_tasks;
void            * userdata;
};
struct ggml_custom_op_params {
ggml_custom_op_t fun;
int              n_tasks;
void           * userdata;
};
typedef uint32_t ggml_bitset_t;
static_assert(sizeof(ggml_bitset_t) == 4, "bitset_t constants must be updated");
#define BITSET_SHR 5
#define BITSET_MASK (sizeof(ggml_bitset_t)*8 - 1)
static size_t ggml_bitset_size(size_t n) {
return (n + BITSET_MASK) >> BITSET_SHR;
}
static inline bool ggml_bitset_get(const ggml_bitset_t * bitset, size_t i) {
return !!(bitset[i >> BITSET_SHR] & (1u << (i & BITSET_MASK)));
}
static inline void ggml_bitset_set(ggml_bitset_t * bitset, size_t i) {
bitset[i >> BITSET_SHR] |= (1u << (i & BITSET_MASK));
}
static inline void ggml_bitset_clear(ggml_bitset_t * bitset, size_t i) {
bitset[i >> BITSET_SHR] &= ~(1u << (i & BITSET_MASK));
}
#define GGML_HASHSET_FULL ((size_t)-1)
#define GGML_HASHSET_ALREADY_EXISTS ((size_t)-2)
struct ggml_hash_set {
size_t size;
ggml_bitset_t * used;
struct ggml_tensor ** keys;
};
struct ggml_hash_set ggml_hash_set_new(size_t size);
void                 ggml_hash_set_free(struct ggml_hash_set * hash_set);
size_t ggml_hash_size(size_t min_sz);
void ggml_hash_set_reset(struct ggml_hash_set * hash_set);
static bool ggml_hash_contains(const struct ggml_hash_set * hash_set, struct ggml_tensor * key);
static size_t ggml_hash_find(const struct ggml_hash_set * hash_set, const struct ggml_tensor * key);
static size_t ggml_hash_insert(struct ggml_hash_set * hash_set, struct ggml_tensor * key);
static size_t ggml_hash_find_or_insert(struct ggml_hash_set * hash_set, struct ggml_tensor * key);
static inline size_t ggml_hash(const struct ggml_tensor * p) {
return (size_t)(uintptr_t)p >> 4;
}
static size_t ggml_hash_find(const struct ggml_hash_set * hash_set, const struct ggml_tensor * key) {
size_t h = ggml_hash(key) % hash_set->size;
size_t i = h;
while (ggml_bitset_get(hash_set->used, i) && hash_set->keys[i] != key) {
i = (i + 1) % hash_set->size;
if (i == h) {
return GGML_HASHSET_FULL;
}
}
return i;
}
static bool ggml_hash_contains(const struct ggml_hash_set * hash_set, struct ggml_tensor * key) {
size_t i = ggml_hash_find(hash_set, key);
return i != GGML_HASHSET_FULL && ggml_bitset_get(hash_set->used, i);
}
static size_t ggml_hash_insert(struct ggml_hash_set * hash_set, struct ggml_tensor * key) {
size_t h = ggml_hash(key) % hash_set->size;
size_t i = h;
do {
if (!ggml_bitset_get(hash_set->used, i)) {
ggml_bitset_set(hash_set->used, i);
hash_set->keys[i] = key;
return i;
}
if (hash_set->keys[i] == key) {
return GGML_HASHSET_ALREADY_EXISTS;
}
i = (i + 1) % hash_set->size;
} while (i != h);
GGML_ABORT("fatal error");
}
static size_t ggml_hash_find_or_insert(struct ggml_hash_set * hash_set, struct ggml_tensor * key) {
size_t h = ggml_hash(key) % hash_set->size;
size_t i = h;
do {
if (!ggml_bitset_get(hash_set->used, i)) {
ggml_bitset_set(hash_set->used, i);
hash_set->keys[i] = key;
return i;
}
if (hash_set->keys[i] == key) {
return i;
}
i = (i + 1) % hash_set->size;
} while (i != h);
GGML_ABORT("fatal error");
}
enum ggml_cgraph_eval_order {
GGML_CGRAPH_EVAL_ORDER_LEFT_TO_RIGHT = 0,
GGML_CGRAPH_EVAL_ORDER_RIGHT_TO_LEFT,
GGML_CGRAPH_EVAL_ORDER_COUNT
};
struct ggml_cgraph {
int size;
int n_nodes;
int n_leafs;
struct ggml_tensor ** nodes;
struct ggml_tensor ** grads;
struct ggml_tensor ** grad_accs;
struct ggml_tensor ** leafs;
struct ggml_hash_set visited_hash_set;
enum ggml_cgraph_eval_order order;
};
struct ggml_cgraph ggml_graph_view(struct ggml_cgraph * cgraph, int i0, int i1);
GGML_API void * ggml_aligned_malloc(size_t size);
GGML_API void ggml_aligned_free(void * ptr, size_t size);
#if defined(__ARM_NEON) && !(defined(__CUDACC__) && __CUDACC_VER_MAJOR__ <= 11) && !defined(__MUSACC__)
#define GGML_COMPUTE_FP16_TO_FP32(x) ggml_compute_fp16_to_fp32(x)
#define GGML_COMPUTE_FP32_TO_FP16(x) ggml_compute_fp32_to_fp16(x)
#define GGML_FP16_TO_FP32(x) ggml_compute_fp16_to_fp32(x)
static inline float ggml_compute_fp16_to_fp32(ggml_fp16_t h) {
__fp16 tmp;
memcpy(&tmp, &h, sizeof(ggml_fp16_t));
return (float)tmp;
}
static inline ggml_fp16_t ggml_compute_fp32_to_fp16(float f) {
ggml_fp16_t res;
__fp16 tmp = f;
memcpy(&res, &tmp, sizeof(ggml_fp16_t));
return res;
}
#elif defined(__F16C__)
#ifdef _MSC_VER
#define GGML_COMPUTE_FP16_TO_FP32(x) _mm_cvtss_f32(_mm_cvtph_ps(_mm_cvtsi32_si128(x)))
#define GGML_COMPUTE_FP32_TO_FP16(x) _mm_extract_epi16(_mm_cvtps_ph(_mm_set_ss(x), 0), 0)
#else
#define GGML_COMPUTE_FP16_TO_FP32(x) _cvtsh_ss(x)
#define GGML_COMPUTE_FP32_TO_FP16(x) _cvtss_sh(x, 0)
#endif
#elif defined(__POWER9_VECTOR__)
#define GGML_COMPUTE_FP16_TO_FP32(x) ggml_compute_fp16_to_fp32(x)
#define GGML_COMPUTE_FP32_TO_FP16(x) ggml_compute_fp32_to_fp16(x)
#define GGML_FP16_TO_FP32(x) GGML_COMPUTE_FP16_TO_FP32(x)
#define GGML_FP32_TO_FP16(x) GGML_COMPUTE_FP32_TO_FP16(x)
static inline float ggml_compute_fp16_to_fp32(ggml_fp16_t h) {
float f;
double d;
__asm__(
"mtfprd %0,%2\n"
"xscvhpdp %0,%0\n"
"frsp %1,%0\n" :
"=d"(d),
"=f"(f):
"r"(h));
return f;
}
static inline ggml_fp16_t ggml_compute_fp32_to_fp16(float f) {
double d;
ggml_fp16_t r;
__asm__(
"xscvdphp %0,%2\n"
"mffprd %1,%0\n" :
"=d"(d),
"=r"(r):
"f"(f));
return r;
}
#elif defined(__riscv) && defined(GGML_RV_ZFH)
static inline float ggml_compute_fp16_to_fp32(ggml_fp16_t h) {
float f;
__asm__(
"fmv.h.x %[f], %[h]\n\t"
"fcvt.s.h %[f], %[f]"
: [f] "=&f" (f)
: [h] "r" (h)
);
return f;
}
static inline ggml_fp16_t ggml_compute_fp32_to_fp16(float f) {
ggml_fp16_t res;
__asm__(
"fcvt.h.s %[f], %[f]\n\t"
"fmv.x.h %[h], %[f]"
: [h] "=&r" (res)
: [f] "f" (f)
);
return res;
}
#define GGML_COMPUTE_FP16_TO_FP32(x) ggml_compute_fp16_to_fp32(x)
#define GGML_COMPUTE_FP32_TO_FP16(x) ggml_compute_fp32_to_fp16(x)
#define GGML_FP16_TO_FP32(x) GGML_COMPUTE_FP16_TO_FP32(x)
#define GGML_FP32_TO_FP16(x) GGML_COMPUTE_FP32_TO_FP16(x)
#else
static inline float fp32_from_bits(uint32_t w) {
union {
uint32_t as_bits;
float as_value;
} fp32;
fp32.as_bits = w;
return fp32.as_value;
}
static inline uint32_t fp32_to_bits(float f) {
union {
float as_value;
uint32_t as_bits;
} fp32;
fp32.as_value = f;
return fp32.as_bits;
}
static inline float ggml_compute_fp16_to_fp32(ggml_fp16_t h) {
const uint32_t w = (uint32_t) h << 16;
const uint32_t sign = w & UINT32_C(0x80000000);
const uint32_t two_w = w + w;
const uint32_t exp_offset = UINT32_C(0xE0) << 23;
#if (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)) && (!defined(__cplusplus) || __cplusplus >= 201703L)
const float exp_scale = 0x1.0p-112f;
#else
const float exp_scale = fp32_from_bits(UINT32_C(0x7800000));
#endif
const float normalized_value = fp32_from_bits((two_w >> 4) + exp_offset) * exp_scale;
const uint32_t magic_mask = UINT32_C(126) << 23;
const float magic_bias = 0.5f;
const float denormalized_value = fp32_from_bits((two_w >> 17) | magic_mask) - magic_bias;
const uint32_t denormalized_cutoff = UINT32_C(1) << 27;
const uint32_t result = sign |
(two_w < denormalized_cutoff ? fp32_to_bits(denormalized_value) : fp32_to_bits(normalized_value));
return fp32_from_bits(result);
}
static inline ggml_fp16_t ggml_compute_fp32_to_fp16(float f) {
#if (defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)) && (!defined(__cplusplus) || __cplusplus >= 201703L)
const float scale_to_inf = 0x1.0p+112f;
const float scale_to_zero = 0x1.0p-110f;
#else
const float scale_to_inf = fp32_from_bits(UINT32_C(0x77800000));
const float scale_to_zero = fp32_from_bits(UINT32_C(0x08800000));
#endif
float base = (fabsf(f) * scale_to_inf) * scale_to_zero;
const uint32_t w = fp32_to_bits(f);
const uint32_t shl1_w = w + w;
const uint32_t sign = w & UINT32_C(0x80000000);
uint32_t bias = shl1_w & UINT32_C(0xFF000000);
if (bias < UINT32_C(0x71000000)) {
bias = UINT32_C(0x71000000);
}
base = fp32_from_bits((bias >> 1) + UINT32_C(0x07800000)) + base;
const uint32_t bits = fp32_to_bits(base);
const uint32_t exp_bits = (bits >> 13) & UINT32_C(0x00007C00);
const uint32_t mantissa_bits = bits & UINT32_C(0x00000FFF);
const uint32_t nonsign = exp_bits + mantissa_bits;
return (sign >> 16) | (shl1_w > UINT32_C(0xFF000000) ? UINT16_C(0x7E00) : nonsign);
}
#define GGML_COMPUTE_FP16_TO_FP32(x) ggml_compute_fp16_to_fp32(x)
#define GGML_COMPUTE_FP32_TO_FP16(x) ggml_compute_fp32_to_fp16(x)
#endif
GGML_API float ggml_table_f32_f16[1 << 16];
#if !defined(GGML_FP16_TO_FP32)
inline static float ggml_lookup_fp16_to_fp32(ggml_fp16_t f) {
uint16_t s;
memcpy(&s, &f, sizeof(uint16_t));
return ggml_table_f32_f16[s];
}
#define GGML_FP16_TO_FP32(x) ggml_lookup_fp16_to_fp32(x)
#endif
#if !defined(GGML_FP32_TO_FP16)
#define GGML_FP32_TO_FP16(x) GGML_COMPUTE_FP32_TO_FP16(x)
#endif
static inline float ggml_compute_bf16_to_fp32(ggml_bf16_t h) {
union {
float f;
uint32_t i;
} u;
u.i = (uint32_t)h.bits << 16;
return u.f;
}
static inline ggml_bf16_t ggml_compute_fp32_to_bf16(float s) {
ggml_bf16_t h;
union {
float f;
uint32_t i;
} u;
u.f = s;
if ((u.i & 0x7fffffff) > 0x7f800000) {
h.bits = (u.i >> 16) | 64;
return h;
}
h.bits = (u.i + (0x7fff + ((u.i >> 16) & 1))) >> 16;
return h;
}
#define GGML_FP32_TO_BF16(x) ggml_compute_fp32_to_bf16(x)
#define GGML_BF16_TO_FP32(x) ggml_compute_bf16_to_fp32(x)
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
#include <vector>
GGML_API size_t gguf_type_size(enum gguf_type type);
GGML_API struct gguf_context * gguf_init_from_file_impl(FILE * file, struct gguf_init_params params);
GGML_API void gguf_write_to_buf(const struct gguf_context * ctx, std::vector<int8_t> & buf, bool only_meta);
#endif