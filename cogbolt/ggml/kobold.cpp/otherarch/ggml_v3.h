#pragma once
#ifdef GGML_V3_SHARED
# if defined(_WIN32) && !defined(__MINGW32__)
# ifdef GGML_V3_BUILD
# define GGML_V3_API __declspec(dllexport)
# else
# define GGML_V3_API __declspec(dllimport)
# endif
# else
# define GGML_V3_API __attribute__ ((visibility ("default")))
# endif
#else
# define GGML_V3_API
#endif
# define GGML_V3_DEPRECATED(func, hint) func
#ifndef __GNUC__
# define GGML_V3_ATTRIBUTE_FORMAT(...)
#elif defined(__MINGW32__)
# define GGML_V3_ATTRIBUTE_FORMAT(...) __attribute__((format(gnu_printf, __VA_ARGS__)))
#else
# define GGML_V3_ATTRIBUTE_FORMAT(...) __attribute__((format(printf, __VA_ARGS__)))
#endif
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#define GGML_V3_FILE_MAGIC 0x67676d6c
#define GGML_V3_FILE_VERSION 1
#define GGML_V3_QNT_VERSION 2
#define GGML_V3_QNT_VERSION_FACTOR 1000
#define GGML_V3_MAX_DIMS 4
#define GGML_V3_MAX_PARAMS 2048
#define GGML_V3_MAX_CONTEXTS 64
#define GGML_V3_MAX_SRC 10
#ifndef GGML_V3_MAX_NAME
#define GGML_V3_MAX_NAME 64
#endif
#define GGML_V3_MAX_OP_PARAMS 64
#define GGML_V3_DEFAULT_N_THREADS 4
#define GGML_V3_DEFAULT_GRAPH_SIZE 2048
#if UINTPTR_MAX == 0xFFFFFFFF
#define GGML_V3_MEM_ALIGN 4
#else
#define GGML_V3_MEM_ALIGN 16
#endif
#define GGML_V3_EXIT_SUCCESS 0
#define GGML_V3_EXIT_ABORTED 1
#define GGUF_V3_MAGIC "GGUF"
#define GGUF_V3_VERSION 3
#define GGUF_V3_DEFAULT_ALIGNMENT 32
#define GGML_V3_UNUSED(x) (void)(x)
#define GGML_V3_PAD(x, n) (((x) + (n) - 1) & ~((n) - 1))
#define GGML_V3_ASSERT_CONTINUE(x) \
do { \
if (!(x)) { \
fprintf(stderr, "GGML_V3_ASSERT_CONTINUE: %s:%d: %s\n", __FILE__, __LINE__, #x); \
} \
} while (0)
#define GGML_V3_ASSERT(x) \
do { \
if (!(x)) { \
fflush(stdout); \
fprintf(stderr, "GGML_V3_ASSERT: %s:%d: %s\n", __FILE__, __LINE__, #x); \
ggml_v3_print_backtrace(); \
abort(); \
} \
} while (0)
#ifndef NDEBUG
#define GGML_V3_UNREACHABLE() GGML_V3_ASSERT(!"statement should not be reached")
#elif defined(__GNUC__)
#define GGML_V3_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
#define GGML_V3_UNREACHABLE() __assume(0)
#else
#define GGML_V3_UNREACHABLE() ((void) 0)
#endif
#define GGML_V3_TENSOR_LOCALS_1(type, prefix, pointer, array) \
const type prefix##0 = (pointer)->array[0]; \
GGML_V3_UNUSED(prefix##0);
#define GGML_V3_TENSOR_LOCALS_2(type, prefix, pointer, array) \
GGML_V3_TENSOR_LOCALS_1 (type, prefix, pointer, array) \
const type prefix##1 = (pointer)->array[1]; \
GGML_V3_UNUSED(prefix##1);
#define GGML_V3_TENSOR_LOCALS_3(type, prefix, pointer, array) \
GGML_V3_TENSOR_LOCALS_2 (type, prefix, pointer, array) \
const type prefix##2 = (pointer)->array[2]; \
GGML_V3_UNUSED(prefix##2);
#define GGML_V3_TENSOR_LOCALS(type, prefix, pointer, array) \
GGML_V3_TENSOR_LOCALS_3 (type, prefix, pointer, array) \
const type prefix##3 = (pointer)->array[3]; \
GGML_V3_UNUSED(prefix##3);
#define GGML_V3_TENSOR_UNARY_OP_LOCALS \
GGML_V3_TENSOR_LOCALS(int64_t, ne0, src0, ne) \
GGML_V3_TENSOR_LOCALS(size_t, nb0, src0, nb) \
GGML_V3_TENSOR_LOCALS(int64_t, ne, dst, ne) \
GGML_V3_TENSOR_LOCALS(size_t, nb, dst, nb)
#define GGML_V3_TENSOR_BINARY_OP_LOCALS \
GGML_V3_TENSOR_LOCALS(int64_t, ne0, src0, ne) \
GGML_V3_TENSOR_LOCALS(size_t, nb0, src0, nb) \
GGML_V3_TENSOR_LOCALS(int64_t, ne1, src1, ne) \
GGML_V3_TENSOR_LOCALS(size_t, nb1, src1, nb) \
GGML_V3_TENSOR_LOCALS(int64_t, ne, dst, ne) \
GGML_V3_TENSOR_LOCALS(size_t, nb, dst, nb)
#ifdef __cplusplus
extern "C" {
#endif
#if defined(__ARM_NEON) && defined(__CUDACC__)
typedef half ggml_v3_fp16_t;
#elif defined(__ARM_NEON) && !defined(_MSC_VER)
typedef __fp16 ggml_v3_fp16_t;
#else
typedef uint16_t ggml_v3_fp16_t;
#endif
GGML_V3_API float ggml_v3_fp16_to_fp32(ggml_v3_fp16_t x);
GGML_V3_API ggml_v3_fp16_t ggml_v3_fp32_to_fp16(float x);
GGML_V3_API void ggml_v3_fp16_to_fp32_row(const ggml_v3_fp16_t * x, float * y, int n);
GGML_V3_API void ggml_v3_fp32_to_fp16_row(const float * x, ggml_v3_fp16_t * y, int n);
struct ggml_v3_object;
struct ggml_v3_context;
enum ggml_v3_type {
GGML_V3_TYPE_F32 = 0,
GGML_V3_TYPE_F16 = 1,
GGML_V3_TYPE_Q4_0 = 2,
GGML_V3_TYPE_Q4_1 = 3,
GGML_V3_TYPE_Q5_0 = 6,
GGML_V3_TYPE_Q5_1 = 7,
GGML_V3_TYPE_Q8_0 = 8,
GGML_V3_TYPE_Q8_1 = 9,
GGML_V3_TYPE_Q2_K = 10,
GGML_V3_TYPE_Q3_K = 11,
GGML_V3_TYPE_Q4_K = 12,
GGML_V3_TYPE_Q5_K = 13,
GGML_V3_TYPE_Q6_K = 14,
GGML_V3_TYPE_Q8_K = 15,
GGML_V3_TYPE_IQ2_XXS = 16,
GGML_V3_TYPE_IQ2_XS = 17,
GGML_V3_TYPE_I8,
GGML_V3_TYPE_I16,
GGML_V3_TYPE_I32,
GGML_V3_TYPE_COUNT,
};
enum ggml_v3_prec {
GGML_V3_PREC_DEFAULT,
GGML_V3_PREC_F32,
};
enum ggml_v3_backend_type {
GGML_V3_BACKEND_CPU = 0,
GGML_V3_BACKEND_GPU = 10,
GGML_V3_BACKEND_GPU_SPLIT = 20,
};
enum ggml_v3_ftype {
GGML_V3_FTYPE_UNKNOWN = -1,
GGML_V3_FTYPE_ALL_F32 = 0,
GGML_V3_FTYPE_MOSTLY_F16 = 1,
GGML_V3_FTYPE_MOSTLY_Q4_0 = 2,
GGML_V3_FTYPE_MOSTLY_Q4_1 = 3,
GGML_V3_FTYPE_MOSTLY_Q4_1_SOME_F16 = 4,
GGML_V3_FTYPE_MOSTLY_Q8_0 = 7,
GGML_V3_FTYPE_MOSTLY_Q5_0 = 8,
GGML_V3_FTYPE_MOSTLY_Q5_1 = 9,
GGML_V3_FTYPE_MOSTLY_Q2_K = 10,
GGML_V3_FTYPE_MOSTLY_Q3_K = 11,
GGML_V3_FTYPE_MOSTLY_Q4_K = 12,
GGML_V3_FTYPE_MOSTLY_Q5_K = 13,
GGML_V3_FTYPE_MOSTLY_Q6_K = 14,
GGML_V3_FTYPE_MOSTLY_IQ2_XXS = 15,
GGML_V3_FTYPE_MOSTLY_IQ2_XS = 16,
};
enum ggml_v3_op {
GGML_V3_OP_NONE = 0,
GGML_V3_OP_DUP,
GGML_V3_OP_ADD,
GGML_V3_OP_ADD1,
GGML_V3_OP_ACC,
GGML_V3_OP_SUB,
GGML_V3_OP_MUL,
GGML_V3_OP_DIV,
GGML_V3_OP_SQR,
GGML_V3_OP_SQRT,
GGML_V3_OP_LOG,
GGML_V3_OP_SUM,
GGML_V3_OP_SUM_ROWS,
GGML_V3_OP_MEAN,
GGML_V3_OP_ARGMAX,
GGML_V3_OP_REPEAT,
GGML_V3_OP_REPEAT_BACK,
GGML_V3_OP_CONCAT,
GGML_V3_OP_SILU_BACK,
GGML_V3_OP_NORM,
GGML_V3_OP_RMS_NORM,
GGML_V3_OP_RMS_NORM_BACK,
GGML_V3_OP_GROUP_NORM,
GGML_V3_OP_MUL_MAT,
GGML_V3_OP_MUL_MAT_ID,
GGML_V3_OP_OUT_PROD,
GGML_V3_OP_SCALE,
GGML_V3_OP_SET,
GGML_V3_OP_CPY,
GGML_V3_OP_CONT,
GGML_V3_OP_RESHAPE,
GGML_V3_OP_VIEW,
GGML_V3_OP_PERMUTE,
GGML_V3_OP_TRANSPOSE,
GGML_V3_OP_GET_ROWS,
GGML_V3_OP_GET_ROWS_BACK,
GGML_V3_OP_DIAG,
GGML_V3_OP_DIAG_MASK_INF,
GGML_V3_OP_DIAG_MASK_ZERO,
GGML_V3_OP_SOFT_MAX,
GGML_V3_OP_SOFT_MAX_BACK,
GGML_V3_OP_ROPE,
GGML_V3_OP_ROPE_BACK,
GGML_V3_OP_ALIBI,
GGML_V3_OP_CLAMP,
GGML_V3_OP_CONV_TRANSPOSE_1D,
GGML_V3_OP_IM2COL,
GGML_V3_OP_CONV_TRANSPOSE_2D,
GGML_V3_OP_POOL_1D,
GGML_V3_OP_POOL_2D,
GGML_V3_OP_UPSCALE,
GGML_V3_OP_PAD,
GGML_V3_OP_ARGSORT,
GGML_V3_OP_LEAKY_RELU,
GGML_V3_OP_FLASH_ATTN,
GGML_V3_OP_FLASH_FF,
GGML_V3_OP_FLASH_ATTN_BACK,
GGML_V3_OP_WIN_PART,
GGML_V3_OP_WIN_UNPART,
GGML_V3_OP_GET_REL_POS,
GGML_V3_OP_ADD_REL_POS,
GGML_V3_OP_UNARY,
GGML_V3_OP_MAP_UNARY,
GGML_V3_OP_MAP_BINARY,
GGML_V3_OP_MAP_CUSTOM1_F32,
GGML_V3_OP_MAP_CUSTOM2_F32,
GGML_V3_OP_MAP_CUSTOM3_F32,
GGML_V3_OP_MAP_CUSTOM1,
GGML_V3_OP_MAP_CUSTOM2,
GGML_V3_OP_MAP_CUSTOM3,
GGML_V3_OP_CROSS_ENTROPY_LOSS,
GGML_V3_OP_CROSS_ENTROPY_LOSS_BACK,
GGML_V3_OP_COUNT,
};
enum ggml_v3_unary_op {
GGML_V3_UNARY_OP_ABS,
GGML_V3_UNARY_OP_SGN,
GGML_V3_UNARY_OP_NEG,
GGML_V3_UNARY_OP_STEP,
GGML_V3_UNARY_OP_TANH,
GGML_V3_UNARY_OP_ELU,
GGML_V3_UNARY_OP_RELU,
GGML_V3_UNARY_OP_GELU,
GGML_V3_UNARY_OP_GELU_QUICK,
GGML_V3_UNARY_OP_SILU,
GGML_V3_UNARY_OP_COUNT,
};
enum ggml_v3_object_type {
GGML_V3_OBJECT_TENSOR,
GGML_V3_OBJECT_GRAPH,
GGML_V3_OBJECT_WORK_BUFFER
};
enum ggml_v3_log_level {
GGML_V3_LOG_LEVEL_ERROR = 2,
GGML_V3_LOG_LEVEL_WARN = 3,
GGML_V3_LOG_LEVEL_INFO = 4,
GGML_V3_LOG_LEVEL_DEBUG = 5
};
struct ggml_v3_object {
size_t offs;
size_t size;
struct ggml_v3_object * next;
enum ggml_v3_object_type type;
char padding[4];
};
static const size_t GGML_V3_OBJECT_SIZE = sizeof(struct ggml_v3_object);
struct ggml_v3_tensor {
enum ggml_v3_type type;
enum ggml_v3_backend_type backend;
struct ggml_v3_backend_buffer * buffer;
int64_t ne[GGML_V3_MAX_DIMS];
size_t nb[GGML_V3_MAX_DIMS];
enum ggml_v3_op op;
int32_t op_params[GGML_V3_MAX_OP_PARAMS / sizeof(int32_t)];
bool is_param;
struct ggml_v3_tensor * grad;
struct ggml_v3_tensor * src[GGML_V3_MAX_SRC];
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
struct ggml_v3_tensor * view_src;
size_t view_offs;
void * data;
char name[GGML_V3_MAX_NAME];
void * extra;
char padding[8];
};
static const size_t GGML_V3_TENSOR_SIZE = sizeof(struct ggml_v3_tensor);
struct ggml_v3_cplan {
size_t work_size;
uint8_t * work_data;
int n_threads;
bool (*abort_callback)(void * data);
void * abort_callback_data;
};
enum ggml_v3_cgraph_eval_order {
GGML_V3_CGRAPH_EVAL_ORDER_LEFT_TO_RIGHT = 0,
GGML_V3_CGRAPH_EVAL_ORDER_RIGHT_TO_LEFT,
GGML_V3_CGRAPH_EVAL_ORDER_COUNT
};
struct ggml_v3_hash_set {
size_t size;
struct ggml_v3_tensor ** keys;
};
struct ggml_v3_cgraph {
int size;
int n_nodes;
int n_leafs;
struct ggml_v3_tensor ** nodes;
struct ggml_v3_tensor ** grads;
struct ggml_v3_tensor ** leafs;
struct ggml_v3_hash_set visited_hash_table;
enum ggml_v3_cgraph_eval_order order;
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
};
struct ggml_v3_scratch {
size_t offs;
size_t size;
void * data;
};
struct ggml_v3_init_params {
size_t mem_size;
void * mem_buffer;
bool no_alloc;
};
enum ggml_v3_task_type {
GGML_V3_TASK_INIT = 0,
GGML_V3_TASK_COMPUTE,
GGML_V3_TASK_FINALIZE,
};
struct ggml_v3_compute_params {
enum ggml_v3_task_type type;
int ith, nth;
size_t wsize;
void * wdata;
};
GGML_V3_API void ggml_v3_time_init(void);
GGML_V3_API int64_t ggml_v3_time_ms(void);
GGML_V3_API int64_t ggml_v3_time_us(void);
GGML_V3_API int64_t ggml_v3_cycles(void);
GGML_V3_API int64_t ggml_v3_cycles_per_ms(void);
GGML_V3_API void ggml_v3_print_backtrace(void);
GGML_V3_API void ggml_v3_numa_init(void);
GGML_V3_API bool ggml_v3_is_numa(void);
GGML_V3_API void ggml_v3_print_object (const struct ggml_v3_object * obj);
GGML_V3_API void ggml_v3_print_objects(const struct ggml_v3_context * ctx);
GGML_V3_API int64_t ggml_v3_nelements (const struct ggml_v3_tensor * tensor);
GGML_V3_API int64_t ggml_v3_nrows (const struct ggml_v3_tensor * tensor);
GGML_V3_API size_t ggml_v3_nbytes (const struct ggml_v3_tensor * tensor);
GGML_V3_API size_t ggml_v3_nbytes_pad (const struct ggml_v3_tensor * tensor);
GGML_V3_API int ggml_v3_blck_size(enum ggml_v3_type type);
GGML_V3_API size_t ggml_v3_type_size(enum ggml_v3_type type);
GGML_V3_API size_t ggml_v3_row_size (enum ggml_v3_type type, int64_t ne);
GGML_V3_DEPRECATED(
GGML_V3_API double ggml_v3_type_sizef(enum ggml_v3_type type),
"use ggml_v3_row_size() instead");
GGML_V3_API const char * ggml_v3_type_name(enum ggml_v3_type type);
GGML_V3_API const char * ggml_v3_op_name (enum ggml_v3_op op);
GGML_V3_API const char * ggml_v3_op_symbol(enum ggml_v3_op op);
GGML_V3_API const char * ggml_v3_unary_op_name(enum ggml_v3_unary_op op);
GGML_V3_API const char * ggml_v3_op_desc(const struct ggml_v3_tensor * t);
GGML_V3_API size_t ggml_v3_element_size(const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_quantized(enum ggml_v3_type type);
GGML_V3_API enum ggml_v3_type ggml_v3_ftype_to_ggml_v3_type(enum ggml_v3_ftype ftype);
GGML_V3_API bool ggml_v3_is_transposed(const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_contiguous(const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_permuted (const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_scalar (const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_vector (const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_matrix (const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_is_3d (const struct ggml_v3_tensor * tensor);
GGML_V3_API int ggml_v3_n_dims (const struct ggml_v3_tensor * tensor);
GGML_V3_API bool ggml_v3_are_same_shape(const struct ggml_v3_tensor * t0, const struct ggml_v3_tensor * t1);
GGML_V3_API size_t ggml_v3_tensor_overhead(void);
GGML_V3_API struct ggml_v3_context * ggml_v3_init(struct ggml_v3_init_params params);
GGML_V3_API void ggml_v3_free(struct ggml_v3_context * ctx);
GGML_V3_API size_t ggml_v3_used_mem(const struct ggml_v3_context * ctx);
GGML_V3_API size_t ggml_v3_set_scratch (struct ggml_v3_context * ctx, struct ggml_v3_scratch scratch);
GGML_V3_API bool ggml_v3_get_no_alloc(struct ggml_v3_context * ctx);
GGML_V3_API void ggml_v3_set_no_alloc(struct ggml_v3_context * ctx, bool no_alloc);
GGML_V3_API void * ggml_v3_get_mem_buffer (const struct ggml_v3_context * ctx);
GGML_V3_API size_t ggml_v3_get_mem_size (const struct ggml_v3_context * ctx);
GGML_V3_API size_t ggml_v3_get_max_tensor_size(const struct ggml_v3_context * ctx);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_tensor(
struct ggml_v3_context * ctx,
enum ggml_v3_type type,
int n_dims,
const int64_t *ne);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_tensor_1d(
struct ggml_v3_context * ctx,
enum ggml_v3_type type,
int64_t ne0);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_tensor_2d(
struct ggml_v3_context * ctx,
enum ggml_v3_type type,
int64_t ne0,
int64_t ne1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_tensor_3d(
struct ggml_v3_context * ctx,
enum ggml_v3_type type,
int64_t ne0,
int64_t ne1,
int64_t ne2);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_tensor_4d(
struct ggml_v3_context * ctx,
enum ggml_v3_type type,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_i32(struct ggml_v3_context * ctx, int32_t value);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_new_f32(struct ggml_v3_context * ctx, float value);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_dup_tensor (struct ggml_v3_context * ctx, const struct ggml_v3_tensor * src);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_view_tensor(struct ggml_v3_context * ctx, struct ggml_v3_tensor * src);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_first_tensor(const struct ggml_v3_context * ctx);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_next_tensor (const struct ggml_v3_context * ctx, struct ggml_v3_tensor * tensor);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_tensor(struct ggml_v3_context * ctx, const char * name);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_zero(struct ggml_v3_tensor * tensor);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_i32 (struct ggml_v3_tensor * tensor, int32_t value);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_f32 (struct ggml_v3_tensor * tensor, float value);
GGML_V3_API void ggml_v3_unravel_index(const struct ggml_v3_tensor * tensor, int64_t i, int64_t * i0, int64_t * i1, int64_t * i2, int64_t * i3);
GGML_V3_API int32_t ggml_v3_get_i32_1d(const struct ggml_v3_tensor * tensor, int i);
GGML_V3_API void ggml_v3_set_i32_1d(const struct ggml_v3_tensor * tensor, int i, int32_t value);
GGML_V3_API int32_t ggml_v3_get_i32_nd(const struct ggml_v3_tensor * tensor, int i0, int i1, int i2, int i3);
GGML_V3_API void ggml_v3_set_i32_nd(const struct ggml_v3_tensor * tensor, int i0, int i1, int i2, int i3, int32_t value);
GGML_V3_API float ggml_v3_get_f32_1d(const struct ggml_v3_tensor * tensor, int i);
GGML_V3_API void ggml_v3_set_f32_1d(const struct ggml_v3_tensor * tensor, int i, float value);
GGML_V3_API float ggml_v3_get_f32_nd(const struct ggml_v3_tensor * tensor, int i0, int i1, int i2, int i3);
GGML_V3_API void ggml_v3_set_f32_nd(const struct ggml_v3_tensor * tensor, int i0, int i1, int i2, int i3, float value);
GGML_V3_API void * ggml_v3_get_data (const struct ggml_v3_tensor * tensor);
GGML_V3_API float * ggml_v3_get_data_f32(const struct ggml_v3_tensor * tensor);
GGML_V3_API enum ggml_v3_unary_op ggml_v3_get_unary_op(const struct ggml_v3_tensor * tensor);
GGML_V3_API const char * ggml_v3_get_name (const struct ggml_v3_tensor * tensor);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_name ( struct ggml_v3_tensor * tensor, const char * name);
GGML_V3_ATTRIBUTE_FORMAT(2, 3)
GGML_V3_API struct ggml_v3_tensor * ggml_v3_format_name( struct ggml_v3_tensor * tensor, const char * fmt, ...);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_dup(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_dup_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add_cast(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
enum ggml_v3_type type);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add1(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add1_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_acc(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_acc_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sub(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sub_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_mul(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_mul_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_div(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_div_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sqr(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sqr_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sqrt(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sqrt_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_log(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_log_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sum(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sum_rows(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_mean(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_argmax(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_repeat(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_repeat_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_concat(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_abs(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_abs_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sgn(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_sgn_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_neg(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_neg_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_step(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_step_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_tanh(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_tanh_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_elu(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_elu_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_relu(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_leaky_relu(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a, float negative_slope, bool inplace);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_relu_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_gelu(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_gelu_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_gelu_quick(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_gelu_quick_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_silu(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_silu_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_silu_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_norm(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float eps);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_norm_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float eps);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rms_norm(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float eps);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rms_norm_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float eps);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_group_norm(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_groups);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_group_norm_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_groups);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rms_norm_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
float eps);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_mul_mat(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API void ggml_v3_mul_mat_set_prec(
struct ggml_v3_tensor * a,
enum ggml_v3_prec prec);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_mul_mat_id(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * const as[],
int n_as,
struct ggml_v3_tensor * ids,
int id,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_out_prod(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_scale(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float s);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_scale_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float s);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_1d_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_set_2d_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
size_t nb1,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cpy(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cont(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cont_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cont_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cont_3d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cont_4d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_reshape(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_reshape_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_reshape_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_reshape_3d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_reshape_4d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_view_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_view_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
size_t nb1,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_view_3d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
size_t nb1,
size_t nb2,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_view_4d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_permute(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int axis0,
int axis1,
int axis2,
int axis3);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_transpose(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_rows(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_rows_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_diag(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_diag_mask_inf(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_past);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_diag_mask_inf_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_past);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_diag_mask_zero(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_past);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_diag_mask_zero_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_past);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_soft_max(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_soft_max_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_soft_max_ext(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * mask,
float scale);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_soft_max_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_soft_max_back_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
int mode,
int n_ctx);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
int mode,
int n_ctx);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope_custom(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
int mode,
int n_ctx,
int n_orig_ctx,
float freq_base,
float freq_scale,
float ext_factor,
float attn_factor,
float beta_fast,
float beta_slow);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope_custom_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
int mode,
int n_ctx,
int n_orig_ctx,
float freq_base,
float freq_scale,
float ext_factor,
float attn_factor,
float beta_fast,
float beta_slow);
void ggml_v3_rope_yarn_corr_dims(
int n_dims, int n_orig_ctx, float freq_base, float beta_fast, float beta_slow, float dims[2]);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope_xpos_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
float base,
bool down);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_rope_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int n_dims,
int mode,
int n_ctx,
int n_orig_ctx,
float freq_base,
float freq_scale,
float ext_factor,
float attn_factor,
float beta_fast,
float beta_slow,
float xpos_base,
bool xpos_down);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_alibi(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int n_past,
int n_head,
float bias_max);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_clamp(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
float min,
float max);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_im2col(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int s0,
int s1,
int p0,
int p1,
int d0,
int d1,
bool is_2D);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int s0,
int p0,
int d0);
GGML_V3_API struct ggml_v3_tensor* ggml_v3_conv_1d_ph(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int s,
int d);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_transpose_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int s0,
int p0,
int d0);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int s0,
int s1,
int p0,
int p1,
int d0,
int d1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_2d_sk_p0(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_2d_s1_ph(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_conv_transpose_2d_p0(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
int stride);
enum ggml_v3_op_pool {
GGML_V3_OP_POOL_MAX,
GGML_V3_OP_POOL_AVG,
GGML_V3_OP_POOL_COUNT,
};
GGML_V3_API struct ggml_v3_tensor * ggml_v3_pool_1d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
enum ggml_v3_op_pool op,
int k0,
int s0,
int p0);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_pool_2d(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
enum ggml_v3_op_pool op,
int k0,
int k1,
int s0,
int s1,
float p0,
float p1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_upscale(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int scale_factor);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_pad(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int p0,
int p1,
int p2,
int p3);
enum ggml_v3_sort_order {
GGML_V3_SORT_ASC,
GGML_V3_SORT_DESC,
};
GGML_V3_API struct ggml_v3_tensor * ggml_v3_argsort(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
enum ggml_v3_sort_order order);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_top_k(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int k);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_flash_attn(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * q,
struct ggml_v3_tensor * k,
struct ggml_v3_tensor * v,
bool masked);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_flash_attn_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * q,
struct ggml_v3_tensor * k,
struct ggml_v3_tensor * v,
struct ggml_v3_tensor * d,
bool masked);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_flash_ff(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b0,
struct ggml_v3_tensor * b1,
struct ggml_v3_tensor * c0,
struct ggml_v3_tensor * c1);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_win_part(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int w);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_win_unpart(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int w0,
int h0,
int w);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_unary(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
enum ggml_v3_unary_op op);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_unary_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
enum ggml_v3_unary_op op);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_get_rel_pos(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
int qh,
int kh);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add_rel_pos(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * pw,
struct ggml_v3_tensor * ph);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_add_rel_pos_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * pw,
struct ggml_v3_tensor * ph);
typedef void (*ggml_v3_unary_op_f32_t) (const int, float *, const float *);
typedef void (*ggml_v3_binary_op_f32_t)(const int, float *, const float *, const float *);
typedef void (*ggml_v3_custom1_op_f32_t)(struct ggml_v3_tensor *, const struct ggml_v3_tensor *);
typedef void (*ggml_v3_custom2_op_f32_t)(struct ggml_v3_tensor *, const struct ggml_v3_tensor *, const struct ggml_v3_tensor *);
typedef void (*ggml_v3_custom3_op_f32_t)(struct ggml_v3_tensor *, const struct ggml_v3_tensor *, const struct ggml_v3_tensor *, const struct ggml_v3_tensor *);
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_unary_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_unary_op_f32_t fun),
"use ggml_v3_map_custom1 instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_unary_inplace_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_unary_op_f32_t fun),
"use ggml_v3_map_custom1_inplace instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_binary_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_binary_op_f32_t fun),
"use ggml_v3_map_custom2 instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_binary_inplace_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_binary_op_f32_t fun),
"use ggml_v3_map_custom2_inplace instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom1_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_custom1_op_f32_t fun),
"use ggml_v3_map_custom1 instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom1_inplace_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_custom1_op_f32_t fun),
"use ggml_v3_map_custom1_inplace instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom2_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_custom2_op_f32_t fun),
"use ggml_v3_map_custom2 instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom2_inplace_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_custom2_op_f32_t fun),
"use ggml_v3_map_custom2_inplace instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom3_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c,
ggml_v3_custom3_op_f32_t fun),
"use ggml_v3_map_custom3 instead");
GGML_V3_DEPRECATED(GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom3_inplace_f32(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c,
ggml_v3_custom3_op_f32_t fun),
"use ggml_v3_map_custom3_inplace instead");
typedef void (*ggml_v3_custom1_op_t)(struct ggml_v3_tensor * dst , const struct ggml_v3_tensor * a, int ith, int nth, void * userdata);
typedef void (*ggml_v3_custom2_op_t)(struct ggml_v3_tensor * dst , const struct ggml_v3_tensor * a, const struct ggml_v3_tensor * b, int ith, int nth, void * userdata);
typedef void (*ggml_v3_custom3_op_t)(struct ggml_v3_tensor * dst , const struct ggml_v3_tensor * a, const struct ggml_v3_tensor * b, const struct ggml_v3_tensor * c, int ith, int nth, void * userdata);
#define GGML_V3_N_TASKS_MAX -1
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom1(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_custom1_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom1_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
ggml_v3_custom1_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom2(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_custom2_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom2_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
ggml_v3_custom2_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom3(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c,
ggml_v3_custom3_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_map_custom3_inplace(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c,
ggml_v3_custom3_op_t fun,
int n_tasks,
void * userdata);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cross_entropy_loss(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_cross_entropy_loss_back(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * a,
struct ggml_v3_tensor * b,
struct ggml_v3_tensor * c);
GGML_V3_API void ggml_v3_set_param(
struct ggml_v3_context * ctx,
struct ggml_v3_tensor * tensor);
GGML_V3_API void ggml_v3_build_forward_expand (struct ggml_v3_cgraph * cgraph, struct ggml_v3_tensor * tensor);
GGML_V3_API void ggml_v3_build_backward_expand(struct ggml_v3_context * ctx, struct ggml_v3_cgraph * gf, struct ggml_v3_cgraph * gb, bool keep);
GGML_V3_API struct ggml_v3_cgraph * ggml_v3_new_graph (struct ggml_v3_context * ctx);
GGML_V3_API struct ggml_v3_cgraph * ggml_v3_new_graph_custom (struct ggml_v3_context * ctx, size_t size, bool grads);
GGML_V3_API struct ggml_v3_cgraph * ggml_v3_graph_dup (struct ggml_v3_context * ctx, struct ggml_v3_cgraph * cgraph);
GGML_V3_API struct ggml_v3_cgraph ggml_v3_graph_view (struct ggml_v3_cgraph * cgraph, int i0, int i1);
GGML_V3_API void ggml_v3_graph_cpy (struct ggml_v3_cgraph * src, struct ggml_v3_cgraph * dst);
GGML_V3_API void ggml_v3_graph_reset (struct ggml_v3_cgraph * cgraph);
GGML_V3_API void ggml_v3_graph_clear (struct ggml_v3_cgraph * cgraph);
GGML_V3_API size_t ggml_v3_graph_overhead(void);
GGML_V3_API size_t ggml_v3_graph_overhead_custom(size_t size, bool grads);
GGML_V3_API struct ggml_v3_cplan ggml_v3_graph_plan (struct ggml_v3_cgraph * cgraph, int n_threads );
GGML_V3_API int ggml_v3_graph_compute(struct ggml_v3_cgraph * cgraph, struct ggml_v3_cplan * cplan);
GGML_V3_API void ggml_v3_graph_compute_with_ctx(struct ggml_v3_context * ctx, struct ggml_v3_cgraph * cgraph, int n_threads);
GGML_V3_API struct ggml_v3_tensor * ggml_v3_graph_get_tensor(struct ggml_v3_cgraph * cgraph, const char * name);
GGML_V3_API void ggml_v3_graph_export(const struct ggml_v3_cgraph * cgraph, const char * fname);
GGML_V3_API struct ggml_v3_cgraph * ggml_v3_graph_import(const char * fname, struct ggml_v3_context ** ctx_data, struct ggml_v3_context ** ctx_eval);
GGML_V3_API void ggml_v3_graph_print(const struct ggml_v3_cgraph * cgraph);
GGML_V3_API void ggml_v3_graph_dump_dot(const struct ggml_v3_cgraph * gb, const struct ggml_v3_cgraph * gf, const char * filename);
GGML_V3_API void ggml_v3_build_backward_gradient_checkpointing(
struct ggml_v3_context * ctx,
struct ggml_v3_cgraph * gf,
struct ggml_v3_cgraph * gb,
struct ggml_v3_cgraph * gb_tmp,
struct ggml_v3_tensor * * checkpoints,
int n_checkpoints);
enum ggml_v3_opt_type {
GGML_V3_OPT_ADAM,
GGML_V3_OPT_LBFGS,
};
enum ggml_v3_linesearch {
GGML_V3_LINESEARCH_DEFAULT = 1,
GGML_V3_LINESEARCH_BACKTRACKING_ARMIJO = 0,
GGML_V3_LINESEARCH_BACKTRACKING_WOLFE = 1,
GGML_V3_LINESEARCH_BACKTRACKING_STRONG_WOLFE = 2,
};
enum ggml_v3_opt_result {
GGML_V3_OPT_OK = 0,
GGML_V3_OPT_DID_NOT_CONVERGE,
GGML_V3_OPT_NO_CONTEXT,
GGML_V3_OPT_INVALID_WOLFE,
GGML_V3_OPT_FAIL,
GGML_V3_OPT_CANCEL,
GGML_V3_LINESEARCH_FAIL = -128,
GGML_V3_LINESEARCH_MINIMUM_STEP,
GGML_V3_LINESEARCH_MAXIMUM_STEP,
GGML_V3_LINESEARCH_MAXIMUM_ITERATIONS,
GGML_V3_LINESEARCH_INVALID_PARAMETERS,
};
typedef void (*ggml_v3_opt_callback)(void * data, int accum_step, float * sched, bool * cancel);
typedef void (*ggml_v3_log_callback)(enum ggml_v3_log_level level, const char * text, void * user_data);
struct ggml_v3_opt_params {
enum ggml_v3_opt_type type;
size_t graph_size;
int n_threads;
int past;
float delta;
int max_no_improvement;
bool print_forward_graph;
bool print_backward_graph;
int n_gradient_accumulation;
struct {
int n_iter;
float sched;
float decay;
int decay_min_ndim;
float alpha;
float beta1;
float beta2;
float eps;
float eps_f;
float eps_g;
float gclip;
} adam;
struct {
int m;
int n_iter;
int max_linesearch;
float eps;
float ftol;
float wolfe;
float min_step;
float max_step;
enum ggml_v3_linesearch linesearch;
} lbfgs;
};
struct ggml_v3_opt_context {
struct ggml_v3_context * ctx;
struct ggml_v3_opt_params params;
int iter;
int64_t nx;
bool just_initialized;
float loss_before;
float loss_after;
struct {
struct ggml_v3_tensor * g;
struct ggml_v3_tensor * m;
struct ggml_v3_tensor * v;
struct ggml_v3_tensor * pf;
float fx_best;
float fx_prev;
int n_no_improvement;
} adam;
struct {
struct ggml_v3_tensor * x;
struct ggml_v3_tensor * xp;
struct ggml_v3_tensor * g;
struct ggml_v3_tensor * gp;
struct ggml_v3_tensor * d;
struct ggml_v3_tensor * pf;
struct ggml_v3_tensor * lmal;
struct ggml_v3_tensor * lmys;
struct ggml_v3_tensor * lms;
struct ggml_v3_tensor * lmy;
float fx_best;
float step;
int j;
int k;
int end;
int n_no_improvement;
} lbfgs;
};
GGML_V3_API struct ggml_v3_opt_params ggml_v3_opt_default_params(enum ggml_v3_opt_type type);
GGML_V3_API enum ggml_v3_opt_result ggml_v3_opt(
struct ggml_v3_context * ctx,
struct ggml_v3_opt_params params,
struct ggml_v3_tensor * f);
GGML_V3_API void ggml_v3_opt_init(
struct ggml_v3_context * ctx,
struct ggml_v3_opt_context * opt,
struct ggml_v3_opt_params params,
int64_t nx);
GGML_V3_API enum ggml_v3_opt_result ggml_v3_opt_resume(
struct ggml_v3_context * ctx,
struct ggml_v3_opt_context * opt,
struct ggml_v3_tensor * f);
GGML_V3_API enum ggml_v3_opt_result ggml_v3_opt_resume_g(
struct ggml_v3_context * ctx,
struct ggml_v3_opt_context * opt,
struct ggml_v3_tensor * f,
struct ggml_v3_cgraph * gf,
struct ggml_v3_cgraph * gb,
ggml_v3_opt_callback callback,
void * callback_data);
GGML_V3_API size_t ggml_v3_quantize_q4_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q4_1(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q5_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q5_1(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q8_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q2_K(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q3_K(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q4_K(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q5_K(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_q6_K(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_iq2_xxs(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_iq2_xs (const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V3_API size_t ggml_v3_quantize_chunk(enum ggml_v3_type type, const float * src, void * dst, int start, int n, int64_t * hist);
typedef void(*ggml_v3_collect_imatrix_t)(const struct ggml_v3_tensor * src0, const struct ggml_v3_tensor * src1);
GGML_V3_API void ggml_v3_set_imatrix_collection(ggml_v3_collect_imatrix_t imatrix_collect);
enum gguf_v3_type {
GGUF_V3_TYPE_UINT8 = 0,
GGUF_V3_TYPE_INT8 = 1,
GGUF_V3_TYPE_UINT16 = 2,
GGUF_V3_TYPE_INT16 = 3,
GGUF_V3_TYPE_UINT32 = 4,
GGUF_V3_TYPE_INT32 = 5,
GGUF_V3_TYPE_FLOAT32 = 6,
GGUF_V3_TYPE_BOOL = 7,
GGUF_V3_TYPE_STRING = 8,
GGUF_V3_TYPE_ARRAY = 9,
GGUF_V3_TYPE_UINT64 = 10,
GGUF_V3_TYPE_INT64 = 11,
GGUF_V3_TYPE_FLOAT64 = 12,
GGUF_V3_TYPE_COUNT,
};
struct gguf_v3_context;
struct gguf_v3_init_params {
bool no_alloc;
struct ggml_v3_context ** ctx;
};
GGML_V3_API struct gguf_v3_context * gguf_v3_init_empty(void);
GGML_V3_API struct gguf_v3_context * gguf_v3_init_from_file(const char * fname, struct gguf_v3_init_params params);
GGML_V3_API void gguf_v3_free(struct gguf_v3_context * ctx);
GGML_V3_API const char * gguf_v3_type_name(enum gguf_v3_type type);
GGML_V3_API int gguf_v3_get_version (const struct gguf_v3_context * ctx);
GGML_V3_API size_t gguf_v3_get_alignment (const struct gguf_v3_context * ctx);
GGML_V3_API size_t gguf_v3_get_data_offset(const struct gguf_v3_context * ctx);
GGML_V3_API void * gguf_v3_get_data (const struct gguf_v3_context * ctx);
GGML_V3_API int gguf_v3_get_n_kv(const struct gguf_v3_context * ctx);
GGML_V3_API int gguf_v3_find_key(const struct gguf_v3_context * ctx, const char * key);
GGML_V3_API const char * gguf_v3_get_key (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API enum gguf_v3_type gguf_v3_get_kv_type (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API enum gguf_v3_type gguf_v3_get_arr_type(const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API uint8_t gguf_v3_get_val_u8 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API int8_t gguf_v3_get_val_i8 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API uint16_t gguf_v3_get_val_u16 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API int16_t gguf_v3_get_val_i16 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API uint32_t gguf_v3_get_val_u32 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API int32_t gguf_v3_get_val_i32 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API float gguf_v3_get_val_f32 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API uint64_t gguf_v3_get_val_u64 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API int64_t gguf_v3_get_val_i64 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API double gguf_v3_get_val_f64 (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API bool gguf_v3_get_val_bool(const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API const char * gguf_v3_get_val_str (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API const void * gguf_v3_get_val_data(const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API int gguf_v3_get_arr_n (const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API const void * gguf_v3_get_arr_data(const struct gguf_v3_context * ctx, int key_id);
GGML_V3_API const char * gguf_v3_get_arr_str (const struct gguf_v3_context * ctx, int key_id, int i);
GGML_V3_API int gguf_v3_get_n_tensors (const struct gguf_v3_context * ctx);
GGML_V3_API int gguf_v3_find_tensor (const struct gguf_v3_context * ctx, const char * name);
GGML_V3_API size_t gguf_v3_get_tensor_offset(const struct gguf_v3_context * ctx, int i);
GGML_V3_API char * gguf_v3_get_tensor_name (const struct gguf_v3_context * ctx, int i);
GGML_V3_API enum ggml_v3_type gguf_v3_get_tensor_type (const struct gguf_v3_context * ctx, int i);
GGML_V3_API void gguf_v3_set_val_u8 (struct gguf_v3_context * ctx, const char * key, uint8_t val);
GGML_V3_API void gguf_v3_set_val_i8 (struct gguf_v3_context * ctx, const char * key, int8_t val);
GGML_V3_API void gguf_v3_set_val_u16 (struct gguf_v3_context * ctx, const char * key, uint16_t val);
GGML_V3_API void gguf_v3_set_val_i16 (struct gguf_v3_context * ctx, const char * key, int16_t val);
GGML_V3_API void gguf_v3_set_val_u32 (struct gguf_v3_context * ctx, const char * key, uint32_t val);
GGML_V3_API void gguf_v3_set_val_i32 (struct gguf_v3_context * ctx, const char * key, int32_t val);
GGML_V3_API void gguf_v3_set_val_f32 (struct gguf_v3_context * ctx, const char * key, float val);
GGML_V3_API void gguf_v3_set_val_u64 (struct gguf_v3_context * ctx, const char * key, uint64_t val);
GGML_V3_API void gguf_v3_set_val_i64 (struct gguf_v3_context * ctx, const char * key, int64_t val);
GGML_V3_API void gguf_v3_set_val_f64 (struct gguf_v3_context * ctx, const char * key, double val);
GGML_V3_API void gguf_v3_set_val_bool(struct gguf_v3_context * ctx, const char * key, bool val);
GGML_V3_API void gguf_v3_set_val_str (struct gguf_v3_context * ctx, const char * key, const char * val);
GGML_V3_API void gguf_v3_set_arr_data(struct gguf_v3_context * ctx, const char * key, enum gguf_v3_type type, const void * data, int n);
GGML_V3_API void gguf_v3_set_arr_str (struct gguf_v3_context * ctx, const char * key, const char ** data, int n);
GGML_V3_API void gguf_v3_set_kv(struct gguf_v3_context * ctx, struct gguf_v3_context * src);
GGML_V3_API void gguf_v3_add_tensor(struct gguf_v3_context * ctx, const struct ggml_v3_tensor * tensor);
GGML_V3_API void gguf_v3_set_tensor_type(struct gguf_v3_context * ctx, const char * name, enum ggml_v3_type type);
GGML_V3_API void gguf_v3_set_tensor_data(struct gguf_v3_context * ctx, const char * name, const void * data, size_t size);
GGML_V3_API void gguf_v3_write_to_file(const struct gguf_v3_context * ctx, const char * fname, bool only_meta);
GGML_V3_API size_t gguf_v3_get_meta_size(const struct gguf_v3_context * ctx);
GGML_V3_API void gguf_v3_get_meta_data(const struct gguf_v3_context * ctx, void * data);
GGML_V3_API int ggml_v3_cpu_has_avx (void);
GGML_V3_API int ggml_v3_cpu_has_avx_vnni (void);
GGML_V3_API int ggml_v3_cpu_has_avx2 (void);
GGML_V3_API int ggml_v3_cpu_has_avx512 (void);
GGML_V3_API int ggml_v3_cpu_has_avx512_vbmi(void);
GGML_V3_API int ggml_v3_cpu_has_avx512_vnni(void);
GGML_V3_API int ggml_v3_cpu_has_fma (void);
GGML_V3_API int ggml_v3_cpu_has_neon (void);
GGML_V3_API int ggml_v3_cpu_has_arm_fma (void);
GGML_V3_API int ggml_v3_cpu_has_metal (void);
GGML_V3_API int ggml_v3_cpu_has_f16c (void);
GGML_V3_API int ggml_v3_cpu_has_fp16_va (void);
GGML_V3_API int ggml_v3_cpu_has_wasm_simd (void);
GGML_V3_API int ggml_v3_cpu_has_blas (void);
GGML_V3_API int ggml_v3_cpu_has_cublas (void);
GGML_V3_API int ggml_v3_cpu_has_clblast (void);
GGML_V3_API int ggml_v3_cpu_has_gpublas (void);
GGML_V3_API int ggml_v3_cpu_has_sse3 (void);
GGML_V3_API int ggml_v3_cpu_has_ssse3 (void);
GGML_V3_API int ggml_v3_cpu_has_vsx (void);
#ifdef __cplusplus
#define GGML_V3_RESTRICT
#else
#define GGML_V3_RESTRICT restrict
#endif
typedef void (*ggml_v3_to_float_t) (const void * GGML_V3_RESTRICT x, float * GGML_V3_RESTRICT y, int k);
typedef void (*ggml_v3_from_float_t)(const float * GGML_V3_RESTRICT x, void * GGML_V3_RESTRICT y, int k);
typedef void (*ggml_v3_vec_dot_t) (const int n, float * GGML_V3_RESTRICT s, const void * GGML_V3_RESTRICT x, const void * GGML_V3_RESTRICT y);
typedef struct {
const char * type_name;
int blck_size;
size_t type_size;
bool is_quantized;
ggml_v3_to_float_t to_float;
ggml_v3_from_float_t from_float;
ggml_v3_from_float_t from_float_reference;
ggml_v3_vec_dot_t vec_dot;
enum ggml_v3_type vec_dot_type;
} ggml_v3_type_traits_t;
GGML_V3_API ggml_v3_type_traits_t ggml_v3_internal_get_type_traits(enum ggml_v3_type type);
GGML_V3_API struct ggml_v3_allocr * ggml_v3_allocr_new(void * data, size_t size, size_t alignment);
GGML_V3_API struct ggml_v3_allocr * ggml_v3_allocr_new_measure(size_t alignment);
GGML_V3_API void ggml_v3_allocr_set_parse_seq(struct ggml_v3_allocr * alloc, const int * list, int n);
GGML_V3_API void ggml_v3_allocr_free(struct ggml_v3_allocr * alloc);
GGML_V3_API bool ggml_v3_allocr_is_measure(struct ggml_v3_allocr * alloc);
GGML_V3_API void ggml_v3_allocr_reset(struct ggml_v3_allocr * alloc);
GGML_V3_API void ggml_v3_allocr_alloc(struct ggml_v3_allocr * alloc, struct ggml_v3_tensor * tensor);
GGML_V3_API size_t ggml_v3_allocr_alloc_graph(struct ggml_v3_allocr * alloc, struct ggml_v3_cgraph * graph);
GGML_V3_API size_t ggml_v3_allocr_max_size(struct ggml_v3_allocr * alloc);
#define GGML_V3_GRAPH_HASHTABLE_SIZE 32771
#define GGML_V3_MAX_NODES 8192
#ifdef __cplusplus
}
#endif