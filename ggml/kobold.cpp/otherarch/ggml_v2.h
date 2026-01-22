#pragma once
#ifdef GGML_V2_SHARED
# if defined(_WIN32) && !defined(__MINGW32__)
# ifdef GGML_V2_BUILD
# define GGML_V2_API __declspec(dllexport)
# else
# define GGML_V2_API __declspec(dllimport)
# endif
# else
# define GGML_V2_API __attribute__ ((visibility ("default")))
# endif
#else
# define GGML_V2_API
#endif
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#define GGML_V2_FILE_MAGIC 0x67676d6c
#define GGML_V2_FILE_VERSION 1
#define GGML_V2_QNT_VERSION 1
#define GGML_V2_QNT_VERSION_FACTOR 1000
#define GGML_V2_MAX_DIMS 4
#define GGML_V2_MAX_NODES 4096
#define GGML_V2_MAX_PARAMS 256
#define GGML_V2_MAX_CONTEXTS 64
#define GGML_V2_MAX_OPT 4
#define GGML_V2_DEFAULT_N_THREADS 4
#define GGML_V2_ASSERT(x) \
do { \
if (!(x)) { \
fprintf(stderr, "GGML_V2_ASSERT: %s:%d: %s\n", __FILE__, __LINE__, #x); \
abort(); \
} \
} while (0)
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __ARM_NEON
typedef __fp16 ggml_v2_fp16_t;
#else
typedef uint16_t ggml_v2_fp16_t;
#endif
GGML_V2_API float ggml_v2_fp16_to_fp32(ggml_v2_fp16_t x);
GGML_V2_API ggml_v2_fp16_t ggml_v2_fp32_to_fp16(float x);
GGML_V2_API void ggml_v2_fp16_to_fp32_row(const ggml_v2_fp16_t * x, float * y, size_t n);
GGML_V2_API void ggml_v2_fp32_to_fp16_row(const float * x, ggml_v2_fp16_t * y, size_t n);
struct ggml_v2_object;
struct ggml_v2_context;
enum ggml_v2_type {
GGML_V2_TYPE_F32 = 0,
GGML_V2_TYPE_F16 = 1,
GGML_V2_TYPE_Q4_0 = 2,
GGML_V2_TYPE_Q4_1 = 3,
GGML_V2_TYPE_Q4_2 = 4,
GGML_V2_TYPE_Q4_3 = 5,
GGML_V2_TYPE_Q5_0 = 6,
GGML_V2_TYPE_Q5_1 = 7,
GGML_V2_TYPE_Q8_0 = 8,
GGML_V2_TYPE_Q8_1 = 9,
GGML_V2_TYPE_I8,
GGML_V2_TYPE_I16,
GGML_V2_TYPE_I32,
GGML_V2_TYPE_Q8_1B = 13,
GGML_V2_TYPE_COUNT,
};
enum ggml_v2_backend {
GGML_V2_BACKEND_CPU = 0,
GGML_V2_BACKEND_CUDA = 1,
GGML_V2_BACKEND_CL = 2,
};
enum ggml_v2_ftype {
GGML_V2_FTYPE_UNKNOWN = -1,
GGML_V2_FTYPE_ALL_F32 = 0,
GGML_V2_FTYPE_MOSTLY_F16 = 1,
GGML_V2_FTYPE_MOSTLY_Q4_0 = 2,
GGML_V2_FTYPE_MOSTLY_Q4_1 = 3,
GGML_V2_FTYPE_MOSTLY_Q4_1_SOME_F16 = 4,
GGML_V2_FTYPE_MOSTLY_Q4_2 = 5,
GGML_V2_FTYPE_MOSTLY_Q4_3 = 6,
GGML_V2_FTYPE_MOSTLY_Q8_0 = 7,
GGML_V2_FTYPE_MOSTLY_Q5_0 = 8,
GGML_V2_FTYPE_MOSTLY_Q5_1 = 9,
};
enum ggml_v2_op {
GGML_V2_OP_NONE = 0,
GGML_V2_OP_DUP,
GGML_V2_OP_ADD,
GGML_V2_OP_ADD1,
GGML_V2_OP_ACC,
GGML_V2_OP_SUB,
GGML_V2_OP_MUL,
GGML_V2_OP_DIV,
GGML_V2_OP_SQR,
GGML_V2_OP_SQRT,
GGML_V2_OP_LOG,
GGML_V2_OP_SUM,
GGML_V2_OP_SUM_ROWS,
GGML_V2_OP_MEAN,
GGML_V2_OP_REPEAT,
GGML_V2_OP_ABS,
GGML_V2_OP_SGN,
GGML_V2_OP_NEG,
GGML_V2_OP_STEP,
GGML_V2_OP_RELU,
GGML_V2_OP_GELU,
GGML_V2_OP_SILU,
GGML_V2_OP_SILU_BACK,
GGML_V2_OP_NORM,
GGML_V2_OP_RMS_NORM,
GGML_V2_OP_RMS_NORM_BACK,
GGML_V2_OP_MUL_MAT,
GGML_V2_OP_SCALE,
GGML_V2_OP_SET,
GGML_V2_OP_CPY,
GGML_V2_OP_CONT,
GGML_V2_OP_RESHAPE,
GGML_V2_OP_VIEW,
GGML_V2_OP_PERMUTE,
GGML_V2_OP_TRANSPOSE,
GGML_V2_OP_GET_ROWS,
GGML_V2_OP_GET_ROWS_BACK,
GGML_V2_OP_DIAG,
GGML_V2_OP_DIAG_MASK_INF,
GGML_V2_OP_DIAG_MASK_ZERO,
GGML_V2_OP_SOFT_MAX,
GGML_V2_OP_ROPE,
GGML_V2_OP_ROPE_BACK,
GGML_V2_OP_ALIBI,
GGML_V2_OP_CONV_1D_1S,
GGML_V2_OP_CONV_1D_2S,
GGML_V2_OP_FLASH_ATTN,
GGML_V2_OP_FLASH_FF,
GGML_V2_OP_MAP_UNARY,
GGML_V2_OP_MAP_BINARY,
GGML_V2_OP_COUNT,
};
struct ggml_v2_object {
size_t offs;
size_t size;
struct ggml_v2_object * next;
char padding[8];
};
static const size_t GGML_V2_OBJECT_SIZE = sizeof(struct ggml_v2_object);
struct ggml_v2_tensor {
enum ggml_v2_type type;
enum ggml_v2_backend backend;
int n_dims;
int64_t ne[GGML_V2_MAX_DIMS];
size_t nb[GGML_V2_MAX_DIMS];
enum ggml_v2_op op;
bool is_param;
struct ggml_v2_tensor * grad;
struct ggml_v2_tensor * src0;
struct ggml_v2_tensor * src1;
struct ggml_v2_tensor * opt[GGML_V2_MAX_OPT];
int n_tasks;
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
void * data;
char name[32];
char padding[16];
};
struct ggml_v2_cgraph {
int n_nodes;
int n_leafs;
int n_threads;
size_t work_size;
struct ggml_v2_tensor * work;
struct ggml_v2_tensor * nodes[GGML_V2_MAX_NODES];
struct ggml_v2_tensor * grads[GGML_V2_MAX_NODES];
struct ggml_v2_tensor * leafs[GGML_V2_MAX_NODES];
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
};
struct ggml_v2_scratch {
size_t offs;
size_t size;
void * data;
};
struct ggml_v2_init_params {
size_t mem_size;
void * mem_buffer;
bool no_alloc;
};
GGML_V2_API void ggml_v2_time_init(void);
GGML_V2_API int64_t ggml_v2_time_ms(void);
GGML_V2_API int64_t ggml_v2_time_us(void);
GGML_V2_API int64_t ggml_v2_cycles(void);
GGML_V2_API int64_t ggml_v2_cycles_per_ms(void);
GGML_V2_API void ggml_v2_print_object (const struct ggml_v2_object * obj);
GGML_V2_API void ggml_v2_print_objects(const struct ggml_v2_context * ctx);
GGML_V2_API int64_t ggml_v2_nelements(const struct ggml_v2_tensor * tensor);
GGML_V2_API size_t ggml_v2_nbytes (const struct ggml_v2_tensor * tensor);
GGML_V2_API int ggml_v2_blck_size (enum ggml_v2_type type);
GGML_V2_API size_t ggml_v2_type_size (enum ggml_v2_type type);
GGML_V2_API float ggml_v2_type_sizef(enum ggml_v2_type type);
GGML_V2_API const char * ggml_v2_type_name(enum ggml_v2_type type);
GGML_V2_API size_t ggml_v2_element_size(const struct ggml_v2_tensor * tensor);
GGML_V2_API bool ggml_v2_is_quantized(enum ggml_v2_type type);
GGML_V2_API enum ggml_v2_type ggml_v2_ftype_to_ggml_v2_type(enum ggml_v2_ftype ftype);
GGML_V2_API struct ggml_v2_context * ggml_v2_init(struct ggml_v2_init_params params);
GGML_V2_API void ggml_v2_free(struct ggml_v2_context * ctx);
GGML_V2_API size_t ggml_v2_used_mem(const struct ggml_v2_context * ctx);
GGML_V2_API size_t ggml_v2_set_scratch(struct ggml_v2_context * ctx, struct ggml_v2_scratch scratch);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_tensor(
struct ggml_v2_context * ctx,
enum ggml_v2_type type,
int n_dims,
const int64_t *ne);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_tensor_1d(
struct ggml_v2_context * ctx,
enum ggml_v2_type type,
int64_t ne0);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_tensor_2d(
struct ggml_v2_context * ctx,
enum ggml_v2_type type,
int64_t ne0,
int64_t ne1);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_tensor_3d(
struct ggml_v2_context * ctx,
enum ggml_v2_type type,
int64_t ne0,
int64_t ne1,
int64_t ne2);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_tensor_4d(
struct ggml_v2_context * ctx,
enum ggml_v2_type type,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_i32(struct ggml_v2_context * ctx, int32_t value);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_new_f32(struct ggml_v2_context * ctx, float value);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_dup_tensor (struct ggml_v2_context * ctx, const struct ggml_v2_tensor * src);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_view_tensor(struct ggml_v2_context * ctx, const struct ggml_v2_tensor * src);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_zero(struct ggml_v2_tensor * tensor);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_i32 (struct ggml_v2_tensor * tensor, int32_t value);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_f32 (struct ggml_v2_tensor * tensor, float value);
GGML_V2_API int32_t ggml_v2_get_i32_1d(const struct ggml_v2_tensor * tensor, int i);
GGML_V2_API void ggml_v2_set_i32_1d(const struct ggml_v2_tensor * tensor, int i, int32_t value);
GGML_V2_API float ggml_v2_get_f32_1d(const struct ggml_v2_tensor * tensor, int i);
GGML_V2_API void ggml_v2_set_f32_1d(const struct ggml_v2_tensor * tensor, int i, float value);
GGML_V2_API void * ggml_v2_get_data (const struct ggml_v2_tensor * tensor);
GGML_V2_API float * ggml_v2_get_data_f32(const struct ggml_v2_tensor * tensor);
GGML_V2_API const char * ggml_v2_get_name(const struct ggml_v2_tensor * tensor);
GGML_V2_API void ggml_v2_set_name(struct ggml_v2_tensor * tensor, const char * name);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_dup(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_add(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_add_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_add1(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_acc(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_acc_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sub(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_mul(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_div(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sqr(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sqrt(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_log(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_log_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sum(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sum_rows(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_mean(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_repeat(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_abs(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_sgn(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_neg(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_step(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_relu(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_gelu(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_silu(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_silu_back(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_norm(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_rms_norm(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_rms_norm_back(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_mul_mat(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_scale(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_scale_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_1d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_1d_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_2d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_set_2d_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
size_t nb1,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_cpy(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_cont(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_reshape(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_reshape_1d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_reshape_2d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_reshape_3d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_reshape_4d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_view_1d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_view_2d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1,
size_t nb1,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_view_3d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
size_t nb1,
size_t nb2,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_view_4d(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int64_t ne0,
int64_t ne1,
int64_t ne2,
int64_t ne3,
size_t nb1,
size_t nb2,
size_t nb3,
size_t offset);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_permute(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int axis0,
int axis1,
int axis2,
int axis3);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_transpose(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_get_rows(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_get_rows_back(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
struct ggml_v2_tensor * c);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_diag(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_diag_mask_inf(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_diag_mask_inf_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_diag_mask_zero(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_diag_mask_zero_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_soft_max(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_soft_max_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_rope(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past,
int n_dims,
int mode);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_rope_inplace(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past,
int n_dims,
int mode);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_rope_back(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past,
int n_dims,
int mode);
struct ggml_v2_tensor * ggml_v2_alibi(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
int n_past,
int n_head);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_conv_1d_1s(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_conv_1d_2s(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_flash_attn(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * q,
struct ggml_v2_tensor * k,
struct ggml_v2_tensor * v,
bool masked);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_flash_ff(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b0,
struct ggml_v2_tensor * b1,
struct ggml_v2_tensor * c0,
struct ggml_v2_tensor * c1);
typedef void (*ggml_v2_unary_op_f32_t)(const int, float *, const float *);
typedef void (*ggml_v2_binary_op_f32_t)(const int, float *, const float *, const float *);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_map_unary_f32(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
ggml_v2_unary_op_f32_t fun);
GGML_V2_API struct ggml_v2_tensor * ggml_v2_map_binary_f32(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * a,
struct ggml_v2_tensor * b,
ggml_v2_binary_op_f32_t fun);
GGML_V2_API void ggml_v2_set_param(
struct ggml_v2_context * ctx,
struct ggml_v2_tensor * tensor);
GGML_V2_API void ggml_v2_build_forward_expand(struct ggml_v2_cgraph * cgraph, struct ggml_v2_tensor * tensor);
GGML_V2_API struct ggml_v2_cgraph ggml_v2_build_forward (struct ggml_v2_tensor * tensor);
GGML_V2_API struct ggml_v2_cgraph ggml_v2_build_backward(struct ggml_v2_context * ctx, struct ggml_v2_cgraph * gf, bool keep);
GGML_V2_API void ggml_v2_graph_compute(struct ggml_v2_context * ctx, struct ggml_v2_cgraph * cgraph);
GGML_V2_API void ggml_v2_graph_reset (struct ggml_v2_cgraph * cgraph);
GGML_V2_API void ggml_v2_graph_print(const struct ggml_v2_cgraph * cgraph);
GGML_V2_API void ggml_v2_graph_dump_dot(const struct ggml_v2_cgraph * gb, const struct ggml_v2_cgraph * gf, const char * filename);
enum ggml_v2_opt_type {
GGML_V2_OPT_ADAM,
GGML_V2_OPT_LBFGS,
};
enum ggml_v2_linesearch {
GGML_V2_LINESEARCH_DEFAULT = 1,
GGML_V2_LINESEARCH_BACKTRACKING_ARMIJO = 0,
GGML_V2_LINESEARCH_BACKTRACKING_WOLFE = 1,
GGML_V2_LINESEARCH_BACKTRACKING_STRONG_WOLFE = 2,
};
enum ggml_v2_opt_result {
GGML_V2_OPT_OK = 0,
GGML_V2_OPT_DID_NOT_CONVERGE,
GGML_V2_OPT_NO_CONTEXT,
GGML_V2_OPT_INVALID_WOLFE,
GGML_V2_OPT_FAIL,
GGML_V2_LINESEARCH_FAIL = -128,
GGML_V2_LINESEARCH_MINIMUM_STEP,
GGML_V2_LINESEARCH_MAXIMUM_STEP,
GGML_V2_LINESEARCH_MAXIMUM_ITERATIONS,
GGML_V2_LINESEARCH_INVALID_PARAMETERS,
};
struct ggml_v2_opt_params {
enum ggml_v2_opt_type type;
int n_threads;
int past;
float delta;
int max_no_improvement;
bool print_forward_graph;
bool print_backward_graph;
struct {
int n_iter;
float alpha;
float beta1;
float beta2;
float eps;
float eps_f;
float eps_g;
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
enum ggml_v2_linesearch linesearch;
} lbfgs;
};
GGML_V2_API struct ggml_v2_opt_params ggml_v2_opt_default_params(enum ggml_v2_opt_type type);
GGML_V2_API enum ggml_v2_opt_result ggml_v2_opt(
struct ggml_v2_context * ctx,
struct ggml_v2_opt_params params,
struct ggml_v2_tensor * f);
GGML_V2_API size_t ggml_v2_quantize_q4_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q4_1(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q5_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q5_1(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q8_0(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q4_0_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q4_1_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q4_2_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q4_3_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q5_0_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q5_1_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_q8_0_v2(const float * src, void * dst, int n, int k, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_chunk(enum ggml_v2_type type, const float * src, void * dst, int start, int n, int64_t * hist);
GGML_V2_API size_t ggml_v2_quantize_chunk_v2(enum ggml_v2_type type, const float * src, void * dst, int start, int n, int64_t * hist);
void SetQuantsUnshuffled(bool unshuffled);
bool GetQuantsUnshuffled();
GGML_V2_API int ggml_v2_cpu_has_avx (void);
GGML_V2_API int ggml_v2_cpu_has_avx2 (void);
GGML_V2_API int ggml_v2_cpu_has_avx512 (void);
GGML_V2_API int ggml_v2_cpu_has_avx512_vbmi(void);
GGML_V2_API int ggml_v2_cpu_has_avx512_vnni(void);
GGML_V2_API int ggml_v2_cpu_has_fma (void);
GGML_V2_API int ggml_v2_cpu_has_neon (void);
GGML_V2_API int ggml_v2_cpu_has_arm_fma (void);
GGML_V2_API int ggml_v2_cpu_has_f16c (void);
GGML_V2_API int ggml_v2_cpu_has_fp16_va (void);
GGML_V2_API int ggml_v2_cpu_has_wasm_simd (void);
GGML_V2_API int ggml_v2_cpu_has_blas (void);
GGML_V2_API int ggml_v2_cpu_has_cublas (void);
GGML_V2_API int ggml_v2_cpu_has_clblast (void);
GGML_V2_API int ggml_v2_cpu_has_gpublas (void);
GGML_V2_API int ggml_v2_cpu_has_sse3 (void);
GGML_V2_API int ggml_v2_cpu_has_vsx (void);
#ifdef __cplusplus
#define GGML_V2_RESTRICT
#else
#define GGML_V2_RESTRICT restrict
#endif
typedef void (*dequantize_row_q_t)(const void * GGML_V2_RESTRICT x, float * GGML_V2_RESTRICT y, int k);
typedef void (*quantize_row_q_t) (const float * GGML_V2_RESTRICT x, void * GGML_V2_RESTRICT y, int k);
typedef void (*vec_dot_q_t) (const int n, float * GGML_V2_RESTRICT s, const void * GGML_V2_RESTRICT x, const void * GGML_V2_RESTRICT y);
typedef struct {
dequantize_row_q_t dequantize_row_q;
quantize_row_q_t quantize_row_q;
quantize_row_q_t quantize_row_q_reference;
quantize_row_q_t quantize_row_q_dot;
vec_dot_q_t vec_dot_q;
enum ggml_v2_type vec_dot_type;
} quantize_fns_t2;
quantize_fns_t2 ggml_v2_internal_get_quantize_fn(size_t i);
#ifdef __cplusplus
}
#endif