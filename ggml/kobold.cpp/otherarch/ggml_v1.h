#pragma once
#ifdef __cplusplus
extern "C" {
#endif
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#define GGML_V1_MAX_DIMS 4
#define GGML_V1_MAX_NODES 4096
#define GGML_V1_MAX_PARAMS 16
#define GGML_V1_MAX_CONTEXTS 64
#define GGML_V1_MAX_OPT 4
#ifdef __ARM_NEON
typedef __fp16 ggml_v1_fp16_t;
#else
typedef uint16_t ggml_v1_fp16_t;
#endif
float ggml_v1_fp16_to_fp32(ggml_v1_fp16_t x);
ggml_v1_fp16_t ggml_v1_fp32_to_fp16(float x);
struct ggml_v1_object;
struct ggml_v1_context;
enum ggml_v1_type {
GGML_V1_TYPE_Q4_0,
GGML_V1_TYPE_Q4_1,
GGML_V1_TYPE_I8,
GGML_V1_TYPE_I16,
GGML_V1_TYPE_I32,
GGML_V1_TYPE_F16,
GGML_V1_TYPE_F32,
GGML_V1_TYPE_COUNT,
};
enum ggml_v1_op {
GGML_V1_OP_NONE = 0,
GGML_V1_OP_DUP,
GGML_V1_OP_ADD,
GGML_V1_OP_SUB,
GGML_V1_OP_MUL,
GGML_V1_OP_DIV,
GGML_V1_OP_SQR,
GGML_V1_OP_SQRT,
GGML_V1_OP_SUM,
GGML_V1_OP_MEAN,
GGML_V1_OP_REPEAT,
GGML_V1_OP_ABS,
GGML_V1_OP_SGN,
GGML_V1_OP_NEG,
GGML_V1_OP_STEP,
GGML_V1_OP_RELU,
GGML_V1_OP_GELU,
GGML_V1_OP_NORM,
GGML_V1_OP_MUL_MAT,
GGML_V1_OP_SCALE,
GGML_V1_OP_CPY,
GGML_V1_OP_RESHAPE,
GGML_V1_OP_VIEW,
GGML_V1_OP_PERMUTE,
GGML_V1_OP_TRANSPOSE,
GGML_V1_OP_GET_ROWS,
GGML_V1_OP_DIAG_MASK_INF,
GGML_V1_OP_SOFT_MAX,
GGML_V1_OP_ROPE,
GGML_V1_OP_CONV_1D_1S,
GGML_V1_OP_CONV_1D_2S,
GGML_V1_OP_FLASH_ATTN,
GGML_V1_OP_FLASH_FF,
GGML_V1_OP_COUNT,
};
struct ggml_v1_tensor {
enum ggml_v1_type type;
int n_dims;
int ne[GGML_V1_MAX_DIMS];
size_t nb[GGML_V1_MAX_DIMS];
enum ggml_v1_op op;
bool is_param;
struct ggml_v1_tensor * grad;
struct ggml_v1_tensor * src0;
struct ggml_v1_tensor * src1;
struct ggml_v1_tensor * opt[GGML_V1_MAX_OPT];
int n_tasks;
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
void * data;
char padding[8];
};
struct ggml_v1_cgraph {
int n_nodes;
int n_leafs;
int n_threads;
size_t work_size;
struct ggml_v1_tensor * work;
struct ggml_v1_tensor * nodes[GGML_V1_MAX_NODES];
struct ggml_v1_tensor * grads[GGML_V1_MAX_NODES];
struct ggml_v1_tensor * leafs[GGML_V1_MAX_NODES];
int perf_runs;
int64_t perf_cycles;
int64_t perf_time_us;
};
struct ggml_v1_scratch {
size_t offs;
size_t size;
void * data;
};
struct ggml_v1_init_params {
size_t mem_size;
void * mem_buffer;
};
void ggml_v1_time_init(void);
int64_t ggml_v1_time_ms(void);
int64_t ggml_v1_time_us(void);
int64_t ggml_v1_cycles(void);
int64_t ggml_v1_cycles_per_ms(void);
void ggml_v1_print_object (const struct ggml_v1_object * obj);
void ggml_v1_print_objects(const struct ggml_v1_context * ctx);
int ggml_v1_nelements(const struct ggml_v1_tensor * tensor);
size_t ggml_v1_nbytes (const struct ggml_v1_tensor * tensor);
int ggml_v1_blck_size (enum ggml_v1_type type);
size_t ggml_v1_type_size (enum ggml_v1_type type);
float ggml_v1_type_sizef(enum ggml_v1_type type);
size_t ggml_v1_element_size(const struct ggml_v1_tensor * tensor);
struct ggml_v1_context * ggml_v1_init(struct ggml_v1_init_params params);
void ggml_v1_free(struct ggml_v1_context * ctx);
size_t ggml_v1_used_mem(const struct ggml_v1_context * ctx);
size_t ggml_v1_set_scratch(struct ggml_v1_context * ctx, struct ggml_v1_scratch scratch);
struct ggml_v1_tensor * ggml_v1_new_tensor(
struct ggml_v1_context * ctx,
enum ggml_v1_type type,
int n_dims,
const int *ne);
struct ggml_v1_tensor * ggml_v1_new_tensor_1d(
struct ggml_v1_context * ctx,
enum ggml_v1_type type,
int ne0);
struct ggml_v1_tensor * ggml_v1_new_tensor_2d(
struct ggml_v1_context * ctx,
enum ggml_v1_type type,
int ne0,
int ne1);
struct ggml_v1_tensor * ggml_v1_new_tensor_3d(
struct ggml_v1_context * ctx,
enum ggml_v1_type type,
int ne0,
int ne1,
int ne2);
struct ggml_v1_tensor * ggml_v1_new_tensor_4d(
struct ggml_v1_context * ctx,
enum ggml_v1_type type,
int ne0,
int ne1,
int ne2,
int ne3);
struct ggml_v1_tensor * ggml_v1_new_i32(struct ggml_v1_context * ctx, int32_t value);
struct ggml_v1_tensor * ggml_v1_new_f32(struct ggml_v1_context * ctx, float value);
struct ggml_v1_tensor * ggml_v1_dup_tensor (struct ggml_v1_context * ctx, const struct ggml_v1_tensor * src);
struct ggml_v1_tensor * ggml_v1_view_tensor(struct ggml_v1_context * ctx, const struct ggml_v1_tensor * src);
struct ggml_v1_tensor * ggml_v1_set_zero(struct ggml_v1_tensor * tensor);
struct ggml_v1_tensor * ggml_v1_set_i32 (struct ggml_v1_tensor * tensor, int32_t value);
struct ggml_v1_tensor * ggml_v1_set_f32 (struct ggml_v1_tensor * tensor, float value);
int32_t ggml_v1_get_i32_1d(const struct ggml_v1_tensor * tensor, int i);
void ggml_v1_set_i32_1d(const struct ggml_v1_tensor * tensor, int i, int32_t value);
float ggml_v1_get_f32_1d(const struct ggml_v1_tensor * tensor, int i);
void ggml_v1_set_f32_1d(const struct ggml_v1_tensor * tensor, int i, float value);
void * ggml_v1_get_data (const struct ggml_v1_tensor * tensor);
float * ggml_v1_get_data_f32(const struct ggml_v1_tensor * tensor);
struct ggml_v1_tensor * ggml_v1_dup(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_add(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_sub(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_mul(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_div(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_sqr(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_sqrt(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_sum(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_mean(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_repeat(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_abs(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_sgn(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_neg(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_step(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_relu(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_gelu(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_norm(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_mul_mat(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_scale(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_cpy(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_reshape(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_reshape_2d(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int ne0,
int ne1);
struct ggml_v1_tensor * ggml_v1_reshape_3d(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int ne0,
int ne1,
int ne2);
struct ggml_v1_tensor * ggml_v1_view_1d(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int ne0,
size_t offset);
struct ggml_v1_tensor * ggml_v1_view_2d(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int ne0,
int ne1,
size_t nb1,
size_t offset);
struct ggml_v1_tensor * ggml_v1_permute(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int axis0,
int axis1,
int axis2,
int axis3);
struct ggml_v1_tensor * ggml_v1_transpose(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_get_rows(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_diag_mask_inf(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int n_past);
struct ggml_v1_tensor * ggml_v1_soft_max(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a);
struct ggml_v1_tensor * ggml_v1_rope(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
int n_past,
int n_dims,
int mode);
struct ggml_v1_tensor * ggml_v1_conv_1d_1s(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_conv_1d_2s(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b);
struct ggml_v1_tensor * ggml_v1_flash_attn(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * q,
struct ggml_v1_tensor * k,
struct ggml_v1_tensor * v,
bool masked);
struct ggml_v1_tensor * ggml_v1_flash_ff(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * a,
struct ggml_v1_tensor * b0,
struct ggml_v1_tensor * b1,
struct ggml_v1_tensor * c0,
struct ggml_v1_tensor * c1);
void ggml_v1_set_param(
struct ggml_v1_context * ctx,
struct ggml_v1_tensor * tensor);
void ggml_v1_build_forward_expand(struct ggml_v1_cgraph * cgraph, struct ggml_v1_tensor * tensor);
struct ggml_v1_cgraph ggml_v1_build_forward (struct ggml_v1_tensor * tensor);
struct ggml_v1_cgraph ggml_v1_build_backward(struct ggml_v1_context * ctx, struct ggml_v1_cgraph * gf, bool keep);
void ggml_v1_graph_compute(struct ggml_v1_context * ctx, struct ggml_v1_cgraph * cgraph);
void ggml_v1_graph_reset (struct ggml_v1_cgraph * cgraph);
void ggml_v1_graph_print(const struct ggml_v1_cgraph * cgraph);
void ggml_v1_graph_dump_dot(const struct ggml_v1_cgraph * gb, const struct ggml_v1_cgraph * gf, const char * filename);
enum ggml_v1_opt_type {
GGML_V1_OPT_ADAM,
GGML_V1_OPT_LBFGS,
};
enum ggml_v1_linesearch {
GGML_V1_LINESEARCH_DEFAULT = 1,
GGML_V1_LINESEARCH_BACKTRACKING_ARMIJO = 0,
GGML_V1_LINESEARCH_BACKTRACKING_WOLFE = 1,
GGML_V1_LINESEARCH_BACKTRACKING_STRONG_WOLFE = 2,
};
enum ggml_v1_opt_result {
GGML_V1_OPT_OK = 0,
GGML_V1_OPT_DID_NOT_CONVERGE,
GGML_V1_OPT_NO_CONTEXT,
GGML_V1_OPT_INVALID_WOLFE,
GGML_V1_OPT_FAIL,
GGML_V1_LINESEARCH_FAIL = -128,
GGML_V1_LINESEARCH_MINIMUM_STEP,
GGML_V1_LINESEARCH_MAXIMUM_STEP,
GGML_V1_LINESEARCH_MAXIMUM_ITERATIONS,
GGML_V1_LINESEARCH_INVALID_PARAMETERS,
};
struct ggml_v1_opt_params {
enum ggml_v1_opt_type type;
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
enum ggml_v1_linesearch linesearch;
} lbfgs;
};
struct ggml_v1_opt_params ggml_v1_opt_default_params(enum ggml_v1_opt_type type);
enum ggml_v1_opt_result ggml_v1_opt(
struct ggml_v1_context * ctx,
struct ggml_v1_opt_params params,
struct ggml_v1_tensor * f);
int ggml_v1_cpu_has_avx(void);
int ggml_v1_cpu_has_avx2(void);
int ggml_v1_cpu_has_avx512(void);
int ggml_v1_cpu_has_fma(void);
int ggml_v1_cpu_has_neon(void);
int ggml_v1_cpu_has_arm_fma(void);
int ggml_v1_cpu_has_f16c(void);
int ggml_v1_cpu_has_fp16_va(void);
int ggml_v1_cpu_has_wasm_simd(void);
int ggml_v1_cpu_has_blas(void);
int ggml_v1_cpu_has_sse3(void);
int ggml_v1_cpu_has_vsx(void);
#ifdef __cplusplus
}
#endif