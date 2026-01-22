#include "ggml.h"
#include "ggml-cpu.h"
#include "ggml-alloc.h"
#include "ggml-backend.h"
#ifdef GGML_USE_CUDA
#include "ggml-cuda.h"
#endif
#ifdef GGML_USE_METAL
#include "ggml-metal.h"
#endif
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <map>
#include <string>
#include <vector>
static void ggml_log_callback_default(ggml_log_level level, const char * text, void * user_data) {
(void) level;
(void) user_data;
fputs(text, stderr);
fflush(stderr);
}
struct simple_model {
struct ggml_tensor * a;
struct ggml_tensor * b;
ggml_backend_t backend = NULL;
ggml_backend_buffer_t buffer;
struct ggml_context * ctx;
};
void load_model(simple_model & model, float * a, float * b, int rows_A, int cols_A, int rows_B, int cols_B) {
ggml_log_set(ggml_log_callback_default, nullptr);
#ifdef GGML_USE_CUDA
fprintf(stderr, "%s: using CUDA backend\n", __func__);
model.backend = ggml_backend_cuda_init(0);
if (!model.backend) {
fprintf(stderr, "%s: ggml_backend_cuda_init() failed\n", __func__);
}
#endif
#ifdef GGML_USE_METAL
fprintf(stderr, "%s: using Metal backend\n", __func__);
model.backend = ggml_backend_metal_init();
if (!model.backend) {
fprintf(stderr, "%s: ggml_backend_metal_init() failed\n", __func__);
}
#endif
if (!model.backend) {
model.backend = ggml_backend_cpu_init();
}
int num_tensors = 2;
struct ggml_init_params params {
ggml_tensor_overhead() * num_tensors,
NULL,
true,
};
model.ctx = ggml_init(params);
model.a = ggml_new_tensor_2d(model.ctx, GGML_TYPE_F32, cols_A, rows_A);
model.b = ggml_new_tensor_2d(model.ctx, GGML_TYPE_F32, cols_B, rows_B);
model.buffer = ggml_backend_alloc_ctx_tensors(model.ctx, model.backend);
ggml_backend_tensor_set(model.a, a, 0, ggml_nbytes(model.a));
ggml_backend_tensor_set(model.b, b, 0, ggml_nbytes(model.b));
}
struct ggml_cgraph * build_graph(const simple_model& model) {
static size_t buf_size = ggml_tensor_overhead()*GGML_DEFAULT_GRAPH_SIZE + ggml_graph_overhead();
static std::vector<uint8_t> buf(buf_size);
struct ggml_init_params params0 = {
buf_size,
buf.data(),
true,
};
struct ggml_context * ctx0 = ggml_init(params0);
struct ggml_cgraph * gf = ggml_new_graph(ctx0);
struct ggml_tensor * result = ggml_mul_mat(ctx0, model.a, model.b);
ggml_build_forward_expand(gf, result);
ggml_free(ctx0);
return gf;
}
struct ggml_tensor * compute(const simple_model & model, ggml_gallocr_t allocr) {
struct ggml_cgraph * gf = build_graph(model);
ggml_gallocr_alloc_graph(allocr, gf);
int n_threads = 1;
if (ggml_backend_is_cpu(model.backend)) {
ggml_backend_cpu_set_n_threads(model.backend, n_threads);
}
ggml_backend_graph_compute(model.backend, gf);
return ggml_graph_node(gf, -1);
}
int main(void) {
ggml_time_init();
const int rows_A = 4, cols_A = 2;
float matrix_A[rows_A * cols_A] = {
2, 8,
5, 1,
4, 2,
8, 6
};
const int rows_B = 3, cols_B = 2;
float matrix_B[rows_B * cols_B] = {
10, 5,
9, 9,
5, 4
};
simple_model model;
load_model(model, matrix_A, matrix_B, rows_A, cols_A, rows_B, cols_B);
ggml_gallocr_t allocr = NULL;
{
allocr = ggml_gallocr_new(ggml_backend_get_default_buffer_type(model.backend));
struct ggml_cgraph * gf = build_graph(model);
ggml_gallocr_reserve(allocr, gf);
size_t mem_size = ggml_gallocr_get_buffer_size(allocr, 0);
fprintf(stderr, "%s: compute buffer size: %.4f KB\n", __func__, mem_size/1024.0);
}
struct ggml_tensor * result = compute(model, allocr);
std::vector<float> out_data(ggml_nelements(result));
ggml_backend_tensor_get(result, out_data.data(), 0, ggml_nbytes(result));
printf("mul mat (%d x %d) (transposed result):\n[", (int) result->ne[0], (int) result->ne[1]);
for (int j = 0; j < result->ne[1] ; j++) {
if (j > 0) {
printf("\n");
}
for (int i = 0; i < result->ne[0] ; i++) {
printf(" %.2f", out_data[j * result->ne[0] + i]);
}
}
printf(" ]\n");
ggml_gallocr_free(allocr);
ggml_free(model.ctx);
ggml_backend_buffer_free(model.buffer);
ggml_backend_free(model.backend);
return 0;
}