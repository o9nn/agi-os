#include "ggml.h"
#include "ggml-cpu.h"
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <map>
#include <string>
#include <vector>
struct simple_model {
struct ggml_tensor * a;
struct ggml_tensor * b;
struct ggml_context * ctx;
};
void load_model(simple_model & model, float * a, float * b, int rows_A, int cols_A, int rows_B, int cols_B) {
size_t ctx_size = 0;
{
ctx_size += rows_A * cols_A * ggml_type_size(GGML_TYPE_F32);
ctx_size += rows_B * cols_B * ggml_type_size(GGML_TYPE_F32);
ctx_size += 2 * ggml_tensor_overhead(),
ctx_size += ggml_graph_overhead();
ctx_size += 1024;
}
struct ggml_init_params params {
ctx_size,
NULL,
false,
};
model.ctx = ggml_init(params);
model.a = ggml_new_tensor_2d(model.ctx, GGML_TYPE_F32, cols_A, rows_A);
model.b = ggml_new_tensor_2d(model.ctx, GGML_TYPE_F32, cols_B, rows_B);
memcpy(model.a->data, a, ggml_nbytes(model.a));
memcpy(model.b->data, b, ggml_nbytes(model.b));
}
struct ggml_cgraph * build_graph(const simple_model& model) {
struct ggml_cgraph * gf = ggml_new_graph(model.ctx);
struct ggml_tensor * result = ggml_mul_mat(model.ctx, model.a, model.b);
ggml_build_forward_expand(gf, result);
return gf;
}
struct ggml_tensor * compute(const simple_model & model) {
struct ggml_cgraph * gf = build_graph(model);
int n_threads = 1;
ggml_graph_compute_with_ctx(model.ctx, gf, n_threads);
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
struct ggml_tensor * result = compute(model);
std::vector<float> out_data(ggml_nelements(result));
memcpy(out_data.data(), result->data, ggml_nbytes(result));
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
ggml_free(model.ctx);
return 0;
}