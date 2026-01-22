#include "outprod.hpp"
void ggml_sycl_op_out_prod(ggml_backend_sycl_context& ctx, ggml_tensor* dst) {
scope_op_debug_print scope_dbg_print(__func__, dst, 2);
const ggml_tensor *src0 = dst->src[0];
const ggml_tensor *src1 = dst->src[1];
GGML_ASSERT(src0->type == GGML_TYPE_F32);
GGML_ASSERT(src1->type == GGML_TYPE_F32);
GGML_ASSERT(dst->type == GGML_TYPE_F32);
GGML_ASSERT(ggml_is_contiguous(src0));
GGML_ASSERT(ggml_is_contiguous(dst));
GGML_TENSOR_BINARY_OP_LOCALS
dpct::queue_ptr stream = ctx.stream();
GGML_ASSERT(ne01 == ne11);
GGML_ASSERT(ne0 == ne00);
GGML_ASSERT(ne1 == ne10);
const float* src0_d = (const float*)src0->data;
const float* src1_d = (const float*)src1->data;
float* dst_d = (float*)dst->data;
const float alpha = 1.0f;
const float beta = 0.0f;
const bool src1_T = ggml_is_transposed(src1);
const oneapi::math::transpose src1_op = src1_T ? oneapi::math::transpose::nontrans : oneapi::math::transpose::trans;
const int64_t ldb = (src1_T ? nb10 : nb11) / sizeof(float);
try {
oneapi::math::blas::column_major::gemm(get_onemath_backend(*stream), oneapi::math::transpose::nontrans, src1_op,
ne0, ne1, ne01, alpha, src0_d, ne00, src1_d, ldb, beta, dst_d, ne0);
}
catch (sycl::exception const& exc) {
std::cerr << exc.what() << std::endl;
GGML_ASSERT(false);
}
}