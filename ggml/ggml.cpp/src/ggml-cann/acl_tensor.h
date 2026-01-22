#ifndef CANN_ACL_TENSOR_H
#define CANN_ACL_TENSOR_H
#include <algorithm>
#include <cstring>
#include <aclnn/aclnn_base.h>
#include "common.h"
aclDataType ggml_cann_type_mapping(ggml_type type);
aclTensor* ggml_cann_create_tensor(const ggml_tensor* tensor, int64_t* ne = nullptr,
size_t* nb = nullptr, int64_t dims = 0,
aclFormat format = ACL_FORMAT_ND,
size_t offset = 0);
template<typename TYPE>
aclTensor* ggml_cann_create_tensor(void* data_ptr, aclDataType dtype,
TYPE type_size, int64_t* ne, TYPE* nb,
int64_t dims,
aclFormat format = ACL_FORMAT_ND,
size_t offset = 0) {
int64_t tmp_ne[GGML_MAX_DIMS * 2];
int64_t tmp_stride[GGML_MAX_DIMS * 2];
memcpy(tmp_ne, ne, dims * sizeof(int64_t));
for (int i = 0; i < dims; i++) {
tmp_stride[i] = nb[i] / type_size;
}
int64_t acl_storage_len = 1;
for (int i = 0; i < dims; i++) {
acl_storage_len += (tmp_ne[i] - 1) * tmp_stride[i];
}
std::reverse(tmp_ne, tmp_ne + dims);
std::reverse(tmp_stride, tmp_stride + dims);
aclTensor* acl_tensor =
aclCreateTensor(tmp_ne, dims, dtype, tmp_stride, offset / type_size,
format, &acl_storage_len, 1, data_ptr);
return acl_tensor;
}
bool ggml_cann_need_bcast(const ggml_tensor* t0, const ggml_tensor* t1);
int64_t ggml_cann_get_bcast_shape(const ggml_tensor* src0, const ggml_tensor* src1,
int64_t* bcast_ne_src0, int64_t* bcast_ne_src1,
size_t* bcast_nb_src0, size_t* bcast_nb_src1);
#define BCAST_SHAPE(src0, src1)                                              \
int64_t bcast_##src0##_ne[GGML_MAX_DIMS * 2];                            \
int64_t bcast_##src1##_ne[GGML_MAX_DIMS * 2];                            \
size_t bcast_##src0##_nb[GGML_MAX_DIMS * 2];                             \
size_t bcast_##src1##_nb[GGML_MAX_DIMS * 2];                             \
int64_t bcast_dims = ggml_cann_get_bcast_shape(                          \
src0, src1, bcast_##src0##_ne, bcast_##src1##_ne, bcast_##src0##_nb, \
bcast_##src1##_nb);
#define BCAST_PARAM(tensor) bcast_##tensor##_ne, bcast_##tensor##_nb, bcast_dims
int64_t ggml_cann_get_mulmat_bcast_shape(
const int64_t* input_ne, const int64_t* weight_ne, const int64_t* dst_ne,
const size_t* input_nb, const size_t* weight_nb, const size_t* dst_nb,
int64_t* bcast_input_ne, int64_t* bcast_weight_ne, int64_t* bcast_dst_ne,
size_t* bcast_input_nb, size_t* bcast_weight_nb, size_t* bcast_dst_nb);
#define BCAST_MUL_MAT_SHAPE(input, weight, dst)                         \
int64_t bcast_##input##_ne[GGML_MAX_DIMS * 2];                      \
int64_t bcast_##weight##_ne[GGML_MAX_DIMS * 2];                     \
int64_t bcast_##dst##_ne[GGML_MAX_DIMS * 2];                        \
size_t bcast_##input##_nb[GGML_MAX_DIMS * 2];                       \
size_t bcast_##weight##_nb[GGML_MAX_DIMS * 2];                      \
size_t bcast_##dst##_nb[GGML_MAX_DIMS * 2];                         \
int64_t bcast_dims = ggml_cann_get_mulmat_bcast_shape(              \
input->ne, weight->ne, dst->ne, input->nb, weight->nb, dst->nb, \
bcast_##input##_ne, bcast_##weight##_ne, bcast_##dst##_ne,      \
bcast_##input##_nb, bcast_##weight##_nb, bcast_##dst##_nb);
#define BCAST_MUL_MAT_PARAM(tensor) \
bcast_##tensor##_ne, bcast_##tensor##_nb, bcast_dims
#endif