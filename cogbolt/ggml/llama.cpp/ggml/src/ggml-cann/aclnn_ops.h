#ifndef CANN_ACLNN_OPS
#define CANN_ACLNN_OPS
#include <unordered_set>
#include <functional>
#include <aclnnop/aclnn_abs.h>
#include <aclnnop/aclnn_neg.h>
#include <aclnnop/aclnn_exp.h>
#include <aclnnop/aclnn_arange.h>
#include <aclnnop/aclnn_argsort.h>
#include <aclnnop/aclnn_cat.h>
#include <aclnnop/aclnn_clamp.h>
#include <aclnnop/aclnn_gelu.h>
#include <aclnnop/aclnn_gelu_v2.h>
#include <aclnnop/aclnn_sigmoid.h>
#include <aclnnop/aclnn_hardsigmoid.h>
#include <aclnnop/aclnn_hardswish.h>
#include <aclnnop/aclnn_leaky_relu.h>
#include <aclnnop/aclnn_relu.h>
#include <aclnnop/aclnn_silu.h>
#include <aclnnop/aclnn_tanh.h>
#include <aclnnop/aclnn_sqrt.h>
#include <aclnnop/aclnn_sin.h>
#include <aclnnop/aclnn_cos.h>
#include <aclnnop/aclnn_log.h>
#include <aclnnop/aclnn_sign.h>
#include "acl_tensor.h"
#include "common.h"
void ggml_cann_repeat(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_leaky_relu(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_concat(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_arange(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_clamp(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_scale(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_argsort(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_norm(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_group_norm(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_acc(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_sum_rows(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_sum(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_upsample_nearest2d(ggml_backend_cann_context& ctx,
ggml_tensor* dst);
void ggml_cann_pad(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_pool2d(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_dup(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_rms_norm(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_diag_mask(ggml_backend_cann_context& ctx, ggml_tensor* dst, float value);
void ggml_cann_im2col(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_timestep_embedding(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_cpy(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_softmax(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_get_rows(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_set_rows(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_mul_mat(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_rope(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_argmax(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void aclnn_add(ggml_backend_cann_context& ctx, aclTensor* acl_src0,
aclTensor* acl_src1, aclTensor* acl_dst = nullptr);
void aclnn_sub(ggml_backend_cann_context& ctx, aclTensor* acl_src0,
aclTensor* acl_src1, aclTensor* acl_dst = nullptr);
void aclnn_mul(ggml_backend_cann_context& ctx, aclTensor* acl_src,
aclTensor* acl_other, aclTensor* acl_dst = nullptr);
void aclnn_div(ggml_backend_cann_context& ctx, aclTensor* acl_src,
aclTensor* acl_other, aclTensor* acl_dst = nullptr);
void aclnn_cos(ggml_backend_cann_context& ctx, aclTensor* acl_src,
aclTensor* acl_dst);
void aclnn_sin(ggml_backend_cann_context& ctx, aclTensor* acl_src,
aclTensor* acl_dst);
void bcast_shape(ggml_tensor * src0, ggml_tensor * src1, ggml_tensor * dst,
aclTensor ** acl_src0, aclTensor ** acl_src1, aclTensor ** acl_dst);
void ggml_cann_conv_transpose_1d(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_elu(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_mean(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_pad_reflect_1d(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_count_equal(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_step(ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_flash_attn_ext(ggml_backend_cann_context& ctx, ggml_tensor* dst);
using any_acl_resource = std::unique_ptr<void, std::function<void(void*)>>;
template<typename T>
struct acl_resource_traits;
template<>
struct acl_resource_traits<aclTensor> {
static void destroy(void* p) {
ACL_CHECK(aclDestroyTensor(static_cast<aclTensor*>(p)));
}
};
template<>
struct acl_resource_traits<aclIntArray> {
static void destroy(void* p) {
ACL_CHECK(aclDestroyIntArray(static_cast<aclIntArray*>(p)));
}
};
template<>
struct acl_resource_traits<aclScalar> {
static void destroy(void* p) {
ACL_CHECK(aclDestroyScalar(static_cast<aclScalar*>(p)));
}
};
template<>
struct acl_resource_traits<aclTensorList> {
static void destroy(void* p) {
ACL_CHECK(aclDestroyTensorList(static_cast<aclTensorList*>(p)));
}
};
template<typename T>
any_acl_resource make_acl_resource(T* ptr) {
return any_acl_resource(
static_cast<void*>(ptr),
[](void* p) {
acl_resource_traits<T>::destroy(p);
}
);
}
template<typename... Args>
void register_acl_resources(std::vector<any_acl_resource>& vec, Args*... args) {
(vec.emplace_back(make_acl_resource(args)), ...);
}
class aclnn_task : public cann_task {
public:
aclnn_task(aclnn_func_t aclnn_func, void * workspace_addr,
uint64_t workspace_size, aclOpExecutor * executor,
aclrtStream stream) :
aclnn_func_(aclnn_func),
workspace_addr_(workspace_addr),
workspace_size_(workspace_size),
executor_(executor),
stream_(stream) {}
virtual void run_task() override {
ACL_CHECK(aclnn_func_(workspace_addr_, workspace_size_, executor_, stream_));
}
private:
aclnn_func_t aclnn_func_;
void * workspace_addr_;
uint64_t workspace_size_;
aclOpExecutor * executor_;
aclrtStream stream_;
};
class release_resource_task : public cann_task {
public:
release_resource_task(std::vector<any_acl_resource>&& resources){
resource_ = std::move(resources);
}
virtual void run_task() override {
resource_.clear();
}
private:
std::vector<any_acl_resource> resource_;
};
class async_memcpy_task : public cann_task {
public:
async_memcpy_task(void* dst, const void* src, size_t size,
aclrtMemcpyKind kind, aclrtStream stream)
: dst_(dst), src_(src), size_(size), kind_(kind), stream_(stream) {}
virtual void run_task() override {
ACL_CHECK(aclrtMemcpyAsync(dst_, size_, src_, size_, kind_, stream_));
}
private:
void* dst_;
const void* src_;
size_t size_;
aclrtMemcpyKind kind_;
aclrtStream stream_;
};
class async_memset_task : public cann_task {
public:
async_memset_task(void* buffer, size_t size, int32_t value, aclrtStream stream)
: buffer_(buffer), size_(size), value_(value), stream_(stream) {}
virtual void run_task() override {
ACL_CHECK(aclrtMemsetAsync(buffer_, size_, value_, size_, stream_));
}
private:
void* buffer_;
size_t size_;
int32_t value_;
aclrtStream stream_;
};
#define GGML_CANN_CALL_ACLNN_OP(CTX, OP_NAME, ...) \
do { \
uint64_t workspaceSize = 0; \
aclOpExecutor * executor; \
void * workspaceAddr = nullptr; \
ACL_CHECK(aclnn##OP_NAME##GetWorkspaceSize(__VA_ARGS__, &workspaceSize, &executor));\
\
if (workspaceSize > 0) { \
ggml_cann_pool_alloc workspace_allocator(CTX.pool(), workspaceSize); \
workspaceAddr = workspace_allocator.get(); \
} \
if (CTX.async_mode) { \
auto task = \
std::make_unique<aclnn_task>(aclnn##OP_NAME, workspaceAddr, workspaceSize, \
executor, CTX.stream()); \
CTX.task_queue.submit_task(std::move(task)); \
} else { \
ACL_CHECK(aclnn##OP_NAME(workspaceAddr, workspaceSize, executor, CTX.stream()));\
} \
} while (0)
template <typename... Args>
void ggml_cann_release_resources(ggml_backend_cann_context & ctx, Args &&... args) {
std::vector<any_acl_resource> resources;
register_acl_resources(resources, std::forward<Args>(args)...);
if(ctx.async_mode) {
auto task = std::make_unique<release_resource_task>(std::move(resources));
ctx.task_queue.submit_task(std::move(task));
}
}
inline void ggml_cann_async_memcpy(ggml_backend_cann_context & ctx, void * dst,
const void * src, size_t len, aclrtMemcpyKind kind) {
if (ctx.async_mode) {
auto task = std::make_unique<async_memcpy_task>(dst, const_cast<void *>(src), len, kind, ctx.stream());
ctx.task_queue.submit_task(std::move(task));
} else {
ACL_CHECK(aclrtMemcpyAsync(dst, len, src, len, kind, ctx.stream()));
}
}
inline void ggml_cann_async_memcpy(ggml_backend_cann_context * ctx, void * dst,
const void * src, size_t len, aclrtMemcpyKind kind) {
if (ctx->async_mode) {
auto task = std::make_unique<async_memcpy_task>(dst, const_cast<void *>(src), len, kind, ctx->stream());
ctx->task_queue.submit_task(std::move(task));
} else {
ACL_CHECK(aclrtMemcpyAsync(dst, len, src, len, kind, ctx->stream()));
}
}
inline void ggml_cann_async_memset(ggml_backend_cann_context & ctx, void * buffer,
size_t size, int value) {
if (ctx.async_mode) {
auto task = std::make_unique<async_memset_task>(buffer, size, value, ctx.stream());
ctx.task_queue.submit_task(std::move(task));
} else {
ACL_CHECK(aclrtMemsetAsync(buffer, size, value, size, ctx.stream()));
}
}
void ggml_cann_mul_mat_id(ggml_backend_cann_context& ctx, ggml_tensor* dst);
static bool is_matmul_weight(const ggml_tensor* tensor) {
std::string name = ggml_get_name(tensor);
static const std::unordered_set<std::string> weight_suffixes{
"output.weight",
"attn_q.weight",
"attn_k.weight",
"attn_v.weight",
"attn_output.weight",
"ffn_gate.weight",
"ffn_up.weight",
"ffn_down.weight"
};
for (const auto& suffix : weight_suffixes) {
if (name.find(suffix) != std::string::npos) {
return true;
}
}
return false;
}
template <auto binary_op>
void ggml_cann_binary_op(ggml_backend_cann_context& ctx, ggml_tensor* dst) {
ggml_tensor* src0 = dst->src[0];
ggml_tensor* src1 = dst->src[1];
aclTensor* acl_src0;
aclTensor* acl_src1;
aclTensor* acl_dst;
bcast_shape(src0, src1, dst, &acl_src0, &acl_src1, &acl_dst);
binary_op(ctx, acl_src0, acl_src1, acl_dst);
ggml_cann_release_resources(ctx, acl_src0, acl_src1, acl_dst);
}
template <void unary_op(ggml_backend_cann_context&, aclTensor*, aclTensor*)>
void ggml_cann_op_unary(ggml_backend_cann_context& ctx, ggml_tensor* dst) {
ggml_tensor* src = dst->src[0];
aclTensor* acl_src = ggml_cann_create_tensor(src);
aclTensor* acl_dst = ggml_cann_create_tensor(dst);
unary_op(ctx, acl_src, acl_dst);
ggml_cann_release_resources(ctx, acl_src, acl_dst);
}
void ggml_cann_op_unary(
std::function<void(ggml_backend_cann_context&, aclTensor*, aclTensor*)> unary_op,
ggml_backend_cann_context& ctx, ggml_tensor* dst);
void ggml_cann_op_unary_gated(
std::function<void(ggml_backend_cann_context&, aclTensor*, aclTensor*)> unary_op,
ggml_backend_cann_context& ctx, ggml_tensor* dst);
#define GGML_CANN_CALL_OP_UNARY(OP_NAME) \
do { \
auto lambda = [](ggml_backend_cann_context& ctx, \
aclTensor* acl_src, \
aclTensor* acl_dst) { \
GGML_CANN_CALL_ACLNN_OP(ctx, OP_NAME, acl_src, acl_dst); \
}; \
ggml_cann_op_unary(lambda, ctx, dst); \
} \
while (0)
#define GGML_CANN_CALL_OP_UNARY_GATED(OP_NAME) \
do { \
auto lambda = [](ggml_backend_cann_context& ctx, \
aclTensor* acl_src, \
aclTensor* acl_dst) { \
GGML_CANN_CALL_ACLNN_OP(ctx, OP_NAME, acl_src, acl_dst); \
}; \
ggml_cann_op_unary_gated(lambda, ctx, dst); \
} \
while (0)
#endif