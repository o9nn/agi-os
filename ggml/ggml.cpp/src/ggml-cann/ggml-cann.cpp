#include "ggml-cann.h"
#include <acl/acl.h>
#include <stdarg.h>
#include <aclnnop/aclnn_trans_matmul_weight.h>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <mutex>
#include <queue>
#include <chrono>
#include <unordered_set>
#include <optional>
#include "ggml-impl.h"
#include "ggml-backend-impl.h"
#include "ggml-cann/aclnn_ops.h"
#include "ggml-cann/common.h"
#include "ggml.h"
#define GGML_COMMON_DECL_C
#include "ggml-common.h"
#define GGML_CANN_NAME "CANN"
[[noreturn]] void ggml_cann_error(const char* stmt, const char* func,
const char* file, int line, const char* msg) {
int32_t id = -1;
aclrtGetDevice(&id);
GGML_LOG_ERROR("CANN error: %s\n", msg);
GGML_LOG_ERROR("  current device: %d, in function %s at %s:%d\n", id, func,
file, line);
GGML_LOG_ERROR("  %s\n", stmt);
GGML_ABORT("CANN error");
}
void ggml_cann_set_device(const int32_t device) {
ACL_CHECK(aclrtSetDevice(device));
}
int32_t ggml_cann_get_device() {
int32_t id;
ACL_CHECK(aclrtGetDevice(&id));
return id;
}
std::optional<std::string> get_env(const std::string& name) {
const char* val = std::getenv(name.c_str());
if (!val) return std::nullopt;
std::string res = std::string(val);
std::transform(res.begin(), res.end(), res.begin(), ::tolower);
return res;
}
bool parse_bool(const std::string& value) {
std::unordered_set<std::string> valid_values = {"on", "1", "yes", "y", "enable", "true"};
return valid_values.find(value) != valid_values.end();
}
static ggml_cann_device_info ggml_cann_init() {
ggml_cann_device_info info = {};
aclError err = aclrtGetDeviceCount((uint32_t*)&info.device_count);
if (err != ACL_SUCCESS) {
GGML_LOG_ERROR("%s: failed to initialize CANN: %s\n",
__func__, aclGetRecentErrMsg());
return info;
}
GGML_ASSERT(info.device_count <= GGML_CANN_MAX_DEVICES);
for (int id = 0; id < info.device_count; ++id) {
aclrtPhysicalMemProp prop = {};
prop.handleType = ACL_MEM_HANDLE_TYPE_NONE;
prop.allocationType = ACL_MEM_ALLOCATION_TYPE_PINNED;
prop.memAttr = ACL_HBM_MEM_HUGE;
prop.location.type = ACL_MEM_LOCATION_TYPE_DEVICE;
prop.location.id = id;
prop.reserve = 0;
err = aclrtMemGetAllocationGranularity(
&prop, ACL_RT_MEM_ALLOC_GRANULARITY_RECOMMENDED,
&info.devices[id].vmm_granularity);
info.devices[id].vmm = err == ACL_SUCCESS;
size_t free, total;
ggml_backend_cann_get_device_memory(id, &free, &total);
info.devices[id].total_vram = free;
}
return info;
}
const ggml_cann_device_info& ggml_cann_info() {
static ggml_cann_device_info info = ggml_cann_init();
return info;
}
struct ggml_cann_pool_buf_prio : public ggml_cann_pool {
static const size_t max_reuse_margin = 1ull << 22;
static const size_t min_free_margin = 1ull << 20;
static const size_t alignment = 128;
int device;
bool disable_clean = false;
struct ggml_cann_buffer {
void* ptr = nullptr;
size_t size = 0;
std::chrono::steady_clock::time_point last_used;
bool operator>(const ggml_cann_buffer& other) const {
return size > other.size;
}
};
std::unordered_map<void*, size_t> buffer_pool;
std::priority_queue<ggml_cann_buffer,
std::vector<ggml_cann_buffer>,
std::greater<>> free_buffers ;
size_t pool_size = 0;
explicit ggml_cann_pool_buf_prio(int device) : device(device) {
disable_clean = parse_bool(get_env("GGML_CANN_DISABLE_BUF_POOL_CLEAN").value_or(""));
}
~ggml_cann_pool_buf_prio() {
ggml_cann_set_device(device);
for (auto& [b_ptr, b_size] : buffer_pool) {
aclrtFree(b_ptr);
pool_size -= b_size;
}
buffer_pool.clear();
GGML_ASSERT(pool_size == 0);
}
void* alloc(size_t size, size_t* actual_size) override {
size = GGML_PAD(size, alignment);
if (size == 0) {
size = alignment;
}
void* ptr = nullptr;
auto now = std::chrono::steady_clock::now();
std::vector<ggml_cann_buffer> free_buffers_rest;
free_buffers_rest.reserve(free_buffers.size());
while (!free_buffers.empty()) {
auto b = free_buffers.top();
free_buffers.pop();
if (b.size >= size) {
const size_t margin = b.size - size;
if (margin <= max_reuse_margin) {
*actual_size = b.size;
ptr = b.ptr;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: reused   %p, "
"pool_size = %5u MB, "
"size = %5u MB, "
"margin = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(margin, 1048576) / 1048576));
#endif
break;
}
}
bool should_clean = !disable_clean &&
b.size > min_free_margin &&
std::chrono::duration_cast<std::chrono::milliseconds>(now - b.last_used).count() > 100;
if (should_clean) {
ACL_CHECK(aclrtFree(b.ptr));
pool_size -= b.size;
buffer_pool.erase(b.ptr);
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: clean    %p, "
"pool_size = %5u MB, "
"size = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(b.size, 1048576) / 1048576));
#endif
continue;
}
free_buffers_rest.push_back(b);
}
for (ggml_cann_buffer &b : free_buffers_rest) {
free_buffers.push(std::move(b));
}
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO("cann pool[%d] free pool_size = %5u MB\n\n", device, (uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576));
#endif
if (ptr != nullptr) {
return ptr;
}
ggml_cann_set_device(device);
ACL_CHECK(aclrtMalloc(&ptr, size, ACL_MEM_MALLOC_HUGE_FIRST));
*actual_size = size;
pool_size += size;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: allocate %p, "
"pool_size = %5u MB, "
"size = %5u MB\n",
device, ptr, (uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(size, 1048576) / 1048576));
#endif
buffer_pool.emplace(ptr, size);
return ptr;
}
void free(void* ptr, size_t size) override {
GGML_UNUSED(size);
auto it = buffer_pool.find(ptr);
if (it == buffer_pool.end()) {
GGML_ABORT("cann pool[%d]: buffer %p not found in pool\n", device, ptr);
}
auto now = std::chrono::steady_clock::now();
free_buffers.emplace(ggml_cann_buffer{ptr, it->second, now});
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: return   %p, "
"pool_size = %5u MB\n",
device, ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576));
#endif
}
};
struct ggml_cann_pool_buf : public ggml_cann_pool {
static const size_t max_reuse_margin = 1ull << 22;
static const size_t min_free_margin = 1ull << 20;
static const size_t alignment = 128;
static const int MAX_BUFFERS = 256;
int device;
bool disable_clean = false;
struct ggml_cann_buffer {
void* ptr = nullptr;
size_t size = 0;
bool used = false;
std::chrono::steady_clock::time_point last_used;
};
ggml_cann_buffer buffer_pool[MAX_BUFFERS] = {};
size_t pool_size = 0;
explicit ggml_cann_pool_buf(int device) : device(device) {
disable_clean = parse_bool(get_env("GGML_CANN_DISABLE_BUF_POOL_CLEAN").value_or(""));
}
~ggml_cann_pool_buf() {
ggml_cann_set_device(device);
for (int i = 0; i < MAX_BUFFERS; ++i) {
ggml_cann_buffer& b = buffer_pool[i];
if (b.ptr != nullptr) {
aclrtFree(b.ptr);
pool_size -= b.size;
}
}
GGML_ASSERT(pool_size == 0);
}
void* alloc(size_t size, size_t* actual_size) override {
size = GGML_PAD(size, alignment);
if (size == 0) {
size = alignment;
}
void* ptr = nullptr;
auto now = std::chrono::steady_clock::now();
int i = 0;
for (; i < MAX_BUFFERS; ++i) {
ggml_cann_buffer& b = buffer_pool[i];
if (b.ptr == nullptr) {
break;
}
if (b.used) {
continue;
}
if (b.size >= size) {
const size_t margin = b.size - size;
if (margin <= max_reuse_margin) {
*actual_size = b.size;
b.used = true;
ptr = b.ptr;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: reused   %p, "
"pool_size = %5u MB, "
"size = %5u MB, "
"margin = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(margin, 1048576) / 1048576));
#endif
break;
}
}
bool should_clean = !disable_clean &&
b.size > min_free_margin &&
std::chrono::duration_cast<std::chrono::milliseconds>(now - b.last_used).count() > 100;
if (should_clean) {
ACL_CHECK(aclrtFree(b.ptr));
pool_size -= b.size;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: clean    %p, "
"pool_size = %5u MB, "
"size = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(b.size, 1048576) / 1048576));
#endif
b.ptr = nullptr;
}
}
if (ptr != nullptr) {
return ptr;
}
if (i < MAX_BUFFERS) {
ggml_cann_buffer& b = buffer_pool[i];
ggml_cann_set_device(device);
ACL_CHECK(aclrtMalloc(&b.ptr, size, ACL_MEM_MALLOC_HUGE_FIRST));
pool_size += size;
*actual_size = size;
b.size = size;
b.used = true;
if (i >= MAX_BUFFERS - 8) {
GGML_LOG_WARN("cann pool[%d]: slots almost full\n", device);
}
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: allocate %p, "
"pool_size = %5u MB, "
"size = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576),
(uint32_t)(GGML_PAD(b.size, 1048576) / 1048576));
#endif
return b.ptr;
}
GGML_ABORT("cann pool[%d]: slots full\n", device);
}
void free(void* ptr, size_t size) override {
GGML_UNUSED(size);
for (int i = 0; i < MAX_BUFFERS; ++i) {
ggml_cann_buffer& b = buffer_pool[i];
if (b.ptr != ptr) {
continue;
}
b.used = false;
b.last_used = std::chrono::steady_clock::now();
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO(
"cann pool[%d]: return   %p, "
"pool_size = %5u MB\n",
device, b.ptr,
(uint32_t)(GGML_PAD(pool_size, 1048576) / 1048576));
#endif
return;
}
GGML_ABORT("cann pool[%d]: slots full\n", device);
}
};
struct ggml_cann_pool_vmm : public ggml_cann_pool {
size_t max_size;
int device;
void* pool_addr = 0;
size_t pool_used = 0;
size_t pool_size = 0;
size_t granularity;
std::vector<aclrtDrvMemHandle> handles;
std::vector<void*> map_offsets;
explicit ggml_cann_pool_vmm(int device)
: device(device) {
auto dev = ggml_cann_info().devices[device];
granularity = dev.vmm_granularity;
max_size = dev.total_vram;
}
~ggml_cann_pool_vmm() {
if (pool_addr != 0) {
for (auto& offset : map_offsets) {
ACL_CHECK(aclrtUnmapMem(offset));
}
for (auto& handle : handles) {
ACL_CHECK(aclrtFreePhysical(handle));
}
ACL_CHECK(aclrtReleaseMemAddress(pool_addr));
}
}
void* alloc(size_t size, size_t* actual_size) override {
const size_t alignment = 128;
size = GGML_PAD(size, alignment);
if (size == 0) {
size = alignment;
}
size_t avail = pool_size - pool_used;
if (size > avail) {
size_t reserve_size = size - avail;
reserve_size = GGML_PAD(reserve_size, granularity);
GGML_ASSERT(pool_size + reserve_size <= max_size);
aclrtPhysicalMemProp prop = {};
prop.handleType = ACL_MEM_HANDLE_TYPE_NONE;
prop.allocationType = ACL_MEM_ALLOCATION_TYPE_PINNED;
prop.memAttr = ACL_HBM_MEM_HUGE;
prop.location.type = ACL_MEM_LOCATION_TYPE_DEVICE;
prop.location.id = device;
prop.reserve = 0;
aclrtDrvMemHandle handle;
ACL_CHECK(aclrtMallocPhysical(&handle, reserve_size, &prop, 0));
if (pool_addr == 0) {
ACL_CHECK(aclrtReserveMemAddress(
&pool_addr, max_size, 0, NULL, 1));
}
ACL_CHECK(aclrtMapMem((char*)pool_addr + pool_size, reserve_size, 0,
handle, 0));
handles.push_back(handle);
map_offsets.push_back((char*)pool_addr + pool_size);
pool_size += reserve_size;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO("cann pool[%d]: size increased to %llu MB (reserved %llu MB)\n",
device, (unsigned long long) (pool_size/1024/1024),
(unsigned long long) (reserve_size/1024/1024));
#endif
}
GGML_ASSERT(pool_addr != 0);
void* ptr = (void*)((char*)pool_addr + pool_used);
*actual_size = size;
pool_used += size;
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO("cann pool[%d]: allocated %llu bytes at %llx\n", device,
(unsigned long long)size, (unsigned long long)ptr);
#endif
return ptr;
}
void free(void* ptr, size_t size) override {
#ifdef DEBUG_CANN_MALLOC
GGML_LOG_INFO("cann pool[%d]: freed %llu bytes at %llx\n", device,
(unsigned long long)size, (unsigned long long)ptr);
#endif
pool_used -= size;
GGML_ASSERT(ptr == (void*)((char*)pool_addr + pool_used));
}
};
std::unique_ptr<ggml_cann_pool> ggml_backend_cann_context::new_pool_for_device(
int device) {
std::string mem_pool_type = get_env("GGML_CANN_MEM_POOL").value_or("");
if (mem_pool_type == "prio") {
GGML_LOG_INFO("%s: device %d use buffer pool with priority queue\n", __func__, device);
return std::unique_ptr<ggml_cann_pool>(new ggml_cann_pool_buf_prio(device));
}
if (ggml_cann_info().devices[device].vmm && mem_pool_type != "leg") {
GGML_LOG_INFO("%s: device %d use vmm pool\n", __func__, device);
return std::unique_ptr<ggml_cann_pool>(new ggml_cann_pool_vmm(device));
}
GGML_LOG_INFO("%s: device %d use buffer pool\n", __func__, device);
return std::unique_ptr<ggml_cann_pool>(new ggml_cann_pool_buf(device));
}
struct ggml_backend_cann_buffer_context {
int32_t device;
void* dev_ptr =
nullptr;
ggml_backend_cann_buffer_context(int32_t device, void* dev_ptr)
: device(device),
dev_ptr(dev_ptr) {}
~ggml_backend_cann_buffer_context() { ACL_CHECK(aclrtFree(dev_ptr)); }
};
static bool ggml_backend_buft_is_cann(ggml_backend_buffer_type_t buft);
static bool ggml_backend_buffer_is_cann(
ggml_backend_buffer_t buffer) {
return ggml_backend_buft_is_cann(buffer->buft);
}
static void ggml_backend_cann_buffer_free_buffer(
ggml_backend_buffer_t buffer) {
ggml_backend_cann_buffer_context* ctx =
(ggml_backend_cann_buffer_context*)buffer->context;
delete ctx;
}
static void* ggml_backend_cann_buffer_get_base(
ggml_backend_buffer_t buffer) {
ggml_backend_cann_buffer_context* ctx =
(ggml_backend_cann_buffer_context*)buffer->context;
return ctx->dev_ptr;
}
static void ggml_backend_cann_transform_q4_0(ggml_tensor* tensor,
const void* src,
void* dst) {
int64_t n_elems = ggml_nelements(tensor);
int64_t groups = n_elems / QK4_0;
size_t quant_bytes = n_elems * sizeof(uint8_t) / 2;
uint8_t* quant_offset = (uint8_t*)dst;
uint16_t* scale_offset = (uint16_t*)((char*)dst + quant_bytes);
for (int i = 0; i < groups; i++) {
const block_q4_0* group =
(const block_q4_0*)((const char*)src + i * sizeof(block_q4_0));
*scale_offset = group->d;
scale_offset++;
for (int j = 0; j < QK4_0 / 2; j += 2) {
(*quant_offset) = (group->qs[j] & 0x0F);
(*quant_offset) |= ((group->qs[j + 1] << 4));
quant_offset++;
}
for (int j = 0; j < QK4_0 / 2; j += 2) {
(*quant_offset) = (group->qs[j] >> 4);
(*quant_offset) |= (group->qs[j + 1] & 0xF0);
quant_offset++;
}
}
for (quant_offset = (uint8_t*)dst;
quant_offset < (uint8_t*)dst + quant_bytes; quant_offset++) {
(*quant_offset) ^= 0x88;
}
}
static void ggml_backend_cann_transform_back_q4_0(
const ggml_tensor* tensor, void* src, void* dst) {
int64_t n_elems = ggml_nelements(tensor);
int64_t groups = n_elems / QK4_0;
size_t quant_bytes = n_elems * sizeof(uint8_t) / 2;
uint8_t* quant_offset = (uint8_t*)src;
uint16_t* scale_offset = (uint16_t*)((char*)src + quant_bytes);
for (; quant_offset < (uint8_t*)src + quant_bytes; quant_offset++) {
(*quant_offset) ^= 0x88;
}
quant_offset = (uint8_t*)src;
for (int i = 0; i < groups; i++) {
block_q4_0* group = (block_q4_0*)((char*)dst + i * sizeof(block_q4_0));
group->d = *scale_offset;
scale_offset++;
for (int j = 0; j < QK4_0 / 2; j += 2) {
group->qs[j] = ((*quant_offset) & 0x0F);
group->qs[j + 1] = ((*quant_offset) >> 4);
quant_offset++;
}
for (int j = 0; j < QK4_0 / 2; j += 2) {
group->qs[j] |= ((*quant_offset) << 4);
group->qs[j + 1] |= ((*quant_offset) & 0xF0);
quant_offset++;
}
}
}
static void ggml_backend_cann_transform_q8_0(ggml_tensor* tensor,
const void* src,
void* dst) {
int64_t n_elems = ggml_nelements(tensor);
int64_t groups = n_elems / QK8_0;
size_t quant_bytes = n_elems * sizeof(uint8_t);
uint8_t* quant_offset = (uint8_t*)dst;
uint16_t* scale_offset = (uint16_t*)((char*)dst + quant_bytes);
for (int i = 0; i < groups; i++) {
const block_q8_0* group =
(const block_q8_0*)((const char*)src + i * sizeof(block_q8_0));
*scale_offset = group->d;
scale_offset++;
size_t group_quant_size = QK8_0 * sizeof(uint8_t);
memcpy(quant_offset, group->qs, group_quant_size);
quant_offset += group_quant_size;
}
}
static void ggml_backend_cann_transform_back_q8_0(
const ggml_tensor* tensor, const void* src, void* dst) {
int64_t n_elems = ggml_nelements(tensor);
int64_t groups = n_elems / QK8_0;
size_t quant_bytes = n_elems * sizeof(uint8_t);
const uint8_t* quant_offset = (const uint8_t*)src;
const uint16_t* scale_offset =
(const uint16_t*)((const char*)src + quant_bytes);
for (int i = 0; i < groups; i++) {
block_q8_0* group = (block_q8_0*)((char*)dst + i * sizeof(block_q8_0));
group->d = *scale_offset;
scale_offset++;
size_t group_quant_size = QK8_0 * sizeof(uint8_t);
memcpy(group->qs, quant_offset, group_quant_size);
quant_offset += group_quant_size;
}
}
static void ggml_backend_cann_transform(ggml_tensor* tensor,
const void* src, void* dst) {
switch (tensor->type) {
case GGML_TYPE_Q4_0:
ggml_backend_cann_transform_q4_0(tensor, src, dst);
break;
case GGML_TYPE_Q8_0:
ggml_backend_cann_transform_q8_0(tensor, src, dst);
break;
default:
break;
}
}
static void ggml_backend_cann_transform_back(
const ggml_tensor* tensor, void* src, void* dst) {
switch (tensor->type) {
case GGML_TYPE_Q4_0:
ggml_backend_cann_transform_back_q4_0(tensor, src, dst);
break;
case GGML_TYPE_Q8_0:
ggml_backend_cann_transform_back_q8_0(tensor, src, dst);
break;
default:
break;
}
}
static bool need_transform(ggml_type type) {
switch (type) {
case GGML_TYPE_Q4_0:
case GGML_TYPE_Q8_0:
return true;
default:
return false;
}
}
static enum ggml_status ggml_backend_cann_buffer_init_tensor(
ggml_backend_buffer_t buffer, ggml_tensor* tensor) {
if (tensor->view_src != NULL && tensor->view_offs == 0) {
GGML_ASSERT(tensor->view_src->buffer->buft == buffer->buft);
return GGML_STATUS_SUCCESS;
}
if (ggml_is_quantized(tensor->type)) {
size_t original_size = ggml_nbytes(tensor);
size_t padded_size =
ggml_backend_buft_get_alloc_size(buffer->buft, tensor);
if (padded_size > original_size && tensor->view_src == nullptr) {
size_t memset_size = padded_size - original_size;
ACL_CHECK(aclrtMemset((char*)tensor->data + original_size,
memset_size, 0, memset_size));
}
}
return GGML_STATUS_SUCCESS;
}
namespace {
void* g_nz_workspace = nullptr;
size_t g_nz_workspace_allocated = 0;
void release_nz_workspace() {
if (g_nz_workspace) {
aclrtFree(g_nz_workspace);
g_nz_workspace = nullptr;
g_nz_workspace_allocated = 0;
}
}
void relloc_nz_workspace(size_t new_size) {
if (new_size > g_nz_workspace_allocated) {
if (g_nz_workspace) {
aclrtFree(g_nz_workspace);
g_nz_workspace = nullptr;
}
ACL_CHECK(aclrtMalloc(&g_nz_workspace, new_size, ACL_MEM_MALLOC_HUGE_FIRST));
g_nz_workspace_allocated = new_size;
}
}
}
static void weight_format_to_nz(ggml_tensor *tensor, const void *data, size_t offset) {
aclTensor* weightTransposed = ggml_cann_create_tensor(tensor, tensor->ne,
tensor->nb, 2, ACL_FORMAT_ND, offset);
uint64_t workspaceSize = 0;
aclOpExecutor *executor;
ACL_CHECK(aclnnTransMatmulWeightGetWorkspaceSize(weightTransposed,
&workspaceSize, &executor));
relloc_nz_workspace(workspaceSize);
ACL_CHECK(aclnnTransMatmulWeight(g_nz_workspace, workspaceSize, executor, nullptr));
ACL_CHECK(aclDestroyTensor(weightTransposed));
}
static void ggml_backend_cann_buffer_set_tensor(
ggml_backend_buffer_t buffer, ggml_tensor *tensor, const void *data,
size_t offset, size_t size) {
ggml_backend_cann_buffer_context *ctx =
(ggml_backend_cann_buffer_context *)buffer->context;
ggml_cann_set_device(ctx->device);
static bool weight_to_nz = parse_bool(get_env("GGML_CANN_WEIGHT_NZ").value_or(""));
if (!need_transform(tensor->type)) {
ACL_CHECK(aclrtMemcpy((char *)tensor->data + offset, size, data, size,
ACL_MEMCPY_HOST_TO_DEVICE));
if (weight_to_nz && is_matmul_weight((const ggml_tensor*)tensor)) {
GGML_ASSERT(tensor->ne[2] == 1);
GGML_ASSERT(tensor->ne[3] == 1);
weight_format_to_nz(tensor, data, offset);
}
} else {
void *transform_buffer = malloc(size);
ggml_backend_cann_transform(tensor, data, transform_buffer);
ACL_CHECK(aclrtMemcpy((char *)tensor->data + offset, size,
transform_buffer, size,
ACL_MEMCPY_HOST_TO_DEVICE));
free(transform_buffer);
}
}
static void ggml_backend_cann_buffer_get_tensor(
ggml_backend_buffer_t buffer, const ggml_tensor* tensor, void* data,
size_t offset, size_t size) {
ggml_backend_cann_buffer_context* ctx =
(ggml_backend_cann_buffer_context*)buffer->context;
ggml_cann_set_device(ctx->device);
if (!need_transform(tensor->type)) {
ACL_CHECK(aclrtMemcpy(data, size, (char*)tensor->data + offset, size,
ACL_MEMCPY_DEVICE_TO_HOST));
} else {
void* transform_buffer = malloc(size);
ACL_CHECK(aclrtMemcpy(transform_buffer, size,
(char*)tensor->data + offset, size,
ACL_MEMCPY_DEVICE_TO_HOST));
ggml_backend_cann_transform_back(tensor, transform_buffer, data);
free(transform_buffer);
}
}
static bool ggml_backend_cann_buffer_cpy_tensor(
ggml_backend_buffer_t buffer, const ggml_tensor* src, ggml_tensor* dst) {
if (ggml_backend_buffer_is_cann(src->buffer)) {
ggml_backend_cann_buffer_context* src_ctx =
(ggml_backend_cann_buffer_context*)src->buffer->context;
ggml_backend_cann_buffer_context* dst_ctx =
(ggml_backend_cann_buffer_context*)buffer->context;
size_t memcpy_size = ggml_nbytes(src);
if (src_ctx->device == dst_ctx->device) {
ACL_CHECK(aclrtMemcpy((char*)dst->data, memcpy_size,
(const char*)src->data, memcpy_size,
ACL_MEMCPY_DEVICE_TO_DEVICE));
return true;
} else {
int32_t canAccessPeer = 0;
ACL_CHECK(aclrtDeviceCanAccessPeer(&canAccessPeer, src_ctx->device,
dst_ctx->device));
if (canAccessPeer) {
ggml_cann_set_device(src_ctx->device);
ACL_CHECK(aclrtDeviceEnablePeerAccess(dst_ctx->device, 0));
ACL_CHECK(aclrtMemcpy((char*)dst->data, memcpy_size,
(const char*)src->data, memcpy_size,
ACL_MEMCPY_DEVICE_TO_DEVICE));
return true;
}
}
}
return false;
}
static void ggml_backend_cann_buffer_clear(
ggml_backend_buffer_t buffer, uint8_t value) {
ggml_backend_cann_buffer_context* ctx =
(ggml_backend_cann_buffer_context*)buffer->context;
ggml_cann_set_device(ctx->device);
ACL_CHECK(aclrtMemset(ctx->dev_ptr, buffer->size, value, buffer->size));
}
static const ggml_backend_buffer_i ggml_backend_cann_buffer_interface = {
ggml_backend_cann_buffer_free_buffer,
ggml_backend_cann_buffer_get_base,
ggml_backend_cann_buffer_init_tensor,
NULL,
ggml_backend_cann_buffer_set_tensor,
ggml_backend_cann_buffer_get_tensor,
ggml_backend_cann_buffer_cpy_tensor,
ggml_backend_cann_buffer_clear,
NULL,
};
struct ggml_backend_cann_buffer_type_context {
int32_t
device;
std::string name;
};
static const char* ggml_backend_cann_buffer_type_name(
ggml_backend_buffer_type_t buft) {
ggml_backend_cann_buffer_type_context* buft_ctx =
(ggml_backend_cann_buffer_type_context*)buft->context;
return buft_ctx->name.c_str();
}
static ggml_backend_buffer_t
ggml_backend_cann_buffer_type_alloc_buffer(ggml_backend_buffer_type_t buft,
size_t size) {
ggml_backend_cann_buffer_type_context* buft_ctx =
(ggml_backend_cann_buffer_type_context*)buft->context;
ggml_cann_set_device(buft_ctx->device);
const size_t alignment = 128;
size = GGML_PAD(size, alignment);
if (size == 0) {
size = alignment;
}
void* dev_ptr;
aclError err = aclrtMalloc(&dev_ptr, size, ACL_MEM_MALLOC_HUGE_FIRST);
if (err != ACL_SUCCESS) {
GGML_LOG_ERROR(
"%s: allocating %.2f MiB on device %d: aclrtMalloc failed: %s\n",
__func__, size / 1024.0 / 1024.0, buft_ctx->device,
aclGetRecentErrMsg());
return nullptr;
}
ggml_backend_cann_buffer_context* ctx =
new ggml_backend_cann_buffer_context(buft_ctx->device, dev_ptr);
return ggml_backend_buffer_init(buft, ggml_backend_cann_buffer_interface,
ctx, size);
}
static size_t ggml_backend_cann_buffer_type_get_alignment(
ggml_backend_buffer_type_t buft) {
return 128;
GGML_UNUSED(buft);
}
static size_t ggml_backend_cann_buffer_type_get_alloc_size(
ggml_backend_buffer_type_t buft, const ggml_tensor* tensor) {
size_t size = ggml_nbytes(tensor);
int64_t ne0 = tensor->ne[0];
static bool weight_to_nz = parse_bool(get_env("GGML_CANN_WEIGHT_NZ").value_or(""));
if (ggml_is_quantized(tensor->type)) {
if (ne0 % MATRIX_ROW_PADDING != 0) {
size += ggml_row_size(
tensor->type, MATRIX_ROW_PADDING - ne0 % MATRIX_ROW_PADDING);
}
} else if (weight_to_nz && is_matmul_weight((const ggml_tensor*)tensor)) {
int64_t shape[] = {tensor->ne[1], tensor->ne[0]};
GGML_ASSERT(tensor->ne[2] == 1);
GGML_ASSERT(tensor->ne[3] == 1);
const aclIntArray *acl_shape = aclCreateIntArray(shape, 2);
size_t new_size;
ACL_CHECK(aclnnCalculateMatmulWeightSizeV2(acl_shape,
ggml_cann_type_mapping(tensor->type), &new_size));
ACL_CHECK(aclDestroyIntArray(acl_shape));
size = std::max(size, new_size);
}
return size;
GGML_UNUSED(buft);
}
static bool ggml_backend_cann_buffer_type_is_host(ggml_backend_buffer_type_t buft) {
return false;
GGML_UNUSED(buft);
}
static const ggml_backend_buffer_type_i ggml_backend_cann_buffer_type_interface = {
ggml_backend_cann_buffer_type_name,
ggml_backend_cann_buffer_type_alloc_buffer,
ggml_backend_cann_buffer_type_get_alignment,
NULL,
ggml_backend_cann_buffer_type_get_alloc_size,
ggml_backend_cann_buffer_type_is_host,
};
ggml_backend_buffer_type_t
ggml_backend_cann_buffer_type(int32_t device) {
static std::mutex mutex;
std::lock_guard<std::mutex> lock(mutex);
if (device >= ggml_backend_cann_get_device_count()) {
return nullptr;
}
static ggml_backend_buffer_type
ggml_backend_cann_buffer_types[GGML_CANN_MAX_DEVICES];
static bool ggml_backend_cann_buffer_type_initialized = false;
if (!ggml_backend_cann_buffer_type_initialized) {
for (int32_t i = 0; i < ggml_cann_info().device_count; i++) {
ggml_backend_cann_buffer_types[i] = {
ggml_backend_cann_buffer_type_interface,
ggml_backend_reg_dev_get(ggml_backend_cann_reg(), i),
new ggml_backend_cann_buffer_type_context{
i, "CANN" + std::to_string(i)},
};
}
ggml_backend_cann_buffer_type_initialized = true;
}
return &ggml_backend_cann_buffer_types[device];
}
static const char * ggml_backend_cann_host_buffer_type_name(ggml_backend_buffer_type_t buft) {
return "CANN_Host";
GGML_UNUSED(buft);
}
static const char * ggml_backend_cann_host_buffer_name(ggml_backend_buffer_t buffer) {
return "CANN_Host";
GGML_UNUSED(buffer);
}
static void ggml_backend_cann_host_buffer_free(ggml_backend_buffer_t buffer) {
ACL_CHECK(aclrtFreeHost(buffer->context));
}
static void * ggml_cann_host_malloc(size_t size) {
if (getenv("GGML_CANN_NO_PINNED") != nullptr) {
return nullptr;
}
const size_t alignment = 128;
size = GGML_PAD(size, alignment);
if (size == 0) {
size = alignment;
}
void * hostPtr = nullptr;
aclError err = aclrtMallocHost((void **) &hostPtr, size);
if (err != ACL_SUCCESS) {
GGML_LOG_WARN("%s: failed to allocate %.2f MiB of pinned memory: %s\n", __func__,
size / 1024.0 / 1024.0, aclGetRecentErrMsg());
return nullptr;
}
return hostPtr;
}
static ggml_backend_buffer_t ggml_backend_cann_host_buffer_type_alloc_buffer(ggml_backend_buffer_type_t buft, size_t size) {
void * hostPtr = ggml_cann_host_malloc(size);
if (hostPtr == nullptr) {
return ggml_backend_buft_alloc_buffer(ggml_backend_cpu_buffer_type(), size);
}
ggml_backend_buffer_t buffer = ggml_backend_cpu_buffer_from_ptr(hostPtr, size);
buffer->buft = buft;
buffer->iface.free_buffer = ggml_backend_cann_host_buffer_free;
return buffer;
}
ggml_backend_buffer_type_t ggml_backend_cann_host_buffer_type() {
static struct ggml_backend_buffer_type ggml_backend_cann_buffer_type_host = {
{
ggml_backend_cann_host_buffer_type_name,
ggml_backend_cann_host_buffer_type_alloc_buffer,
ggml_backend_cpu_buffer_type()->iface.get_alignment,
NULL,
ggml_backend_cpu_buffer_type()->iface.get_alloc_size,
ggml_backend_cpu_buffer_type()->iface.is_host,
},
ggml_backend_reg_dev_get(ggml_backend_cann_reg(), 0),
nullptr,
};
return &ggml_backend_cann_buffer_type_host;
}
static bool ggml_cann_compute_forward(ggml_backend_cann_context& ctx,
struct ggml_tensor* dst) {
switch (dst->op) {
case GGML_OP_REPEAT:
ggml_cann_repeat(ctx, dst);
break;
case GGML_OP_GET_ROWS:
ggml_cann_get_rows(ctx, dst);
break;
case GGML_OP_SET_ROWS:
ggml_cann_set_rows(ctx, dst);
break;
case GGML_OP_DUP:
ggml_cann_dup(ctx, dst);
break;
case GGML_OP_ADD:
case GGML_OP_ADD1:
ggml_cann_binary_op<aclnn_add>(ctx, dst);
break;
case GGML_OP_SUB:
ggml_cann_binary_op<aclnn_sub>(ctx, dst);
break;
case GGML_OP_ACC:
ggml_cann_acc(ctx, dst);
break;
case GGML_OP_MUL:
ggml_cann_binary_op<aclnn_mul>(ctx, dst);
break;
case GGML_OP_DIV:
ggml_cann_binary_op<aclnn_div>(ctx, dst);
break;
case GGML_OP_UNARY:
switch (ggml_get_unary_op(dst)) {
case GGML_UNARY_OP_ABS:
GGML_CANN_CALL_OP_UNARY(Abs);
break;
case GGML_UNARY_OP_NEG:
GGML_CANN_CALL_OP_UNARY(Neg);
break;
case GGML_UNARY_OP_GELU:
case GGML_UNARY_OP_GELU_ERF:
GGML_CANN_CALL_OP_UNARY(Gelu);
break;
case GGML_UNARY_OP_SILU:
GGML_CANN_CALL_OP_UNARY(Silu);
break;
case GGML_UNARY_OP_GELU_QUICK: {
auto lambda = [](ggml_backend_cann_context& ctx,
aclTensor* acl_src,
aclTensor* acl_dst) {
GGML_CANN_CALL_ACLNN_OP(ctx, GeluV2, acl_src, 0, acl_dst);
};
ggml_cann_op_unary(lambda, ctx, dst);
} break;
case GGML_UNARY_OP_TANH:
GGML_CANN_CALL_OP_UNARY(Tanh);
break;
case GGML_UNARY_OP_RELU:
GGML_CANN_CALL_OP_UNARY(Relu);
break;
case GGML_UNARY_OP_SIGMOID:
GGML_CANN_CALL_OP_UNARY(Sigmoid);
break;
case GGML_UNARY_OP_HARDSIGMOID:
GGML_CANN_CALL_OP_UNARY(Hardsigmoid);
break;
case GGML_UNARY_OP_HARDSWISH:
GGML_CANN_CALL_OP_UNARY(Hardswish);
break;
case GGML_UNARY_OP_EXP:
GGML_CANN_CALL_OP_UNARY(Exp);
break;
case GGML_UNARY_OP_ELU:
ggml_cann_elu(ctx, dst);
break;
case GGML_UNARY_OP_SGN:
GGML_CANN_CALL_OP_UNARY(Sign);
break;
case GGML_UNARY_OP_STEP:
ggml_cann_step(ctx, dst);
break;
default:
return false;
}
break;
case GGML_OP_GLU:
switch (ggml_get_glu_op(dst)) {
case GGML_GLU_OP_REGLU:
GGML_CANN_CALL_OP_UNARY_GATED(Relu);
break;
case GGML_GLU_OP_GEGLU:
case GGML_GLU_OP_GEGLU_ERF:
GGML_CANN_CALL_OP_UNARY_GATED(Gelu);
break;
case GGML_GLU_OP_SWIGLU:
GGML_CANN_CALL_OP_UNARY_GATED(Silu);
break;
case GGML_GLU_OP_GEGLU_QUICK: {
auto lambda = [](ggml_backend_cann_context& ctx,
aclTensor* acl_src,
aclTensor* acl_dst) {
GGML_CANN_CALL_ACLNN_OP(ctx, GeluV2, acl_src, 0, acl_dst);
};
ggml_cann_op_unary_gated(lambda, ctx, dst);
} break;
default:
return false;
}
break;
case GGML_OP_NORM:
ggml_cann_norm(ctx, dst);
break;
case GGML_OP_GROUP_NORM:
ggml_cann_group_norm(ctx, dst);
break;
case GGML_OP_CONCAT:
ggml_cann_concat(ctx, dst);
break;
case GGML_OP_UPSCALE:
ggml_cann_upsample_nearest2d(ctx, dst);
break;
case GGML_OP_PAD:
ggml_cann_pad(ctx, dst);
break;
case GGML_OP_ARANGE:
ggml_cann_arange(ctx, dst);
break;
case GGML_OP_TIMESTEP_EMBEDDING:
ggml_cann_timestep_embedding(ctx, dst);
break;
case GGML_OP_LEAKY_RELU:
ggml_cann_leaky_relu(ctx, dst);
break;
case GGML_OP_RMS_NORM:
ggml_cann_rms_norm(ctx, dst);
break;
case GGML_OP_MUL_MAT:
ggml_cann_mul_mat(ctx, dst);
break;
case GGML_OP_MUL_MAT_ID:
ggml_cann_mul_mat_id(ctx, dst);
break;
case GGML_OP_SCALE:
ggml_cann_scale(ctx, dst);
break;
case GGML_OP_SQR:
GGML_ASSERT(dst->src[1] == nullptr);
dst->src[1] = dst->src[0];
ggml_cann_binary_op<aclnn_mul>(ctx, dst);
break;
case GGML_OP_SQRT:
GGML_CANN_CALL_OP_UNARY(Sqrt);
break;
case GGML_OP_CLAMP:
ggml_cann_clamp(ctx, dst);
break;
case GGML_OP_CPY:
ggml_cann_cpy(ctx, dst);
break;
case GGML_OP_CONT:
ggml_cann_dup(ctx, dst);
break;
case GGML_OP_NONE:
case GGML_OP_RESHAPE:
case GGML_OP_VIEW:
case GGML_OP_PERMUTE:
case GGML_OP_TRANSPOSE:
break;
case GGML_OP_DIAG_MASK_INF:
ggml_cann_diag_mask(ctx, dst, -INFINITY);
break;
case GGML_OP_SOFT_MAX:
ggml_cann_softmax(ctx, dst);
break;
case GGML_OP_ROPE:
ggml_cann_rope(ctx, dst);
break;
case GGML_OP_IM2COL:
ggml_cann_im2col(ctx, dst);
break;
case GGML_OP_POOL_2D:
ggml_cann_pool2d(ctx, dst);
break;
case GGML_OP_SUM:
ggml_cann_sum(ctx, dst);
break;
case GGML_OP_SUM_ROWS:
ggml_cann_sum_rows(ctx, dst);
break;
case GGML_OP_ARGSORT:
ggml_cann_argsort(ctx, dst);
break;
case GGML_OP_ARGMAX:
ggml_cann_argmax(ctx, dst);
break;
case GGML_OP_COS:
ggml_cann_op_unary<aclnn_cos>(ctx, dst);
break;
case GGML_OP_SIN:
ggml_cann_op_unary<aclnn_sin>(ctx, dst);
break;
case GGML_OP_CONV_TRANSPOSE_1D:
ggml_cann_conv_transpose_1d(ctx, dst);
break;
case GGML_OP_LOG:
GGML_CANN_CALL_OP_UNARY(Log);
break;
case GGML_OP_MEAN:
ggml_cann_mean(ctx, dst);
break;
case GGML_OP_PAD_REFLECT_1D:
ggml_cann_pad_reflect_1d(ctx, dst);
break;
case GGML_OP_COUNT_EQUAL:
ggml_cann_count_equal(ctx, dst);
break;
case GGML_OP_FLASH_ATTN_EXT:
ggml_cann_flash_attn_ext(ctx, dst);
break;
default:
return false;
}
return true;
}
static const char* ggml_backend_cann_name(ggml_backend_t backend) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
return cann_ctx->name.c_str();
}
static void ggml_backend_cann_free(ggml_backend_t backend) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
ACL_CHECK(aclrtSynchronizeDevice());
ACL_CHECK(aclrtResetDevice(cann_ctx->device));
delete cann_ctx;
delete backend;
}
static void ggml_backend_cann_set_tensor_async(ggml_backend_t backend,
ggml_tensor *tensor,
const void *data,
size_t offset,
size_t size) {
ggml_backend_cann_context *cann_ctx =
(ggml_backend_cann_context *)backend->context;
ggml_backend_buffer_t buf =
tensor->view_src ? tensor->view_src->buffer : tensor->buffer;
GGML_ASSERT(buf->buft == ggml_backend_cann_buffer_type(cann_ctx->device) &&
"unsupported buffer type");
GGML_ASSERT(!ggml_is_quantized(tensor->type));
ggml_cann_async_memcpy(cann_ctx, (char *)tensor->data + offset, data, size,
ACL_MEMCPY_HOST_TO_DEVICE);
}
static void ggml_backend_cann_get_tensor_async(
ggml_backend_t backend, const ggml_tensor *tensor, void *data,
size_t offset, size_t size) {
ggml_backend_cann_context *cann_ctx =
(ggml_backend_cann_context *)backend->context;
ggml_backend_buffer_t buf =
tensor->view_src ? tensor->view_src->buffer : tensor->buffer;
GGML_ASSERT(buf->buft == ggml_backend_cann_buffer_type(cann_ctx->device) &&
"unsupported buffer type");
GGML_ASSERT(!ggml_is_quantized(tensor->type));
ggml_cann_async_memcpy(cann_ctx, data, (char *)tensor->data + offset, size,
ACL_MEMCPY_DEVICE_TO_HOST);
}
static bool ggml_backend_cann_cpy_tensor_async(
ggml_backend_t backend_src, ggml_backend_t backend_dst,
const ggml_tensor* src, ggml_tensor* dst) {
GGML_ASSERT(ggml_backend_is_cann(backend_src) ||
ggml_backend_is_cann(backend_dst));
if (!ggml_backend_buffer_is_cann(src->buffer) ||
!ggml_backend_buffer_is_cann(dst->buffer)) {
return false;
}
ggml_backend_buffer_t buf_src =
src->view_src ? src->view_src->buffer : src->buffer;
ggml_backend_buffer_t buf_dst =
dst->view_src ? dst->view_src->buffer : dst->buffer;
ggml_backend_cann_context* cann_ctx_src =
(ggml_backend_cann_context*)backend_src->context;
ggml_backend_cann_context* cann_ctx_dst =
(ggml_backend_cann_context*)backend_dst->context;
size_t copy_size = ggml_nbytes(dst);
if (copy_size == 0) {
return true;
}
if (backend_src != backend_dst) {
ggml_backend_cann_buffer_context* buf_ctx_src =
(ggml_backend_cann_buffer_context*)buf_src->context;
ggml_backend_cann_buffer_context* buf_ctx_dst =
(ggml_backend_cann_buffer_context*)buf_dst->context;
GGML_ASSERT(cann_ctx_src->device == buf_ctx_src->device);
GGML_ASSERT(cann_ctx_dst->device == buf_ctx_dst->device);
int32_t canAccessPeer = 0;
ACL_CHECK(aclrtDeviceCanAccessPeer(&canAccessPeer, cann_ctx_src->device,
cann_ctx_dst->device));
if (!canAccessPeer) {
return false;
}
ggml_cann_set_device(cann_ctx_dst->device);
ACL_CHECK(aclrtDeviceEnablePeerAccess(cann_ctx_src->device, 0));
ggml_cann_set_device(cann_ctx_src->device);
ACL_CHECK(aclrtDeviceEnablePeerAccess(cann_ctx_dst->device, 0));
cann_ctx_src->task_queue.wait();
ACL_CHECK(aclrtMemcpyAsync(dst->data, copy_size, src->data, copy_size,
ACL_MEMCPY_DEVICE_TO_DEVICE,
cann_ctx_src->stream()));
aclrtSynchronizeStream(cann_ctx_src->stream());
} else {
ACL_CHECK(aclrtMemcpyAsync(dst->data, copy_size, src->data, copy_size,
ACL_MEMCPY_DEVICE_TO_DEVICE,
cann_ctx_dst->stream()));
}
return true;
}
static void ggml_backend_cann_synchronize(ggml_backend_t backend) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
cann_ctx->task_queue.wait();
ggml_cann_set_device(cann_ctx->device);
ACL_CHECK(aclrtSynchronizeStream(cann_ctx->stream()));
}
static enum ggml_status ggml_backend_cann_graph_compute(
ggml_backend_t backend, ggml_cgraph* cgraph) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
ggml_cann_set_device(cann_ctx->device);
release_nz_workspace();
for (int i = 0; i < cgraph->n_nodes; i++) {
ggml_tensor* node = cgraph->nodes[i];
if (ggml_is_empty(node) || node->op == GGML_OP_NONE) {
continue;
}
bool ok = ggml_cann_compute_forward(*cann_ctx, node);
if (!ok) {
GGML_LOG_ERROR("%s: error: op not supported %s (%s)\n", __func__,
node->name, ggml_op_name(node->op));
}
GGML_ASSERT(ok);
}
return GGML_STATUS_SUCCESS;
}
static bool ggml_backend_cann_supports_op(ggml_backend_dev_t dev,
const ggml_tensor* op) {
switch (op->op) {
case GGML_OP_UNARY:
switch (ggml_get_unary_op(op)) {
case GGML_UNARY_OP_ABS:
case GGML_UNARY_OP_NEG:
case GGML_UNARY_OP_GELU:
case GGML_UNARY_OP_SILU:
case GGML_UNARY_OP_RELU:
case GGML_UNARY_OP_SIGMOID:
case GGML_UNARY_OP_HARDSIGMOID:
case GGML_UNARY_OP_HARDSWISH:
case GGML_UNARY_OP_GELU_QUICK:
case GGML_UNARY_OP_TANH:
case GGML_UNARY_OP_EXP:
case GGML_UNARY_OP_ELU:
case GGML_UNARY_OP_SGN:
case GGML_UNARY_OP_STEP:
case GGML_UNARY_OP_GELU_ERF:
return true;
default:
return false;
}
case GGML_OP_GLU:
switch (ggml_get_glu_op(op)) {
case GGML_GLU_OP_REGLU:
case GGML_GLU_OP_GEGLU:
case GGML_GLU_OP_SWIGLU:
case GGML_GLU_OP_GEGLU_ERF:
case GGML_GLU_OP_GEGLU_QUICK:
return true;
default:
return false;
}
break;
case GGML_OP_MUL_MAT: {
switch (op->src[0]->type) {
case GGML_TYPE_F16:
case GGML_TYPE_F32:
return true;
case GGML_TYPE_Q8_0:
case GGML_TYPE_Q4_0:
#ifdef ASCEND_310P
return false;
#endif
return ggml_is_contiguous(op->src[0]) &&
ggml_is_contiguous(op->src[1]);
default:
return false;
}
}
case GGML_OP_MUL_MAT_ID:
switch (op->src[0]->type) {
case GGML_TYPE_F16:
case GGML_TYPE_F32:
return true;
case GGML_TYPE_Q8_0:
case GGML_TYPE_Q4_0:
#ifdef ASCEND_310P
return false;
#endif
return ggml_is_contiguous(op->src[0]) &&
ggml_is_contiguous(op->src[1]);
default:
return false;
}
case GGML_OP_GET_ROWS: {
switch (op->src[0]->type) {
case GGML_TYPE_F32:
case GGML_TYPE_F16:
case GGML_TYPE_Q8_0:
return true;
default:
return false;
}
} break;
case GGML_OP_SET_ROWS: {
switch (op->type) {
case GGML_TYPE_F32:
case GGML_TYPE_F16:
return true;
default:
return false;
}
} break;
case GGML_OP_CPY: {
ggml_tensor *src = op->src[0];
if ((op->type != GGML_TYPE_F32 && op->type != GGML_TYPE_F16) ||
(src->type != GGML_TYPE_F32 &&
src->type != GGML_TYPE_F16)) {
return false;
}
if (!ggml_are_same_shape(op, src) && !ggml_is_contiguous(op)) {
return false;
}
return true;
} break;
case GGML_OP_CONT: {
switch (op->src[0]->type) {
case GGML_TYPE_F32:
case GGML_TYPE_F16:
return true;
default:
return false;
}
}
case GGML_OP_ROPE: {
float ext_factor = 0.0f;
memcpy(&ext_factor, (const float *) op->op_params + 7, sizeof(float));
if (op->src[0]->ne[0] != op->op_params[1]) {
return false;
}
if (ext_factor != 0) {
return false;
}
const int mode = ((const int32_t *) op->op_params)[2];
if (mode & GGML_ROPE_TYPE_MROPE) {
return false;
}
if (mode & GGML_ROPE_TYPE_VISION) {
return false;
}
if(!ggml_is_contiguous(op->src[0])){
return false;
}
return true;
}
case GGML_OP_UPSCALE: {
if (op->src[0]->ne[2] * op->ne[3] != op->src[0]->ne[3] * op->ne[2]) {
return false;
}
if (op->op_params[0] != GGML_SCALE_MODE_NEAREST) {
return false;
}
return true;
}
case GGML_OP_POOL_2D: {
const int32_t * opts = (const int32_t *) op->op_params;
#ifdef ASCEND_310P
enum ggml_op_pool opt = static_cast<ggml_op_pool>(opts[0]);
if(opt == GGML_OP_POOL_MAX){
return false;
}
#endif
const int k0 = opts[1];
const int k1 = opts[2];
const int p0 = opts[5];
const int p1 = opts[6];
return (p0 <= (k0 / 2)) && (p1 <= (k1 / 2));
}
case GGML_OP_SUM:
case GGML_OP_DUP:
case GGML_OP_IM2COL:
case GGML_OP_CONCAT:
case GGML_OP_REPEAT:
case GGML_OP_NONE:
case GGML_OP_RESHAPE:
case GGML_OP_VIEW:
case GGML_OP_PERMUTE:
case GGML_OP_TRANSPOSE:
case GGML_OP_NORM:
case GGML_OP_ADD:
case GGML_OP_ADD1:
case GGML_OP_SUB:
case GGML_OP_MUL:
case GGML_OP_DIV:
case GGML_OP_RMS_NORM:
case GGML_OP_SQR:
case GGML_OP_SQRT:
case GGML_OP_CLAMP:
case GGML_OP_DIAG_MASK_INF:
case GGML_OP_SUM_ROWS:
case GGML_OP_ARGSORT:
case GGML_OP_ACC:
case GGML_OP_GROUP_NORM:
case GGML_OP_PAD:
case GGML_OP_ARANGE:
case GGML_OP_TIMESTEP_EMBEDDING:
case GGML_OP_LEAKY_RELU:
case GGML_OP_ARGMAX:
case GGML_OP_COS:
case GGML_OP_SIN:
case GGML_OP_CONV_TRANSPOSE_1D:
case GGML_OP_LOG:
case GGML_OP_MEAN:
case GGML_OP_PAD_REFLECT_1D:
case GGML_OP_COUNT_EQUAL:
return true;
case GGML_OP_SCALE:
float bias;
memcpy(&bias, (float*)op->op_params + 1, sizeof(float));
return bias == 0.0f;
case GGML_OP_SOFT_MAX:
return !op->src[1] || (op->src[1]->ne[2] == 1 && op->src[1]->ne[3] == 1);
case GGML_OP_FLASH_ATTN_EXT:{
if(op->src[1]->type != GGML_TYPE_F16 || op->src[2]->type != GGML_TYPE_F16){
return false;
}
if(op->src[1]->type != GGML_TYPE_F16 && op->src[1]->type != GGML_TYPE_F32 && op->src[1]->type != GGML_TYPE_BF16){
return false;
}
if(op->type != GGML_TYPE_F16 && op->type != GGML_TYPE_F32 && op->type != GGML_TYPE_BF16){
return false;
}
if (op->src[1]->ne[0] != op->src[2]->ne[0]) {
return false;
}
if (op->src[0]->ne[0] == 192) {
return false;
}
if (op->src[0]->ne[0] == 576) {
return false;
}
if (op->src[0]->ne[3] != 1) {
return false;
}
float logitSoftcap = 0.0f;
memcpy(&logitSoftcap, (float*)op->op_params + 2, sizeof(float));
if(logitSoftcap != 0.0f) {
return false;
}
return true;
}
default:
return false;
}
GGML_UNUSED(dev);
}
static bool ggml_backend_buft_is_cann(ggml_backend_buffer_type_t buft) {
return buft->iface.get_name == ggml_backend_cann_buffer_type_name;
}
static bool ggml_backend_cann_offload_op(ggml_backend_dev_t dev,
const ggml_tensor* op) {
const int min_batch_size = 32;
GGML_UNUSED(dev);
return op->ne[1] >= min_batch_size && op->op != GGML_OP_GET_ROWS;
}
static void ggml_backend_cann_event_record(ggml_backend_t backend, ggml_backend_event_t event) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
ACL_CHECK(aclrtRecordEvent((aclrtEvent)event->context, cann_ctx->stream()));
}
static void ggml_backend_cann_event_wait(ggml_backend_t backend,
ggml_backend_event_t event) {
ggml_backend_cann_context* cann_ctx =
(ggml_backend_cann_context*)backend->context;
if (ggml_backend_is_cann(backend)) {
ACL_CHECK(aclrtStreamWaitEvent(cann_ctx->stream(),
(aclrtEvent)event->context));
} else {
GGML_ABORT("fatal error");
}
}
static const ggml_backend_i ggml_backend_cann_interface = {
ggml_backend_cann_name,
ggml_backend_cann_free,
ggml_backend_cann_set_tensor_async,
ggml_backend_cann_get_tensor_async,
ggml_backend_cann_cpy_tensor_async,
ggml_backend_cann_synchronize,
NULL,
NULL,
NULL,
NULL,
ggml_backend_cann_graph_compute,
ggml_backend_cann_event_record,
ggml_backend_cann_event_wait,
};
static ggml_guid_t ggml_backend_cann_guid() {
static ggml_guid guid = {0xa1, 0x94, 0xaf, 0xac, 0xbd, 0x4f, 0x47, 0x34,
0xbe, 0x1a, 0x9e, 0x71, 0x1f, 0x9e, 0xed, 0x64};
return &guid;
}
struct ggml_backend_cann_device_context {
int device;
std::string name;
std::string description;
};
static const char * ggml_backend_cann_device_get_name(ggml_backend_dev_t dev) {
ggml_backend_cann_device_context * ctx = (ggml_backend_cann_device_context *)dev->context;
return ctx->name.c_str();
}
static const char* ggml_backend_cann_device_get_description(ggml_backend_dev_t dev) {
ggml_backend_cann_device_context * ctx = (ggml_backend_cann_device_context *)dev->context;
return ctx->description.c_str();
}
static void ggml_backend_cann_device_get_memory(ggml_backend_dev_t dev, size_t * free, size_t * total) {
ggml_backend_cann_device_context * ctx = (ggml_backend_cann_device_context *)dev->context;
ggml_backend_cann_get_device_memory(ctx->device, free, total);
}
static enum ggml_backend_dev_type ggml_backend_cann_device_get_type(ggml_backend_dev_t dev) {
GGML_UNUSED(dev);
return GGML_BACKEND_DEVICE_TYPE_GPU;
}
static void ggml_backend_cann_device_get_props(ggml_backend_dev_t dev, ggml_backend_dev_props * props) {
props->name = ggml_backend_cann_device_get_name(dev);
props->description = ggml_backend_cann_device_get_description(dev);
props->type = ggml_backend_cann_device_get_type(dev);
ggml_backend_cann_device_get_memory(dev, &props->memory_free, &props->memory_total);
bool host_buffer = getenv("GGML_CANN_NO_PINNED") == nullptr;
props->caps = {
false,
host_buffer,
false,
true,
};
}
static ggml_backend_t ggml_backend_cann_device_init(ggml_backend_dev_t dev, const char * params) {
GGML_UNUSED(params);
ggml_backend_cann_device_context * ctx = (ggml_backend_cann_device_context *)dev->context;
return ggml_backend_cann_init(ctx->device);
}
static bool ggml_backend_cann_supports_buft(
ggml_backend_dev_t dev, ggml_backend_buffer_type_t buft) {
if (ggml_backend_buft_is_cann(buft)) {
ggml_backend_cann_device_context * dev_ctx = (ggml_backend_cann_device_context *)dev->context;
ggml_backend_cann_buffer_type_context * buft_ctx =
(ggml_backend_cann_buffer_type_context *)buft->context;
return buft_ctx->device == dev_ctx->device;
}
return false;
}
static ggml_backend_buffer_type_t ggml_backend_cann_device_get_buffer_type(ggml_backend_dev_t dev) {
ggml_backend_cann_device_context * ctx = (ggml_backend_cann_device_context *)dev->context;
return ggml_backend_cann_buffer_type(ctx->device);
}
static ggml_backend_buffer_type_t ggml_backend_cann_device_get_host_buffer_type(ggml_backend_dev_t dev) {
GGML_UNUSED(dev);
return ggml_backend_cann_host_buffer_type();
}
static ggml_backend_event_t ggml_backend_cann_device_event_new(
ggml_backend_dev_t dev) {
ggml_backend_cann_device_context * dev_ctx = (ggml_backend_cann_device_context *)dev->context;
ggml_cann_set_device(dev_ctx->device);
aclrtEvent event;
ACL_CHECK(aclrtCreateEvent(&event));
return new ggml_backend_event{
ggml_backend_reg_dev_get(ggml_backend_cann_reg(), dev_ctx->device),
event,
};
}
static void ggml_backend_cann_device_event_free(ggml_backend_dev_t dev, ggml_backend_event_t event) {
ACL_CHECK(aclrtDestroyEvent((aclrtEvent)event->context));
delete event;
GGML_UNUSED(dev);
}
static void ggml_backend_cann_device_event_synchronize(ggml_backend_dev_t dev, ggml_backend_event_t event) {
ACL_CHECK(aclrtSynchronizeEvent((aclrtEvent)event->context));
GGML_UNUSED(dev);
}
static const ggml_backend_device_i ggml_backend_cann_device_interface = {
ggml_backend_cann_device_get_name,
ggml_backend_cann_device_get_description,
ggml_backend_cann_device_get_memory,
ggml_backend_cann_device_get_type,
ggml_backend_cann_device_get_props,
ggml_backend_cann_device_init,
ggml_backend_cann_device_get_buffer_type,
ggml_backend_cann_device_get_host_buffer_type,
NULL,
ggml_backend_cann_supports_op,
ggml_backend_cann_supports_buft,
ggml_backend_cann_offload_op,
ggml_backend_cann_device_event_new,
ggml_backend_cann_device_event_free,
ggml_backend_cann_device_event_synchronize,
};
struct ggml_backend_cann_reg_context {
std::vector<ggml_backend_dev_t> devices;
};
static const char * ggml_backend_cann_reg_get_name(ggml_backend_reg_t reg) {
GGML_UNUSED(reg);
return GGML_CANN_NAME;
}
static size_t ggml_backend_cann_reg_get_device_count(ggml_backend_reg_t reg) {
ggml_backend_cann_reg_context * ctx = (ggml_backend_cann_reg_context *)reg->context;
return ctx->devices.size();
}
static ggml_backend_dev_t ggml_backend_cann_reg_get_device(ggml_backend_reg_t reg, size_t index) {
ggml_backend_cann_reg_context * ctx = (ggml_backend_cann_reg_context *)reg->context;
GGML_ASSERT(index < ctx->devices.size());
return ctx->devices[index];
}
static void * ggml_backend_cann_reg_get_proc_address(ggml_backend_reg_t reg, const char * name) {
GGML_UNUSED(reg);
GGML_UNUSED(name);
return nullptr;
}
static const ggml_backend_reg_i ggml_backend_cann_reg_interface = {
ggml_backend_cann_reg_get_name,
ggml_backend_cann_reg_get_device_count,
ggml_backend_cann_reg_get_device,
ggml_backend_cann_reg_get_proc_address,
};
ggml_backend_reg_t ggml_backend_cann_reg() {
static ggml_backend_reg reg;
static bool initialized = false;
{
static std::mutex mutex;
std::lock_guard<std::mutex> lock(mutex);
if (!initialized) {
aclInit(nullptr);
ggml_backend_cann_reg_context * ctx = new ggml_backend_cann_reg_context;
for (int i = 0; i < ggml_cann_info().device_count; i++) {
ggml_backend_cann_device_context* dev_ctx = new ggml_backend_cann_device_context();
dev_ctx->description = aclrtGetSocName();
dev_ctx->device = i;
dev_ctx->name = GGML_CANN_NAME + std::to_string(i);
ggml_cann_set_device(i);
ggml_backend_dev_t dev = new ggml_backend_device {
ggml_backend_cann_device_interface,
&reg,
dev_ctx
};
ctx->devices.push_back(dev);
}
reg = ggml_backend_reg {
GGML_BACKEND_API_VERSION,
ggml_backend_cann_reg_interface,
ctx
};
}
initialized = true;
}
return &reg;
}
ggml_backend_t ggml_backend_cann_init(int32_t device) {
aclInit(nullptr);
if (device < 0 || device >= ggml_backend_cann_get_device_count()) {
GGML_LOG_ERROR("%s: error: invalid device %d\n", __func__, device);
return nullptr;
}
ggml_backend_cann_context* ctx = new ggml_backend_cann_context(device);
if (ctx == nullptr) {
GGML_LOG_ERROR("%s: error: failed to allocate context\n", __func__);
return nullptr;
}
ggml_cann_set_device(ctx->device);
ggml_backend_t cann_backend =
new ggml_backend{ ggml_backend_cann_guid(),
ggml_backend_cann_interface,
ggml_backend_reg_dev_get(ggml_backend_cann_reg(), device),
ctx};
return cann_backend;
}
bool ggml_backend_is_cann(ggml_backend_t backend) {
return backend != NULL &&
ggml_guid_matches(backend->guid, ggml_backend_cann_guid());
}
int32_t ggml_backend_cann_get_device_count() {
return ggml_cann_info().device_count;
}
void ggml_backend_cann_get_device_description(
int32_t device, char* description, size_t description_size) {
ggml_cann_set_device(device);
const char* soc_name = aclrtGetSocName();
snprintf(description, description_size, "%s", soc_name);
}
void ggml_backend_cann_get_device_memory(int32_t device, size_t* free,
size_t* total) {
ggml_cann_set_device(device);
ACL_CHECK(aclrtGetMemInfo(ACL_HBM_MEM, free, total));
}
GGML_BACKEND_DL_IMPL(ggml_backend_cann_reg)