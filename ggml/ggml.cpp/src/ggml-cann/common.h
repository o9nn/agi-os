#ifndef CANN_COMMON_H
#define CANN_COMMON_H
#include <acl/acl.h>
#include <cstdio>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <unistd.h>
#include <functional>
#include <optional>
#include "../include/ggml-cann.h"
#include "../include/ggml.h"
#include "../ggml-impl.h"
#define MATRIX_ROW_PADDING 512
#define GGML_CANN_MAX_STREAMS 8
[[noreturn]] void ggml_cann_error(const char* stmt, const char* func,
const char* file, int line, const char* msg);
#define ACL_CHECK_GEN(stmt, success, error_fn)                                \
do {                                                                      \
int err_code = (stmt);                                                \
if (err_code != (success)) {                                          \
ggml_cann_error(#stmt, __func__, __FILE__, __LINE__, error_fn()); \
}                                                                     \
} while (0);
#define ACL_CHECK(stmt) ACL_CHECK_GEN(stmt, 0, aclGetRecentErrMsg)
struct ggml_cann_device_info {
int32_t device_count;
struct cann_device_info {
int cc;
size_t smpb;
bool vmm;
size_t vmm_granularity;
size_t total_vram;
};
cann_device_info devices[GGML_CANN_MAX_DEVICES] =
{};
};
const ggml_cann_device_info& ggml_cann_info();
void ggml_cann_set_device(int32_t device);
int32_t ggml_cann_get_device();
std::optional<std::string> get_env(const std::string& name);
bool parse_bool(const std::string& value);
struct ggml_cann_pool {
virtual ~ggml_cann_pool() = default;
virtual void* alloc(size_t size, size_t* actual_size) = 0;
virtual void free(void* ptr, size_t size) = 0;
};
struct ggml_cann_pool_alloc {
ggml_cann_pool* pool = nullptr;
void* ptr = nullptr;
size_t actual_size = 0;
ggml_cann_pool_alloc() = default;
explicit ggml_cann_pool_alloc(ggml_cann_pool& pool) : pool(&pool) {}
ggml_cann_pool_alloc(ggml_cann_pool& pool, size_t size) : pool(&pool) {
alloc(size);
}
~ggml_cann_pool_alloc() {
if (ptr != nullptr) {
pool->free(ptr, actual_size);
}
}
void* alloc(size_t size) {
GGML_ASSERT(pool != nullptr);
GGML_ASSERT(ptr == nullptr);
ptr = pool->alloc(size, &this->actual_size);
return ptr;
}
void* alloc(ggml_cann_pool& pool, size_t size) {
this->pool = &pool;
return alloc(size);
}
void* get() { return ptr; }
ggml_cann_pool_alloc(const ggml_cann_pool_alloc&) = delete;
ggml_cann_pool_alloc(ggml_cann_pool_alloc&&) = delete;
ggml_cann_pool_alloc& operator=(const ggml_cann_pool_alloc&) = delete;
ggml_cann_pool_alloc& operator=(ggml_cann_pool_alloc&&) = delete;
};
using aclnn_func_t = aclnnStatus (*)(void*, uint64_t, aclOpExecutor*, aclrtStream);
class cann_task {
public:
virtual void run_task() {}
};
class cann_task_queue {
public:
explicit cann_task_queue(size_t capacity, int32_t device)
: buffer_(capacity), capacity_(capacity), head_(0), tail_(0),
running_(false), device_(device) {
GGML_ASSERT((capacity & (capacity - 1)) == 0 && "capacity must be power of 2");
mask_ = capacity_ - 1;
}
bool enqueue(std::unique_ptr<cann_task>&& item) {
size_t next_tail = (tail_ + 1) & mask_;
if (next_tail == head_) {
return false;
}
buffer_[tail_] = std::move(item);
std::atomic_thread_fence(std::memory_order_release);
tail_ = next_tail;
return true;
}
void submit_task(std::unique_ptr<cann_task>&& task) {
while(!enqueue(std::move(task))) {
std::this_thread::yield();
continue;
}
if (!running_) {
running_ = true;
thread_ = std::thread(&cann_task_queue::execute, this);
}
}
void wait() {
while (running_ && head_ != tail_) {
std::this_thread::yield();
continue;
}
}
void stop() {
running_ = false;
if (thread_.joinable()) {
thread_.join();
}
}
private:
void execute() {
ggml_cann_set_device(device_);
while (running_) {
if(head_ == tail_) {
std::this_thread::yield();
continue;
}
std::atomic_thread_fence(std::memory_order_acquire);
buffer_[head_]->run_task();
buffer_[head_].reset();
head_ = (head_ + 1) & mask_;
}
}
std::vector<std::unique_ptr<cann_task>> buffer_;
const size_t capacity_;
size_t mask_;
size_t head_;
size_t tail_;
bool running_;
std::thread thread_;
int32_t device_;
};
struct ggml_backend_cann_context {
int32_t device;
std::string name;
std::string description;
aclrtEvent copy_event = nullptr;
cann_task_queue task_queue;
bool async_mode;
aclrtStream streams[GGML_CANN_MAX_STREAMS] = {nullptr};
explicit ggml_backend_cann_context(int device)
: device(device), name("CANN" + std::to_string(device)), task_queue(1024, device) {
ggml_cann_set_device(device);
description = aclrtGetSocName();
async_mode = parse_bool(get_env("GGML_CANN_ASYNC_MODE").value_or(""));
GGML_LOG_INFO("%s: device %d async operator submission is %s\n", __func__,
device, async_mode ? "ON" : "OFF");
}
~ggml_backend_cann_context() {
ggml_cann_set_device(device);
task_queue.stop();
if (copy_event != nullptr) {
ACL_CHECK(aclrtDestroyEvent(copy_event));
}
for (int i = 0; i < GGML_CANN_MAX_STREAMS; ++i) {
if (streams[i] != nullptr) {
ACL_CHECK(aclrtDestroyStream(streams[i]));
}
}
}
aclrtStream stream(int stream) {
if (streams[stream] == nullptr) {
ggml_cann_set_device(device);
ACL_CHECK(aclrtCreateStream(&streams[stream]));
}
return streams[stream];
}
aclrtStream stream() { return stream(0); }
std::unique_ptr<ggml_cann_pool>
mem_pool;
static std::unique_ptr<ggml_cann_pool> new_pool_for_device(int device);
ggml_cann_pool& pool() {
if (mem_pool == nullptr) {
mem_pool = new_pool_for_device(device);
}
return *mem_pool;
}
};
#endif