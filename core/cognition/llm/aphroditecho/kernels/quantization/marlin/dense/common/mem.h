#pragma once
__device__ inline void cp_async4_pred(void* smem_ptr, const void* glob_ptr,
bool pred = true) {
const int BYTES = 16;
uint32_t smem = static_cast<uint32_t>(__cvta_generic_to_shared(smem_ptr));
asm volatile(
"{\n"
"   .reg .pred p;\n"
"   setp.ne.b32 p, %0, 0;\n"
"   @p cp.async.cg.shared.global [%1], [%2], %3;\n"
"}\n" ::"r"((int)pred),
"r"(smem), "l"(glob_ptr), "n"(BYTES));
}
__device__ inline void cp_async4(void* smem_ptr, const void* glob_ptr) {
const int BYTES = 16;
uint32_t smem = static_cast<uint32_t>(__cvta_generic_to_shared(smem_ptr));
asm volatile(
"{\n"
"   cp.async.cg.shared.global [%0], [%1], %2;\n"
"}\n" ::"r"(smem),
"l"(glob_ptr), "n"(BYTES));
}
__device__ inline void cp_async_fence() {
asm volatile("cp.async.commit_group;\n" ::);
}
template <int n>
__device__ inline void cp_async_wait() {
asm volatile("cp.async.wait_group %0;\n" ::"n"(n));
}
__device__ inline void barrier_acquire(int* lock, int count) {
if (threadIdx.x == 0) {
int state = -1;
do
asm volatile("ld.global.acquire.gpu.b32 %0, [%1];\n"
: "=r"(state)
: "l"(lock));
while (state != count);
}
__syncthreads();
}
__device__ inline void barrier_release(int* lock, bool reset = false) {
__syncthreads();
if (threadIdx.x == 0) {
if (reset) {
lock[0] = 0;
return;
}
int val = 1;
asm volatile("fence.acq_rel.gpu;\n");
asm volatile("red.relaxed.gpu.global.add.s32 [%0], %1;\n"
:
: "l"(lock), "r"(val));
}
}