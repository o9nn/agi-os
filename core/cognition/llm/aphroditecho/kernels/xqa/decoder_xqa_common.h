#pragma once
#include <cassert>
#include <limits.h>
#include <stdint.h>
#include <torch/torch.h>
#include <c10/cuda/CUDAGuard.h>
#include <c10/cuda/CUDAStream.h>
#include "cuda_compat.h"
#include <ATen/cuda/CUDAContext.h>
#define HOST_DEVICE_FUNC __host__ __device__
#define DEVICE_FUNC __device__
inline void cuErrCheck_(CUresult stat, char const* file, int line) {
if (stat != CUDA_SUCCESS) {
char const* msg = nullptr;
cuGetErrorName(stat, &msg);
fprintf(stderr, "CUDA Error: %s %s %d\n", msg, file, line);
}
}
#define cuErrCheck(stat)                     \
{                                          \
cuErrCheck_((stat), __FILE__, __LINE__); \
}
#define CUDACHECK(cmd)                                              \
do {                                                              \
cudaError_t e = cmd;                                            \
if (e != cudaSuccess) {                                         \
printf("Failed: Cuda error %s:%d '%s'\n", __FILE__, __LINE__, \
cudaGetErrorString(e));                                \
exit(EXIT_FAILURE);                                           \
}                                                               \
} while (0)
inline constexpr int kMinHistoryTokensPerBlock = 128;
inline constexpr float kEnableMinBlockFactor = 4.0;
inline constexpr int kTargetWaveFactor = 8;
inline constexpr int kXQA_MAX_NUM_SUB_SEQ = 6000;
inline constexpr int kMaxBeamWidth = 1;
inline int getDevice() {
int current_dev_id = 0;
CUDACHECK(cudaGetDevice(&current_dev_id));
return current_dev_id;
}
inline int getSMVersion() {
int device{-1};
CUDACHECK(cudaGetDevice(&device));
int sm_major = 0;
int sm_minor = 0;
CUDACHECK(cudaDeviceGetAttribute(&sm_major, cudaDevAttrComputeCapabilityMajor,
device));
CUDACHECK(cudaDeviceGetAttribute(&sm_minor, cudaDevAttrComputeCapabilityMinor,
device));
return sm_major * 10 + sm_minor;
}
enum Data_type {
DATA_TYPE_FP16,
DATA_TYPE_BF16,
DATA_TYPE_FP32,
DATA_TYPE_INT8,
DATA_TYPE_INT32,
DATA_TYPE_E4M3,
DATA_TYPE_E5M2,
DATA_TYPE_UNKNOWN
};
template <typename T>
struct TypeToDataType {
static constexpr Data_type value = Data_type::DATA_TYPE_UNKNOWN;
};
template <>
struct TypeToDataType<__nv_bfloat16> {
static constexpr Data_type value = Data_type::DATA_TYPE_BF16;
};
template <>
struct TypeToDataType<__half> {
static constexpr Data_type value = Data_type::DATA_TYPE_FP16;
};
template <>
struct TypeToDataType<uint8_t> {
static constexpr Data_type value = Data_type::DATA_TYPE_E4M3;
};
static inline size_t get_size_in_bytes(size_t n, Data_type dtype) {
switch (dtype) {
case DATA_TYPE_FP32:
return n * 4;
case DATA_TYPE_FP16:
return n * 2;
case DATA_TYPE_INT32:
return n * 4;
case DATA_TYPE_INT8:
return n;
case DATA_TYPE_BF16:
return n * 2;
case DATA_TYPE_E4M3:
return n;
case DATA_TYPE_E5M2:
return n;
default:
TORCH_CHECK(false, "FMHA Data Type is not supported.");
return 0;
}
}
static inline size_t get_size_in_bytes(Data_type dtype) {
return get_size_in_bytes(1, dtype);
}
constexpr int32_t kSM_70 = 70;
constexpr int32_t kSM_72 = 72;
constexpr int32_t kSM_75 = 75;
constexpr int32_t kSM_80 = 80;
constexpr int32_t kSM_86 = 86;
constexpr int32_t kSM_89 = 89;
constexpr int32_t kSM_90 = 90;