#pragma once
#include <cuda_fp16.h>
#include <cuda_bf16.h>
#include <cuda_runtime.h>
#include <memory>
#include <vector>
#include "gemm_s4_f16_kernel.h"
#include "metric.h"
namespace aphrodite {
namespace autoquant {
extern bool g_dump_kernel_info_once;
enum Type
{
kGemm,
kFusedSiluFfn
};
template <typename T_BC, typename T_Q>
struct Impl{
using Kernels = std::vector<std::unique_ptr<IGemmKernel<T_BC, T_Q>>>;
void Generate(std::vector<Kernels>& kernels);
void Measure(T_BC* C,
const uint* A,
const T_BC* B,
const T_Q* Q,
int m,
int n,
int k,
int group_size,
Type type,
std::vector<Metric>& metrics,
cudaStream_t st,
std::vector<Kernels>& _kernels);
static bool Compare(const Metric& a, const Metric& b)
{
if (a.feasible != b.feasible) {
return a.feasible > b.feasible;
}
if (a.prefer != b.prefer) {
return a.prefer > b.prefer;
}
return a.grid_norm < b.grid_norm;
}
int Estimate(int m, int n, int k, Kernels& kernels);
void Run(T_BC* C,
const uint* A,
const T_BC* B,
const T_Q* Q,
int m,
int n,
int k,
int group_size,
Type type,
int algo_id,
cudaStream_t st,
std::vector<Kernels>& kernels);
Impl();
~Impl();
std::vector<Kernels> kernels_;
std::vector<int> group_sizes_;
static constexpr int kWarmup = 10;
static constexpr int kMeasure = 100;
cudaEvent_t ev_start_{};
cudaEvent_t ev_end_{};
};
template <typename T_BC, typename T_Q>
class GemmS4F16 {
public:
GemmS4F16();
~GemmS4F16();
void Measure(T_BC* C,
const uint* A,
const T_BC* B,
const T_Q* Q,
int m,
int n,
int k,
int group_size,
Type type,
std::vector<Metric>& metrics,
cudaStream_t st);
void Run(T_BC* C,
const uint* A,
const T_BC* B,
const T_Q* Q,
int m,
int n,
int k,
int group_size,
Type type,
int algo_id,
cudaStream_t st);
private:
std::unique_ptr<Impl<T_BC, T_Q>> impl_;
};
}
}