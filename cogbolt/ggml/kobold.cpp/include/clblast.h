#ifndef CLBLAST_CLBLAST_H_
#define CLBLAST_CLBLAST_H_
#include <cstdlib>
#include <string>
#include <unordered_map>
#if defined(__APPLE__) || defined(__MACOSX)
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif
#if defined(_WIN32) && defined(CLBLAST_DLL)
#if defined(COMPILING_DLL)
#define PUBLIC_API __declspec(dllexport)
#else
#define PUBLIC_API __declspec(dllimport)
#endif
#else
#define PUBLIC_API
#endif
#define CLBLAST_VERSION_MAJOR 1
#define CLBLAST_VERSION_MINOR 6
#define CLBLAST_VERSION_PATCH 0
namespace clblast {
enum class StatusCode {
kSuccess                   =   0,
kOpenCLCompilerNotAvailable=  -3,
kTempBufferAllocFailure    =  -4,
kOpenCLOutOfResources      =  -5,
kOpenCLOutOfHostMemory     =  -6,
kOpenCLBuildProgramFailure = -11,
kInvalidValue              = -30,
kInvalidCommandQueue       = -36,
kInvalidMemObject          = -38,
kInvalidBinary             = -42,
kInvalidBuildOptions       = -43,
kInvalidProgram            = -44,
kInvalidProgramExecutable  = -45,
kInvalidKernelName         = -46,
kInvalidKernelDefinition   = -47,
kInvalidKernel             = -48,
kInvalidArgIndex           = -49,
kInvalidArgValue           = -50,
kInvalidArgSize            = -51,
kInvalidKernelArgs         = -52,
kInvalidLocalNumDimensions = -53,
kInvalidLocalThreadsTotal  = -54,
kInvalidLocalThreadsDim    = -55,
kInvalidGlobalOffset       = -56,
kInvalidEventWaitList      = -57,
kInvalidEvent              = -58,
kInvalidOperation          = -59,
kInvalidBufferSize         = -61,
kInvalidGlobalWorkSize     = -63,
kNotImplemented            = -1024,
kInvalidMatrixA            = -1022,
kInvalidMatrixB            = -1021,
kInvalidMatrixC            = -1020,
kInvalidVectorX            = -1019,
kInvalidVectorY            = -1018,
kInvalidDimension          = -1017,
kInvalidLeadDimA           = -1016,
kInvalidLeadDimB           = -1015,
kInvalidLeadDimC           = -1014,
kInvalidIncrementX         = -1013,
kInvalidIncrementY         = -1012,
kInsufficientMemoryA       = -1011,
kInsufficientMemoryB       = -1010,
kInsufficientMemoryC       = -1009,
kInsufficientMemoryX       = -1008,
kInsufficientMemoryY       = -1007,
kInsufficientMemoryTemp    = -2050,
kInvalidBatchCount         = -2049,
kInvalidOverrideKernel     = -2048,
kMissingOverrideParameter  = -2047,
kInvalidLocalMemUsage      = -2046,
kNoHalfPrecision           = -2045,
kNoDoublePrecision         = -2044,
kInvalidVectorScalar       = -2043,
kInsufficientMemoryScalar  = -2042,
kDatabaseError             = -2041,
kUnknownError              = -2040,
kUnexpectedError           = -2039,
};
enum class Layout { kRowMajor = 101, kColMajor = 102 };
enum class Transpose { kNo = 111, kYes = 112, kConjugate = 113 };
enum class Triangle { kUpper = 121, kLower = 122 };
enum class Diagonal { kNonUnit = 131, kUnit = 132 };
enum class Side { kLeft = 141, kRight = 142 };
enum class KernelMode { kCrossCorrelation = 151, kConvolution = 152 };
enum class Precision { kHalf = 16, kSingle = 32, kDouble = 64,
kComplexSingle = 3232, kComplexDouble = 6464, kAny = -1 };
template <typename T>
StatusCode Rotg(cl_mem sa_buffer, const size_t sa_offset,
cl_mem sb_buffer, const size_t sb_offset,
cl_mem sc_buffer, const size_t sc_offset,
cl_mem ss_buffer, const size_t ss_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Rotmg(cl_mem sd1_buffer, const size_t sd1_offset,
cl_mem sd2_buffer, const size_t sd2_offset,
cl_mem sx1_buffer, const size_t sx1_offset,
const cl_mem sy1_buffer, const size_t sy1_offset,
cl_mem sparam_buffer, const size_t sparam_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Rot(const size_t n,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
const T cos,
const T sin,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Rotm(const size_t n,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem sparam_buffer, const size_t sparam_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Swap(const size_t n,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Scal(const size_t n,
const T alpha,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Copy(const size_t n,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Axpy(const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Dot(const size_t n,
cl_mem dot_buffer, const size_t dot_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Dotu(const size_t n,
cl_mem dot_buffer, const size_t dot_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Dotc(const size_t n,
cl_mem dot_buffer, const size_t dot_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Nrm2(const size_t n,
cl_mem nrm2_buffer, const size_t nrm2_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Asum(const size_t n,
cl_mem asum_buffer, const size_t asum_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Sum(const size_t n,
cl_mem sum_buffer, const size_t sum_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Amax(const size_t n,
cl_mem imax_buffer, const size_t imax_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Amin(const size_t n,
cl_mem imin_buffer, const size_t imin_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Max(const size_t n,
cl_mem imax_buffer, const size_t imax_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Min(const size_t n,
cl_mem imin_buffer, const size_t imin_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Gemv(const Layout layout, const Transpose a_transpose,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Gbmv(const Layout layout, const Transpose a_transpose,
const size_t m, const size_t n, const size_t kl, const size_t ku,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hemv(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hbmv(const Layout layout, const Triangle triangle,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hpmv(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem ap_buffer, const size_t ap_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Symv(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Sbmv(const Layout layout, const Triangle triangle,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Spmv(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem ap_buffer, const size_t ap_offset,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const T beta,
cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Trmv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Tbmv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n, const size_t k,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Tpmv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n,
const cl_mem ap_buffer, const size_t ap_offset,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Trsv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Tbsv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n, const size_t k,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Tpsv(const Layout layout, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t n,
const cl_mem ap_buffer, const size_t ap_offset,
cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Ger(const Layout layout,
const size_t m, const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Geru(const Layout layout,
const size_t m, const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Gerc(const Layout layout,
const size_t m, const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Her(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hpr(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem ap_buffer, const size_t ap_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Her2(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hpr2(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem ap_buffer, const size_t ap_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Syr(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Spr(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
cl_mem ap_buffer, const size_t ap_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Syr2(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Spr2(const Layout layout, const Triangle triangle,
const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
cl_mem ap_buffer, const size_t ap_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Gemm(const Layout layout, const Transpose a_transpose, const Transpose b_transpose,
const size_t m, const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr,
cl_mem temp_buffer = nullptr);
template <typename T>
StatusCode Symm(const Layout layout, const Side side, const Triangle triangle,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Hemm(const Layout layout, const Side side, const Triangle triangle,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Syrk(const Layout layout, const Triangle triangle, const Transpose a_transpose,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Herk(const Layout layout, const Triangle triangle, const Transpose a_transpose,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Syr2k(const Layout layout, const Triangle triangle, const Transpose ab_transpose,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T, typename U>
StatusCode Her2k(const Layout layout, const Triangle triangle, const Transpose ab_transpose,
const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
const U beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Trmm(const Layout layout, const Side side, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Trsm(const Layout layout, const Side side, const Triangle triangle, const Transpose a_transpose, const Diagonal diagonal,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Had(const size_t n,
const T alpha,
const cl_mem x_buffer, const size_t x_offset, const size_t x_inc,
const cl_mem y_buffer, const size_t y_offset, const size_t y_inc,
const T beta,
cl_mem z_buffer, const size_t z_offset, const size_t z_inc,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Omatcopy(const Layout layout, const Transpose a_transpose,
const size_t m, const size_t n,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld,
cl_mem b_buffer, const size_t b_offset, const size_t b_ld,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Im2col(const KernelMode kernel_mode,
const size_t channels, const size_t height, const size_t width, const size_t kernel_h, const size_t kernel_w, const size_t pad_h, const size_t pad_w, const size_t stride_h, const size_t stride_w, const size_t dilation_h, const size_t dilation_w,
const cl_mem im_buffer, const size_t im_offset,
cl_mem col_buffer, const size_t col_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Col2im(const KernelMode kernel_mode,
const size_t channels, const size_t height, const size_t width, const size_t kernel_h, const size_t kernel_w, const size_t pad_h, const size_t pad_w, const size_t stride_h, const size_t stride_w, const size_t dilation_h, const size_t dilation_w,
const cl_mem col_buffer, const size_t col_offset,
cl_mem im_buffer, const size_t im_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode Convgemm(const KernelMode kernel_mode,
const size_t channels, const size_t height, const size_t width, const size_t kernel_h, const size_t kernel_w, const size_t pad_h, const size_t pad_w, const size_t stride_h, const size_t stride_w, const size_t dilation_h, const size_t dilation_w, const size_t num_kernels, const size_t batch_count,
const cl_mem im_buffer, const size_t im_offset,
const cl_mem kernel_buffer, const size_t kernel_offset,
cl_mem result_buffer, const size_t result_offset,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode AxpyBatched(const size_t n,
const T *alphas,
const cl_mem x_buffer, const size_t *x_offsets, const size_t x_inc,
cl_mem y_buffer, const size_t *y_offsets, const size_t y_inc,
const size_t batch_count,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode GemmBatched(const Layout layout, const Transpose a_transpose, const Transpose b_transpose,
const size_t m, const size_t n, const size_t k,
const T *alphas,
const cl_mem a_buffer, const size_t *a_offsets, const size_t a_ld,
const cl_mem b_buffer, const size_t *b_offsets, const size_t b_ld,
const T *betas,
cl_mem c_buffer, const size_t *c_offsets, const size_t c_ld,
const size_t batch_count,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode GemmStridedBatched(const Layout layout, const Transpose a_transpose, const Transpose b_transpose,
const size_t m, const size_t n, const size_t k,
const T alpha,
const cl_mem a_buffer, const size_t a_offset, const size_t a_ld, const size_t a_stride,
const cl_mem b_buffer, const size_t b_offset, const size_t b_ld, const size_t b_stride,
const T beta,
cl_mem c_buffer, const size_t c_offset, const size_t c_ld, const size_t c_stride,
const size_t batch_count,
cl_command_queue* queue, cl_event* event = nullptr);
template <typename T>
StatusCode GemmTempBufferSize(const Layout layout, const Transpose a_transpose, const Transpose b_transpose,
const size_t m, const size_t n, const size_t k,
const size_t a_offset, const size_t a_ld,
const size_t b_offset, const size_t b_ld,
const size_t c_offset, const size_t c_ld,
cl_command_queue* queue, size_t& temp_buffer_size);
StatusCode PUBLIC_API ClearCache();
StatusCode PUBLIC_API FillCache(const cl_device_id device);
StatusCode PUBLIC_API RetrieveParameters(const cl_device_id device, const std::string &kernel_name,
const Precision precision,
std::unordered_map<std::string,size_t> &parameters);
StatusCode PUBLIC_API OverrideParameters(const cl_device_id device, const std::string &kernel_name,
const Precision precision,
const std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXaxpy(cl_command_queue* queue, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXdot(cl_command_queue* queue, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXgemv(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXger(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXgemm(cl_command_queue* queue, const size_t m, const size_t n, const size_t k,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneXgemmDirect(cl_command_queue* queue, const size_t m, const size_t n, const size_t k,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneCopy(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TunePad(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneTranspose(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TunePadtranspose(cl_command_queue* queue, const size_t m, const size_t n,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
template <typename T>
StatusCode TuneInvert(cl_command_queue* queue, const size_t m, const size_t n, const size_t k,
const double fraction, std::unordered_map<std::string,size_t> &parameters);
}
#endif