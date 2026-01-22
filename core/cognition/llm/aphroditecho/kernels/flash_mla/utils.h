#pragma once
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <cuda_bf16.h>
#include <cute/tensor.hpp>
#include <cutlass/array.h>
#include <cutlass/cutlass.h>
#include <cutlass/numeric_conversion.h>
#include <cutlass/numeric_types.h>
namespace flash {
template <typename T>
struct MaxOp {
__device__ __forceinline__ T operator()(T const& x, T const& y) {
return x > y ? x : y;
}
};
template <>
struct MaxOp<float> {
__device__ __forceinline__ float operator()(float const& x, float const& y) {
return max(x, y);
}
};
template <typename T>
struct SumOp {
__device__ __forceinline__ T operator()(T const& x, T const& y) {
return x + y;
}
};
template <int THREADS>
struct Allreduce {
static_assert(THREADS == 32 || THREADS == 16 || THREADS == 8 || THREADS == 4);
template <typename T, typename Operator>
static __device__ __forceinline__ T run(T x, Operator& op) {
constexpr int OFFSET = THREADS / 2;
x = op(x, __shfl_xor_sync(uint32_t(-1), x, OFFSET));
return Allreduce<OFFSET>::run(x, op);
}
};
template <>
struct Allreduce<2> {
template <typename T, typename Operator>
static __device__ __forceinline__ T run(T x, Operator& op) {
x = op(x, __shfl_xor_sync(uint32_t(-1), x, 1));
return x;
}
};
template <bool zero_init = false, int wg_wait = 0, bool arrive = true,
bool commit = true, typename Tensor0, typename Tensor1,
typename Tensor2, typename TiledMma>
__forceinline__ __device__ void gemm(TiledMma& tiled_mma, Tensor0 const& tCrA,
Tensor1 const& tCrB, Tensor2& tCrC) {
constexpr bool Is_RS = !cute::is_base_of<cute::GMMA::DescriptorIterator,
typename TiledMma::FrgTypeA>::value;
if constexpr (Is_RS) {
cute::warpgroup_fence_operand(const_cast<Tensor0&>(tCrA));
}
warpgroup_fence_operand(tCrC);
if constexpr (arrive) {
warpgroup_arrive();
}
if constexpr (zero_init) {
tiled_mma.accumulate_ = GMMA::ScaleOut::Zero;
CUTLASS_PRAGMA_UNROLL
for (int k_block = 0; k_block < size<2>(tCrA); ++k_block) {
cute::gemm(tiled_mma, tCrA(_, _, k_block), tCrB(_, _, k_block), tCrC);
tiled_mma.accumulate_ = GMMA::ScaleOut::One;
}
} else {
CUTLASS_PRAGMA_UNROLL
for (int k_block = 0; k_block < size<2>(tCrA); ++k_block) {
cute::gemm(tiled_mma, tCrA(_, _, k_block), tCrB(_, _, k_block), tCrC);
tiled_mma.accumulate_ = GMMA::ScaleOut::One;
}
}
if constexpr (commit) {
warpgroup_commit_batch();
}
if constexpr (wg_wait >= 0) {
warpgroup_wait<wg_wait>();
}
warpgroup_fence_operand(tCrC);
if constexpr (Is_RS) {
warpgroup_fence_operand(const_cast<Tensor0&>(tCrA));
}
}
template <bool Transposed = false, typename Layout0>
__forceinline__ __device__ auto convert_layout_acc_rowcol(Layout0 acc_layout) {
if constexpr (decltype(rank<0>(acc_layout))::value == 3) {
static_assert(decltype(size<0, 0>(acc_layout))::value == 2);
static_assert(decltype(size<0, 1>(acc_layout))::value == 2);
static_assert(decltype(rank(acc_layout))::value == 3);
auto l = acc_layout;
if constexpr (!Transposed) {
return make_layout(make_layout(get<0, 1>(l), get<1>(l)),
make_layout(get<0, 0>(l), get<0, 2>(l), get<2>(l)));
} else {
return make_layout(make_layout(get<0, 0>(l), get<0, 2>(l), get<2>(l)),
make_layout(get<0, 1>(l), get<1>(l)));
}
} else {
static_assert(decltype(size<0>(acc_layout))::value == 4);
static_assert(decltype(rank(acc_layout))::value == 3);
auto l = logical_divide(acc_layout, Shape<_2>{});
if constexpr (!Transposed) {
return make_layout(make_layout(get<0, 1>(l), get<1>(l)),
make_layout(get<0, 0>(l), get<2>(l)));
} else {
return make_layout(make_layout(get<0, 0>(l), get<2>(l)),
make_layout(get<0, 1>(l), get<1>(l)));
}
}
};
template <typename MMA_Traits, typename Layout0>
__forceinline__ __device__ auto convert_layout_acc_Aregs(Layout0 acc_layout) {
using X = Underscore;
if constexpr (decltype(rank<0>(acc_layout))::value == 3) {
static_assert(decltype(size<0, 0>(acc_layout))::value == 2);
static_assert(decltype(size<0, 1>(acc_layout))::value == 2);
static_assert(decltype(rank(acc_layout))::value == 3);
static_assert(decltype(rank(get<0>(acc_layout)))::value == 3);
if constexpr (sizeof(typename MMA_Traits::ValTypeA) == 2) {
auto l =
logical_divide(get<0, 2>(acc_layout), Tile<_2>{});
return make_layout(
make_layout(get<0, 0>(acc_layout), get<0, 1>(acc_layout),
get<0, 0>(l)),
get<1>(acc_layout),
coalesce(make_layout(get<0, 1>(l), get<2>(acc_layout))));
} else {
static_assert(sizeof(typename MMA_Traits::ValTypeA) == 1);
static_assert(decltype(stride<0, 0>(acc_layout))::value == 1);
static_assert(decltype(stride<0, 1>(acc_layout))::value == 2);
auto l =
logical_divide(get<0, 2>(acc_layout),
Tile<Layout<Shape<_2, _2>>>{});
return make_layout(
make_layout(Layout<_4>{}, get<0, 0, 0>(l), get<0, 0, 1>(l)),
get<1>(acc_layout),
coalesce(make_layout(
get<0, 1>(l),
get<2>(acc_layout))));
}
} else {
static_assert(decltype(size<0>(acc_layout))::value == 4);
static_assert(decltype(rank(acc_layout))::value == 3);
constexpr int mma_shape_K = get<2>(typename MMA_Traits::Shape_MNK{});
static_assert(mma_shape_K == 8 || mma_shape_K == 16);
if constexpr (mma_shape_K == 8) {
return acc_layout;
} else {
auto l = logical_divide(
acc_layout, Shape<X, X, _2>{});
return make_layout(make_layout(get<0>(l), get<2, 0>(l)), get<1>(l),
get<2, 1>(l));
}
}
};
template <typename To_type, typename Engine, typename Layout>
__forceinline__ __device__ auto convert_type(
Tensor<Engine, Layout> const& tensor) {
using From_type = typename Engine::value_type;
constexpr int numel = decltype(size(tensor))::value;
cutlass::NumericArrayConverter<To_type, From_type, numel> convert_op;
auto frag =
convert_op(*reinterpret_cast<const cutlass::Array<From_type, numel>*>(
tensor.data()));
return make_tensor(make_rmem_ptr<To_type>(&frag), tensor.layout());
}
template <int N>
CUTE_HOST_DEVICE void cp_async_wait() {
#if defined(CUTE_ARCH_CP_ASYNC_SM80_ENABLED)
asm volatile("cp.async.wait_group %0;\n" ::"n"(N));
#endif
}
template <bool Is_even_MN = true, bool Is_even_K = true,
bool Clear_OOB_MN = false, bool Clear_OOB_K = true,
typename TiledCopy, typename Engine0, typename Layout0,
typename Engine1, typename Layout1, typename Engine2,
typename Layout2, typename Engine3, typename Layout3>
__forceinline__ __device__ void copy(
TiledCopy tiled_copy, Tensor<Engine0, Layout0> const& S,
Tensor<Engine1, Layout1>& D, Tensor<Engine2, Layout2> const& identity_MN,
Tensor<Engine3, Layout3> const& predicate_K, const int max_MN = 0) {
CUTE_STATIC_ASSERT_V(rank(S) == Int<3>{});
CUTE_STATIC_ASSERT_V(rank(D) == Int<3>{});
CUTE_STATIC_ASSERT_V(size<0>(S) == size<0>(D));
CUTE_STATIC_ASSERT_V(size<1>(S) == size<1>(D));
CUTE_STATIC_ASSERT_V(size<2>(S) == size<2>(D));
static_assert(!(Clear_OOB_MN && !Clear_OOB_K));
#pragma unroll
for (int m = 0; m < size<1>(S); ++m) {
if (Is_even_MN || get<0>(identity_MN(0, m, 0)) < max_MN) {
#pragma unroll
for (int k = 0; k < size<2>(S); ++k) {
if (Is_even_K || predicate_K(k)) {
cute::copy(tiled_copy, S(_, m, k), D(_, m, k));
} else if (Clear_OOB_K) {
cute::clear(D(_, m, k));
}
}
} else if (Clear_OOB_MN) {
cute::clear(D(_, m, _));
}
}
}
}