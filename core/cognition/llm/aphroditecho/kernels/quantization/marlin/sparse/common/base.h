#pragma once
namespace marlin_24 {
constexpr int ceildiv(int a, int b) { return (a + b - 1) / b; }
template <typename T, int n>
struct Vec {
T elems[n];
__device__ T& operator[](int i) { return elems[i]; }
};
template <int M_, int N_, int K_>
struct ShapeBase {
static constexpr int M = M_, N = N_, K = K_;
};
using I4 = Vec<int, 4>;
using FragA = Vec<half2, 4>;
using FragB = Vec<half2, 2>;
using FragM = Vec<uint, 1>;
using FragC = Vec<float, 4>;
using FragS = Vec<half2, 1>;
}