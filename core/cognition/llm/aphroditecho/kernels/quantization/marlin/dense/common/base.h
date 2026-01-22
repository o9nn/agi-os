#pragma once
constexpr int ceildiv(int a, int b) { return (a + b - 1) / b; }
template <typename T, int n>
struct Vec {
T elems[n];
__device__ T& operator[](int i) { return elems[i]; }
};