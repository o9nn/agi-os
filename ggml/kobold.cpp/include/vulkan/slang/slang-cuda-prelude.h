#define SLANG_PRELUDE_EXPORT
#ifdef __CUDACC_RTC__
#define SLANG_CUDA_RTC 1
#else
#define SLANG_CUDA_RTC 0
#endif
#if SLANG_CUDA_RTC
#else
#include <cstdint>
#include <stdio.h>
#endif
#ifdef SLANG_CUDA_ENABLE_HALF
#define __CUDA_NO_HALF2_OPERATORS__
#include <cuda_fp16.h>
#endif
#ifdef SLANG_CUDA_ENABLE_OPTIX
#include <optix.h>
#endif
#ifndef SLANG_OFFSET_OF
#define SLANG_OFFSET_OF(type, member) (size_t)((char*)&(((type*)0)->member) - (char*)0)
#endif
#ifndef SLANG_ALIGN_OF
#define SLANG_ALIGN_OF(type) __alignof__(type)
#endif
#ifndef SLANG_INFINITY
#define SLANG_INFINITY ((float)(1e+300 * 1e+300))
#endif
#define SLANG_PRELUDE_ASSERT(x)
#ifndef SLANG_CUDA_WARP_SIZE
#define SLANG_CUDA_WARP_SIZE 32
#endif
#define SLANG_CUDA_WARP_MASK \
(SLANG_CUDA_WARP_SIZE - 1)
#define SLANG_CUDA_WARP_BITMASK (~int(0))
#define SLANG_FORCE_INLINE inline
#define SLANG_CUDA_CALL __device__
#define SLANG_FORCE_INLINE inline
#define SLANG_INLINE inline
#define SLANG_BOUND_ASSERT(index, count) SLANG_PRELUDE_ASSERT(index < count);
#define SLANG_BOUND_ASSERT_BYTE_ADDRESS(index, elemSize, sizeInBytes) \
SLANG_PRELUDE_ASSERT(index <= (sizeInBytes - elemSize) && (index & 3) == 0);
#define SLANG_BOUND_ZERO_INDEX(index, count) index = (index < count) ? index : 0;
#define SLANG_BOUND_ZERO_INDEX_BYTE_ADDRESS(index, elemSize, sizeInBytes) \
index = (index <= (sizeInBytes - elemSize)) ? index : 0;
#ifdef SLANG_ENABLE_BOUND_ZERO_INDEX
#define SLANG_BOUND_FIX(index, count) SLANG_BOUND_ZERO_INDEX(index, count)
#define SLANG_BOUND_FIX_BYTE_ADDRESS(index, elemSize, sizeInBytes) \
SLANG_BOUND_ZERO_INDEX_BYTE_ADDRESS(index, elemSize, sizeInBytes)
#define SLANG_BOUND_FIX_FIXED_ARRAY(index, count) \
SLANG_BOUND_ZERO_INDEX(index, count) SLANG_BOUND_ZERO_INDEX(index, count)
#else
#define SLANG_BOUND_FIX(index, count)
#define SLANG_BOUND_FIX_BYTE_ADDRESS(index, elemSize, sizeInBytes)
#define SLANG_BOUND_FIX_FIXED_ARRAY(index, count)
#endif
#ifndef SLANG_BOUND_CHECK
#define SLANG_BOUND_CHECK(index, count) \
SLANG_BOUND_ASSERT(index, count) SLANG_BOUND_FIX(index, count)
#endif
#ifndef SLANG_BOUND_CHECK_BYTE_ADDRESS
#define SLANG_BOUND_CHECK_BYTE_ADDRESS(index, elemSize, sizeInBytes) \
SLANG_BOUND_ASSERT_BYTE_ADDRESS(index, elemSize, sizeInBytes) \
SLANG_BOUND_FIX_BYTE_ADDRESS(index, elemSize, sizeInBytes)
#endif
#ifndef SLANG_BOUND_CHECK_FIXED_ARRAY
#define SLANG_BOUND_CHECK_FIXED_ARRAY(index, count) \
SLANG_BOUND_ASSERT(index, count) SLANG_BOUND_FIX_FIXED_ARRAY(index, count)
#endif
#ifndef SLANG_CUDA_BOUNDARY_MODE
#define SLANG_CUDA_BOUNDARY_MODE cudaBoundaryModeZero
#define SLANG_PTX_BOUNDARY_MODE "zero"
#endif
struct TypeInfo
{
size_t typeSize;
};
template<typename T, size_t SIZE>
struct FixedArray
{
SLANG_CUDA_CALL const T& operator[](size_t index) const
{
SLANG_BOUND_CHECK_FIXED_ARRAY(index, SIZE);
return m_data[index];
}
SLANG_CUDA_CALL T& operator[](size_t index)
{
SLANG_BOUND_CHECK_FIXED_ARRAY(index, SIZE);
return m_data[index];
}
T m_data[SIZE];
};
template<typename T>
struct Array
{
SLANG_CUDA_CALL const T& operator[](size_t index) const
{
SLANG_BOUND_CHECK(index, count);
return data[index];
}
SLANG_CUDA_CALL T& operator[](size_t index)
{
SLANG_BOUND_CHECK(index, count);
return data[index];
}
T* data;
size_t count;
};
typedef unsigned long long CUtexObject;
typedef unsigned long long CUsurfObject;
struct SamplerStateUnused;
typedef SamplerStateUnused* SamplerState;
typedef size_t NonUniformResourceIndex;
template<typename T, int ROWS, int COLS>
struct Matrix;
typedef int1 bool1;
typedef int2 bool2;
typedef int3 bool3;
typedef int4 bool4;
#if SLANG_CUDA_RTC
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef ptrdiff_t intptr_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef size_t uintptr_t;
#endif
typedef long long longlong;
typedef unsigned long long ulonglong;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
union Union32
{
uint32_t u;
int32_t i;
float f;
};
union Union64
{
uint64_t u;
int64_t i;
double d;
};
template<typename T>
SLANG_FORCE_INLINE SLANG_CUDA_CALL float make_float(T val)
{
return (float)val;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float _slang_fmod(float x, float y)
{
return ::fmodf(x, y);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double _slang_fmod(double x, double y)
{
return ::fmod(x, y);
}
#if SLANG_CUDA_ENABLE_HALF
struct __half1
{
__half x;
};
struct __align__(4) __half3
{
__half x, y, z;
};
struct __align__(4) __half4
{
__half x, y, z, w;
};
#endif
#define SLANG_VECTOR_GET_ELEMENT(T) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_vector_get_element(T##1 x, int index) \
{ \
return ((T*)(&x))[index]; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_vector_get_element(T##2 x, int index) \
{ \
return ((T*)(&x))[index]; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_vector_get_element(T##3 x, int index) \
{ \
return ((T*)(&x))[index]; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_vector_get_element(T##4 x, int index) \
{ \
return ((T*)(&x))[index]; \
}
SLANG_VECTOR_GET_ELEMENT(int)
SLANG_VECTOR_GET_ELEMENT(uint)
SLANG_VECTOR_GET_ELEMENT(short)
SLANG_VECTOR_GET_ELEMENT(ushort)
SLANG_VECTOR_GET_ELEMENT(char)
SLANG_VECTOR_GET_ELEMENT(uchar)
SLANG_VECTOR_GET_ELEMENT(longlong)
SLANG_VECTOR_GET_ELEMENT(ulonglong)
SLANG_VECTOR_GET_ELEMENT(float)
SLANG_VECTOR_GET_ELEMENT(double)
#define SLANG_VECTOR_GET_ELEMENT_PTR(T) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T* _slang_vector_get_element_ptr(T##1 * x, int index) \
{ \
return ((T*)(x)) + index; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T* _slang_vector_get_element_ptr(T##2 * x, int index) \
{ \
return ((T*)(x)) + index; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T* _slang_vector_get_element_ptr(T##3 * x, int index) \
{ \
return ((T*)(x)) + index; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T* _slang_vector_get_element_ptr(T##4 * x, int index) \
{ \
return ((T*)(x)) + index; \
}
SLANG_VECTOR_GET_ELEMENT_PTR(int)
SLANG_VECTOR_GET_ELEMENT_PTR(uint)
SLANG_VECTOR_GET_ELEMENT_PTR(short)
SLANG_VECTOR_GET_ELEMENT_PTR(ushort)
SLANG_VECTOR_GET_ELEMENT_PTR(char)
SLANG_VECTOR_GET_ELEMENT_PTR(uchar)
SLANG_VECTOR_GET_ELEMENT_PTR(longlong)
SLANG_VECTOR_GET_ELEMENT_PTR(ulonglong)
SLANG_VECTOR_GET_ELEMENT_PTR(float)
SLANG_VECTOR_GET_ELEMENT_PTR(double)
#if SLANG_CUDA_ENABLE_HALF
SLANG_VECTOR_GET_ELEMENT(__half)
SLANG_VECTOR_GET_ELEMENT_PTR(__half)
#endif
#define SLANG_CUDA_VECTOR_BINARY_OP(T, n, op) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##n operator op(T##n thisVal, T##n other) \
{ \
T##n result; \
for (int i = 0; i < n; i++) \
*_slang_vector_get_element_ptr(&result, i) = \
_slang_vector_get_element(thisVal, i) op _slang_vector_get_element(other, i); \
return result; \
}
#define SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, op) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool##n operator op(T##n thisVal, T##n other) \
{ \
bool##n result; \
for (int i = 0; i < n; i++) \
*_slang_vector_get_element_ptr(&result, i) = \
(int)(_slang_vector_get_element(thisVal, i) \
op _slang_vector_get_element(other, i)); \
return result; \
}
#define SLANG_CUDA_VECTOR_UNARY_OP(T, n, op) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##n operator op(T##n thisVal) \
{ \
T##n result; \
for (int i = 0; i < n; i++) \
*_slang_vector_get_element_ptr(&result, i) = op _slang_vector_get_element(thisVal, i); \
return result; \
}
#define SLANG_CUDA_VECTOR_INT_OP(T, n) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, +) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, -) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, *) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, /) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, %) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, ^) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, &) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, |) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, &&) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, ||) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, >>) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, <<) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, >) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, <) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, >=) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, <=) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, ==) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, !=) \
SLANG_CUDA_VECTOR_UNARY_OP(T, n, !) \
SLANG_CUDA_VECTOR_UNARY_OP(T, n, -) \
SLANG_CUDA_VECTOR_UNARY_OP(T, n, ~)
#define SLANG_CUDA_VECTOR_INT_OPS(T) \
SLANG_CUDA_VECTOR_INT_OP(T, 2) \
SLANG_CUDA_VECTOR_INT_OP(T, 3) \
SLANG_CUDA_VECTOR_INT_OP(T, 4)
SLANG_CUDA_VECTOR_INT_OPS(int)
SLANG_CUDA_VECTOR_INT_OPS(uint)
SLANG_CUDA_VECTOR_INT_OPS(ushort)
SLANG_CUDA_VECTOR_INT_OPS(short)
SLANG_CUDA_VECTOR_INT_OPS(char)
SLANG_CUDA_VECTOR_INT_OPS(uchar)
SLANG_CUDA_VECTOR_INT_OPS(longlong)
SLANG_CUDA_VECTOR_INT_OPS(ulonglong)
#define SLANG_CUDA_VECTOR_FLOAT_OP(T, n) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, +) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, -) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, *) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, /) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, &&) \
SLANG_CUDA_VECTOR_BINARY_OP(T, n, ||) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, >) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, <) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, >=) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, <=) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, ==) \
SLANG_CUDA_VECTOR_BINARY_COMPARE_OP(T, n, !=) \
SLANG_CUDA_VECTOR_UNARY_OP(T, n, -)
#define SLANG_CUDA_VECTOR_FLOAT_OPS(T) \
SLANG_CUDA_VECTOR_FLOAT_OP(T, 2) \
SLANG_CUDA_VECTOR_FLOAT_OP(T, 3) \
SLANG_CUDA_VECTOR_FLOAT_OP(T, 4)
SLANG_CUDA_VECTOR_FLOAT_OPS(float)
SLANG_CUDA_VECTOR_FLOAT_OPS(double)
#if SLANG_CUDA_ENABLE_HALF
SLANG_CUDA_VECTOR_FLOAT_OPS(__half)
#endif
#define SLANG_CUDA_FLOAT_VECTOR_MOD_IMPL(T, n) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##n operator%(const T##n& left, const T##n& right) \
{ \
T##n result; \
for (int i = 0; i < n; i++) \
*_slang_vector_get_element_ptr(&result, i) = _slang_fmod( \
_slang_vector_get_element(left, i), \
_slang_vector_get_element(right, i)); \
return result; \
}
#define SLANG_CUDA_FLOAT_VECTOR_MOD(T) \
SLANG_CUDA_FLOAT_VECTOR_MOD_IMPL(T, 2) \
SLANG_CUDA_FLOAT_VECTOR_MOD_IMPL(T, 3) \
SLANG_CUDA_FLOAT_VECTOR_MOD_IMPL(T, 4)
SLANG_CUDA_FLOAT_VECTOR_MOD(float)
SLANG_CUDA_FLOAT_VECTOR_MOD(double)
#if SLANG_CUDA_RTC || SLANG_CUDA_ENABLE_HALF
#define SLANG_MAKE_VECTOR(T) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##2 make_##T##2(T x, T y) \
{ \
return T##2 {x, y}; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##3 make_##T##3(T x, T y, T z) \
{ \
return T##3 {x, y, z}; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##4 make_##T##4(T x, T y, T z, T w) \
{ \
return T##4 {x, y, z, w}; \
}
#endif
#if SLANG_CUDA_RTC
SLANG_MAKE_VECTOR(int)
SLANG_MAKE_VECTOR(uint)
SLANG_MAKE_VECTOR(short)
SLANG_MAKE_VECTOR(ushort)
SLANG_MAKE_VECTOR(char)
SLANG_MAKE_VECTOR(uchar)
SLANG_MAKE_VECTOR(float)
SLANG_MAKE_VECTOR(double)
SLANG_MAKE_VECTOR(longlong)
SLANG_MAKE_VECTOR(ulonglong)
#endif
#if SLANG_CUDA_ENABLE_HALF
SLANG_MAKE_VECTOR(__half)
#endif
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool1 make_bool1(bool x)
{
return bool1{x};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool2 make_bool2(bool x, bool y)
{
return bool2{x, y};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool3 make_bool3(bool x, bool y, bool z)
{
return bool3{x, y, z};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool4 make_bool4(bool x, bool y, bool z, bool w)
{
return bool4{x, y, z, w};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool2 make_bool2(bool x)
{
return bool2{x, x};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool3 make_bool3(bool x)
{
return bool3{x, x, x};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool4 make_bool4(bool x)
{
return bool4{x, x, x, x};
}
#if SLANG_CUDA_RTC
#define SLANG_MAKE_VECTOR_FROM_SCALAR(T) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##1 make_##T##1(T x) \
{ \
return T##1 {x}; \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##2 make_##T##2(T x) \
{ \
return make_##T##2(x, x); \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##3 make_##T##3(T x) \
{ \
return make_##T##3(x, x, x); \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##4 make_##T##4(T x) \
{ \
return make_##T##4(x, x, x, x); \
}
#else
#define SLANG_MAKE_VECTOR_FROM_SCALAR(T) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##2 make_##T##2(T x) \
{ \
return make_##T##2(x, x); \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##3 make_##T##3(T x) \
{ \
return make_##T##3(x, x, x); \
} \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##4 make_##T##4(T x) \
{ \
return make_##T##4(x, x, x, x); \
}
#endif
SLANG_MAKE_VECTOR_FROM_SCALAR(int)
SLANG_MAKE_VECTOR_FROM_SCALAR(uint)
SLANG_MAKE_VECTOR_FROM_SCALAR(short)
SLANG_MAKE_VECTOR_FROM_SCALAR(ushort)
SLANG_MAKE_VECTOR_FROM_SCALAR(char)
SLANG_MAKE_VECTOR_FROM_SCALAR(uchar)
SLANG_MAKE_VECTOR_FROM_SCALAR(longlong)
SLANG_MAKE_VECTOR_FROM_SCALAR(ulonglong)
SLANG_MAKE_VECTOR_FROM_SCALAR(float)
SLANG_MAKE_VECTOR_FROM_SCALAR(double)
#if SLANG_CUDA_ENABLE_HALF
SLANG_MAKE_VECTOR_FROM_SCALAR(__half)
#if !SLANG_CUDA_RTC
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half1 make___half1(__half x)
{
return __half1{x};
}
#endif
#endif
#define SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(Fn, T, N) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T##N Fn(T##N* address, T##N val) \
{ \
T##N result; \
for (int i = 0; i < N; i++) \
*_slang_vector_get_element_ptr(&result, i) = \
Fn(_slang_vector_get_element_ptr(address, i), _slang_vector_get_element(val, i)); \
return result; \
}
#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ < 900
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, float, 2)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, float, 4)
#endif
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, float, 3)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, int, 2)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, int, 3)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, int, 4)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, uint, 2)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, uint, 3)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, uint, 4)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, ulonglong, 2)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, ulonglong, 3)
SLANG_CUDA_VECTOR_ATOMIC_BINARY_IMPL(atomicAdd, ulonglong, 4)
template<typename T, int n>
struct GetVectorTypeImpl
{
};
#define GET_VECTOR_TYPE_IMPL(T, n) \
template<> \
struct GetVectorTypeImpl<T, n> \
{ \
typedef T##n type; \
static SLANG_FORCE_INLINE SLANG_CUDA_CALL T##n fromScalar(T v) \
{ \
return make_##T##n(v); \
} \
};
#define GET_VECTOR_TYPE_IMPL_N(T) \
GET_VECTOR_TYPE_IMPL(T, 1) \
GET_VECTOR_TYPE_IMPL(T, 2) \
GET_VECTOR_TYPE_IMPL(T, 3) \
GET_VECTOR_TYPE_IMPL(T, 4)
GET_VECTOR_TYPE_IMPL_N(int)
GET_VECTOR_TYPE_IMPL_N(uint)
GET_VECTOR_TYPE_IMPL_N(short)
GET_VECTOR_TYPE_IMPL_N(ushort)
GET_VECTOR_TYPE_IMPL_N(char)
GET_VECTOR_TYPE_IMPL_N(uchar)
GET_VECTOR_TYPE_IMPL_N(longlong)
GET_VECTOR_TYPE_IMPL_N(ulonglong)
GET_VECTOR_TYPE_IMPL_N(float)
GET_VECTOR_TYPE_IMPL_N(double)
#if SLANG_CUDA_ENABLE_HALF
GET_VECTOR_TYPE_IMPL_N(__half)
#endif
template<typename T, int n>
using Vector = typename GetVectorTypeImpl<T, n>::type;
template<typename T, int n, typename OtherT, int m>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Vector<T, n> _slang_vector_reshape(const Vector<OtherT, m> other)
{
Vector<T, n> result;
for (int i = 0; i < n; i++)
{
OtherT otherElement = T(0);
if (i < m)
otherElement = _slang_vector_get_element(other, i);
*_slang_vector_get_element_ptr(&result, i) = (T)otherElement;
}
return result;
}
template<typename T, int ROWS, int COLS>
struct Matrix
{
Vector<T, COLS> rows[ROWS];
SLANG_FORCE_INLINE SLANG_CUDA_CALL Vector<T, COLS>& operator[](size_t index)
{
return rows[index];
}
};
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(T scalar)
{
Matrix<T, ROWS, COLS> result;
for (int i = 0; i < ROWS; i++)
result.rows[i] = GetVectorTypeImpl<T, COLS>::fromScalar(scalar);
return result;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(const Vector<T, COLS>& row0)
{
Matrix<T, ROWS, COLS> result;
result.rows[0] = row0;
return result;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
const Vector<T, COLS>& row0,
const Vector<T, COLS>& row1)
{
Matrix<T, ROWS, COLS> result;
result.rows[0] = row0;
result.rows[1] = row1;
return result;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
const Vector<T, COLS>& row0,
const Vector<T, COLS>& row1,
const Vector<T, COLS>& row2)
{
Matrix<T, ROWS, COLS> result;
result.rows[0] = row0;
result.rows[1] = row1;
result.rows[2] = row2;
return result;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
const Vector<T, COLS>& row0,
const Vector<T, COLS>& row1,
const Vector<T, COLS>& row2,
const Vector<T, COLS>& row3)
{
Matrix<T, ROWS, COLS> result;
result.rows[0] = row0;
result.rows[1] = row1;
result.rows[2] = row2;
result.rows[3] = row3;
return result;
}
template<typename T, int ROWS, int COLS, typename U, int otherRow, int otherCol>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
const Matrix<U, otherRow, otherCol>& other)
{
Matrix<T, ROWS, COLS> result;
int minRow = ROWS;
int minCol = COLS;
if (minRow > otherRow)
minRow = otherRow;
if (minCol > otherCol)
minCol = otherCol;
for (int i = 0; i < minRow; i++)
for (int j = 0; j < minCol; j++)
*_slang_vector_get_element_ptr(result.rows + i, j) =
(T)_slang_vector_get_element(other.rows[i], j);
return result;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(T v0, T v1, T v2, T v3)
{
Matrix<T, ROWS, COLS> rs;
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[1].x = v2;
rs.rows[1].y = v3;
return rs;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
T v0,
T v1,
T v2,
T v3,
T v4,
T v5)
{
Matrix<T, ROWS, COLS> rs;
if (COLS == 3)
{
*_slang_vector_get_element_ptr(&rs.rows[0], 0) = v0;
*_slang_vector_get_element_ptr(&rs.rows[0], 1) = v1;
*_slang_vector_get_element_ptr(&rs.rows[0], 2) = v2;
*_slang_vector_get_element_ptr(&rs.rows[1], 0) = v3;
*_slang_vector_get_element_ptr(&rs.rows[1], 1) = v4;
*_slang_vector_get_element_ptr(&rs.rows[1], 2) = v5;
}
else
{
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[1].x = v2;
rs.rows[1].y = v3;
rs.rows[2].x = v4;
rs.rows[2].y = v5;
}
return rs;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
T v0,
T v1,
T v2,
T v3,
T v4,
T v5,
T v6,
T v7)
{
Matrix<T, ROWS, COLS> rs;
if (COLS == 4)
{
*_slang_vector_get_element_ptr(&rs.rows[0], 0) = v0;
*_slang_vector_get_element_ptr(&rs.rows[0], 1) = v1;
*_slang_vector_get_element_ptr(&rs.rows[0], 2) = v2;
*_slang_vector_get_element_ptr(&rs.rows[0], 3) = v3;
*_slang_vector_get_element_ptr(&rs.rows[1], 0) = v4;
*_slang_vector_get_element_ptr(&rs.rows[1], 1) = v5;
*_slang_vector_get_element_ptr(&rs.rows[1], 2) = v6;
*_slang_vector_get_element_ptr(&rs.rows[1], 3) = v7;
}
else
{
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[1].x = v2;
rs.rows[1].y = v3;
rs.rows[2].x = v4;
rs.rows[2].y = v5;
rs.rows[3].x = v6;
rs.rows[3].y = v7;
}
return rs;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
T v0,
T v1,
T v2,
T v3,
T v4,
T v5,
T v6,
T v7,
T v8)
{
Matrix<T, ROWS, COLS> rs;
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[0].z = v2;
rs.rows[1].x = v3;
rs.rows[1].y = v4;
rs.rows[1].z = v5;
rs.rows[2].x = v6;
rs.rows[2].y = v7;
rs.rows[2].z = v8;
return rs;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
T v0,
T v1,
T v2,
T v3,
T v4,
T v5,
T v6,
T v7,
T v8,
T v9,
T v10,
T v11)
{
Matrix<T, ROWS, COLS> rs;
if (COLS == 4)
{
*_slang_vector_get_element_ptr(&rs.rows[0], 0) = v0;
*_slang_vector_get_element_ptr(&rs.rows[0], 1) = v1;
*_slang_vector_get_element_ptr(&rs.rows[0], 2) = v2;
*_slang_vector_get_element_ptr(&rs.rows[0], 3) = v3;
*_slang_vector_get_element_ptr(&rs.rows[1], 0) = v4;
*_slang_vector_get_element_ptr(&rs.rows[1], 1) = v5;
*_slang_vector_get_element_ptr(&rs.rows[1], 2) = v6;
*_slang_vector_get_element_ptr(&rs.rows[1], 3) = v7;
*_slang_vector_get_element_ptr(&rs.rows[2], 0) = v8;
*_slang_vector_get_element_ptr(&rs.rows[2], 1) = v9;
*_slang_vector_get_element_ptr(&rs.rows[2], 2) = v10;
*_slang_vector_get_element_ptr(&rs.rows[2], 3) = v11;
}
else
{
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[0].z = v2;
rs.rows[1].x = v3;
rs.rows[1].y = v4;
rs.rows[1].z = v5;
rs.rows[2].x = v6;
rs.rows[2].y = v7;
rs.rows[2].z = v8;
rs.rows[3].x = v9;
rs.rows[3].y = v10;
rs.rows[3].z = v11;
}
return rs;
}
template<typename T, int ROWS, int COLS>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, ROWS, COLS> makeMatrix(
T v0,
T v1,
T v2,
T v3,
T v4,
T v5,
T v6,
T v7,
T v8,
T v9,
T v10,
T v11,
T v12,
T v13,
T v14,
T v15)
{
Matrix<T, ROWS, COLS> rs;
rs.rows[0].x = v0;
rs.rows[0].y = v1;
rs.rows[0].z = v2;
rs.rows[0].w = v3;
rs.rows[1].x = v4;
rs.rows[1].y = v5;
rs.rows[1].z = v6;
rs.rows[1].w = v7;
rs.rows[2].x = v8;
rs.rows[2].y = v9;
rs.rows[2].z = v10;
rs.rows[2].w = v11;
rs.rows[3].x = v12;
rs.rows[3].y = v13;
rs.rows[3].z = v14;
rs.rows[3].w = v15;
return rs;
}
#define SLANG_MATRIX_BINARY_OP(T, op) \
template<int R, int C> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, R, C> operator op( \
const Matrix<T, R, C>& thisVal, \
const Matrix<T, R, C>& other) \
{ \
Matrix<T, R, C> result; \
for (int i = 0; i < R; i++) \
for (int j = 0; j < C; j++) \
*_slang_vector_get_element_ptr(result.rows + i, j) = \
_slang_vector_get_element(thisVal.rows[i], j) \
op _slang_vector_get_element(other.rows[i], j); \
return result; \
}
#define SLANG_MATRIX_UNARY_OP(T, op) \
template<int R, int C> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, R, C> operator op(const Matrix<T, R, C>& thisVal) \
{ \
Matrix<T, R, C> result; \
for (int i = 0; i < R; i++) \
for (int j = 0; j < C; j++) \
*_slang_vector_get_element_ptr(result.rows + i, j) = \
op _slang_vector_get_element(thisVal.rows[i], j); \
return result; \
}
#define SLANG_INT_MATRIX_OPS(T) \
SLANG_MATRIX_BINARY_OP(T, +) \
SLANG_MATRIX_BINARY_OP(T, -) \
SLANG_MATRIX_BINARY_OP(T, *) \
SLANG_MATRIX_BINARY_OP(T, /) \
SLANG_MATRIX_BINARY_OP(T, &) \
SLANG_MATRIX_BINARY_OP(T, |) \
SLANG_MATRIX_BINARY_OP(T, &&) \
SLANG_MATRIX_BINARY_OP(T, ||) \
SLANG_MATRIX_BINARY_OP(T, ^) \
SLANG_MATRIX_BINARY_OP(T, %) \
SLANG_MATRIX_UNARY_OP(T, !) \
SLANG_MATRIX_UNARY_OP(T, ~)
#define SLANG_FLOAT_MATRIX_OPS(T) \
SLANG_MATRIX_BINARY_OP(T, +) \
SLANG_MATRIX_BINARY_OP(T, -) \
SLANG_MATRIX_BINARY_OP(T, *) \
SLANG_MATRIX_BINARY_OP(T, /) \
SLANG_MATRIX_UNARY_OP(T, -)
SLANG_INT_MATRIX_OPS(int)
SLANG_INT_MATRIX_OPS(uint)
SLANG_INT_MATRIX_OPS(short)
SLANG_INT_MATRIX_OPS(ushort)
SLANG_INT_MATRIX_OPS(char)
SLANG_INT_MATRIX_OPS(uchar)
SLANG_INT_MATRIX_OPS(longlong)
SLANG_INT_MATRIX_OPS(ulonglong)
SLANG_FLOAT_MATRIX_OPS(float)
SLANG_FLOAT_MATRIX_OPS(double)
#if SLANG_CUDA_ENABLE_HALF
SLANG_FLOAT_MATRIX_OPS(__half)
#endif
#define SLANG_MATRIX_INT_NEG_OP(T) \
template<int R, int C> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, R, C> operator-(Matrix<T, R, C> thisVal) \
{ \
Matrix<T, R, C> result; \
for (int i = 0; i < R; i++) \
for (int j = 0; j < C; j++) \
*_slang_vector_get_element_ptr(result.rows + i, j) = \
0 - _slang_vector_get_element(thisVal.rows[i], j); \
return result; \
}
SLANG_MATRIX_INT_NEG_OP(int)
SLANG_MATRIX_INT_NEG_OP(uint)
SLANG_MATRIX_INT_NEG_OP(short)
SLANG_MATRIX_INT_NEG_OP(ushort)
SLANG_MATRIX_INT_NEG_OP(char)
SLANG_MATRIX_INT_NEG_OP(uchar)
SLANG_MATRIX_INT_NEG_OP(longlong)
SLANG_MATRIX_INT_NEG_OP(ulonglong)
#define SLANG_FLOAT_MATRIX_MOD(T) \
template<int R, int C> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<T, R, C> operator%( \
Matrix<T, R, C> left, \
Matrix<T, R, C> right) \
{ \
Matrix<T, R, C> result; \
for (int i = 0; i < R; i++) \
for (int j = 0; j < C; j++) \
*_slang_vector_get_element_ptr(result.rows + i, j) = _slang_fmod( \
_slang_vector_get_element(left.rows[i], j), \
_slang_vector_get_element(right.rows[i], j)); \
return result; \
}
SLANG_FLOAT_MATRIX_MOD(float)
SLANG_FLOAT_MATRIX_MOD(double)
#if SLANG_CUDA_ENABLE_HALF
template<int R, int C>
SLANG_FORCE_INLINE SLANG_CUDA_CALL Matrix<__half, R, C> operator%(
Matrix<__half, R, C> left,
Matrix<__half, R, C> right)
{
Matrix<__half, R, C> result;
for (int i = 0; i < R; i++)
for (int j = 0; j < C; j++)
*_slang_vector_get_element_ptr(result.rows + i, j) = __float2half(_slang_fmod(
__half2float(_slang_vector_get_element(left.rows[i], j)),
__half2float(_slang_vector_get_element(right.rows[i], j))));
return result;
}
#endif
#undef SLANG_FLOAT_MATRIX_MOD
#undef SLANG_MATRIX_BINARY_OP
#undef SLANG_MATRIX_UNARY_OP
#undef SLANG_INT_MATRIX_OPS
#undef SLANG_FLOAT_MATRIX_OPS
#undef SLANG_MATRIX_INT_NEG_OP
#undef SLANG_FLOAT_MATRIX_MOD
#define SLANG_SELECT_IMPL(T, N) \
SLANG_FORCE_INLINE SLANG_CUDA_CALL Vector<T, N> _slang_select( \
bool##N condition, \
Vector<T, N> v0, \
Vector<T, N> v1) \
{ \
Vector<T, N> result; \
for (int i = 0; i < N; i++) \
{ \
*_slang_vector_get_element_ptr(&result, i) = _slang_vector_get_element(condition, i) \
? _slang_vector_get_element(v0, i) \
: _slang_vector_get_element(v1, i); \
} \
return result; \
}
#define SLANG_SELECT_T(T) \
SLANG_SELECT_IMPL(T, 2) \
SLANG_SELECT_IMPL(T, 3) \
SLANG_SELECT_IMPL(T, 4)
SLANG_SELECT_T(int)
SLANG_SELECT_T(uint)
SLANG_SELECT_T(short)
SLANG_SELECT_T(ushort)
SLANG_SELECT_T(char)
SLANG_SELECT_T(uchar)
SLANG_SELECT_T(float)
SLANG_SELECT_T(double)
template<typename T>
SLANG_FORCE_INLINE SLANG_CUDA_CALL T _slang_select(bool condition, T v0, T v1)
{
return condition ? v0 : v1;
}
#if SLANG_CUDA_ENABLE_HALF
SLANG_SELECT_T(__half)
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half2 __ushort_as_half(const ushort2& i)
{
return __halves2half2(__ushort_as_half(i.x), __ushort_as_half(i.y));
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half3 __ushort_as_half(const ushort3& i)
{
return __half3{__ushort_as_half(i.x), __ushort_as_half(i.y), __ushort_as_half(i.z)};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half4 __ushort_as_half(const ushort4& i)
{
return __half4{
__ushort_as_half(i.x),
__ushort_as_half(i.y),
__ushort_as_half(i.z),
__ushort_as_half(i.w)};
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL ushort2 __half_as_ushort(const __half2& i)
{
return make_ushort2(__half_as_ushort(i.x), __half_as_ushort(i.y));
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL ushort3 __half_as_ushort(const __half3& i)
{
return make_ushort3(__half_as_ushort(i.x), __half_as_ushort(i.y), __half_as_ushort(i.z));
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL ushort4 __half_as_ushort(const __half4& i)
{
return make_ushort4(
__half_as_ushort(i.x),
__half_as_ushort(i.y),
__half_as_ushort(i.z),
__half_as_ushort(i.w));
}
template<>
struct __nv_isurf_trait<__half>
{
typedef void type;
};
template<>
struct __nv_isurf_trait<__half2>
{
typedef void type;
};
template<>
struct __nv_isurf_trait<__half4>
{
typedef void type;
};
#define SLANG_DROP_PARENS(...) __VA_ARGS__
#define SLANG_SURFACE_READ(FUNC_NAME, TYPE_ARGS, ARGS) \
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half FUNC_NAME<__half>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
return __ushort_as_half(FUNC_NAME<ushort>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half2 FUNC_NAME<__half2>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
return __ushort_as_half( \
FUNC_NAME<ushort2>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL __half4 FUNC_NAME<__half4>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
return __ushort_as_half( \
FUNC_NAME<ushort4>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
}
SLANG_SURFACE_READ(surf1Dread, (int x), (x))
SLANG_SURFACE_READ(surf2Dread, (int x, int y), (x, y))
SLANG_SURFACE_READ(surf3Dread, (int x, int y, int z), (x, y, z))
SLANG_SURFACE_READ(surf1DLayeredread, (int x, int layer), (x, layer))
SLANG_SURFACE_READ(surf2DLayeredread, (int x, int y, int layer), (x, y, layer))
SLANG_SURFACE_READ(surfCubemapread, (int x, int y, int face), (x, y, face))
SLANG_SURFACE_READ(surfCubemapLayeredread, (int x, int y, int layerFace), (x, y, layerFace))
#define SLANG_SURFACE_WRITE(FUNC_NAME, TYPE_ARGS, ARGS) \
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL void FUNC_NAME<__half>( \
__half data, \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
FUNC_NAME<ushort>(__half_as_ushort(data), surfObj, SLANG_DROP_PARENS ARGS, boundaryMode); \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL void FUNC_NAME<__half2>( \
__half2 data, \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
FUNC_NAME<ushort2>(__half_as_ushort(data), surfObj, SLANG_DROP_PARENS ARGS, boundaryMode); \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL void FUNC_NAME<__half4>( \
__half4 data, \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
FUNC_NAME<ushort4>(__half_as_ushort(data), surfObj, SLANG_DROP_PARENS ARGS, boundaryMode); \
}
SLANG_SURFACE_WRITE(surf1Dwrite, (int x), (x))
SLANG_SURFACE_WRITE(surf2Dwrite, (int x, int y), (x, y))
SLANG_SURFACE_WRITE(surf3Dwrite, (int x, int y, int z), (x, y, z))
SLANG_SURFACE_WRITE(surf1DLayeredwrite, (int x, int layer), (x, layer))
SLANG_SURFACE_WRITE(surf2DLayeredwrite, (int x, int y, int layer), (x, y, layer))
SLANG_SURFACE_WRITE(surfCubemapwrite, (int x, int y, int face), (x, y, face))
SLANG_SURFACE_WRITE(surfCubemapLayeredwrite, (int x, int y, int layerFace), (x, y, layerFace))
#define SLANG_SURFACE_READ_HALF_CONVERT(FUNC_NAME, TYPE_ARGS, ARGS) \
\
template<typename T> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL T FUNC_NAME##_convert( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode); \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL float FUNC_NAME##_convert<float>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
return __ushort_as_half( \
FUNC_NAME<uint16_t>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL float2 FUNC_NAME##_convert<float2>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
const __half2 v = \
__ushort_as_half(FUNC_NAME<ushort2>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
return float2{v.x, v.y}; \
} \
\
template<> \
SLANG_FORCE_INLINE SLANG_CUDA_CALL float4 FUNC_NAME##_convert<float4>( \
cudaSurfaceObject_t surfObj, \
SLANG_DROP_PARENS TYPE_ARGS, \
cudaSurfaceBoundaryMode boundaryMode) \
{ \
const __half4 v = \
__ushort_as_half(FUNC_NAME<ushort4>(surfObj, SLANG_DROP_PARENS ARGS, boundaryMode)); \
return float4{v.x, v.y, v.z, v.w}; \
}
SLANG_SURFACE_READ_HALF_CONVERT(surf1Dread, (int x), (x))
SLANG_SURFACE_READ_HALF_CONVERT(surf2Dread, (int x, int y), (x, y))
SLANG_SURFACE_READ_HALF_CONVERT(surf3Dread, (int x, int y, int z), (x, y, z))
#endif
template<typename T>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf1Dwrite_convert(
T,
cudaSurfaceObject_t surfObj,
int x,
cudaSurfaceBoundaryMode boundaryMode);
template<typename T>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf2Dwrite_convert(
T,
cudaSurfaceObject_t surfObj,
int x,
int y,
cudaSurfaceBoundaryMode boundaryMode);
template<typename T>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf3Dwrite_convert(
T,
cudaSurfaceObject_t surfObj,
int x,
int y,
int z,
cudaSurfaceBoundaryMode boundaryMode);
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf1Dwrite_convert<float>(
float v,
cudaSurfaceObject_t surfObj,
int x,
cudaSurfaceBoundaryMode boundaryMode)
{
asm volatile(
"{sust.p.1d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1}], {%2};}\n\t" ::"l"(surfObj),
"r"(x),
"f"(v));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf2Dwrite_convert<float>(
float v,
cudaSurfaceObject_t surfObj,
int x,
int y,
cudaSurfaceBoundaryMode boundaryMode)
{
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1,%2}], {%3};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"f"(v));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf3Dwrite_convert<float>(
float v,
cudaSurfaceObject_t surfObj,
int x,
int y,
int z,
cudaSurfaceBoundaryMode boundaryMode)
{
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1,%2,%3}], {%4};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"r"(z),
"f"(v));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf1Dwrite_convert<float2>(
float2 v,
cudaSurfaceObject_t surfObj,
int x,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y;
asm volatile(
"{sust.p.1d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1}], {%2,%3};}\n\t" ::"l"(surfObj),
"r"(x),
"f"(vx),
"f"(vy));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf2Dwrite_convert<float2>(
float2 v,
cudaSurfaceObject_t surfObj,
int x,
int y,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y;
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1,%2}], {%3,%4};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"f"(vx),
"f"(vy));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf3Dwrite_convert<float2>(
float2 v,
cudaSurfaceObject_t surfObj,
int x,
int y,
int z,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y;
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1,%2,%3}], {%4,%5};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"r"(z),
"f"(vx),
"f"(vy));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf1Dwrite_convert<float4>(
float4 v,
cudaSurfaceObject_t surfObj,
int x,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y, vz = v.z, vw = v.w;
asm volatile(
"{sust.p.1d.b32." SLANG_PTX_BOUNDARY_MODE " [%0, {%1}], {%2,%3,%4,%5};}\n\t" ::"l"(surfObj),
"r"(x),
"f"(vx),
"f"(vy),
"f"(vz),
"f"(vw));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf2Dwrite_convert<float4>(
float4 v,
cudaSurfaceObject_t surfObj,
int x,
int y,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y, vz = v.z, vw = v.w;
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE
" [%0, {%1,%2}], {%3,%4,%5,%6};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"f"(vx),
"f"(vy),
"f"(vz),
"f"(vw));
}
template<>
SLANG_FORCE_INLINE SLANG_CUDA_CALL void surf3Dwrite_convert<float4>(
float4 v,
cudaSurfaceObject_t surfObj,
int x,
int y,
int z,
cudaSurfaceBoundaryMode boundaryMode)
{
const float vx = v.x, vy = v.y, vz = v.z, vw = v.w;
asm volatile(
"{sust.p.2d.b32." SLANG_PTX_BOUNDARY_MODE
" [%0, {%1,%2,%3}], {%4,%5,%6,%7};}\n\t" ::"l"(surfObj),
"r"(x),
"r"(y),
"r"(z),
"f"(vx),
"f"(vy),
"f"(vz),
"f"(vw));
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_ceil(float f)
{
return ::ceilf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_floor(float f)
{
return ::floorf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_round(float f)
{
return ::roundf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_sin(float f)
{
return ::sinf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_cos(float f)
{
return ::cosf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL void F32_sincos(float f, float* s, float* c)
{
::sincosf(f, s, c);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_tan(float f)
{
return ::tanf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_asin(float f)
{
return ::asinf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_acos(float f)
{
return ::acosf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_atan(float f)
{
return ::atanf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_sinh(float f)
{
return ::sinhf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_cosh(float f)
{
return ::coshf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_tanh(float f)
{
return ::tanhf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_log2(float f)
{
return ::log2f(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_log(float f)
{
return ::logf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_log10(float f)
{
return ::log10f(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_exp2(float f)
{
return ::exp2f(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_exp(float f)
{
return ::expf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_abs(float f)
{
return ::fabsf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_trunc(float f)
{
return ::truncf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_sqrt(float f)
{
return ::sqrtf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_rsqrt(float f)
{
return ::rsqrtf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_sign(float f)
{
return (f == 0.0f) ? f : ((f < 0.0f) ? -1.0f : 1.0f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_frac(float f)
{
return f - F32_floor(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F32_isnan(float f)
{
return isnan(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F32_isfinite(float f)
{
return isfinite(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F32_isinf(float f)
{
return isinf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_min(float a, float b)
{
return ::fminf(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_max(float a, float b)
{
return ::fmaxf(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_pow(float a, float b)
{
return ::powf(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_fmod(float a, float b)
{
return ::fmodf(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_remainder(float a, float b)
{
return ::remainderf(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_atan2(float a, float b)
{
return float(::atan2(a, b));
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_frexp(float x, int* e)
{
return frexpf(x, e);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_modf(float x, float* ip)
{
return ::modff(x, ip);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t F32_asuint(float f)
{
Union32 u;
u.f = f;
return u.u;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int32_t F32_asint(float f)
{
Union32 u;
u.f = f;
return u.i;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float F32_fma(float a, float b, float c)
{
return ::fmaf(a, b, c);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_ceil(double f)
{
return ::ceil(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_floor(double f)
{
return ::floor(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_round(double f)
{
return ::round(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_sin(double f)
{
return ::sin(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_cos(double f)
{
return ::cos(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL void F64_sincos(double f, double* s, double* c)
{
::sincos(f, s, c);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_tan(double f)
{
return ::tan(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_asin(double f)
{
return ::asin(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_acos(double f)
{
return ::acos(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_atan(double f)
{
return ::atan(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_sinh(double f)
{
return ::sinh(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_cosh(double f)
{
return ::cosh(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_tanh(double f)
{
return ::tanh(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_log2(double f)
{
return ::log2(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_log(double f)
{
return ::log(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_log10(float f)
{
return ::log10(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_exp2(double f)
{
return ::exp2(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_exp(double f)
{
return ::exp(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_abs(double f)
{
return ::fabs(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_trunc(double f)
{
return ::trunc(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_sqrt(double f)
{
return ::sqrt(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_rsqrt(double f)
{
return ::rsqrt(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_sign(double f)
{
return (f == 0.0) ? f : ((f < 0.0) ? -1.0 : 1.0);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_frac(double f)
{
return f - F64_floor(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F64_isnan(double f)
{
return isnan(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F64_isfinite(double f)
{
return isfinite(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL bool F64_isinf(double f)
{
return isinf(f);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_min(double a, double b)
{
return ::fmin(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_max(double a, double b)
{
return ::fmax(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_pow(double a, double b)
{
return ::pow(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_fmod(double a, double b)
{
return ::fmod(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_remainder(double a, double b)
{
return ::remainder(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_atan2(double a, double b)
{
return ::atan2(a, b);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_frexp(double x, int* e)
{
return ::frexp(x, e);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_modf(double x, double* ip)
{
return ::modf(x, ip);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL void F64_asuint(double d, uint32_t* low, uint32_t* hi)
{
Union64 u;
u.d = d;
*low = uint32_t(u.u);
*hi = uint32_t(u.u >> 32);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL void F64_asint(double d, int32_t* low, int32_t* hi)
{
Union64 u;
u.d = d;
*low = int32_t(u.u);
*hi = int32_t(u.u >> 32);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double F64_fma(double a, double b, double c)
{
return ::fma(a, b, c);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int32_t I32_abs(int32_t f)
{
return (f < 0) ? -f : f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int32_t I32_min(int32_t a, int32_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int32_t I32_max(int32_t a, int32_t b)
{
return a > b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float I32_asfloat(int32_t x)
{
Union32 u;
u.i = x;
return u.f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t I32_asuint(int32_t x)
{
return uint32_t(x);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double I32_asdouble(int32_t low, int32_t hi)
{
Union64 u;
u.u = (uint64_t(hi) << 32) | uint32_t(low);
return u.d;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U32_abs(uint32_t f)
{
return f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U32_min(uint32_t a, uint32_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U32_max(uint32_t a, uint32_t b)
{
return a > b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL float U32_asfloat(uint32_t x)
{
Union32 u;
u.u = x;
return u.f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U32_asint(int32_t x)
{
return uint32_t(x);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL double U32_asdouble(uint32_t low, uint32_t hi)
{
Union64 u;
u.u = (uint64_t(hi) << 32) | low;
return u.d;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U32_countbits(uint32_t v)
{
return __popc(v);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t I64_abs(int64_t f)
{
return (f < 0) ? -f : f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t I64_min(int64_t a, int64_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t I64_max(int64_t a, int64_t b)
{
return a > b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t U64_abs(uint64_t f)
{
return f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t U64_min(uint64_t a, uint64_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL int64_t U64_max(uint64_t a, uint64_t b)
{
return a > b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uint32_t U64_countbits(uint64_t v)
{
return __popcll(v);
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL intptr_t IPTR_abs(intptr_t f)
{
return (f < 0) ? -f : f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL intptr_t IPTR_min(intptr_t a, intptr_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL intptr_t IPTR_max(intptr_t a, intptr_t b)
{
return a > b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uintptr_t UPTR_abs(uintptr_t f)
{
return f;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uintptr_t UPTR_min(uintptr_t a, uintptr_t b)
{
return a < b ? a : b;
}
SLANG_FORCE_INLINE SLANG_CUDA_CALL uintptr_t UPTR_max(uintptr_t a, uintptr_t b)
{
return a > b ? a : b;
}
template<typename T>
struct StructuredBuffer
{
SLANG_CUDA_CALL const T& operator[](size_t index) const
{
#ifndef SLANG_CUDA_STRUCTURED_BUFFER_NO_COUNT
SLANG_BOUND_CHECK(index, count);
#endif
return data[index];
}
SLANG_CUDA_CALL const T& Load(size_t index) const
{
#ifndef SLANG_CUDA_STRUCTURED_BUFFER_NO_COUNT
SLANG_BOUND_CHECK(index, count);
#endif
return data[index];
}
#ifndef SLANG_CUDA_STRUCTURED_BUFFER_NO_COUNT
SLANG_CUDA_CALL void GetDimensions(uint32_t* outNumStructs, uint32_t* outStride)
{
*outNumStructs = uint32_t(count);
*outStride = uint32_t(sizeof(T));
}
#endif
T* data;
#ifndef SLANG_CUDA_STRUCTURED_BUFFER_NO_COUNT
size_t count;
#endif
};
template<typename T>
struct RWStructuredBuffer : StructuredBuffer<T>
{
SLANG_CUDA_CALL T& operator[](size_t index) const
{
#ifndef SLANG_CUDA_STRUCTURED_BUFFER_NO_COUNT
SLANG_BOUND_CHECK(index, this->count);
#endif
return this->data[index];
}
};
struct ByteAddressBuffer
{
SLANG_CUDA_CALL void GetDimensions(uint32_t* outDim) const { *outDim = uint32_t(sizeInBytes); }
SLANG_CUDA_CALL uint32_t Load(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 4, sizeInBytes);
return data[index >> 2];
}
SLANG_CUDA_CALL uint2 Load2(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 8, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint2{data[dataIdx], data[dataIdx + 1]};
}
SLANG_CUDA_CALL uint3 Load3(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 12, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint3{data[dataIdx], data[dataIdx + 1], data[dataIdx + 2]};
}
SLANG_CUDA_CALL uint4 Load4(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 16, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint4{data[dataIdx], data[dataIdx + 1], data[dataIdx + 2], data[dataIdx + 3]};
}
template<typename T>
SLANG_CUDA_CALL T Load(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, sizeof(T), sizeInBytes);
T data;
memcpy(&data, ((const char*)this->data) + index, sizeof(T));
return data;
}
template<typename T>
SLANG_CUDA_CALL StructuredBuffer<T> asStructuredBuffer() const
{
StructuredBuffer<T> rs;
rs.data = (T*)data;
rs.count = sizeInBytes / sizeof(T);
return rs;
}
const uint32_t* data;
size_t sizeInBytes;
};
struct RWByteAddressBuffer
{
SLANG_CUDA_CALL void GetDimensions(uint32_t* outDim) const { *outDim = uint32_t(sizeInBytes); }
SLANG_CUDA_CALL uint32_t Load(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 4, sizeInBytes);
return data[index >> 2];
}
SLANG_CUDA_CALL uint2 Load2(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 8, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint2{data[dataIdx], data[dataIdx + 1]};
}
SLANG_CUDA_CALL uint3 Load3(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 12, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint3{data[dataIdx], data[dataIdx + 1], data[dataIdx + 2]};
}
SLANG_CUDA_CALL uint4 Load4(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 16, sizeInBytes);
const size_t dataIdx = index >> 2;
return uint4{data[dataIdx], data[dataIdx + 1], data[dataIdx + 2], data[dataIdx + 3]};
}
template<typename T>
SLANG_CUDA_CALL T Load(size_t index) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, sizeof(T), sizeInBytes);
T data;
memcpy(&data, ((const char*)this->data) + index, sizeof(T));
return data;
}
SLANG_CUDA_CALL void Store(size_t index, uint32_t v) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 4, sizeInBytes);
data[index >> 2] = v;
}
SLANG_CUDA_CALL void Store2(size_t index, uint2 v) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 8, sizeInBytes);
const size_t dataIdx = index >> 2;
data[dataIdx + 0] = v.x;
data[dataIdx + 1] = v.y;
}
SLANG_CUDA_CALL void Store3(size_t index, uint3 v) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 12, sizeInBytes);
const size_t dataIdx = index >> 2;
data[dataIdx + 0] = v.x;
data[dataIdx + 1] = v.y;
data[dataIdx + 2] = v.z;
}
SLANG_CUDA_CALL void Store4(size_t index, uint4 v) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, 16, sizeInBytes);
const size_t dataIdx = index >> 2;
data[dataIdx + 0] = v.x;
data[dataIdx + 1] = v.y;
data[dataIdx + 2] = v.z;
data[dataIdx + 3] = v.w;
}
template<typename T>
SLANG_CUDA_CALL void Store(size_t index, T const& value) const
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, sizeof(T), sizeInBytes);
memcpy((char*)data + index, &value, sizeof(T));
}
template<typename T>
SLANG_CUDA_CALL T* _getPtrAt(size_t index)
{
SLANG_BOUND_CHECK_BYTE_ADDRESS(index, sizeof(T), sizeInBytes);
return (T*)(((char*)data) + index);
}
template<typename T>
SLANG_CUDA_CALL RWStructuredBuffer<T> asStructuredBuffer() const
{
RWStructuredBuffer<T> rs;
rs.data = (T*)data;
rs.count = sizeInBytes / sizeof(T);
return rs;
}
uint32_t* data;
size_t sizeInBytes;
};
#ifndef SLANG_USE_ASM_LANE_ID
__forceinline__ __device__ uint32_t _getLaneId()
{
return threadIdx.x & SLANG_CUDA_WARP_MASK;
}
#else
__forceinline__ __device__ uint32_t _getLaneId()
{
unsigned ret;
asm volatile("mov.u32 %0, %laneid;" : "=r"(ret));
return ret;
}
#endif
typedef int WarpMask;
__forceinline__ __device__ WarpMask _getLaneLtMask()
{
return (int(1) << _getLaneId()) - 1;
}
__forceinline__ __device__ WarpMask _getActiveMask()
{
return __ballot_sync(__activemask(), true);
}
__forceinline__ __device__ WarpMask _getMultiPrefixMask(int mask)
{
return mask;
}
__inline__ __device__ bool _waveIsSingleLane(WarpMask mask)
{
return (mask & (mask - 1)) == 0;
}
__inline__ __device__ int _waveCalcPow2Offset(WarpMask mask)
{
if (mask == SLANG_CUDA_WARP_BITMASK)
{
return SLANG_CUDA_WARP_SIZE;
}
if ((mask & (mask + 1)) == 0)
{
const int offset = 32 - __clz(mask);
if ((offset & (offset - 1)) == 0)
{
return offset;
}
}
return 0;
}
__inline__ __device__ bool _waveIsFirstLane()
{
const WarpMask mask = __activemask();
return (mask & 1) || ((__ffs(mask) - 1) == _getLaneId());
}
template<typename T>
struct WaveOpOr
{
__inline__ __device__ static T getInitial(T a) { return 0; }
__inline__ __device__ static T doOp(T a, T b) { return a | b; }
};
template<typename T>
struct WaveOpAnd
{
__inline__ __device__ static T getInitial(T a) { return ~T(0); }
__inline__ __device__ static T doOp(T a, T b) { return a & b; }
};
template<typename T>
struct WaveOpXor
{
__inline__ __device__ static T getInitial(T a) { return 0; }
__inline__ __device__ static T doOp(T a, T b) { return a ^ b; }
__inline__ __device__ static T doInverse(T a, T b) { return a ^ b; }
};
template<typename T>
struct WaveOpAdd
{
__inline__ __device__ static T getInitial(T a) { return 0; }
__inline__ __device__ static T doOp(T a, T b) { return a + b; }
__inline__ __device__ static T doInverse(T a, T b) { return a - b; }
};
template<typename T>
struct WaveOpMul
{
__inline__ __device__ static T getInitial(T a) { return T(1); }
__inline__ __device__ static T doOp(T a, T b) { return a * b; }
__inline__ __device__ static T doInverse(T a, T b) { return a / b; }
};
template<typename T>
struct WaveOpMax
{
__inline__ __device__ static T getInitial(T a) { return a; }
__inline__ __device__ static T doOp(T a, T b) { return a > b ? a : b; }
};
template<typename T>
struct WaveOpMin
{
__inline__ __device__ static T getInitial(T a) { return a; }
__inline__ __device__ static T doOp(T a, T b) { return a < b ? a : b; }
};
template<typename T>
struct ElementTypeTrait;
template<>
struct ElementTypeTrait<int>
{
typedef int Type;
};
template<>
struct ElementTypeTrait<uint>
{
typedef uint Type;
};
template<>
struct ElementTypeTrait<float>
{
typedef float Type;
};
template<>
struct ElementTypeTrait<double>
{
typedef double Type;
};
template<>
struct ElementTypeTrait<uint64_t>
{
typedef uint64_t Type;
};
template<>
struct ElementTypeTrait<int64_t>
{
typedef int64_t Type;
};
template<>
struct ElementTypeTrait<int1>
{
typedef int Type;
};
template<>
struct ElementTypeTrait<int2>
{
typedef int Type;
};
template<>
struct ElementTypeTrait<int3>
{
typedef int Type;
};
template<>
struct ElementTypeTrait<int4>
{
typedef int Type;
};
template<>
struct ElementTypeTrait<uint1>
{
typedef uint Type;
};
template<>
struct ElementTypeTrait<uint2>
{
typedef uint Type;
};
template<>
struct ElementTypeTrait<uint3>
{
typedef uint Type;
};
template<>
struct ElementTypeTrait<uint4>
{
typedef uint Type;
};
template<>
struct ElementTypeTrait<float1>
{
typedef float Type;
};
template<>
struct ElementTypeTrait<float2>
{
typedef float Type;
};
template<>
struct ElementTypeTrait<float3>
{
typedef float Type;
};
template<>
struct ElementTypeTrait<float4>
{
typedef float Type;
};
template<>
struct ElementTypeTrait<double1>
{
typedef double Type;
};
template<>
struct ElementTypeTrait<double2>
{
typedef double Type;
};
template<>
struct ElementTypeTrait<double3>
{
typedef double Type;
};
template<>
struct ElementTypeTrait<double4>
{
typedef double Type;
};
template<typename T, int ROWS, int COLS>
struct ElementTypeTrait<Matrix<T, ROWS, COLS>>
{
typedef T Type;
};
template<typename INTF, typename T>
__device__ T _waveReduceScalar(WarpMask mask, T val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
if (offsetSize > 0)
{
for (int offset = offsetSize >> 1; offset > 0; offset >>= 1)
{
val = INTF::doOp(val, __shfl_xor_sync(mask, val, offset));
}
}
else if (!_waveIsSingleLane(mask))
{
T result = INTF::getInitial(val);
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
result = INTF::doOp(result, __shfl_sync(mask, val, srcLane));
remaining &= ~laneBit;
}
return result;
}
return val;
}
template<typename INTF, typename T, size_t COUNT>
__device__ void _waveReduceMultiple(WarpMask mask, T* val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
if (offsetSize > 0)
{
for (int offset = offsetSize >> 1; offset > 0; offset >>= 1)
{
for (size_t i = 0; i < COUNT; ++i)
{
val[i] = INTF::doOp(val[i], __shfl_xor_sync(mask, val[i], offset));
}
}
}
else if (!_waveIsSingleLane(mask))
{
T originalVal[COUNT];
for (size_t i = 0; i < COUNT; ++i)
{
const T v = val[i];
originalVal[i] = v;
val[i] = INTF::getInitial(v);
}
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
for (size_t i = 0; i < COUNT; ++i)
{
val[i] = INTF::doOp(val[i], __shfl_sync(mask, originalVal[i], srcLane));
}
remaining &= ~laneBit;
}
}
}
template<typename INTF, typename T>
__device__ void _waveReduceMultiple(WarpMask mask, T* val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<INTF, ElemType, sizeof(T) / sizeof(ElemType)>(mask, (ElemType*)val);
}
template<typename T>
__inline__ __device__ T _waveOr(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpOr<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveAnd(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpAnd<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveXor(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpXor<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveProduct(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpMul<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveSum(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpAdd<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveMin(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpMin<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _waveMax(WarpMask mask, T val)
{
return _waveReduceScalar<WaveOpMax<T>, T>(mask, val);
}
#if __CUDA_ARCH__ >= 800
template<>
__inline__ __device__ unsigned _waveOr<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_or_sync(mask, val);
}
template<>
__inline__ __device__ unsigned _waveAnd<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_and_sync(mask, val);
}
template<>
__inline__ __device__ unsigned _waveXor<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_xor_sync(mask, val);
}
template<>
__inline__ __device__ unsigned _waveSum<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_add_sync(mask, val);
}
template<>
__inline__ __device__ int _waveSum<int>(WarpMask mask, int val)
{
return __reduce_add_sync(mask, val);
}
template<>
__inline__ __device__ unsigned _waveMin<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_min_sync(mask, val);
}
template<>
__inline__ __device__ int _waveMin<int>(WarpMask mask, int val)
{
return __reduce_min_sync(mask, val);
}
template<>
__inline__ __device__ unsigned _waveMax<unsigned>(WarpMask mask, unsigned val)
{
return __reduce_max_sync(mask, val);
}
template<>
__inline__ __device__ int _waveMax<int>(WarpMask mask, int val)
{
return __reduce_max_sync(mask, val);
}
#endif
template<typename T>
__inline__ __device__ T _waveOrMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpOr<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveAndMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpAnd<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveXorMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpXor<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveProductMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpMul<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveSumMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpAdd<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveMinMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpMin<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ T _waveMaxMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_waveReduceMultiple<WaveOpMax<ElemType>>(mask, &val);
return val;
}
template<typename T>
__inline__ __device__ bool _waveAllEqual(WarpMask mask, T val)
{
int pred;
__match_all_sync(mask, val, &pred);
return pred != 0;
}
template<typename T>
__inline__ __device__ bool _waveAllEqualMultiple(WarpMask mask, T inVal)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
const size_t count = sizeof(T) / sizeof(ElemType);
int pred;
const ElemType* src = (const ElemType*)&inVal;
for (size_t i = 0; i < count; ++i)
{
__match_all_sync(mask, src[i], &pred);
if (pred == 0)
{
return false;
}
}
return true;
}
template<typename T>
__inline__ __device__ T _waveReadFirst(WarpMask mask, T val)
{
const int lowestLaneId = __ffs(mask) - 1;
return __shfl_sync(mask, val, lowestLaneId);
}
template<typename T>
__inline__ __device__ T _waveReadFirstMultiple(WarpMask mask, T inVal)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
const size_t count = sizeof(T) / sizeof(ElemType);
T outVal;
const ElemType* src = (const ElemType*)&inVal;
ElemType* dst = (ElemType*)&outVal;
const int lowestLaneId = __ffs(mask) - 1;
for (size_t i = 0; i < count; ++i)
{
dst[i] = __shfl_sync(mask, src[i], lowestLaneId);
}
return outVal;
}
template<typename T>
__inline__ __device__ T _waveShuffleMultiple(WarpMask mask, T inVal, int lane)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
const size_t count = sizeof(T) / sizeof(ElemType);
T outVal;
const ElemType* src = (const ElemType*)&inVal;
ElemType* dst = (ElemType*)&outVal;
for (size_t i = 0; i < count; ++i)
{
dst[i] = __shfl_sync(mask, src[i], lane);
}
return outVal;
}
template<typename INTF, typename T>
__device__ T _wavePrefixInvertableScalar(WarpMask mask, T val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
const int laneId = _getLaneId();
T result;
if (offsetSize > 0)
{
result = val;
for (int i = 1; i < offsetSize; i += i)
{
const T readVal = __shfl_up_sync(mask, result, i, offsetSize);
if (laneId >= i)
{
result = INTF::doOp(result, readVal);
}
}
result = INTF::doInverse(result, val);
}
else
{
result = INTF::getInitial(val);
if (!_waveIsSingleLane(mask))
{
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
const T readValue = __shfl_sync(mask, val, srcLane);
if (srcLane < laneId)
{
result = INTF::doOp(result, readValue);
}
remaining &= ~laneBit;
}
}
}
return result;
}
template<typename INTF, typename T>
__device__ T _wavePrefixScalar(WarpMask mask, T val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
const int laneId = _getLaneId();
T result = INTF::getInitial(val);
if (offsetSize > 0)
{
for (int i = 1; i < offsetSize; i += i)
{
const T readVal = __shfl_up_sync(mask, val, i, offsetSize);
if (laneId >= i)
{
result = INTF::doOp(result, readVal);
val = INTF::doOp(val, readVal);
}
}
}
else
{
if (!_waveIsSingleLane(mask))
{
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
const T readValue = __shfl_sync(mask, val, srcLane);
if (srcLane < laneId)
{
result = INTF::doOp(result, readValue);
}
remaining &= ~laneBit;
}
}
}
return result;
}
template<typename INTF, typename T, size_t COUNT>
__device__ T _waveOpCopy(T* dst, const T* src)
{
for (size_t j = 0; j < COUNT; ++j)
{
dst[j] = src[j];
}
}
template<typename INTF, typename T, size_t COUNT>
__device__ T _waveOpDoInverse(T* inOut, const T* val)
{
for (size_t j = 0; j < COUNT; ++j)
{
inOut[j] = INTF::doInverse(inOut[j], val[j]);
}
}
template<typename INTF, typename T, size_t COUNT>
__device__ T _waveOpSetInitial(T* out, const T* val)
{
for (size_t j = 0; j < COUNT; ++j)
{
out[j] = INTF::getInitial(val[j]);
}
}
template<typename INTF, typename T, size_t COUNT>
__device__ T _wavePrefixInvertableMultiple(WarpMask mask, T* val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
const int laneId = _getLaneId();
T originalVal[COUNT];
_waveOpCopy<INTF, T, COUNT>(originalVal, val);
if (offsetSize > 0)
{
for (int i = 1; i < offsetSize; i += i)
{
for (size_t j = 0; j < COUNT; ++j)
{
const T readVal = __shfl_up_sync(mask, val[j], i, offsetSize);
if (laneId >= i)
{
val[j] = INTF::doOp(val[j], readVal);
}
}
}
_waveOpDoInverse<INTF, T, COUNT>(val, originalVal);
}
else
{
_waveOpSetInitial<INTF, T, COUNT>(val, val);
if (!_waveIsSingleLane(mask))
{
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
for (size_t j = 0; j < COUNT; ++j)
{
const T readValue = __shfl_sync(mask, originalVal[j], srcLane);
if (srcLane < laneId)
{
val[j] = INTF::doOp(val[j], readValue);
}
remaining &= ~laneBit;
}
}
}
}
}
template<typename INTF, typename T, size_t COUNT>
__device__ T _wavePrefixMultiple(WarpMask mask, T* val)
{
const int offsetSize = _waveCalcPow2Offset(mask);
const int laneId = _getLaneId();
T work[COUNT];
_waveOpCopy<INTF, T, COUNT>(work, val);
_waveOpSetInitial<INTF, T, COUNT>(val, val);
if (offsetSize > 0)
{
for (int i = 1; i < offsetSize; i += i)
{
for (size_t j = 0; j < COUNT; ++j)
{
const T readVal = __shfl_up_sync(mask, work[j], i, offsetSize);
if (laneId >= i)
{
work[j] = INTF::doOp(work[j], readVal);
val[j] = INTF::doOp(val[j], readVal);
}
}
}
}
else
{
if (!_waveIsSingleLane(mask))
{
int remaining = mask;
while (remaining)
{
const int laneBit = remaining & -remaining;
const int srcLane = __ffs(laneBit) - 1;
for (size_t j = 0; j < COUNT; ++j)
{
const T readValue = __shfl_sync(mask, work[j], srcLane);
if (srcLane < laneId)
{
val[j] = INTF::doOp(val[j], readValue);
}
}
remaining &= ~laneBit;
}
}
}
}
template<typename T>
__inline__ __device__ T _wavePrefixProduct(WarpMask mask, T val)
{
return _wavePrefixScalar<WaveOpMul<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _wavePrefixSum(WarpMask mask, T val)
{
return _wavePrefixInvertableScalar<WaveOpAdd<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _wavePrefixXor(WarpMask mask, T val)
{
return _wavePrefixInvertableScalar<WaveOpXor<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _wavePrefixOr(WarpMask mask, T val)
{
return _wavePrefixScalar<WaveOpOr<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _wavePrefixAnd(WarpMask mask, T val)
{
return _wavePrefixScalar<WaveOpAnd<T>, T>(mask, val);
}
template<typename T>
__inline__ __device__ T _wavePrefixProductMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_wavePrefixInvertableMultiple<WaveOpMul<ElemType>, ElemType, sizeof(T) / sizeof(ElemType)>(
mask,
(ElemType*)&val);
return val;
}
template<typename T>
__inline__ __device__ T _wavePrefixSumMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_wavePrefixInvertableMultiple<WaveOpAdd<ElemType>, ElemType, sizeof(T) / sizeof(ElemType)>(
mask,
(ElemType*)&val);
return val;
}
template<typename T>
__inline__ __device__ T _wavePrefixXorMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_wavePrefixInvertableMultiple<WaveOpXor<ElemType>, ElemType, sizeof(T) / sizeof(ElemType)>(
mask,
(ElemType*)&val);
return val;
}
template<typename T>
__inline__ __device__ T _wavePrefixOrMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_wavePrefixMultiple<WaveOpOr<ElemType>, ElemType, sizeof(T) / sizeof(ElemType)>(
mask,
(ElemType*)&val);
return val;
}
template<typename T>
__inline__ __device__ T _wavePrefixAndMultiple(WarpMask mask, T val)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
_wavePrefixMultiple<WaveOpAnd<ElemType>, ElemType, sizeof(T) / sizeof(ElemType)>(
mask,
(ElemType*)&val);
return val;
}
template<typename T>
__inline__ __device__ uint4 _waveMatchScalar(WarpMask mask, T val)
{
int pred;
return make_uint4(__match_all_sync(mask, val, &pred), 0, 0, 0);
}
template<typename T>
__inline__ __device__ uint4 _waveMatchMultiple(WarpMask mask, const T& inVal)
{
typedef typename ElementTypeTrait<T>::Type ElemType;
const size_t count = sizeof(T) / sizeof(ElemType);
int pred;
const ElemType* src = (const ElemType*)&inVal;
uint matchBits = 0xffffffff;
for (size_t i = 0; i < count && matchBits; ++i)
{
matchBits = matchBits & __match_all_sync(mask, src[i], &pred);
}
return make_uint4(matchBits, 0, 0, 0);
}
__device__ uint getAt(dim3 a, int b)
{
SLANG_PRELUDE_ASSERT(b >= 0 && b < 3);
return (&a.x)[b];
}
__device__ uint3 operator*(uint3 a, dim3 b)
{
uint3 r;
r.x = a.x * b.x;
r.y = a.y * b.y;
r.z = a.z * b.z;
return r;
}
template<typename TResult, typename TInput>
__inline__ __device__ TResult slang_bit_cast(TInput val)
{
return *(TResult*)(&val);
}
struct UniformEntryPointParams;
struct UniformState;
#ifdef SLANG_CUDA_ENABLE_OPTIX
struct RayDesc
{
float3 Origin;
float TMin;
float3 Direction;
float TMax;
};
static __forceinline__ __device__ void* unpackOptiXRayPayloadPointer(uint32_t i0, uint32_t i1)
{
const uint64_t uptr = static_cast<uint64_t>(i0) << 32 | i1;
void* ptr = reinterpret_cast<void*>(uptr);
return ptr;
}
static __forceinline__ __device__ void packOptiXRayPayloadPointer(
void* ptr,
uint32_t& i0,
uint32_t& i1)
{
const uint64_t uptr = reinterpret_cast<uint64_t>(ptr);
i0 = uptr >> 32;
i1 = uptr & 0x00000000ffffffff;
}
static __forceinline__ __device__ void* getOptiXRayPayloadPtr()
{
const uint32_t u0 = optixGetPayload_0();
const uint32_t u1 = optixGetPayload_1();
return unpackOptiXRayPayloadPointer(u0, u1);
}
template<typename T>
__forceinline__ __device__ void* traceOptiXRay(
OptixTraversableHandle AccelerationStructure,
uint32_t RayFlags,
uint32_t InstanceInclusionMask,
uint32_t RayContributionToHitGroupIndex,
uint32_t MultiplierForGeometryContributionToHitGroupIndex,
uint32_t MissShaderIndex,
RayDesc Ray,
T* Payload)
{
uint32_t r0, r1;
packOptiXRayPayloadPointer((void*)Payload, r0, r1);
optixTrace(
AccelerationStructure,
Ray.Origin,
Ray.Direction,
Ray.TMin,
Ray.TMax,
0.f,
InstanceInclusionMask,
RayFlags,
RayContributionToHitGroupIndex,
MultiplierForGeometryContributionToHitGroupIndex,
MissShaderIndex,
r0,
r1);
}
#endif
static const int kSlangTorchTensorMaxDim = 5;
struct TensorView
{
uint8_t* data;
uint32_t strides[kSlangTorchTensorMaxDim];
uint32_t sizes[kSlangTorchTensorMaxDim];
uint32_t dimensionCount;
template<typename T>
__device__ T* data_ptr()
{
return reinterpret_cast<T*>(data);
}
template<typename T>
__device__ T* data_ptr_at(uint32_t index)
{
uint64_t offset = strides[0] * index;
return reinterpret_cast<T*>(data + offset);
}
template<typename T>
__device__ T* data_ptr_at(uint2 index)
{
uint64_t offset = strides[0] * index.x + strides[1] * index.y;
return reinterpret_cast<T*>(data + offset);
}
template<typename T>
__device__ T* data_ptr_at(uint3 index)
{
uint64_t offset = strides[0] * index.x + strides[1] * index.y + strides[2] * index.z;
return reinterpret_cast<T*>(data + offset);
}
template<typename T>
__device__ T* data_ptr_at(uint4 index)
{
uint64_t offset = strides[0] * index.x + strides[1] * index.y + strides[2] * index.z +
strides[3] * index.w;
return reinterpret_cast<T*>(data + offset);
}
template<typename T, unsigned int N>
__device__ T* data_ptr_at(uint index[N])
{
uint64_t offset = 0;
for (unsigned int i = 0; i < N; ++i)
{
offset += strides[i] * index[i];
}
return reinterpret_cast<T*>(data + offset);
}
template<typename T>
__device__ T& load(uint32_t x)
{
return *reinterpret_cast<T*>(data + strides[0] * x);
}
template<typename T>
__device__ T& load(uint32_t x, uint32_t y)
{
return *reinterpret_cast<T*>(data + strides[0] * x + strides[1] * y);
}
template<typename T>
__device__ T& load(uint2 index)
{
return *reinterpret_cast<T*>(data + strides[0] * index.x + strides[1] * index.y);
}
template<typename T>
__device__ T& load(uint32_t x, uint32_t y, uint32_t z)
{
return *reinterpret_cast<T*>(data + strides[0] * x + strides[1] * y + strides[2] * z);
}
template<typename T>
__device__ T& load(uint3 index)
{
return *reinterpret_cast<T*>(
data + strides[0] * index.x + strides[1] * index.y + strides[2] * index.z);
}
template<typename T>
__device__ T& load(uint32_t x, uint32_t y, uint32_t z, uint32_t w)
{
return *reinterpret_cast<T*>(
data + strides[0] * x + strides[1] * y + strides[2] * z + strides[3] * w);
}
template<typename T>
__device__ T& load(uint4 index)
{
return *reinterpret_cast<T*>(
data + strides[0] * index.x + strides[1] * index.y + strides[2] * index.z +
strides[3] * index.w);
}
template<typename T>
__device__ T& load(uint32_t i0, uint32_t i1, uint32_t i2, uint32_t i3, uint32_t i4)
{
return *reinterpret_cast<T*>(
data + strides[0] * i0 + strides[1] * i1 + strides[2] * i2 + strides[3] * i3 +
strides[4] * i4);
}
template<typename T, unsigned int N>
__device__ T& load(uint index[N])
{
uint64_t offset = 0;
for (unsigned int i = 0; i < N; ++i)
{
offset += strides[i] * index[i];
}
return *reinterpret_cast<T*>(data + offset);
}
template<typename T>
__device__ void store(uint32_t x, T val)
{
*reinterpret_cast<T*>(data + strides[0] * x) = val;
}
template<typename T>
__device__ void store(uint32_t x, uint32_t y, T val)
{
*reinterpret_cast<T*>(data + strides[0] * x + strides[1] * y) = val;
}
template<typename T>
__device__ void store(uint2 index, T val)
{
*reinterpret_cast<T*>(data + strides[0] * index.x + strides[1] * index.y) = val;
}
template<typename T>
__device__ void store(uint32_t x, uint32_t y, uint32_t z, T val)
{
*reinterpret_cast<T*>(data + strides[0] * x + strides[1] * y + strides[2] * z) = val;
}
template<typename T>
__device__ void store(uint3 index, T val)
{
*reinterpret_cast<T*>(
data + strides[0] * index.x + strides[1] * index.y + strides[2] * index.z) = val;
}
template<typename T>
__device__ void store(uint32_t x, uint32_t y, uint32_t z, uint32_t w, T val)
{
*reinterpret_cast<T*>(
data + strides[0] * x + strides[1] * y + strides[2] * z + strides[3] * w) = val;
}
template<typename T>
__device__ void store(uint4 index, T val)
{
*reinterpret_cast<T*>(
data + strides[0] * index.x + strides[1] * index.y + strides[2] * index.z +
strides[3] * index.w) = val;
}
template<typename T>
__device__ void store(uint32_t i0, uint32_t i1, uint32_t i2, uint32_t i3, uint32_t i4, T val)
{
*reinterpret_cast<T*>(
data + strides[0] * i0 + strides[1] * i1 + strides[2] * i2 + strides[3] * i3 +
strides[4] * i4) = val;
}
template<typename T, unsigned int N>
__device__ void store(uint index[N], T val)
{
uint64_t offset = 0;
for (unsigned int i = 0; i < N; ++i)
{
offset += strides[i] * index[i];
}
*reinterpret_cast<T*>(data + offset) = val;
}
};