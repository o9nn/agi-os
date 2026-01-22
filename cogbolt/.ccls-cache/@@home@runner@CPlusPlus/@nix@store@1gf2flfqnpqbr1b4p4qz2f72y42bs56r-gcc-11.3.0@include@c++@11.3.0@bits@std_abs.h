#ifndef _GLIBCXX_BITS_STD_ABS_H
#define _GLIBCXX_BITS_STD_ABS_H
#pragma GCC system_header
#include <bits/c++config.h>
#define _GLIBCXX_INCLUDE_NEXT_C_HEADERS
#include_next <stdlib.h>
#ifdef __CORRECT_ISO_CPP_MATH_H_PROTO
# include_next <math.h>
#endif
#undef _GLIBCXX_INCLUDE_NEXT_C_HEADERS
#undef abs
extern "C++"
{
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
using ::abs;
#ifndef __CORRECT_ISO_CPP_STDLIB_H_PROTO
inline long
abs(long __i) { return __builtin_labs(__i); }
#endif
#ifdef _GLIBCXX_USE_LONG_LONG
inline long long
abs(long long __x) { return __builtin_llabs (__x); }
#endif
#ifndef __CORRECT_ISO_CPP_MATH_H_PROTO
inline _GLIBCXX_CONSTEXPR double
abs(double __x)
{ return __builtin_fabs(__x); }
inline _GLIBCXX_CONSTEXPR float
abs(float __x)
{ return __builtin_fabsf(__x); }
inline _GLIBCXX_CONSTEXPR long double
abs(long double __x)
{ return __builtin_fabsl(__x); }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_0)
inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_0
abs(__GLIBCXX_TYPE_INT_N_0 __x) { return __x >= 0 ? __x : -__x; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_1)
inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_1
abs(__GLIBCXX_TYPE_INT_N_1 __x) { return __x >= 0 ? __x : -__x; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_2)
inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_2
abs(__GLIBCXX_TYPE_INT_N_2 __x) { return __x >= 0 ? __x : -__x; }
#endif
#if defined(__GLIBCXX_TYPE_INT_N_3)
inline _GLIBCXX_CONSTEXPR __GLIBCXX_TYPE_INT_N_3
abs(__GLIBCXX_TYPE_INT_N_3 __x) { return __x >= 0 ? __x : -__x; }
#endif
#if !defined(__STRICT_ANSI__) && defined(_GLIBCXX_USE_FLOAT128)
inline _GLIBCXX_CONSTEXPR
__float128
abs(__float128 __x)
{ return __x < 0 ? -__x : __x; }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
}
#endif