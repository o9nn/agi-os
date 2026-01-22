#ifndef _GLIBCXX_CXX_CONFIG_H
#define _GLIBCXX_CXX_CONFIG_H 1
#define _GLIBCXX_RELEASE 13
#define __GLIBCXX__ 20230727
#ifndef _GLIBCXX_PURE
# define _GLIBCXX_PURE __attribute__ ((__pure__))
#endif
#ifndef _GLIBCXX_CONST
# define _GLIBCXX_CONST __attribute__ ((__const__))
#endif
#ifndef _GLIBCXX_NORETURN
# define _GLIBCXX_NORETURN __attribute__ ((__noreturn__))
#endif
#ifndef _GLIBCXX_NOTHROW
# ifndef __cplusplus
# define _GLIBCXX_NOTHROW __attribute__((__nothrow__))
# endif
#endif
# define _GLIBCXX_HAVE_ATTRIBUTE_VISIBILITY 1
#if _GLIBCXX_HAVE_ATTRIBUTE_VISIBILITY
# define _GLIBCXX_VISIBILITY(V) __attribute__ ((__visibility__ (#V)))
#else
# define _GLIBCXX_VISIBILITY(V) _GLIBCXX_PSEUDO_VISIBILITY(V)
#endif
#ifndef _GLIBCXX_USE_DEPRECATED
# define _GLIBCXX_USE_DEPRECATED 1
#endif
#if defined(__DEPRECATED)
# define _GLIBCXX_DEPRECATED __attribute__ ((__deprecated__))
# define _GLIBCXX_DEPRECATED_SUGGEST(ALT) \
__attribute__ ((__deprecated__ ("use '" ALT "' instead")))
#else
# define _GLIBCXX_DEPRECATED
# define _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#endif
#if defined(__DEPRECATED) && (__cplusplus >= 201103L)
# define _GLIBCXX11_DEPRECATED _GLIBCXX_DEPRECATED
# define _GLIBCXX11_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#else
# define _GLIBCXX11_DEPRECATED
# define _GLIBCXX11_DEPRECATED_SUGGEST(ALT)
#endif
#if defined(__DEPRECATED) && (__cplusplus >= 201402L)
# define _GLIBCXX14_DEPRECATED _GLIBCXX_DEPRECATED
# define _GLIBCXX14_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#else
# define _GLIBCXX14_DEPRECATED
# define _GLIBCXX14_DEPRECATED_SUGGEST(ALT)
#endif
#if defined(__DEPRECATED) && (__cplusplus >= 201703L)
# define _GLIBCXX17_DEPRECATED [[__deprecated__]]
# define _GLIBCXX17_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#else
# define _GLIBCXX17_DEPRECATED
# define _GLIBCXX17_DEPRECATED_SUGGEST(ALT)
#endif
#if defined(__DEPRECATED) && (__cplusplus >= 202002L)
# define _GLIBCXX20_DEPRECATED [[__deprecated__]]
# define _GLIBCXX20_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#else
# define _GLIBCXX20_DEPRECATED
# define _GLIBCXX20_DEPRECATED_SUGGEST(ALT)
#endif
#if defined(__DEPRECATED) && (__cplusplus >= 202100L)
# define _GLIBCXX23_DEPRECATED [[__deprecated__]]
# define _GLIBCXX23_DEPRECATED_SUGGEST(ALT) _GLIBCXX_DEPRECATED_SUGGEST(ALT)
#else
# define _GLIBCXX23_DEPRECATED
# define _GLIBCXX23_DEPRECATED_SUGGEST(ALT)
#endif
#ifndef _GLIBCXX_ABI_TAG_CXX11
# define _GLIBCXX_ABI_TAG_CXX11 __attribute ((__abi_tag__ ("cxx11")))
#endif
#if __cplusplus >= 201703L
# define _GLIBCXX_NODISCARD [[__nodiscard__]]
#else
# define _GLIBCXX_NODISCARD
#endif
#if __cplusplus
#ifndef _GLIBCXX_CONSTEXPR
# if __cplusplus >= 201103L
# define _GLIBCXX_CONSTEXPR constexpr
# define _GLIBCXX_USE_CONSTEXPR constexpr
# else
# define _GLIBCXX_CONSTEXPR
# define _GLIBCXX_USE_CONSTEXPR const
# endif
#endif
#ifndef _GLIBCXX14_CONSTEXPR
# if __cplusplus >= 201402L
# define _GLIBCXX14_CONSTEXPR constexpr
# else
# define _GLIBCXX14_CONSTEXPR
# endif
#endif
#ifndef _GLIBCXX17_CONSTEXPR
# if __cplusplus >= 201703L
# define _GLIBCXX17_CONSTEXPR constexpr
# else
# define _GLIBCXX17_CONSTEXPR
# endif
#endif
#ifndef _GLIBCXX20_CONSTEXPR
# if __cplusplus >= 202002L
# define _GLIBCXX20_CONSTEXPR constexpr
# else
# define _GLIBCXX20_CONSTEXPR
# endif
#endif
#ifndef _GLIBCXX23_CONSTEXPR
# if __cplusplus >= 202100L
# define _GLIBCXX23_CONSTEXPR constexpr
# else
# define _GLIBCXX23_CONSTEXPR
# endif
#endif
#ifndef _GLIBCXX17_INLINE
# if __cplusplus >= 201703L
# define _GLIBCXX17_INLINE inline
# else
# define _GLIBCXX17_INLINE
# endif
#endif
#ifndef _GLIBCXX_NOEXCEPT
# if __cplusplus >= 201103L
# define _GLIBCXX_NOEXCEPT noexcept
# define _GLIBCXX_NOEXCEPT_IF(...) noexcept(__VA_ARGS__)
# define _GLIBCXX_USE_NOEXCEPT noexcept
# define _GLIBCXX_THROW(_EXC)
# else
# define _GLIBCXX_NOEXCEPT
# define _GLIBCXX_NOEXCEPT_IF(...)
# define _GLIBCXX_USE_NOEXCEPT throw()
# define _GLIBCXX_THROW(_EXC) throw(_EXC)
# endif
#endif
#ifndef _GLIBCXX_NOTHROW
# define _GLIBCXX_NOTHROW _GLIBCXX_USE_NOEXCEPT
#endif
#ifndef _GLIBCXX_THROW_OR_ABORT
# if __cpp_exceptions
# define _GLIBCXX_THROW_OR_ABORT(_EXC) (throw (_EXC))
# else
# define _GLIBCXX_THROW_OR_ABORT(_EXC) (__builtin_abort())
# endif
#endif
#if __cpp_noexcept_function_type
#define _GLIBCXX_NOEXCEPT_PARM , bool _NE
#define _GLIBCXX_NOEXCEPT_QUAL noexcept (_NE)
#else
#define _GLIBCXX_NOEXCEPT_PARM
#define _GLIBCXX_NOEXCEPT_QUAL
#endif
# define _GLIBCXX_EXTERN_TEMPLATE 1
namespace std
{
typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#if __cplusplus >= 201103L
typedef decltype(nullptr) nullptr_t;
#endif
#pragma GCC visibility push(default)
extern "C++" __attribute__ ((__noreturn__, __always_inline__))
inline void __terminate() _GLIBCXX_USE_NOEXCEPT
{
void terminate() _GLIBCXX_USE_NOEXCEPT __attribute__ ((__noreturn__));
terminate();
}
#pragma GCC visibility pop
}
# define _GLIBCXX_USE_DUAL_ABI 1
#if ! _GLIBCXX_USE_DUAL_ABI
# undef _GLIBCXX_USE_CXX11_ABI
#endif
#ifndef _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_USE_CXX11_ABI 1
#endif
#if _GLIBCXX_USE_CXX11_ABI
namespace std
{
inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
namespace __gnu_cxx
{
inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
}
# define _GLIBCXX_NAMESPACE_CXX11 __cxx11::
# define _GLIBCXX_BEGIN_NAMESPACE_CXX11 namespace __cxx11 {
# define _GLIBCXX_END_NAMESPACE_CXX11 }
# define _GLIBCXX_DEFAULT_ABI_TAG _GLIBCXX_ABI_TAG_CXX11
#else
# define _GLIBCXX_NAMESPACE_CXX11
# define _GLIBCXX_BEGIN_NAMESPACE_CXX11
# define _GLIBCXX_END_NAMESPACE_CXX11
# define _GLIBCXX_DEFAULT_ABI_TAG
#endif
# define _GLIBCXX_INLINE_VERSION 0
#if _GLIBCXX_INLINE_VERSION
# define _GLIBCXX_BEGIN_NAMESPACE_VERSION namespace __8 {
# define _GLIBCXX_END_NAMESPACE_VERSION }
# define _GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(X)
# define _GLIBCXX_END_INLINE_ABI_NAMESPACE(X)
namespace std
{
inline _GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cplusplus >= 201402L
inline namespace literals {
inline namespace chrono_literals { }
inline namespace complex_literals { }
inline namespace string_literals { }
#if __cplusplus > 201402L
inline namespace string_view_literals { }
#endif
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
namespace __gnu_cxx
{
inline _GLIBCXX_BEGIN_NAMESPACE_VERSION
_GLIBCXX_END_NAMESPACE_VERSION
}
#else
# define _GLIBCXX_BEGIN_NAMESPACE_VERSION
# define _GLIBCXX_END_NAMESPACE_VERSION
# define _GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(X) inline namespace X {
# define _GLIBCXX_END_INLINE_ABI_NAMESPACE(X) }
#endif
#if defined(_GLIBCXX_DEBUG) && !__STDC_HOSTED__
#undef _GLIBCXX_DEBUG
#define _GLIBCXX_ASSERTIONS 1
#endif
#if defined(_GLIBCXX_DEBUG) || defined(_GLIBCXX_PARALLEL)
namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __cxx1998
{
# if _GLIBCXX_USE_CXX11_ABI
inline namespace __cxx11 __attribute__((__abi_tag__ ("cxx11"))) { }
# endif
}
_GLIBCXX_END_NAMESPACE_VERSION
# ifdef _GLIBCXX_DEBUG
inline namespace __debug { }
# endif
# ifdef _GLIBCXX_PARALLEL
inline namespace __parallel { }
# endif
}
# if defined(_GLIBCXX_DEBUG) && defined(_GLIBCXX_PARALLEL)
# error illegal use of multiple inlined namespaces
# endif
# if __NO_INLINE__ && !__GXX_WEAK__
# warning currently using inlined namespace mode which may fail \
without inlining due to lack of weak symbols
# endif
#endif
#if defined(_GLIBCXX_DEBUG)
# define _GLIBCXX_STD_C __cxx1998
# define _GLIBCXX_BEGIN_NAMESPACE_CONTAINER \
namespace _GLIBCXX_STD_C {
# define _GLIBCXX_END_NAMESPACE_CONTAINER }
#else
# define _GLIBCXX_STD_C std
# define _GLIBCXX_BEGIN_NAMESPACE_CONTAINER
# define _GLIBCXX_END_NAMESPACE_CONTAINER
#endif
#ifdef _GLIBCXX_PARALLEL
# define _GLIBCXX_STD_A __cxx1998
# define _GLIBCXX_BEGIN_NAMESPACE_ALGO \
namespace _GLIBCXX_STD_A {
# define _GLIBCXX_END_NAMESPACE_ALGO }
#else
# define _GLIBCXX_STD_A std
# define _GLIBCXX_BEGIN_NAMESPACE_ALGO
# define _GLIBCXX_END_NAMESPACE_ALGO
#endif
#undef _GLIBCXX_LONG_DOUBLE_COMPAT
#ifndef __clang__
#undef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
#endif
#if defined _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT \
&& defined __LONG_DOUBLE_IEEE128__
namespace std
{
inline namespace __gnu_cxx_ieee128 { }
inline namespace __gnu_cxx11_ieee128 { }
}
# define _GLIBCXX_NAMESPACE_LDBL __gnu_cxx_ieee128::
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL namespace __gnu_cxx_ieee128 {
# define _GLIBCXX_END_NAMESPACE_LDBL }
# define _GLIBCXX_NAMESPACE_LDBL_OR_CXX11 __gnu_cxx11_ieee128::
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11 namespace __gnu_cxx11_ieee128 {
# define _GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11 }
#else
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined __LONG_DOUBLE_128__
namespace std
{
inline namespace __gnu_cxx_ldbl128 { }
}
# define _GLIBCXX_NAMESPACE_LDBL __gnu_cxx_ldbl128::
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL namespace __gnu_cxx_ldbl128 {
# define _GLIBCXX_END_NAMESPACE_LDBL }
#else
# define _GLIBCXX_NAMESPACE_LDBL
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL
# define _GLIBCXX_END_NAMESPACE_LDBL
#endif
#if _GLIBCXX_USE_CXX11_ABI
# define _GLIBCXX_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_NAMESPACE_CXX11
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_BEGIN_NAMESPACE_CXX11
# define _GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_END_NAMESPACE_CXX11
#else
# define _GLIBCXX_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_NAMESPACE_LDBL
# define _GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_BEGIN_NAMESPACE_LDBL
# define _GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11 _GLIBCXX_END_NAMESPACE_LDBL
#endif
#endif
namespace std
{
#pragma GCC visibility push(default)
_GLIBCXX_CONSTEXPR inline bool
__is_constant_evaluated() _GLIBCXX_NOEXCEPT
{
#if __cpp_if_consteval >= 202106L
# define _GLIBCXX_HAVE_IS_CONSTANT_EVALUATED 1
if consteval { return true; } else { return false; }
#elif __cplusplus >= 201103L && __has_builtin(__builtin_is_constant_evaluated)
# define _GLIBCXX_HAVE_IS_CONSTANT_EVALUATED 1
return __builtin_is_constant_evaluated();
#else
return false;
#endif
}
#pragma GCC visibility pop
}
#if defined(_GLIBCXX_DEBUG) && !defined(_GLIBCXX_ASSERTIONS)
# define _GLIBCXX_ASSERTIONS 1
#endif
#ifdef _GLIBCXX_ASSERTIONS
# undef _GLIBCXX_EXTERN_TEMPLATE
# define _GLIBCXX_EXTERN_TEMPLATE -1
#endif
#if _GLIBCXX_HAVE_IS_CONSTANT_EVALUATED
# define __glibcxx_constexpr_assert(cond) \
if (std::__is_constant_evaluated() && !bool(cond)) \
__builtin_unreachable()
#else
# define __glibcxx_constexpr_assert(unevaluated)
#endif
#define _GLIBCXX_VERBOSE_ASSERT 1
#if defined(_GLIBCXX_ASSERTIONS) \
|| defined(_GLIBCXX_PARALLEL) || defined(_GLIBCXX_PARALLEL_ASSERTIONS)
# ifdef _GLIBCXX_VERBOSE_ASSERT
namespace std
{
#pragma GCC visibility push(default)
extern "C++" _GLIBCXX_NORETURN
void
__glibcxx_assert_fail(const char* __file, int __line,
const char* __function, const char* __condition)
_GLIBCXX_NOEXCEPT;
#pragma GCC visibility pop
}
#define __glibcxx_assert_impl(_Condition) \
if (__builtin_expect(!bool(_Condition), false)) \
{ \
__glibcxx_constexpr_assert(false); \
std::__glibcxx_assert_fail(__FILE__, __LINE__, __PRETTY_FUNCTION__, \
#_Condition); \
}
# else
# define __glibcxx_assert_impl(_Condition) \
if (__builtin_expect(!bool(_Condition), false)) \
{ \
__glibcxx_constexpr_assert(false); \
__builtin_abort(); \
}
# endif
#endif
#if defined(_GLIBCXX_ASSERTIONS)
# define __glibcxx_assert(cond) \
do { __glibcxx_assert_impl(cond); } while (false)
#else
# define __glibcxx_assert(cond) \
do { __glibcxx_constexpr_assert(cond); } while (false)
#endif
#if __SANITIZE_THREAD__
# define _GLIBCXX_TSAN 1
#elif defined __has_feature
# if __has_feature(thread_sanitizer)
# define _GLIBCXX_TSAN 1
# endif
#endif
#ifndef _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE
# define _GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(A)
#endif
#ifndef _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER
# define _GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(A)
#endif
# define _GLIBCXX_BEGIN_EXTERN_C extern "C" {
# define _GLIBCXX_END_EXTERN_C }
# define _GLIBCXX_USE_ALLOCATOR_NEW 1
#ifdef __SIZEOF_INT128__
#if ! defined __GLIBCXX_TYPE_INT_N_0 && ! defined __STRICT_ANSI__
#warning "__STRICT_ANSI__ seems to have been undefined; this is not supported"
#endif
#endif
#else
# define _GLIBCXX_BEGIN_EXTERN_C
# define _GLIBCXX_END_EXTERN_C
#endif
#include <bits/os_defines.h>
#include <bits/cpu_defines.h>
#ifndef _GLIBCXX_PSEUDO_VISIBILITY
# define _GLIBCXX_PSEUDO_VISIBILITY(V)
#endif
#ifndef _GLIBCXX_WEAK_DEFINITION
# define _GLIBCXX_WEAK_DEFINITION
#endif
#ifndef _GLIBCXX_USE_WEAK_REF
# define _GLIBCXX_USE_WEAK_REF __GXX_WEAK__
#endif
#if __cplusplus >= 201103L && _GLIBCXX_USE_CXX11_ABI \
&& _GLIBCXX_USE_DUAL_ABI && __cpp_transactional_memory >= 201500L \
&& !_GLIBCXX_FULLY_DYNAMIC_STRING && _GLIBCXX_USE_WEAK_REF \
&& _GLIBCXX_USE_ALLOCATOR_NEW
#define _GLIBCXX_TXN_SAFE transaction_safe
#define _GLIBCXX_TXN_SAFE_DYN transaction_safe_dynamic
#else
#define _GLIBCXX_TXN_SAFE
#define _GLIBCXX_TXN_SAFE_DYN
#endif
#if __cplusplus > 201402L
# define _GLIBCXX_USE_STD_SPEC_FUNCS 1
#elif __cplusplus >= 201103L && __STDCPP_WANT_MATH_SPEC_FUNCS__ != 0
# define _GLIBCXX_USE_STD_SPEC_FUNCS 1
#endif
#ifdef __FAST_MATH__
# define _GLIBCXX_FAST_MATH 1
#else
# define _GLIBCXX_FAST_MATH 0
#endif
#define __N(msgid) (msgid)
#undef min
#undef max
#if __cplusplus >= 201103L
# ifndef _GLIBCXX_USE_C99_MATH
# define _GLIBCXX_USE_C99_MATH _GLIBCXX11_USE_C99_MATH
# endif
# ifndef _GLIBCXX_USE_C99_COMPLEX
# define _GLIBCXX_USE_C99_COMPLEX _GLIBCXX11_USE_C99_COMPLEX
# endif
# ifndef _GLIBCXX_USE_C99_STDIO
# define _GLIBCXX_USE_C99_STDIO _GLIBCXX11_USE_C99_STDIO
# endif
# ifndef _GLIBCXX_USE_C99_STDLIB
# define _GLIBCXX_USE_C99_STDLIB _GLIBCXX11_USE_C99_STDLIB
# endif
# ifndef _GLIBCXX_USE_C99_WCHAR
# define _GLIBCXX_USE_C99_WCHAR _GLIBCXX11_USE_C99_WCHAR
# endif
#else
# ifndef _GLIBCXX_USE_C99_MATH
# define _GLIBCXX_USE_C99_MATH _GLIBCXX98_USE_C99_MATH
# endif
# ifndef _GLIBCXX_USE_C99_COMPLEX
# define _GLIBCXX_USE_C99_COMPLEX _GLIBCXX98_USE_C99_COMPLEX
# endif
# ifndef _GLIBCXX_USE_C99_STDIO
# define _GLIBCXX_USE_C99_STDIO _GLIBCXX98_USE_C99_STDIO
# endif
# ifndef _GLIBCXX_USE_C99_STDLIB
# define _GLIBCXX_USE_C99_STDLIB _GLIBCXX98_USE_C99_STDLIB
# endif
# ifndef _GLIBCXX_USE_C99_WCHAR
# define _GLIBCXX_USE_C99_WCHAR _GLIBCXX98_USE_C99_WCHAR
# endif
#endif
#ifndef _GLIBCXX_USE_CHAR8_T
# ifdef __cpp_char8_t
# define _GLIBCXX_USE_CHAR8_T 1
# endif
#endif
#ifdef _GLIBCXX_USE_CHAR8_T
# define __cpp_lib_char8_t 201907L
#endif
#if defined(__FLOAT128__) || defined(__SIZEOF_FLOAT128__)
# if !(defined(_GLIBCXX_LONG_DOUBLE_ALT128_COMPAT) && defined(__LONG_DOUBLE_IEEE128__))
# define _GLIBCXX_USE_FLOAT128 1
# endif
#endif
#if __FLT_MANT_DIG__ == 24 \
&& __FLT_MIN_EXP__ == -125 \
&& __FLT_MAX_EXP__ == 128
# define _GLIBCXX_FLOAT_IS_IEEE_BINARY32 1
#endif
#if __DBL_MANT_DIG__ == 53 \
&& __DBL_MIN_EXP__ == -1021 \
&& __DBL_MAX_EXP__ == 1024
# define _GLIBCXX_DOUBLE_IS_IEEE_BINARY64 1
#endif
#if __LDBL_MANT_DIG__ == 113 \
&& __LDBL_MIN_EXP__ == -16381 \
&& __LDBL_MAX_EXP__ == 16384
# define _GLIBCXX_LDOUBLE_IS_IEEE_BINARY128 1
#endif
#ifdef __STDCPP_BFLOAT16_T__
namespace __gnu_cxx
{
using __bfloat16_t = decltype(0.0bf16);
}
#endif
#ifdef __has_builtin
# ifdef __is_identifier
# define _GLIBCXX_HAS_BUILTIN(B) __has_builtin(B) || ! __is_identifier(B)
# else
# define _GLIBCXX_HAS_BUILTIN(B) __has_builtin(B)
# endif
#endif
#if _GLIBCXX_HAS_BUILTIN(__has_unique_object_representations)
# define _GLIBCXX_HAVE_BUILTIN_HAS_UNIQ_OBJ_REP 1
#endif
#if _GLIBCXX_HAS_BUILTIN(__is_aggregate)
# define _GLIBCXX_HAVE_BUILTIN_IS_AGGREGATE 1
#endif
#if _GLIBCXX_HAS_BUILTIN(__is_same)
# define _GLIBCXX_HAVE_BUILTIN_IS_SAME 1
#endif
#if _GLIBCXX_HAS_BUILTIN(__builtin_launder)
# define _GLIBCXX_HAVE_BUILTIN_LAUNDER 1
#endif
#undef _GLIBCXX_HAS_BUILTIN
#define _GLIBCXX_DOXYGEN_ONLY(X)
#if __cplusplus >= 201703L
#if __has_include(<pstl/pstl_config.h>)
# ifndef _GLIBCXX_USE_TBB_PAR_BACKEND
# define _GLIBCXX_USE_TBB_PAR_BACKEND __has_include(<tbb/tbb.h>)
# endif
# if _GLIBCXX_USE_TBB_PAR_BACKEND
# define _PSTL_PAR_BACKEND_TBB
# else
# define _PSTL_PAR_BACKEND_SERIAL
# endif
# define _PSTL_ASSERT(_Condition) __glibcxx_assert(_Condition)
# define _PSTL_ASSERT_MSG(_Condition, _Message) __glibcxx_assert(_Condition)
#include <pstl/pstl_config.h>
#endif
#endif
#define _GLIBCXX_HAVE_ACOSF 1
#define _GLIBCXX_HAVE_ACOSL 1
#define _GLIBCXX_HAVE_ALIGNED_ALLOC 1
#define _GLIBCXX_HAVE_ARC4RANDOM 1
#define _GLIBCXX_HAVE_ARPA_INET_H 1
#define _GLIBCXX_HAVE_ASINF 1
#define _GLIBCXX_HAVE_ASINL 1
#define _GLIBCXX_HAVE_AS_SYMVER_DIRECTIVE 1
#define _GLIBCXX_HAVE_ATAN2F 1
#define _GLIBCXX_HAVE_ATAN2L 1
#define _GLIBCXX_HAVE_ATANF 1
#define _GLIBCXX_HAVE_ATANL 1
#define _GLIBCXX_HAVE_ATOMIC_LOCK_POLICY 1
#define _GLIBCXX_HAVE_AT_QUICK_EXIT 1
#define _GLIBCXX_HAVE_CEILF 1
#define _GLIBCXX_HAVE_CEILL 1
#define _GLIBCXX_HAVE_COMPLEX_H 1
#define _GLIBCXX_HAVE_COSF 1
#define _GLIBCXX_HAVE_COSHF 1
#define _GLIBCXX_HAVE_COSHL 1
#define _GLIBCXX_HAVE_COSL 1
#define _GLIBCXX_HAVE_DECL_STRNLEN 1
#define _GLIBCXX_HAVE_DIRENT_H 1
#define _GLIBCXX_HAVE_DIRFD 1
#define _GLIBCXX_HAVE_DLFCN_H 1
#define _GLIBCXX_HAVE_ENDIAN_H 1
#define _GLIBCXX_HAVE_EXCEPTION_PTR_SINCE_GCC46 1
#define _GLIBCXX_HAVE_EXECINFO_H 1
#define _GLIBCXX_HAVE_EXPF 1
#define _GLIBCXX_HAVE_EXPL 1
#define _GLIBCXX_HAVE_FABSF 1
#define _GLIBCXX_HAVE_FABSL 1
#define _GLIBCXX_HAVE_FCNTL_H 1
#define _GLIBCXX_HAVE_FDOPENDIR 1
#define _GLIBCXX_HAVE_FENV_H 1
#define _GLIBCXX_HAVE_FINITE 1
#define _GLIBCXX_HAVE_FINITEF 1
#define _GLIBCXX_HAVE_FINITEL 1
#define _GLIBCXX_HAVE_FLOAT_H 1
#define _GLIBCXX_HAVE_FLOORF 1
#define _GLIBCXX_HAVE_FLOORL 1
#define _GLIBCXX_HAVE_FMODF 1
#define _GLIBCXX_HAVE_FMODL 1
#define _GLIBCXX_HAVE_FREXPF 1
#define _GLIBCXX_HAVE_FREXPL 1
#define _GLIBCXX_HAVE_GETENTROPY 1
#define _GLIBCXX_HAVE_GETIPINFO 1
#define _GLIBCXX_HAVE_GETS 1
#define _GLIBCXX_HAVE_HYPOT 1
#define _GLIBCXX_HAVE_HYPOTF 1
#define _GLIBCXX_HAVE_HYPOTL 1
#define _GLIBCXX_HAVE_ICONV 1
#define _GLIBCXX_HAVE_INTTYPES_H 1
#define _GLIBCXX_HAVE_ISINFF 1
#define _GLIBCXX_HAVE_ISINFL 1
#define _GLIBCXX_HAVE_ISNANF 1
#define _GLIBCXX_HAVE_ISNANL 1
#define _GLIBCXX_HAVE_ISWBLANK 1
#define _GLIBCXX_HAVE_LC_MESSAGES 1
#define _GLIBCXX_HAVE_LDEXPF 1
#define _GLIBCXX_HAVE_LDEXPL 1
#define _GLIBCXX_HAVE_LIBINTL_H 1
#define _GLIBCXX_HAVE_LIMIT_AS 1
#define _GLIBCXX_HAVE_LIMIT_DATA 1
#define _GLIBCXX_HAVE_LIMIT_FSIZE 1
#define _GLIBCXX_HAVE_LIMIT_RSS 1
#define _GLIBCXX_HAVE_LIMIT_VMEM 0
#define _GLIBCXX_HAVE_LINK 1
#define _GLIBCXX_HAVE_LINK_H 1
#define _GLIBCXX_HAVE_LINUX_FUTEX 1
#define _GLIBCXX_HAVE_LINUX_RANDOM_H 1
#define _GLIBCXX_HAVE_LINUX_TYPES_H 1
#define _GLIBCXX_HAVE_LOCALE_H 1
#define _GLIBCXX_HAVE_LOG10F 1
#define _GLIBCXX_HAVE_LOG10L 1
#define _GLIBCXX_HAVE_LOGF 1
#define _GLIBCXX_HAVE_LOGL 1
#define _GLIBCXX_HAVE_MBSTATE_T 1
#define _GLIBCXX_HAVE_MEMALIGN 1
#define _GLIBCXX_HAVE_MEMORY_H 1
#define _GLIBCXX_HAVE_MODF 1
#define _GLIBCXX_HAVE_MODFF 1
#define _GLIBCXX_HAVE_MODFL 1
#define _GLIBCXX_HAVE_NETDB_H 1
#define _GLIBCXX_HAVE_NETINET_IN_H 1
#define _GLIBCXX_HAVE_NETINET_TCP_H 1
#define _GLIBCXX_HAVE_OPENAT 1
#define _GLIBCXX_HAVE_POLL 1
#define _GLIBCXX_HAVE_POLL_H 1
#define _GLIBCXX_HAVE_POSIX_MEMALIGN 1
#define _GLIBCXX_HAVE_POSIX_SEMAPHORE 1
#define _GLIBCXX_HAVE_POWF 1
#define _GLIBCXX_HAVE_POWL 1
#define _GLIBCXX_HAVE_QUICK_EXIT 1
#define _GLIBCXX_HAVE_READLINK 1
#define _GLIBCXX_HAVE_SECURE_GETENV 1
#define _GLIBCXX_HAVE_SETENV 1
#define _GLIBCXX_HAVE_SINCOS 1
#define _GLIBCXX_HAVE_SINCOSF 1
#define _GLIBCXX_HAVE_SINCOSL 1
#define _GLIBCXX_HAVE_SINF 1
#define _GLIBCXX_HAVE_SINHF 1
#define _GLIBCXX_HAVE_SINHL 1
#define _GLIBCXX_HAVE_SINL 1
#define _GLIBCXX_HAVE_SOCKATMARK 1
#define _GLIBCXX_HAVE_SQRTF 1
#define _GLIBCXX_HAVE_SQRTL 1
#define _GLIBCXX_HAVE_STDALIGN_H 1
#define _GLIBCXX_HAVE_STDBOOL_H 1
#define _GLIBCXX_HAVE_STDINT_H 1
#define _GLIBCXX_HAVE_STDLIB_H 1
#define _GLIBCXX_HAVE_STRERROR_L 1
#define _GLIBCXX_HAVE_STRERROR_R 1
#define _GLIBCXX_HAVE_STRINGS_H 1
#define _GLIBCXX_HAVE_STRING_H 1
#define _GLIBCXX_HAVE_STRTOF 1
#define _GLIBCXX_HAVE_STRTOLD 1
#define _GLIBCXX_HAVE_STRUCT_DIRENT_D_TYPE 1
#define _GLIBCXX_HAVE_STRXFRM_L 1
#define _GLIBCXX_HAVE_SYMLINK 1
#define _GLIBCXX_HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT 1
#define _GLIBCXX_HAVE_SYS_IOCTL_H 1
#define _GLIBCXX_HAVE_SYS_IPC_H 1
#define _GLIBCXX_HAVE_SYS_PARAM_H 1
#define _GLIBCXX_HAVE_SYS_RESOURCE_H 1
#define _GLIBCXX_HAVE_SYS_SEM_H 1
#define _GLIBCXX_HAVE_SYS_SOCKET_H 1
#define _GLIBCXX_HAVE_SYS_STATVFS_H 1
#define _GLIBCXX_HAVE_SYS_STAT_H 1
#define _GLIBCXX_HAVE_SYS_SYSINFO_H 1
#define _GLIBCXX_HAVE_SYS_TIME_H 1
#define _GLIBCXX_HAVE_SYS_TYPES_H 1
#define _GLIBCXX_HAVE_SYS_UIO_H 1
#define _GLIBCXX_HAVE_S_ISREG 1
#define _GLIBCXX_HAVE_TANF 1
#define _GLIBCXX_HAVE_TANHF 1
#define _GLIBCXX_HAVE_TANHL 1
#define _GLIBCXX_HAVE_TANL 1
#define _GLIBCXX_HAVE_TGMATH_H 1
#define _GLIBCXX_HAVE_TIMESPEC_GET 1
#define _GLIBCXX_HAVE_TLS 1
#define _GLIBCXX_HAVE_TRUNCATE 1
#define _GLIBCXX_HAVE_UCHAR_H 1
#define _GLIBCXX_HAVE_UNISTD_H 1
#define _GLIBCXX_HAVE_UNLINKAT 1
#define _GLIBCXX_HAVE_USELOCALE 1
#define _GLIBCXX_HAVE_UTIME_H 1
#define _GLIBCXX_HAVE_VFWSCANF 1
#define _GLIBCXX_HAVE_VSWSCANF 1
#define _GLIBCXX_HAVE_VWSCANF 1
#define _GLIBCXX_HAVE_WCHAR_H 1
#define _GLIBCXX_HAVE_WCSTOF 1
#define _GLIBCXX_HAVE_WCTYPE_H 1
#define _GLIBCXX_HAVE_WRITEV 1
#define _GLIBCXX_HAVE___CXA_THREAD_ATEXIT_IMPL 1
#define _GLIBCXX_ICONV_CONST
#define _GLIBCXX_LT_OBJDIR ".libs/"
#define _GLIBCXX_PACKAGE_BUGREPORT ""
#define _GLIBCXX_PACKAGE_NAME "package-unused"
#define _GLIBCXX_PACKAGE_STRING "package-unused version-unused"
#define _GLIBCXX_PACKAGE_TARNAME "libstdc++"
#define _GLIBCXX_PACKAGE_URL ""
#define _GLIBCXX_PACKAGE__GLIBCXX_VERSION "version-unused"
#define _GLIBCXX_STDC_HEADERS 1
#ifndef _GLIBCXX_DARWIN_USE_64_BIT_INODE
# define _GLIBCXX_DARWIN_USE_64_BIT_INODE 1
#endif
#define _GLIBCXX11_USE_C99_COMPLEX 1
#define _GLIBCXX11_USE_C99_MATH 1
#define _GLIBCXX11_USE_C99_STDIO 1
#define _GLIBCXX11_USE_C99_STDLIB 1
#define _GLIBCXX11_USE_C99_WCHAR 1
#define _GLIBCXX98_USE_C99_COMPLEX 1
#define _GLIBCXX98_USE_C99_MATH 1
#define _GLIBCXX98_USE_C99_STDIO 1
#define _GLIBCXX98_USE_C99_STDLIB 1
#define _GLIBCXX98_USE_C99_WCHAR 1
#define _GLIBCXX_ATOMIC_BUILTINS 1
#define _GLIBCXX_CAN_ALIGNAS_DESTRUCTIVE_SIZE 1
#define _GLIBCXX_FULLY_DYNAMIC_STRING 0
#define _GLIBCXX_HAS_GTHREADS 1
#define _GLIBCXX_HOSTED __STDC_HOSTED__
#define _GLIBCXX_MANGLE_SIZE_T m
#define _GLIBCXX_RES_LIMITS 1
#define _GLIBCXX_STATIC_TZDATA 1
#define _GLIBCXX_STDIO_EOF -1
#define _GLIBCXX_STDIO_SEEK_CUR 1
#define _GLIBCXX_STDIO_SEEK_END 2
#define _GLIBCXX_SYMVER 1
#define _GLIBCXX_SYMVER_GNU 1
#define _GLIBCXX_USE_C11_UCHAR_CXX11 1
#define _GLIBCXX_USE_C99 1
#define _GLIBCXX_USE_C99_COMPLEX_TR1 1
#define _GLIBCXX_USE_C99_CTYPE_TR1 1
#define _GLIBCXX_USE_C99_FENV_TR1 1
#define _GLIBCXX_USE_C99_INTTYPES_TR1 1
#define _GLIBCXX_USE_C99_INTTYPES_WCHAR_T_TR1 1
#define _GLIBCXX_USE_C99_MATH_TR1 1
#define _GLIBCXX_USE_C99_STDINT_TR1 1
#define _GLIBCXX_USE_CLOCK_MONOTONIC 1
#define _GLIBCXX_USE_CLOCK_REALTIME 1
#define _GLIBCXX_USE_DECIMAL_FLOAT 1
#define _GLIBCXX_USE_DEV_RANDOM 1
#define _GLIBCXX_USE_FCHMOD 1
#define _GLIBCXX_USE_FCHMODAT 1
#define _GLIBCXX_USE_FSEEKO_FTELLO 1
#define _GLIBCXX_USE_GETTIMEOFDAY 1
#define _GLIBCXX_USE_GET_NPROCS 1
#define _GLIBCXX_USE_INIT_PRIORITY_ATTRIBUTE 1
#define _GLIBCXX_USE_LFS 1
#define _GLIBCXX_USE_LONG_LONG 1
#define _GLIBCXX_USE_LSTAT 1
#define _GLIBCXX_USE_NANOSLEEP 1
#define _GLIBCXX_USE_NLS 1
#define _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT 1
#define _GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK 1
#define _GLIBCXX_USE_PTHREAD_RWLOCK_CLOCKLOCK 1
#define _GLIBCXX_USE_PTHREAD_RWLOCK_T 1
#define _GLIBCXX_USE_RANDOM_TR1 1
#define _GLIBCXX_USE_REALPATH 1
#define _GLIBCXX_USE_SCHED_YIELD 1
#define _GLIBCXX_USE_SC_NPROCESSORS_ONLN 1
#define _GLIBCXX_USE_SENDFILE 1
#define _GLIBCXX_USE_ST_MTIM 1
#define _GLIBCXX_USE_TMPNAM 1
#define _GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_CXX20 1
#define _GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_FCHAR8_T 1
#define _GLIBCXX_USE_UTIME 1
#define _GLIBCXX_USE_UTIMENSAT 1
#define _GLIBCXX_USE_WCHAR_T 1
#define _GLIBCXX_VERBOSE 1
#define _GLIBCXX_X86_RDRAND 1
#define _GLIBCXX_X86_RDSEED 1
#define _GLIBCXX_ZONEINFO_DIR "/usr/share/zoneinfo"
#define _GTHREAD_USE_MUTEX_TIMEDLOCK 1
#if __cplusplus >= 201103L
#endif
#if __cplusplus >= 201103L
#endif
#if defined (_GLIBCXX_HAVE__ACOSF) && ! defined (_GLIBCXX_HAVE_ACOSF)
# define _GLIBCXX_HAVE_ACOSF 1
# define acosf _acosf
#endif
#if defined (_GLIBCXX_HAVE__ACOSL) && ! defined (_GLIBCXX_HAVE_ACOSL)
# define _GLIBCXX_HAVE_ACOSL 1
# define acosl _acosl
#endif
#if defined (_GLIBCXX_HAVE__ASINF) && ! defined (_GLIBCXX_HAVE_ASINF)
# define _GLIBCXX_HAVE_ASINF 1
# define asinf _asinf
#endif
#if defined (_GLIBCXX_HAVE__ASINL) && ! defined (_GLIBCXX_HAVE_ASINL)
# define _GLIBCXX_HAVE_ASINL 1
# define asinl _asinl
#endif
#if defined (_GLIBCXX_HAVE__ATAN2F) && ! defined (_GLIBCXX_HAVE_ATAN2F)
# define _GLIBCXX_HAVE_ATAN2F 1
# define atan2f _atan2f
#endif
#if defined (_GLIBCXX_HAVE__ATAN2L) && ! defined (_GLIBCXX_HAVE_ATAN2L)
# define _GLIBCXX_HAVE_ATAN2L 1
# define atan2l _atan2l
#endif
#if defined (_GLIBCXX_HAVE__ATANF) && ! defined (_GLIBCXX_HAVE_ATANF)
# define _GLIBCXX_HAVE_ATANF 1
# define atanf _atanf
#endif
#if defined (_GLIBCXX_HAVE__ATANL) && ! defined (_GLIBCXX_HAVE_ATANL)
# define _GLIBCXX_HAVE_ATANL 1
# define atanl _atanl
#endif
#if defined (_GLIBCXX_HAVE__CEILF) && ! defined (_GLIBCXX_HAVE_CEILF)
# define _GLIBCXX_HAVE_CEILF 1
# define ceilf _ceilf
#endif
#if defined (_GLIBCXX_HAVE__CEILL) && ! defined (_GLIBCXX_HAVE_CEILL)
# define _GLIBCXX_HAVE_CEILL 1
# define ceill _ceill
#endif
#if defined (_GLIBCXX_HAVE__COSF) && ! defined (_GLIBCXX_HAVE_COSF)
# define _GLIBCXX_HAVE_COSF 1
# define cosf _cosf
#endif
#if defined (_GLIBCXX_HAVE__COSHF) && ! defined (_GLIBCXX_HAVE_COSHF)
# define _GLIBCXX_HAVE_COSHF 1
# define coshf _coshf
#endif
#if defined (_GLIBCXX_HAVE__COSHL) && ! defined (_GLIBCXX_HAVE_COSHL)
# define _GLIBCXX_HAVE_COSHL 1
# define coshl _coshl
#endif
#if defined (_GLIBCXX_HAVE__COSL) && ! defined (_GLIBCXX_HAVE_COSL)
# define _GLIBCXX_HAVE_COSL 1
# define cosl _cosl
#endif
#if defined (_GLIBCXX_HAVE__EXPF) && ! defined (_GLIBCXX_HAVE_EXPF)
# define _GLIBCXX_HAVE_EXPF 1
# define expf _expf
#endif
#if defined (_GLIBCXX_HAVE__EXPL) && ! defined (_GLIBCXX_HAVE_EXPL)
# define _GLIBCXX_HAVE_EXPL 1
# define expl _expl
#endif
#if defined (_GLIBCXX_HAVE__FABSF) && ! defined (_GLIBCXX_HAVE_FABSF)
# define _GLIBCXX_HAVE_FABSF 1
# define fabsf _fabsf
#endif
#if defined (_GLIBCXX_HAVE__FABSL) && ! defined (_GLIBCXX_HAVE_FABSL)
# define _GLIBCXX_HAVE_FABSL 1
# define fabsl _fabsl
#endif
#if defined (_GLIBCXX_HAVE__FINITE) && ! defined (_GLIBCXX_HAVE_FINITE)
# define _GLIBCXX_HAVE_FINITE 1
# define finite _finite
#endif
#if defined (_GLIBCXX_HAVE__FINITEF) && ! defined (_GLIBCXX_HAVE_FINITEF)
# define _GLIBCXX_HAVE_FINITEF 1
# define finitef _finitef
#endif
#if defined (_GLIBCXX_HAVE__FINITEL) && ! defined (_GLIBCXX_HAVE_FINITEL)
# define _GLIBCXX_HAVE_FINITEL 1
# define finitel _finitel
#endif
#if defined (_GLIBCXX_HAVE__FLOORF) && ! defined (_GLIBCXX_HAVE_FLOORF)
# define _GLIBCXX_HAVE_FLOORF 1
# define floorf _floorf
#endif
#if defined (_GLIBCXX_HAVE__FLOORL) && ! defined (_GLIBCXX_HAVE_FLOORL)
# define _GLIBCXX_HAVE_FLOORL 1
# define floorl _floorl
#endif
#if defined (_GLIBCXX_HAVE__FMODF) && ! defined (_GLIBCXX_HAVE_FMODF)
# define _GLIBCXX_HAVE_FMODF 1
# define fmodf _fmodf
#endif
#if defined (_GLIBCXX_HAVE__FMODL) && ! defined (_GLIBCXX_HAVE_FMODL)
# define _GLIBCXX_HAVE_FMODL 1
# define fmodl _fmodl
#endif
#if defined (_GLIBCXX_HAVE__FPCLASS) && ! defined (_GLIBCXX_HAVE_FPCLASS)
# define _GLIBCXX_HAVE_FPCLASS 1
# define fpclass _fpclass
#endif
#if defined (_GLIBCXX_HAVE__FREXPF) && ! defined (_GLIBCXX_HAVE_FREXPF)
# define _GLIBCXX_HAVE_FREXPF 1
# define frexpf _frexpf
#endif
#if defined (_GLIBCXX_HAVE__FREXPL) && ! defined (_GLIBCXX_HAVE_FREXPL)
# define _GLIBCXX_HAVE_FREXPL 1
# define frexpl _frexpl
#endif
#if defined (_GLIBCXX_HAVE__HYPOT) && ! defined (_GLIBCXX_HAVE_HYPOT)
# define _GLIBCXX_HAVE_HYPOT 1
# define hypot _hypot
#endif
#if defined (_GLIBCXX_HAVE__HYPOTF) && ! defined (_GLIBCXX_HAVE_HYPOTF)
# define _GLIBCXX_HAVE_HYPOTF 1
# define hypotf _hypotf
#endif
#if defined (_GLIBCXX_HAVE__HYPOTL) && ! defined (_GLIBCXX_HAVE_HYPOTL)
# define _GLIBCXX_HAVE_HYPOTL 1
# define hypotl _hypotl
#endif
#if defined (_GLIBCXX_HAVE__ISINF) && ! defined (_GLIBCXX_HAVE_ISINF)
# define _GLIBCXX_HAVE_ISINF 1
# define isinf _isinf
#endif
#if defined (_GLIBCXX_HAVE__ISINFF) && ! defined (_GLIBCXX_HAVE_ISINFF)
# define _GLIBCXX_HAVE_ISINFF 1
# define isinff _isinff
#endif
#if defined (_GLIBCXX_HAVE__ISINFL) && ! defined (_GLIBCXX_HAVE_ISINFL)
# define _GLIBCXX_HAVE_ISINFL 1
# define isinfl _isinfl
#endif
#if defined (_GLIBCXX_HAVE__ISNAN) && ! defined (_GLIBCXX_HAVE_ISNAN)
# define _GLIBCXX_HAVE_ISNAN 1
# define isnan _isnan
#endif
#if defined (_GLIBCXX_HAVE__ISNANF) && ! defined (_GLIBCXX_HAVE_ISNANF)
# define _GLIBCXX_HAVE_ISNANF 1
# define isnanf _isnanf
#endif
#if defined (_GLIBCXX_HAVE__ISNANL) && ! defined (_GLIBCXX_HAVE_ISNANL)
# define _GLIBCXX_HAVE_ISNANL 1
# define isnanl _isnanl
#endif
#if defined (_GLIBCXX_HAVE__LDEXPF) && ! defined (_GLIBCXX_HAVE_LDEXPF)
# define _GLIBCXX_HAVE_LDEXPF 1
# define ldexpf _ldexpf
#endif
#if defined (_GLIBCXX_HAVE__LDEXPL) && ! defined (_GLIBCXX_HAVE_LDEXPL)
# define _GLIBCXX_HAVE_LDEXPL 1
# define ldexpl _ldexpl
#endif
#if defined (_GLIBCXX_HAVE__LOG10F) && ! defined (_GLIBCXX_HAVE_LOG10F)
# define _GLIBCXX_HAVE_LOG10F 1
# define log10f _log10f
#endif
#if defined (_GLIBCXX_HAVE__LOG10L) && ! defined (_GLIBCXX_HAVE_LOG10L)
# define _GLIBCXX_HAVE_LOG10L 1
# define log10l _log10l
#endif
#if defined (_GLIBCXX_HAVE__LOGF) && ! defined (_GLIBCXX_HAVE_LOGF)
# define _GLIBCXX_HAVE_LOGF 1
# define logf _logf
#endif
#if defined (_GLIBCXX_HAVE__LOGL) && ! defined (_GLIBCXX_HAVE_LOGL)
# define _GLIBCXX_HAVE_LOGL 1
# define logl _logl
#endif
#if defined (_GLIBCXX_HAVE__MODF) && ! defined (_GLIBCXX_HAVE_MODF)
# define _GLIBCXX_HAVE_MODF 1
# define modf _modf
#endif
#if defined (_GLIBCXX_HAVE__MODFF) && ! defined (_GLIBCXX_HAVE_MODFF)
# define _GLIBCXX_HAVE_MODFF 1
# define modff _modff
#endif
#if defined (_GLIBCXX_HAVE__MODFL) && ! defined (_GLIBCXX_HAVE_MODFL)
# define _GLIBCXX_HAVE_MODFL 1
# define modfl _modfl
#endif
#if defined (_GLIBCXX_HAVE__POWF) && ! defined (_GLIBCXX_HAVE_POWF)
# define _GLIBCXX_HAVE_POWF 1
# define powf _powf
#endif
#if defined (_GLIBCXX_HAVE__POWL) && ! defined (_GLIBCXX_HAVE_POWL)
# define _GLIBCXX_HAVE_POWL 1
# define powl _powl
#endif
#if defined (_GLIBCXX_HAVE__QFPCLASS) && ! defined (_GLIBCXX_HAVE_QFPCLASS)
# define _GLIBCXX_HAVE_QFPCLASS 1
# define qfpclass _qfpclass
#endif
#if defined (_GLIBCXX_HAVE__SINCOS) && ! defined (_GLIBCXX_HAVE_SINCOS)
# define _GLIBCXX_HAVE_SINCOS 1
# define sincos _sincos
#endif
#if defined (_GLIBCXX_HAVE__SINCOSF) && ! defined (_GLIBCXX_HAVE_SINCOSF)
# define _GLIBCXX_HAVE_SINCOSF 1
# define sincosf _sincosf
#endif
#if defined (_GLIBCXX_HAVE__SINCOSL) && ! defined (_GLIBCXX_HAVE_SINCOSL)
# define _GLIBCXX_HAVE_SINCOSL 1
# define sincosl _sincosl
#endif
#if defined (_GLIBCXX_HAVE__SINF) && ! defined (_GLIBCXX_HAVE_SINF)
# define _GLIBCXX_HAVE_SINF 1
# define sinf _sinf
#endif
#if defined (_GLIBCXX_HAVE__SINHF) && ! defined (_GLIBCXX_HAVE_SINHF)
# define _GLIBCXX_HAVE_SINHF 1
# define sinhf _sinhf
#endif
#if defined (_GLIBCXX_HAVE__SINHL) && ! defined (_GLIBCXX_HAVE_SINHL)
# define _GLIBCXX_HAVE_SINHL 1
# define sinhl _sinhl
#endif
#if defined (_GLIBCXX_HAVE__SINL) && ! defined (_GLIBCXX_HAVE_SINL)
# define _GLIBCXX_HAVE_SINL 1
# define sinl _sinl
#endif
#if defined (_GLIBCXX_HAVE__SQRTF) && ! defined (_GLIBCXX_HAVE_SQRTF)
# define _GLIBCXX_HAVE_SQRTF 1
# define sqrtf _sqrtf
#endif
#if defined (_GLIBCXX_HAVE__SQRTL) && ! defined (_GLIBCXX_HAVE_SQRTL)
# define _GLIBCXX_HAVE_SQRTL 1
# define sqrtl _sqrtl
#endif
#if defined (_GLIBCXX_HAVE__STRTOF) && ! defined (_GLIBCXX_HAVE_STRTOF)
# define _GLIBCXX_HAVE_STRTOF 1
# define strtof _strtof
#endif
#if defined (_GLIBCXX_HAVE__STRTOLD) && ! defined (_GLIBCXX_HAVE_STRTOLD)
# define _GLIBCXX_HAVE_STRTOLD 1
# define strtold _strtold
#endif
#if defined (_GLIBCXX_HAVE__TANF) && ! defined (_GLIBCXX_HAVE_TANF)
# define _GLIBCXX_HAVE_TANF 1
# define tanf _tanf
#endif
#if defined (_GLIBCXX_HAVE__TANHF) && ! defined (_GLIBCXX_HAVE_TANHF)
# define _GLIBCXX_HAVE_TANHF 1
# define tanhf _tanhf
#endif
#if defined (_GLIBCXX_HAVE__TANHL) && ! defined (_GLIBCXX_HAVE_TANHL)
# define _GLIBCXX_HAVE_TANHL 1
# define tanhl _tanhl
#endif
#if defined (_GLIBCXX_HAVE__TANL) && ! defined (_GLIBCXX_HAVE_TANL)
# define _GLIBCXX_HAVE_TANL 1
# define tanl _tanl
#endif
#endif