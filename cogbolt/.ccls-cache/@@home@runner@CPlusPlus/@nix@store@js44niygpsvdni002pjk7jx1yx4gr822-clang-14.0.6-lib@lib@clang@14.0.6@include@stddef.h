#if !defined(__STDDEF_H) || defined(__need_ptrdiff_t) ||                       \
defined(__need_size_t) || defined(__need_wchar_t) ||                       \
defined(__need_NULL) || defined(__need_wint_t)
#if !defined(__need_ptrdiff_t) && !defined(__need_size_t) &&                   \
!defined(__need_wchar_t) && !defined(__need_NULL) &&                       \
!defined(__need_wint_t)
#if !__has_feature(modules)
#define __STDDEF_H
#endif
#define __need_ptrdiff_t
#define __need_size_t
#define __need_wchar_t
#define __need_NULL
#define __need_STDDEF_H_misc
#endif
#if defined(__need_ptrdiff_t)
#if !defined(_PTRDIFF_T) || __has_feature(modules)
#if !__has_feature(modules)
#define _PTRDIFF_T
#endif
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#endif
#undef __need_ptrdiff_t
#endif
#if defined(__need_size_t)
#if !defined(_SIZE_T) || __has_feature(modules)
#if !__has_feature(modules)
#define _SIZE_T
#endif
typedef __SIZE_TYPE__ size_t;
#endif
#undef __need_size_t
#endif
#if defined(__need_STDDEF_H_misc)
#if (defined(__STDC_WANT_LIB_EXT1__) && __STDC_WANT_LIB_EXT1__ >= 1 && \
!defined(_RSIZE_T)) || __has_feature(modules)
#if !__has_feature(modules)
#define _RSIZE_T
#endif
typedef __SIZE_TYPE__ rsize_t;
#endif
#endif
#if defined(__need_wchar_t)
#ifndef __cplusplus
#if !defined(_WCHAR_T) || __has_feature(modules)
#if !__has_feature(modules)
#define _WCHAR_T
#if defined(_MSC_EXTENSIONS)
#define _WCHAR_T_DEFINED
#endif
#endif
typedef __WCHAR_TYPE__ wchar_t;
#endif
#endif
#undef __need_wchar_t
#endif
#if defined(__need_NULL)
#undef NULL
#ifdef __cplusplus
#  if !defined(__MINGW32__) && !defined(_MSC_VER)
#    define NULL __null
#  else
#    define NULL 0
#  endif
#else
#  define NULL ((void*)0)
#endif
#ifdef __cplusplus
#if defined(_MSC_EXTENSIONS) && defined(_NATIVE_NULLPTR_SUPPORTED)
namespace std { typedef decltype(nullptr) nullptr_t; }
using ::std::nullptr_t;
#endif
#endif
#undef __need_NULL
#endif
#if defined(__need_STDDEF_H_misc)
#if __STDC_VERSION__ >= 201112L || __cplusplus >= 201103L
#include "__stddef_max_align_t.h"
#endif
#define offsetof(t, d) __builtin_offsetof(t, d)
#undef __need_STDDEF_H_misc
#endif
#if defined(__need_wint_t)
#if !defined(_WINT_T) || __has_feature(modules)
#if !__has_feature(modules)
#define _WINT_T
#endif
typedef __WINT_TYPE__ wint_t;
#endif
#undef __need_wint_t
#endif
#endif