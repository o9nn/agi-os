#ifndef _CXXABI_INIT_EXCEPTION_H
#define _CXXABI_INIT_EXCEPTION_H 1
#pragma GCC system_header
#pragma GCC visibility push(default)
#include <stddef.h>
#include <bits/c++config.h>
#ifndef _GLIBCXX_CDTOR_CALLABI
#define _GLIBCXX_CDTOR_CALLABI
#define _GLIBCXX_HAVE_CDTOR_CALLABI 0
#else
#define _GLIBCXX_HAVE_CDTOR_CALLABI 1
#endif
#ifdef __cplusplus
namespace std
{
class type_info;
}
namespace __cxxabiv1
{
struct __cxa_refcounted_exception;
extern "C"
{
void*
__cxa_allocate_exception(size_t) _GLIBCXX_NOTHROW;
void
__cxa_free_exception(void*) _GLIBCXX_NOTHROW;
__cxa_refcounted_exception*
__cxa_init_primary_exception(void *__object, std::type_info *__tinfo,
void (_GLIBCXX_CDTOR_CALLABI *__dest) (void *))
_GLIBCXX_NOTHROW;
}
}
#endif
#pragma GCC visibility pop
#endif