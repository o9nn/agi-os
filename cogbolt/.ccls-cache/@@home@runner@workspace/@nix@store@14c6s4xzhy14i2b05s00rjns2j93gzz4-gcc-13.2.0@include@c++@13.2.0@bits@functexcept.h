#ifndef _FUNCTEXCEPT_H
#define _FUNCTEXCEPT_H 1
#include <bits/c++config.h>
#include <bits/exception_defines.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if _GLIBCXX_HOSTED
void
__throw_bad_exception(void) __attribute__((__noreturn__));
void
__throw_bad_alloc(void) __attribute__((__noreturn__));
void
__throw_bad_array_new_length(void) __attribute__((__noreturn__));
void
__throw_bad_cast(void) __attribute__((__noreturn__));
void
__throw_bad_typeid(void) __attribute__((__noreturn__));
void
__throw_logic_error(const char*) __attribute__((__noreturn__));
void
__throw_domain_error(const char*) __attribute__((__noreturn__));
void
__throw_invalid_argument(const char*) __attribute__((__noreturn__));
void
__throw_length_error(const char*) __attribute__((__noreturn__));
void
__throw_out_of_range(const char*) __attribute__((__noreturn__));
void
__throw_out_of_range_fmt(const char*, ...) __attribute__((__noreturn__))
__attribute__((__format__(__gnu_printf__, 1, 2)));
void
__throw_runtime_error(const char*) __attribute__((__noreturn__));
void
__throw_range_error(const char*) __attribute__((__noreturn__));
void
__throw_overflow_error(const char*) __attribute__((__noreturn__));
void
__throw_underflow_error(const char*) __attribute__((__noreturn__));
void
__throw_ios_failure(const char*) __attribute__((__noreturn__));
void
__throw_ios_failure(const char*, int) __attribute__((__noreturn__));
void
__throw_system_error(int) __attribute__((__noreturn__));
void
__throw_future_error(int) __attribute__((__noreturn__));
void
__throw_bad_function_call() __attribute__((__noreturn__));
#else
__attribute__((__noreturn__)) inline void
__throw_invalid_argument(const char*)
{ std::__terminate(); }
__attribute__((__noreturn__)) inline void
__throw_out_of_range(const char*)
{ std::__terminate(); }
__attribute__((__noreturn__)) inline void
__throw_out_of_range_fmt(const char*, ...)
{ std::__terminate(); }
__attribute__((__noreturn__)) inline void
__throw_runtime_error(const char*)
{ std::__terminate(); }
__attribute__((__noreturn__)) inline void
__throw_overflow_error(const char*)
{ std::__terminate(); }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif