#ifndef _GLIBCXX_ALIGN_H
#define _GLIBCXX_ALIGN_H 1
#include <bits/c++config.h>
#include <bit>
#include <stdint.h>
#include <debug/assertions.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
inline void*
align(size_t __align, size_t __size, void*& __ptr, size_t& __space) noexcept
{
if (__space < __size)
return nullptr;
const auto __intptr = reinterpret_cast<uintptr_t>(__ptr);
const auto __aligned = (__intptr - 1u + __align) & -__align;
const auto __diff = __aligned - __intptr;
if (__diff > (__space - __size))
return nullptr;
else
{
__space -= __diff;
return __ptr = reinterpret_cast<void*>(__aligned);
}
}
#if __cplusplus > 201703L
#define __cpp_lib_assume_aligned 201811L
template<size_t _Align, class _Tp>
[[nodiscard,__gnu__::__always_inline__]]
constexpr _Tp*
assume_aligned(_Tp* __ptr) noexcept
{
static_assert(std::has_single_bit(_Align));
if (std::is_constant_evaluated())
return __ptr;
else
{
_GLIBCXX_DEBUG_ASSERT((uintptr_t)__ptr % _Align == 0);
return static_cast<_Tp*>(__builtin_assume_aligned(__ptr, _Align));
}
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif