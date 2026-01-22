#ifndef _ALIGNED_BUFFER_H
#define _ALIGNED_BUFFER_H 1
#pragma GCC system_header
#if __cplusplus >= 201103L
# include <type_traits>
#else
# include <bits/c++0x_warning.h>
#endif
namespace __gnu_cxx
{
template<typename _Tp>
struct __aligned_membuf
{
struct _Tp2 { _Tp _M_t; };
alignas(__alignof__(_Tp2::_M_t)) unsigned char _M_storage[sizeof(_Tp)];
__aligned_membuf() = default;
__aligned_membuf(std::nullptr_t) { }
void*
_M_addr() noexcept
{ return static_cast<void*>(&_M_storage); }
const void*
_M_addr() const noexcept
{ return static_cast<const void*>(&_M_storage); }
_Tp*
_M_ptr() noexcept
{ return static_cast<_Tp*>(_M_addr()); }
const _Tp*
_M_ptr() const noexcept
{ return static_cast<const _Tp*>(_M_addr()); }
};
#if _GLIBCXX_INLINE_VERSION
template<typename _Tp>
using __aligned_buffer = __aligned_membuf<_Tp>;
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _Tp>
struct __aligned_buffer
: std::aligned_storage<sizeof(_Tp), __alignof__(_Tp)>
{
typename
std::aligned_storage<sizeof(_Tp), __alignof__(_Tp)>::type _M_storage;
__aligned_buffer() = default;
__aligned_buffer(std::nullptr_t) { }
void*
_M_addr() noexcept
{
return static_cast<void*>(&_M_storage);
}
const void*
_M_addr() const noexcept
{
return static_cast<const void*>(&_M_storage);
}
_Tp*
_M_ptr() noexcept
{ return static_cast<_Tp*>(_M_addr()); }
const _Tp*
_M_ptr() const noexcept
{ return static_cast<const _Tp*>(_M_addr()); }
};
#pragma GCC diagnostic pop
#endif
}
#endif