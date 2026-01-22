#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H 1
#include <bits/c++allocator.h>
#include <bits/memoryfwd.h>
#if __cplusplus >= 201103L
#include <type_traits>
#endif
#define __cpp_lib_incomplete_container_elements 201505L
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<>
class allocator<void>
{
public:
typedef void        value_type;
typedef size_t      size_type;
typedef ptrdiff_t   difference_type;
#if __cplusplus <= 201703L
typedef void*       pointer;
typedef const void* const_pointer;
template<typename _Tp1>
struct rebind
{ typedef allocator<_Tp1> other; };
#endif
#if __cplusplus >= 201103L
using propagate_on_container_move_assignment = true_type;
using is_always_equal
_GLIBCXX20_DEPRECATED_SUGGEST("std::allocator_traits::is_always_equal")
= true_type;
#if __cplusplus >= 202002L
allocator() = default;
~allocator() = default;
template<typename _Up>
__attribute__((__always_inline__))
constexpr
allocator(const allocator<_Up>&) noexcept { }
#endif
#endif
};
template<typename _Tp>
class allocator : public __allocator_base<_Tp>
{
public:
typedef _Tp        value_type;
typedef size_t     size_type;
typedef ptrdiff_t  difference_type;
#if __cplusplus <= 201703L
typedef _Tp*       pointer;
typedef const _Tp* const_pointer;
typedef _Tp&       reference;
typedef const _Tp& const_reference;
template<typename _Tp1>
struct rebind
{ typedef allocator<_Tp1> other; };
#endif
#if __cplusplus >= 201103L
using propagate_on_container_move_assignment = true_type;
using is_always_equal
_GLIBCXX20_DEPRECATED_SUGGEST("std::allocator_traits::is_always_equal")
= true_type;
#endif
__attribute__((__always_inline__))
_GLIBCXX20_CONSTEXPR
allocator() _GLIBCXX_NOTHROW { }
__attribute__((__always_inline__))
_GLIBCXX20_CONSTEXPR
allocator(const allocator& __a) _GLIBCXX_NOTHROW
: __allocator_base<_Tp>(__a) { }
#if __cplusplus >= 201103L
allocator& operator=(const allocator&) = default;
#endif
template<typename _Tp1>
__attribute__((__always_inline__))
_GLIBCXX20_CONSTEXPR
allocator(const allocator<_Tp1>&) _GLIBCXX_NOTHROW { }
__attribute__((__always_inline__))
#if __cpp_constexpr_dynamic_alloc
constexpr
#endif
~allocator() _GLIBCXX_NOTHROW { }
#if __cplusplus > 201703L
[[nodiscard,__gnu__::__always_inline__]]
constexpr _Tp*
allocate(size_t __n)
{
if (std::__is_constant_evaluated())
{
if (__builtin_mul_overflow(__n, sizeof(_Tp), &__n))
std::__throw_bad_array_new_length();
return static_cast<_Tp*>(::operator new(__n));
}
return __allocator_base<_Tp>::allocate(__n, 0);
}
[[__gnu__::__always_inline__]]
constexpr void
deallocate(_Tp* __p, size_t __n)
{
if (std::__is_constant_evaluated())
{
::operator delete(__p);
return;
}
__allocator_base<_Tp>::deallocate(__p, __n);
}
#endif
friend __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
bool
operator==(const allocator&, const allocator&) _GLIBCXX_NOTHROW
{ return true; }
#if __cpp_impl_three_way_comparison < 201907L
friend __attribute__((__always_inline__)) _GLIBCXX20_CONSTEXPR
bool
operator!=(const allocator&, const allocator&) _GLIBCXX_NOTHROW
{ return false; }
#endif
};
template<typename _T1, typename _T2>
__attribute__((__always_inline__))
inline _GLIBCXX20_CONSTEXPR bool
operator==(const allocator<_T1>&, const allocator<_T2>&)
_GLIBCXX_NOTHROW
{ return true; }
#if __cpp_impl_three_way_comparison < 201907L
template<typename _T1, typename _T2>
__attribute__((__always_inline__))
inline _GLIBCXX20_CONSTEXPR bool
operator!=(const allocator<_T1>&, const allocator<_T2>&)
_GLIBCXX_NOTHROW
{ return false; }
#endif
template<typename _Tp>
class allocator<const _Tp>
{
public:
typedef _Tp value_type;
template<typename _Up> allocator(const allocator<_Up>&) { }
};
template<typename _Tp>
class allocator<volatile _Tp>
{
public:
typedef _Tp value_type;
template<typename _Up> allocator(const allocator<_Up>&) { }
};
template<typename _Tp>
class allocator<const volatile _Tp>
{
public:
typedef _Tp value_type;
template<typename _Up> allocator(const allocator<_Up>&) { }
};
#if _GLIBCXX_EXTERN_TEMPLATE
extern template class allocator<char>;
extern template class allocator<wchar_t>;
#endif
#undef __allocator_base
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif