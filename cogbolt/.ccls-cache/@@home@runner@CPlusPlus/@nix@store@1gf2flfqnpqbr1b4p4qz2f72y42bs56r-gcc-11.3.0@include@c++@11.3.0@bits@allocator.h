#ifndef _ALLOCATOR_H
#define _ALLOCATOR_H 1
#include <bits/c++allocator.h>
#include <bits/memoryfwd.h>
#if __cplusplus >= 201103L
#include <type_traits>
#endif
#define __cpp_lib_incomplete_container_elements 201505
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<>
class allocator<void>
{
public:
typedef void value_type;
typedef size_t size_type;
typedef ptrdiff_t difference_type;
#if __cplusplus <= 201703L
typedef void* pointer;
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
constexpr
allocator(const allocator<_Up>&) noexcept { }
#endif
#endif
};
template<typename _Tp>
class allocator : public __allocator_base<_Tp>
{
public:
typedef _Tp value_type;
typedef size_t size_type;
typedef ptrdiff_t difference_type;
#if __cplusplus <= 201703L
typedef _Tp* pointer;
typedef const _Tp* const_pointer;
typedef _Tp& reference;
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
_GLIBCXX20_CONSTEXPR
allocator() _GLIBCXX_NOTHROW { }
_GLIBCXX20_CONSTEXPR
allocator(const allocator& __a) _GLIBCXX_NOTHROW
: __allocator_base<_Tp>(__a) { }
#if __cplusplus >= 201103L
allocator& operator=(const allocator&) = default;
#endif
template<typename _Tp1>
_GLIBCXX20_CONSTEXPR
allocator(const allocator<_Tp1>&) _GLIBCXX_NOTHROW { }
#if __cpp_constexpr_dynamic_alloc
constexpr
#endif
~allocator() _GLIBCXX_NOTHROW { }
#if __cplusplus > 201703L
[[nodiscard,__gnu__::__always_inline__]]
constexpr _Tp*
allocate(size_t __n)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
#endif
return __allocator_base<_Tp>::allocate(__n, 0);
}
[[__gnu__::__always_inline__]]
constexpr void
deallocate(_Tp* __p, size_t __n)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
{
::operator delete(__p);
return;
}
#endif
__allocator_base<_Tp>::deallocate(__p, __n);
}
#endif
friend _GLIBCXX20_CONSTEXPR bool
operator==(const allocator&, const allocator&) _GLIBCXX_NOTHROW
{ return true; }
#if __cpp_impl_three_way_comparison < 201907L
friend _GLIBCXX20_CONSTEXPR bool
operator!=(const allocator&, const allocator&) _GLIBCXX_NOTHROW
{ return false; }
#endif
};
template<typename _T1, typename _T2>
inline _GLIBCXX20_CONSTEXPR bool
operator==(const allocator<_T1>&, const allocator<_T2>&)
_GLIBCXX_NOTHROW
{ return true; }
#if __cpp_impl_three_way_comparison < 201907L
template<typename _T1, typename _T2>
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
template<typename _Alloc, bool = __is_empty(_Alloc)>
struct __alloc_swap
{ static void _S_do_it(_Alloc&, _Alloc&) _GLIBCXX_NOEXCEPT { } };
template<typename _Alloc>
struct __alloc_swap<_Alloc, false>
{
static void
_S_do_it(_Alloc& __one, _Alloc& __two) _GLIBCXX_NOEXCEPT
{
if (__one != __two)
swap(__one, __two);
}
};
template<typename _Alloc, bool = __is_empty(_Alloc)>
struct __alloc_neq
{
static bool
_S_do_it(const _Alloc&, const _Alloc&)
{ return false; }
};
template<typename _Alloc>
struct __alloc_neq<_Alloc, false>
{
static bool
_S_do_it(const _Alloc& __one, const _Alloc& __two)
{ return __one != __two; }
};
#if __cplusplus >= 201103L
template<typename _Tp, bool
= __or_<is_copy_constructible<typename _Tp::value_type>,
is_nothrow_move_constructible<typename _Tp::value_type>>::value>
struct __shrink_to_fit_aux
{ static bool _S_do_it(_Tp&) noexcept { return false; } };
template<typename _Tp>
struct __shrink_to_fit_aux<_Tp, true>
{
static bool
_S_do_it(_Tp& __c) noexcept
{
#if __cpp_exceptions
try
{
_Tp(__make_move_if_noexcept_iterator(__c.begin()),
__make_move_if_noexcept_iterator(__c.end()),
__c.get_allocator()).swap(__c);
return true;
}
catch(...)
{ return false; }
#else
return false;
#endif
}
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif