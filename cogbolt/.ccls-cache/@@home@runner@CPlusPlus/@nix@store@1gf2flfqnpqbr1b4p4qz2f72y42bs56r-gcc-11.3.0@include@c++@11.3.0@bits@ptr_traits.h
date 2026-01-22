#ifndef _PTR_TRAITS_H
#define _PTR_TRAITS_H 1
#if __cplusplus >= 201103L
#include <bits/move.h>
#if __cplusplus > 201703L
#define __cpp_lib_constexpr_memory 201811L
namespace __gnu_debug { struct _Safe_iterator_base; }
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class __undefined;
template<typename _Tp, typename _Up>
struct __replace_first_arg
{ };
template<template<typename, typename...> class _Template, typename _Up,
typename _Tp, typename... _Types>
struct __replace_first_arg<_Template<_Tp, _Types...>, _Up>
{ using type = _Template<_Up, _Types...>; };
template<typename _Tp, typename _Up>
using __replace_first_arg_t = typename __replace_first_arg<_Tp, _Up>::type;
template<typename _Tp>
using __make_not_void
= typename conditional<is_void<_Tp>::value, __undefined, _Tp>::type;
template<typename _Ptr>
struct __ptr_traits_elem_1
{ };
template<template<typename, typename...> class _SomePointer, typename _Tp,
typename... _Args>
struct __ptr_traits_elem_1<_SomePointer<_Tp, _Args...>>
{
using element_type = _Tp;
using pointer = _SomePointer<_Tp, _Args...>;
static pointer
pointer_to(__make_not_void<element_type>& __e)
{ return pointer::pointer_to(__e); }
};
template<typename _Ptr, typename = void>
struct __ptr_traits_elem : __ptr_traits_elem_1<_Ptr>
{ };
template<typename _Ptr>
struct __ptr_traits_elem<_Ptr, __void_t<typename _Ptr::element_type>>
{
using element_type = typename _Ptr::element_type;
static _Ptr
pointer_to(__make_not_void<element_type>& __e)
{ return _Ptr::pointer_to(__e); }
};
template<typename _Ptr>
struct pointer_traits : __ptr_traits_elem<_Ptr>
{
private:
template<typename _Tp>
using __difference_type = typename _Tp::difference_type;
template<typename _Tp, typename _Up, typename = void>
struct __rebind : __replace_first_arg<_Tp, _Up> { };
template<typename _Tp, typename _Up>
struct __rebind<_Tp, _Up, __void_t<typename _Tp::template rebind<_Up>>>
{ using type = typename _Tp::template rebind<_Up>; };
public:
using pointer = _Ptr;
using difference_type
= __detected_or_t<ptrdiff_t, __difference_type, _Ptr>;
template<typename _Up>
using rebind = typename __rebind<_Ptr, _Up>::type;
};
template<typename _Tp>
struct pointer_traits<_Tp*>
{
typedef _Tp* pointer;
typedef _Tp  element_type;
typedef ptrdiff_t difference_type;
template<typename _Up>
using rebind = _Up*;
static _GLIBCXX20_CONSTEXPR pointer
pointer_to(__make_not_void<element_type>& __r) noexcept
{ return std::addressof(__r); }
};
template<typename _Ptr, typename _Tp>
using __ptr_rebind = typename pointer_traits<_Ptr>::template rebind<_Tp>;
template<typename _Tp>
constexpr _Tp*
__to_address(_Tp* __ptr) noexcept
{
static_assert(!std::is_function<_Tp>::value, "not a function pointer");
return __ptr;
}
#if __cplusplus <= 201703L
template<typename _Ptr>
constexpr typename std::pointer_traits<_Ptr>::element_type*
__to_address(const _Ptr& __ptr)
{ return std::__to_address(__ptr.operator->()); }
#else
template<typename _Ptr>
constexpr auto
__to_address(const _Ptr& __ptr) noexcept
-> decltype(std::pointer_traits<_Ptr>::to_address(__ptr))
{ return std::pointer_traits<_Ptr>::to_address(__ptr); }
template<typename _Ptr, typename... _None>
constexpr auto
__to_address(const _Ptr& __ptr, _None...) noexcept
{
if constexpr (is_base_of_v<__gnu_debug::_Safe_iterator_base, _Ptr>)
return std::__to_address(__ptr.base().operator->());
else
return std::__to_address(__ptr.operator->());
}
#define __cpp_lib_to_address 201711L
template<typename _Tp>
constexpr _Tp*
to_address(_Tp* __ptr) noexcept
{ return std::__to_address(__ptr); }
template<typename _Ptr>
constexpr auto
to_address(const _Ptr& __ptr) noexcept
{ return std::__to_address(__ptr); }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif