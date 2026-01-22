#ifndef _PTR_TRAITS_H
#define _PTR_TRAITS_H 1
#if __cplusplus >= 201103L
#include <bits/move.h>
#if __cplusplus > 202002L && defined(__cpp_constexpr_dynamic_alloc)
# define __cpp_lib_constexpr_memory 202202L
#elif __cplusplus > 201703L
# define __cpp_lib_constexpr_memory 201811L
#endif
#if __cplusplus > 201703L
#include <concepts>
namespace __gnu_debug { struct _Safe_iterator_base; }
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class __undefined;
template<typename _Tp>
struct __get_first_arg
{ using type = __undefined; };
template<template<typename, typename...> class _SomeTemplate, typename _Tp,
typename... _Types>
struct __get_first_arg<_SomeTemplate<_Tp, _Types...>>
{ using type = _Tp; };
template<typename _Tp, typename _Up>
struct __replace_first_arg
{ };
template<template<typename, typename...> class _SomeTemplate, typename _Up,
typename _Tp, typename... _Types>
struct __replace_first_arg<_SomeTemplate<_Tp, _Types...>, _Up>
{ using type = _SomeTemplate<_Up, _Types...>; };
template<typename _Ptr, typename = void>
struct __ptr_traits_elem : __get_first_arg<_Ptr>
{ };
#if __cpp_concepts
template<typename _Ptr> requires requires { typename _Ptr::element_type; }
struct __ptr_traits_elem<_Ptr, void>
{ using type = typename _Ptr::element_type; };
#else
template<typename _Ptr>
struct __ptr_traits_elem<_Ptr, __void_t<typename _Ptr::element_type>>
{ using type = typename _Ptr::element_type; };
#endif
template<typename _Ptr>
using __ptr_traits_elem_t = typename __ptr_traits_elem<_Ptr>::type;
template<typename _Ptr, typename _Elt, bool = is_void<_Elt>::value>
struct __ptr_traits_ptr_to
{
using pointer = _Ptr;
using element_type = _Elt;
static pointer
pointer_to(element_type& __r)
#if __cpp_lib_concepts
requires requires {
{ pointer::pointer_to(__r) } -> convertible_to<pointer>;
}
#endif
{ return pointer::pointer_to(__r); }
};
template<typename _Ptr, typename _Elt>
struct __ptr_traits_ptr_to<_Ptr, _Elt, true>
{ };
template<typename _Tp>
struct __ptr_traits_ptr_to<_Tp*, _Tp, false>
{
using pointer = _Tp*;
using element_type = _Tp;
static _GLIBCXX20_CONSTEXPR pointer
pointer_to(element_type& __r) noexcept
{ return std::addressof(__r); }
};
template<typename _Ptr, typename _Elt>
struct __ptr_traits_impl : __ptr_traits_ptr_to<_Ptr, _Elt>
{
private:
template<typename _Tp>
using __diff_t = typename _Tp::difference_type;
template<typename _Tp, typename _Up>
using __rebind = __type_identity<typename _Tp::template rebind<_Up>>;
public:
using pointer = _Ptr;
using element_type = _Elt;
using difference_type = __detected_or_t<ptrdiff_t, __diff_t, _Ptr>;
template<typename _Up>
using rebind = typename __detected_or_t<__replace_first_arg<_Ptr, _Up>,
__rebind, _Ptr, _Up>::type;
};
template<typename _Ptr>
struct __ptr_traits_impl<_Ptr, __undefined>
{ };
template<typename _Ptr>
struct pointer_traits : __ptr_traits_impl<_Ptr, __ptr_traits_elem_t<_Ptr>>
{ };
template<typename _Tp>
struct pointer_traits<_Tp*> : __ptr_traits_ptr_to<_Tp*, _Tp>
{
typedef _Tp* pointer;
typedef _Tp element_type;
typedef ptrdiff_t difference_type;
template<typename _Up> using rebind = _Up*;
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