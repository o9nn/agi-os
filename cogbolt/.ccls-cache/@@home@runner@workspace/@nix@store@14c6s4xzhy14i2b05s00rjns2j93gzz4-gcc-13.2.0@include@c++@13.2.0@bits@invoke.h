#ifndef _GLIBCXX_INVOKE_H
#define _GLIBCXX_INVOKE_H 1
#pragma GCC system_header
#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else
#include <type_traits>
#include <bits/move.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Tp, typename _Up = typename __inv_unwrap<_Tp>::type>
constexpr _Up&&
__invfwd(typename remove_reference<_Tp>::type& __t) noexcept
{ return static_cast<_Up&&>(__t); }
template<typename _Res, typename _Fn, typename... _Args>
constexpr _Res
__invoke_impl(__invoke_other, _Fn&& __f, _Args&&... __args)
{ return std::forward<_Fn>(__f)(std::forward<_Args>(__args)...); }
template<typename _Res, typename _MemFun, typename _Tp, typename... _Args>
constexpr _Res
__invoke_impl(__invoke_memfun_ref, _MemFun&& __f, _Tp&& __t,
_Args&&... __args)
{ return (__invfwd<_Tp>(__t).*__f)(std::forward<_Args>(__args)...); }
template<typename _Res, typename _MemFun, typename _Tp, typename... _Args>
constexpr _Res
__invoke_impl(__invoke_memfun_deref, _MemFun&& __f, _Tp&& __t,
_Args&&... __args)
{
return ((*std::forward<_Tp>(__t)).*__f)(std::forward<_Args>(__args)...);
}
template<typename _Res, typename _MemPtr, typename _Tp>
constexpr _Res
__invoke_impl(__invoke_memobj_ref, _MemPtr&& __f, _Tp&& __t)
{ return __invfwd<_Tp>(__t).*__f; }
template<typename _Res, typename _MemPtr, typename _Tp>
constexpr _Res
__invoke_impl(__invoke_memobj_deref, _MemPtr&& __f, _Tp&& __t)
{ return (*std::forward<_Tp>(__t)).*__f; }
template<typename _Callable, typename... _Args>
constexpr typename __invoke_result<_Callable, _Args...>::type
__invoke(_Callable&& __fn, _Args&&... __args)
noexcept(__is_nothrow_invocable<_Callable, _Args...>::value)
{
using __result = __invoke_result<_Callable, _Args...>;
using __type = typename __result::type;
using __tag = typename __result::__invoke_type;
return std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
std::forward<_Args>(__args)...);
}
#if __cplusplus >= 201703L
template<typename _Res, typename _Callable, typename... _Args>
constexpr enable_if_t<is_invocable_r_v<_Res, _Callable, _Args...>, _Res>
__invoke_r(_Callable&& __fn, _Args&&... __args)
noexcept(is_nothrow_invocable_r_v<_Res, _Callable, _Args...>)
{
using __result = __invoke_result<_Callable, _Args...>;
using __type = typename __result::type;
using __tag = typename __result::__invoke_type;
if constexpr (is_void_v<_Res>)
std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
std::forward<_Args>(__args)...);
else
return std::__invoke_impl<__type>(__tag{},
std::forward<_Callable>(__fn),
std::forward<_Args>(__args)...);
}
#else
template<typename _Res, typename _Callable, typename... _Args>
constexpr __enable_if_t<!is_void<_Res>::value, _Res>
__invoke_r(_Callable&& __fn, _Args&&... __args)
{
using __result = __invoke_result<_Callable, _Args...>;
using __type = typename __result::type;
#if __has_builtin(__reference_converts_from_temporary)
static_assert(!__reference_converts_from_temporary(_Res, __type),
"INVOKE<R> must not create a dangling reference");
#endif
using __tag = typename __result::__invoke_type;
return std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
std::forward<_Args>(__args)...);
}
template<typename _Res, typename _Callable, typename... _Args>
_GLIBCXX14_CONSTEXPR __enable_if_t<is_void<_Res>::value, _Res>
__invoke_r(_Callable&& __fn, _Args&&... __args)
{
using __result = __invoke_result<_Callable, _Args...>;
using __type = typename __result::type;
using __tag = typename __result::__invoke_type;
std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
std::forward<_Args>(__args)...);
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif