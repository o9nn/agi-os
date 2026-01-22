#ifndef _STL_CONSTRUCT_H
#define _STL_CONSTRUCT_H 1
#include <new>
#include <bits/move.h>
#include <bits/stl_iterator_base_types.h>
#include <bits/stl_iterator_base_funcs.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cplusplus >= 201703L
template <typename _Tp>
_GLIBCXX20_CONSTEXPR inline void
destroy_at(_Tp* __location)
{
if constexpr (__cplusplus > 201703L && is_array_v<_Tp>)
{
for (auto& __x : *__location)
std::destroy_at(std::__addressof(__x));
}
else
__location->~_Tp();
}
#if __cplusplus >= 202002L
template<typename _Tp, typename... _Args>
constexpr auto
construct_at(_Tp* __location, _Args&&... __args)
noexcept(noexcept(::new((void*)0) _Tp(std::declval<_Args>()...)))
-> decltype(::new((void*)0) _Tp(std::declval<_Args>()...))
{ return ::new((void*)__location) _Tp(std::forward<_Args>(__args)...); }
#endif
#endif
#if __cplusplus >= 201103L
template<typename _Tp, typename... _Args>
_GLIBCXX20_CONSTEXPR
inline void
_Construct(_Tp* __p, _Args&&... __args)
{
#if __cplusplus >= 202002L
if (std::__is_constant_evaluated())
{
std::construct_at(__p, std::forward<_Args>(__args)...);
return;
}
#endif
::new((void*)__p) _Tp(std::forward<_Args>(__args)...);
}
#else
template<typename _T1, typename _T2>
inline void
_Construct(_T1* __p, const _T2& __value)
{
::new(static_cast<void*>(__p)) _T1(__value);
}
#endif
template<typename _T1>
inline void
_Construct_novalue(_T1* __p)
{ ::new((void*)__p) _T1; }
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR void
_Destroy(_ForwardIterator __first, _ForwardIterator __last);
template<typename _Tp>
_GLIBCXX14_CONSTEXPR inline void
_Destroy(_Tp* __pointer)
{
#if __cplusplus > 201703L
std::destroy_at(__pointer);
#else
__pointer->~_Tp();
#endif
}
template<bool>
struct _Destroy_aux
{
template<typename _ForwardIterator>
static _GLIBCXX20_CONSTEXPR void
__destroy(_ForwardIterator __first, _ForwardIterator __last)
{
for (; __first != __last; ++__first)
std::_Destroy(std::__addressof(*__first));
}
};
template<>
struct _Destroy_aux<true>
{
template<typename _ForwardIterator>
static void
__destroy(_ForwardIterator, _ForwardIterator) { }
};
template<typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR inline void
_Destroy(_ForwardIterator __first, _ForwardIterator __last)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_Value_type;
#if __cplusplus >= 201103L
static_assert(is_destructible<_Value_type>::value,
"value type is destructible");
#endif
#if __cplusplus >= 202002L
if (std::__is_constant_evaluated())
return std::_Destroy_aux<false>::__destroy(__first, __last);
#endif
std::_Destroy_aux<__has_trivial_destructor(_Value_type)>::
__destroy(__first, __last);
}
template<bool>
struct _Destroy_n_aux
{
template<typename _ForwardIterator, typename _Size>
static _GLIBCXX20_CONSTEXPR _ForwardIterator
__destroy_n(_ForwardIterator __first, _Size __count)
{
for (; __count > 0; (void)++__first, --__count)
std::_Destroy(std::__addressof(*__first));
return __first;
}
};
template<>
struct _Destroy_n_aux<true>
{
template<typename _ForwardIterator, typename _Size>
static _ForwardIterator
__destroy_n(_ForwardIterator __first, _Size __count)
{
std::advance(__first, __count);
return __first;
}
};
template<typename _ForwardIterator, typename _Size>
_GLIBCXX20_CONSTEXPR inline _ForwardIterator
_Destroy_n(_ForwardIterator __first, _Size __count)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_Value_type;
#if __cplusplus >= 201103L
static_assert(is_destructible<_Value_type>::value,
"value type is destructible");
#endif
#if __cplusplus >= 202002L
if (std::__is_constant_evaluated())
return std::_Destroy_n_aux<false>::__destroy_n(__first, __count);
#endif
return std::_Destroy_n_aux<__has_trivial_destructor(_Value_type)>::
__destroy_n(__first, __count);
}
#if __cplusplus >= 201703L
template <typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR inline void
destroy(_ForwardIterator __first, _ForwardIterator __last)
{
std::_Destroy(__first, __last);
}
template <typename _ForwardIterator, typename _Size>
_GLIBCXX20_CONSTEXPR inline _ForwardIterator
destroy_n(_ForwardIterator __first, _Size __count)
{
return std::_Destroy_n(__first, __count);
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif