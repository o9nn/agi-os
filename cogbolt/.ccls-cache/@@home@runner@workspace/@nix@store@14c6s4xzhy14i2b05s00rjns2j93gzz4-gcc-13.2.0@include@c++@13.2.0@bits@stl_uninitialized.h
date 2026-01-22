#ifndef _STL_UNINITIALIZED_H
#define _STL_UNINITIALIZED_H 1
#if __cplusplus >= 201103L
#include <type_traits>
#endif
#include <bits/stl_algobase.h>
#include <ext/alloc_traits.h>
#if __cplusplus >= 201703L
#include <bits/stl_pair.h>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cplusplus >= 201103L
template<typename _ValueType, typename _Tp>
constexpr bool
__check_constructible()
{
static_assert(is_constructible<_ValueType, _Tp>::value,
"result type must be constructible from input type");
return true;
}
# define _GLIBCXX_USE_ASSIGN_FOR_INIT(T, U) \
__is_trivial(T) && __is_assignable(T&, U) \
&& std::__check_constructible<T, U>()
#else
# define _GLIBCXX_USE_ASSIGN_FOR_INIT(T, U) \
__is_trivial(T) && __is_assignable(T&, U)
#endif
template<typename _InputIterator, typename _ForwardIterator>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__do_uninit_copy(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result)
{
_ForwardIterator __cur = __result;
__try
{
for (; __first != __last; ++__first, (void)++__cur)
std::_Construct(std::__addressof(*__cur), *__first);
return __cur;
}
__catch(...)
{
std::_Destroy(__result, __cur);
__throw_exception_again;
}
}
template<bool _TrivialValueTypes>
struct __uninitialized_copy
{
template<typename _InputIterator, typename _ForwardIterator>
static _ForwardIterator
__uninit_copy(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result)
{ return std::__do_uninit_copy(__first, __last, __result); }
};
template<>
struct __uninitialized_copy<true>
{
template<typename _InputIterator, typename _ForwardIterator>
static _ForwardIterator
__uninit_copy(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result)
{ return std::copy(__first, __last, __result); }
};
template<typename _InputIterator, typename _ForwardIterator>
inline _ForwardIterator
uninitialized_copy(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result)
{
typedef typename iterator_traits<_InputIterator>::value_type
_ValueType1;
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType2;
const bool __can_memmove = __is_trivial(_ValueType1);
#if __cplusplus < 201103L
typedef typename iterator_traits<_InputIterator>::reference _From;
#else
using _From = decltype(*__first);
#endif
const bool __assignable
= _GLIBCXX_USE_ASSIGN_FOR_INIT(_ValueType2, _From);
return std::__uninitialized_copy<__can_memmove && __assignable>::
__uninit_copy(__first, __last, __result);
}
template<typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR void
__do_uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x)
{
_ForwardIterator __cur = __first;
__try
{
for (; __cur != __last; ++__cur)
std::_Construct(std::__addressof(*__cur), __x);
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
template<bool _TrivialValueType>
struct __uninitialized_fill
{
template<typename _ForwardIterator, typename _Tp>
static void
__uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x)
{ std::__do_uninit_fill(__first, __last, __x); }
};
template<>
struct __uninitialized_fill<true>
{
template<typename _ForwardIterator, typename _Tp>
static void
__uninit_fill(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x)
{ std::fill(__first, __last, __x); }
};
template<typename _ForwardIterator, typename _Tp>
inline void
uninitialized_fill(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
const bool __can_fill
= _GLIBCXX_USE_ASSIGN_FOR_INIT(_ValueType, const _Tp&);
std::__uninitialized_fill<__can_fill>::
__uninit_fill(__first, __last, __x);
}
template<typename _ForwardIterator, typename _Size, typename _Tp>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__do_uninit_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)
{
_ForwardIterator __cur = __first;
__try
{
for (; __n > 0; --__n, (void) ++__cur)
std::_Construct(std::__addressof(*__cur), __x);
return __cur;
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
template<bool _TrivialValueType>
struct __uninitialized_fill_n
{
template<typename _ForwardIterator, typename _Size, typename _Tp>
static _ForwardIterator
__uninit_fill_n(_ForwardIterator __first, _Size __n,
const _Tp& __x)
{ return std::__do_uninit_fill_n(__first, __n, __x); }
};
template<>
struct __uninitialized_fill_n<true>
{
template<typename _ForwardIterator, typename _Size, typename _Tp>
static _ForwardIterator
__uninit_fill_n(_ForwardIterator __first, _Size __n,
const _Tp& __x)
{ return std::fill_n(__first, __n, __x); }
};
template<typename _ForwardIterator, typename _Size, typename _Tp>
inline _ForwardIterator
uninitialized_fill_n(_ForwardIterator __first, _Size __n, const _Tp& __x)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
const bool __can_fill
= _GLIBCXX_USE_ASSIGN_FOR_INIT(_ValueType, const _Tp&)
&& __is_integer<_Size>::__value;
return __uninitialized_fill_n<__can_fill>::
__uninit_fill_n(__first, __n, __x);
}
#undef _GLIBCXX_USE_ASSIGN_FOR_INIT
template<typename _InputIterator, typename _ForwardIterator,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__uninitialized_copy_a(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, _Allocator& __alloc)
{
_ForwardIterator __cur = __result;
__try
{
typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
for (; __first != __last; ++__first, (void)++__cur)
__traits::construct(__alloc, std::__addressof(*__cur), *__first);
return __cur;
}
__catch(...)
{
std::_Destroy(__result, __cur, __alloc);
__throw_exception_again;
}
}
#if _GLIBCXX_HOSTED
template<typename _InputIterator, typename _ForwardIterator, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_copy_a(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, allocator<_Tp>&)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
return std::__do_uninit_copy(__first, __last, __result);
#endif
return std::uninitialized_copy(__first, __last, __result);
}
#endif
template<typename _InputIterator, typename _ForwardIterator,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_move_a(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, _Allocator& __alloc)
{
return std::__uninitialized_copy_a(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
_GLIBCXX_MAKE_MOVE_ITERATOR(__last),
__result, __alloc);
}
template<typename _InputIterator, typename _ForwardIterator,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_move_if_noexcept_a(_InputIterator __first,
_InputIterator __last,
_ForwardIterator __result,
_Allocator& __alloc)
{
return std::__uninitialized_copy_a
(_GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(__first),
_GLIBCXX_MAKE_MOVE_IF_NOEXCEPT_ITERATOR(__last), __result, __alloc);
}
template<typename _ForwardIterator, typename _Tp, typename _Allocator>
_GLIBCXX20_CONSTEXPR
void
__uninitialized_fill_a(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x, _Allocator& __alloc)
{
_ForwardIterator __cur = __first;
__try
{
typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
for (; __cur != __last; ++__cur)
__traits::construct(__alloc, std::__addressof(*__cur), __x);
}
__catch(...)
{
std::_Destroy(__first, __cur, __alloc);
__throw_exception_again;
}
}
#if _GLIBCXX_HOSTED
template<typename _ForwardIterator, typename _Tp, typename _Tp2>
_GLIBCXX20_CONSTEXPR
inline void
__uninitialized_fill_a(_ForwardIterator __first, _ForwardIterator __last,
const _Tp& __x, allocator<_Tp2>&)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
return std::__do_uninit_fill(__first, __last, __x);
#endif
std::uninitialized_fill(__first, __last, __x);
}
#endif
template<typename _ForwardIterator, typename _Size, typename _Tp,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
_ForwardIterator
__uninitialized_fill_n_a(_ForwardIterator __first, _Size __n,
const _Tp& __x, _Allocator& __alloc)
{
_ForwardIterator __cur = __first;
__try
{
typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
for (; __n > 0; --__n, (void) ++__cur)
__traits::construct(__alloc, std::__addressof(*__cur), __x);
return __cur;
}
__catch(...)
{
std::_Destroy(__first, __cur, __alloc);
__throw_exception_again;
}
}
#if _GLIBCXX_HOSTED
template<typename _ForwardIterator, typename _Size, typename _Tp,
typename _Tp2>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_fill_n_a(_ForwardIterator __first, _Size __n,
const _Tp& __x, allocator<_Tp2>&)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
return std::__do_uninit_fill_n(__first, __n, __x);
#endif
return std::uninitialized_fill_n(__first, __n, __x);
}
#endif
template<typename _InputIterator1, typename _InputIterator2,
typename _ForwardIterator, typename _Allocator>
inline _ForwardIterator
__uninitialized_copy_move(_InputIterator1 __first1,
_InputIterator1 __last1,
_InputIterator2 __first2,
_InputIterator2 __last2,
_ForwardIterator __result,
_Allocator& __alloc)
{
_ForwardIterator __mid = std::__uninitialized_copy_a(__first1, __last1,
__result,
__alloc);
__try
{
return std::__uninitialized_move_a(__first2, __last2, __mid, __alloc);
}
__catch(...)
{
std::_Destroy(__result, __mid, __alloc);
__throw_exception_again;
}
}
template<typename _InputIterator1, typename _InputIterator2,
typename _ForwardIterator, typename _Allocator>
inline _ForwardIterator
__uninitialized_move_copy(_InputIterator1 __first1,
_InputIterator1 __last1,
_InputIterator2 __first2,
_InputIterator2 __last2,
_ForwardIterator __result,
_Allocator& __alloc)
{
_ForwardIterator __mid = std::__uninitialized_move_a(__first1, __last1,
__result,
__alloc);
__try
{
return std::__uninitialized_copy_a(__first2, __last2, __mid, __alloc);
}
__catch(...)
{
std::_Destroy(__result, __mid, __alloc);
__throw_exception_again;
}
}
template<typename _ForwardIterator, typename _Tp, typename _InputIterator,
typename _Allocator>
inline _ForwardIterator
__uninitialized_fill_move(_ForwardIterator __result, _ForwardIterator __mid,
const _Tp& __x, _InputIterator __first,
_InputIterator __last, _Allocator& __alloc)
{
std::__uninitialized_fill_a(__result, __mid, __x, __alloc);
__try
{
return std::__uninitialized_move_a(__first, __last, __mid, __alloc);
}
__catch(...)
{
std::_Destroy(__result, __mid, __alloc);
__throw_exception_again;
}
}
template<typename _InputIterator, typename _ForwardIterator, typename _Tp,
typename _Allocator>
inline void
__uninitialized_move_fill(_InputIterator __first1, _InputIterator __last1,
_ForwardIterator __first2,
_ForwardIterator __last2, const _Tp& __x,
_Allocator& __alloc)
{
_ForwardIterator __mid2 = std::__uninitialized_move_a(__first1, __last1,
__first2,
__alloc);
__try
{
std::__uninitialized_fill_a(__mid2, __last2, __x, __alloc);
}
__catch(...)
{
std::_Destroy(__first2, __mid2, __alloc);
__throw_exception_again;
}
}
#if __cplusplus >= 201103L
template<bool _TrivialValueType>
struct __uninitialized_default_1
{
template<typename _ForwardIterator>
static void
__uninit_default(_ForwardIterator __first, _ForwardIterator __last)
{
_ForwardIterator __cur = __first;
__try
{
for (; __cur != __last; ++__cur)
std::_Construct(std::__addressof(*__cur));
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
};
template<>
struct __uninitialized_default_1<true>
{
template<typename _ForwardIterator>
static void
__uninit_default(_ForwardIterator __first, _ForwardIterator __last)
{
if (__first == __last)
return;
typename iterator_traits<_ForwardIterator>::value_type* __val
= std::__addressof(*__first);
std::_Construct(__val);
if (++__first != __last)
std::fill(__first, __last, *__val);
}
};
template<bool _TrivialValueType>
struct __uninitialized_default_n_1
{
template<typename _ForwardIterator, typename _Size>
_GLIBCXX20_CONSTEXPR
static _ForwardIterator
__uninit_default_n(_ForwardIterator __first, _Size __n)
{
_ForwardIterator __cur = __first;
__try
{
for (; __n > 0; --__n, (void) ++__cur)
std::_Construct(std::__addressof(*__cur));
return __cur;
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
};
template<>
struct __uninitialized_default_n_1<true>
{
template<typename _ForwardIterator, typename _Size>
_GLIBCXX20_CONSTEXPR
static _ForwardIterator
__uninit_default_n(_ForwardIterator __first, _Size __n)
{
if (__n > 0)
{
typename iterator_traits<_ForwardIterator>::value_type* __val
= std::__addressof(*__first);
std::_Construct(__val);
++__first;
__first = std::fill_n(__first, __n - 1, *__val);
}
return __first;
}
};
template<typename _ForwardIterator>
inline void
__uninitialized_default(_ForwardIterator __first,
_ForwardIterator __last)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
const bool __assignable = is_copy_assignable<_ValueType>::value;
std::__uninitialized_default_1<__is_trivial(_ValueType)
&& __assignable>::
__uninit_default(__first, __last);
}
template<typename _ForwardIterator, typename _Size>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_default_n(_ForwardIterator __first, _Size __n)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
return __uninitialized_default_n_1<false>::
__uninit_default_n(__first, __n);
#endif
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
constexpr bool __can_fill
= __and_<is_integral<_Size>, is_copy_assignable<_ValueType>>::value;
return __uninitialized_default_n_1<__is_trivial(_ValueType)
&& __can_fill>::
__uninit_default_n(__first, __n);
}
template<typename _ForwardIterator, typename _Allocator>
void
__uninitialized_default_a(_ForwardIterator __first,
_ForwardIterator __last,
_Allocator& __alloc)
{
_ForwardIterator __cur = __first;
__try
{
typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
for (; __cur != __last; ++__cur)
__traits::construct(__alloc, std::__addressof(*__cur));
}
__catch(...)
{
std::_Destroy(__first, __cur, __alloc);
__throw_exception_again;
}
}
#if _GLIBCXX_HOSTED
template<typename _ForwardIterator, typename _Tp>
inline void
__uninitialized_default_a(_ForwardIterator __first,
_ForwardIterator __last,
allocator<_Tp>&)
{ std::__uninitialized_default(__first, __last); }
#endif
template<typename _ForwardIterator, typename _Size, typename _Allocator>
_GLIBCXX20_CONSTEXPR _ForwardIterator
__uninitialized_default_n_a(_ForwardIterator __first, _Size __n,
_Allocator& __alloc)
{
_ForwardIterator __cur = __first;
__try
{
typedef __gnu_cxx::__alloc_traits<_Allocator> __traits;
for (; __n > 0; --__n, (void) ++__cur)
__traits::construct(__alloc, std::__addressof(*__cur));
return __cur;
}
__catch(...)
{
std::_Destroy(__first, __cur, __alloc);
__throw_exception_again;
}
}
#if _GLIBCXX_HOSTED
template<typename _ForwardIterator, typename _Size, typename _Tp>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__uninitialized_default_n_a(_ForwardIterator __first, _Size __n,
allocator<_Tp>&)
{ return std::__uninitialized_default_n(__first, __n); }
#endif
template<bool _TrivialValueType>
struct __uninitialized_default_novalue_1
{
template<typename _ForwardIterator>
static void
__uninit_default_novalue(_ForwardIterator __first,
_ForwardIterator __last)
{
_ForwardIterator __cur = __first;
__try
{
for (; __cur != __last; ++__cur)
std::_Construct_novalue(std::__addressof(*__cur));
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
};
template<>
struct __uninitialized_default_novalue_1<true>
{
template<typename _ForwardIterator>
static void
__uninit_default_novalue(_ForwardIterator __first,
_ForwardIterator __last)
{
}
};
template<bool _TrivialValueType>
struct __uninitialized_default_novalue_n_1
{
template<typename _ForwardIterator, typename _Size>
static _ForwardIterator
__uninit_default_novalue_n(_ForwardIterator __first, _Size __n)
{
_ForwardIterator __cur = __first;
__try
{
for (; __n > 0; --__n, (void) ++__cur)
std::_Construct_novalue(std::__addressof(*__cur));
return __cur;
}
__catch(...)
{
std::_Destroy(__first, __cur);
__throw_exception_again;
}
}
};
template<>
struct __uninitialized_default_novalue_n_1<true>
{
template<typename _ForwardIterator, typename _Size>
static _ForwardIterator
__uninit_default_novalue_n(_ForwardIterator __first, _Size __n)
{ return std::next(__first, __n); }
};
template<typename _ForwardIterator>
inline void
__uninitialized_default_novalue(_ForwardIterator __first,
_ForwardIterator __last)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
std::__uninitialized_default_novalue_1<
is_trivially_default_constructible<_ValueType>::value>::
__uninit_default_novalue(__first, __last);
}
template<typename _ForwardIterator, typename _Size>
inline _ForwardIterator
__uninitialized_default_novalue_n(_ForwardIterator __first, _Size __n)
{
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType;
return __uninitialized_default_novalue_n_1<
is_trivially_default_constructible<_ValueType>::value>::
__uninit_default_novalue_n(__first, __n);
}
template<typename _InputIterator, typename _Size,
typename _ForwardIterator>
_ForwardIterator
__uninitialized_copy_n(_InputIterator __first, _Size __n,
_ForwardIterator __result, input_iterator_tag)
{
_ForwardIterator __cur = __result;
__try
{
for (; __n > 0; --__n, (void) ++__first, ++__cur)
std::_Construct(std::__addressof(*__cur), *__first);
return __cur;
}
__catch(...)
{
std::_Destroy(__result, __cur);
__throw_exception_again;
}
}
template<typename _RandomAccessIterator, typename _Size,
typename _ForwardIterator>
inline _ForwardIterator
__uninitialized_copy_n(_RandomAccessIterator __first, _Size __n,
_ForwardIterator __result,
random_access_iterator_tag)
{ return std::uninitialized_copy(__first, __first + __n, __result); }
template<typename _InputIterator, typename _Size,
typename _ForwardIterator>
pair<_InputIterator, _ForwardIterator>
__uninitialized_copy_n_pair(_InputIterator __first, _Size __n,
_ForwardIterator __result, input_iterator_tag)
{
_ForwardIterator __cur = __result;
__try
{
for (; __n > 0; --__n, (void) ++__first, ++__cur)
std::_Construct(std::__addressof(*__cur), *__first);
return {__first, __cur};
}
__catch(...)
{
std::_Destroy(__result, __cur);
__throw_exception_again;
}
}
template<typename _RandomAccessIterator, typename _Size,
typename _ForwardIterator>
inline pair<_RandomAccessIterator, _ForwardIterator>
__uninitialized_copy_n_pair(_RandomAccessIterator __first, _Size __n,
_ForwardIterator __result,
random_access_iterator_tag)
{
auto __second_res = uninitialized_copy(__first, __first + __n, __result);
auto __first_res = std::next(__first, __n);
return {__first_res, __second_res};
}
template<typename _InputIterator, typename _Size, typename _ForwardIterator>
inline _ForwardIterator
uninitialized_copy_n(_InputIterator __first, _Size __n,
_ForwardIterator __result)
{ return std::__uninitialized_copy_n(__first, __n, __result,
std::__iterator_category(__first)); }
template<typename _InputIterator, typename _Size, typename _ForwardIterator>
inline pair<_InputIterator, _ForwardIterator>
__uninitialized_copy_n_pair(_InputIterator __first, _Size __n,
_ForwardIterator __result)
{
return
std::__uninitialized_copy_n_pair(__first, __n, __result,
std::__iterator_category(__first));
}
#endif
#if __cplusplus >= 201703L
# define __cpp_lib_raw_memory_algorithms 201606L
template <typename _ForwardIterator>
inline void
uninitialized_default_construct(_ForwardIterator __first,
_ForwardIterator __last)
{
__uninitialized_default_novalue(__first, __last);
}
template <typename _ForwardIterator, typename _Size>
inline _ForwardIterator
uninitialized_default_construct_n(_ForwardIterator __first, _Size __count)
{
return __uninitialized_default_novalue_n(__first, __count);
}
template <typename _ForwardIterator>
inline void
uninitialized_value_construct(_ForwardIterator __first,
_ForwardIterator __last)
{
return __uninitialized_default(__first, __last);
}
template <typename _ForwardIterator, typename _Size>
inline _ForwardIterator
uninitialized_value_construct_n(_ForwardIterator __first, _Size __count)
{
return __uninitialized_default_n(__first, __count);
}
template <typename _InputIterator, typename _ForwardIterator>
inline _ForwardIterator
uninitialized_move(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result)
{
return std::uninitialized_copy
(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
_GLIBCXX_MAKE_MOVE_ITERATOR(__last), __result);
}
template <typename _InputIterator, typename _Size, typename _ForwardIterator>
inline pair<_InputIterator, _ForwardIterator>
uninitialized_move_n(_InputIterator __first, _Size __count,
_ForwardIterator __result)
{
auto __res = std::__uninitialized_copy_n_pair
(_GLIBCXX_MAKE_MOVE_ITERATOR(__first),
__count, __result);
return {__res.first.base(), __res.second};
}
#endif
#if __cplusplus >= 201103L
template<typename _Tp, typename _Up, typename _Allocator>
_GLIBCXX20_CONSTEXPR
inline void
__relocate_object_a(_Tp* __restrict __dest, _Up* __restrict __orig,
_Allocator& __alloc)
noexcept(noexcept(std::allocator_traits<_Allocator>::construct(__alloc,
__dest, std::move(*__orig)))
&& noexcept(std::allocator_traits<_Allocator>::destroy(
__alloc, std::__addressof(*__orig))))
{
typedef std::allocator_traits<_Allocator> __traits;
__traits::construct(__alloc, __dest, std::move(*__orig));
__traits::destroy(__alloc, std::__addressof(*__orig));
}
template<typename _Tp, typename = void>
struct __is_bitwise_relocatable
: is_trivial<_Tp> { };
template <typename _InputIterator, typename _ForwardIterator,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__relocate_a_1(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, _Allocator& __alloc)
noexcept(noexcept(std::__relocate_object_a(std::addressof(*__result),
std::addressof(*__first),
__alloc)))
{
typedef typename iterator_traits<_InputIterator>::value_type
_ValueType;
typedef typename iterator_traits<_ForwardIterator>::value_type
_ValueType2;
static_assert(std::is_same<_ValueType, _ValueType2>::value,
"relocation is only possible for values of the same type");
_ForwardIterator __cur = __result;
for (; __first != __last; ++__first, (void)++__cur)
std::__relocate_object_a(std::__addressof(*__cur),
std::__addressof(*__first), __alloc);
return __cur;
}
#if _GLIBCXX_HOSTED
template <typename _Tp, typename _Up>
_GLIBCXX20_CONSTEXPR
inline __enable_if_t<std::__is_bitwise_relocatable<_Tp>::value, _Tp*>
__relocate_a_1(_Tp* __first, _Tp* __last,
_Tp* __result,
[[__maybe_unused__]] allocator<_Up>& __alloc) noexcept
{
ptrdiff_t __count = __last - __first;
if (__count > 0)
{
#ifdef __cpp_lib_is_constant_evaluated
if (std::is_constant_evaluated())
{
__gnu_cxx::__normal_iterator<_Tp*, void> __out(__result);
__out = std::__relocate_a_1(__first, __last, __out, __alloc);
return __out.base();
}
#endif
__builtin_memmove(__result, __first, __count * sizeof(_Tp));
}
return __result + __count;
}
#endif
template <typename _InputIterator, typename _ForwardIterator,
typename _Allocator>
_GLIBCXX20_CONSTEXPR
inline _ForwardIterator
__relocate_a(_InputIterator __first, _InputIterator __last,
_ForwardIterator __result, _Allocator& __alloc)
noexcept(noexcept(__relocate_a_1(std::__niter_base(__first),
std::__niter_base(__last),
std::__niter_base(__result), __alloc)))
{
return std::__relocate_a_1(std::__niter_base(__first),
std::__niter_base(__last),
std::__niter_base(__result), __alloc);
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif