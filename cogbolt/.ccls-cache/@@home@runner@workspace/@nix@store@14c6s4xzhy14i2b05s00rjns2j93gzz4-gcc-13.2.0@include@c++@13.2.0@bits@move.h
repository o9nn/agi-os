#ifndef _MOVE_H
#define _MOVE_H 1
#include <bits/c++config.h>
#if __cplusplus < 201103L
# include <bits/concept_check.h>
#else
# include <type_traits>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Tp>
inline _GLIBCXX_CONSTEXPR _Tp*
__addressof(_Tp& __r) _GLIBCXX_NOEXCEPT
{ return __builtin_addressof(__r); }
#if __cplusplus >= 201103L
template<typename _Tp>
_GLIBCXX_NODISCARD
constexpr _Tp&&
forward(typename std::remove_reference<_Tp>::type& __t) noexcept
{ return static_cast<_Tp&&>(__t); }
template<typename _Tp>
_GLIBCXX_NODISCARD
constexpr _Tp&&
forward(typename std::remove_reference<_Tp>::type&& __t) noexcept
{
static_assert(!std::is_lvalue_reference<_Tp>::value,
"std::forward must not be used to convert an rvalue to an lvalue");
return static_cast<_Tp&&>(__t);
}
template<typename _Tp>
_GLIBCXX_NODISCARD
constexpr typename std::remove_reference<_Tp>::type&&
move(_Tp&& __t) noexcept
{ return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
template<typename _Tp>
struct __move_if_noexcept_cond
: public __and_<__not_<is_nothrow_move_constructible<_Tp>>,
is_copy_constructible<_Tp>>::type { };
template<typename _Tp>
_GLIBCXX_NODISCARD
constexpr
__conditional_t<__move_if_noexcept_cond<_Tp>::value, const _Tp&, _Tp&&>
move_if_noexcept(_Tp& __x) noexcept
{ return std::move(__x); }
#if __cplusplus > 201402L
# define __cpp_lib_addressof_constexpr 201603L
#endif
template<typename _Tp>
_GLIBCXX_NODISCARD
inline _GLIBCXX17_CONSTEXPR _Tp*
addressof(_Tp& __r) noexcept
{ return std::__addressof(__r); }
template<typename _Tp>
const _Tp* addressof(const _Tp&&) = delete;
template <typename _Tp, typename _Up = _Tp>
_GLIBCXX20_CONSTEXPR
inline _Tp
__exchange(_Tp& __obj, _Up&& __new_val)
{
_Tp __old_val = std::move(__obj);
__obj = std::forward<_Up>(__new_val);
return __old_val;
}
#define _GLIBCXX_FWDREF(_Tp) _Tp&&
#define _GLIBCXX_MOVE(__val) std::move(__val)
#define _GLIBCXX_FORWARD(_Tp, __val) std::forward<_Tp>(__val)
#else
#define _GLIBCXX_FWDREF(_Tp) const _Tp&
#define _GLIBCXX_MOVE(__val) (__val)
#define _GLIBCXX_FORWARD(_Tp, __val) (__val)
#endif
template<typename _Tp>
_GLIBCXX20_CONSTEXPR
inline
#if __cplusplus >= 201103L
typename enable_if<__and_<__not_<__is_tuple_like<_Tp>>,
is_move_constructible<_Tp>,
is_move_assignable<_Tp>>::value>::type
#else
void
#endif
swap(_Tp& __a, _Tp& __b)
_GLIBCXX_NOEXCEPT_IF(__and_<is_nothrow_move_constructible<_Tp>,
is_nothrow_move_assignable<_Tp>>::value)
{
#if __cplusplus < 201103L
__glibcxx_function_requires(_SGIAssignableConcept<_Tp>)
#endif
_Tp __tmp = _GLIBCXX_MOVE(__a);
__a = _GLIBCXX_MOVE(__b);
__b = _GLIBCXX_MOVE(__tmp);
}
template<typename _Tp, size_t _Nm>
_GLIBCXX20_CONSTEXPR
inline
#if __cplusplus >= 201103L
typename enable_if<__is_swappable<_Tp>::value>::type
#else
void
#endif
swap(_Tp (&__a)[_Nm], _Tp (&__b)[_Nm])
_GLIBCXX_NOEXCEPT_IF(__is_nothrow_swappable<_Tp>::value)
{
for (size_t __n = 0; __n < _Nm; ++__n)
swap(__a[__n], __b[__n]);
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif