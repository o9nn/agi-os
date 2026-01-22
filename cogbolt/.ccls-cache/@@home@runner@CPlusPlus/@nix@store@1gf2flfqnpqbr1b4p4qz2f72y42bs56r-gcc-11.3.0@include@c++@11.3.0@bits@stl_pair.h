#ifndef _STL_PAIR_H
#define _STL_PAIR_H 1
#include <bits/move.h>
#if __cplusplus >= 201103L
# include <type_traits>
#endif
#if __cplusplus > 201703L
# include <compare>
# define __cpp_lib_constexpr_utility 201811L
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cplusplus >= 201103L
struct piecewise_construct_t { explicit piecewise_construct_t() = default; };
_GLIBCXX17_INLINE constexpr piecewise_construct_t piecewise_construct =
piecewise_construct_t();
template<typename...>
class tuple;
template<size_t...>
struct _Index_tuple;
template <bool, typename _T1, typename _T2>
struct _PCC
{
template <typename _U1, typename _U2>
static constexpr bool _ConstructiblePair()
{
return __and_<is_constructible<_T1, const _U1&>,
is_constructible<_T2, const _U2&>>::value;
}
template <typename _U1, typename _U2>
static constexpr bool _ImplicitlyConvertiblePair()
{
return __and_<is_convertible<const _U1&, _T1>,
is_convertible<const _U2&, _T2>>::value;
}
template <typename _U1, typename _U2>
static constexpr bool _MoveConstructiblePair()
{
return __and_<is_constructible<_T1, _U1&&>,
is_constructible<_T2, _U2&&>>::value;
}
template <typename _U1, typename _U2>
static constexpr bool _ImplicitlyMoveConvertiblePair()
{
return __and_<is_convertible<_U1&&, _T1>,
is_convertible<_U2&&, _T2>>::value;
}
template <bool __implicit, typename _U1, typename _U2>
static constexpr bool _CopyMovePair()
{
using __do_converts = __and_<is_convertible<const _U1&, _T1>,
is_convertible<_U2&&, _T2>>;
using __converts = typename conditional<__implicit,
__do_converts,
__not_<__do_converts>>::type;
return __and_<is_constructible<_T1, const _U1&>,
is_constructible<_T2, _U2&&>,
__converts
>::value;
}
template <bool __implicit, typename _U1, typename _U2>
static constexpr bool _MoveCopyPair()
{
using __do_converts = __and_<is_convertible<_U1&&, _T1>,
is_convertible<const _U2&, _T2>>;
using __converts = typename conditional<__implicit,
__do_converts,
__not_<__do_converts>>::type;
return __and_<is_constructible<_T1, _U1&&>,
is_constructible<_T2, const _U2&&>,
__converts
>::value;
}
};
template <typename _T1, typename _T2>
struct _PCC<false, _T1, _T2>
{
template <typename _U1, typename _U2>
static constexpr bool _ConstructiblePair()
{
return false;
}
template <typename _U1, typename _U2>
static constexpr bool _ImplicitlyConvertiblePair()
{
return false;
}
template <typename _U1, typename _U2>
static constexpr bool _MoveConstructiblePair()
{
return false;
}
template <typename _U1, typename _U2>
static constexpr bool _ImplicitlyMoveConvertiblePair()
{
return false;
}
};
#endif
template<typename _U1, typename _U2> class __pair_base
{
#if __cplusplus >= 201103L
template<typename _T1, typename _T2> friend struct pair;
__pair_base() = default;
~__pair_base() = default;
__pair_base(const __pair_base&) = default;
__pair_base& operator=(const __pair_base&) = delete;
#endif
};
template<typename _T1, typename _T2>
struct pair
: private __pair_base<_T1, _T2>
{
typedef _T1 first_type;
typedef _T2 second_type;
_T1 first;
_T2 second;
#if __cplusplus >= 201103L
template <typename _U1 = _T1,
typename _U2 = _T2,
typename enable_if<__and_<
__is_implicitly_default_constructible<_U1>,
__is_implicitly_default_constructible<_U2>>
::value, bool>::type = true>
#endif
_GLIBCXX_CONSTEXPR pair()
: first(), second() { }
#if __cplusplus >= 201103L
template <typename _U1 = _T1,
typename _U2 = _T2,
typename enable_if<__and_<
is_default_constructible<_U1>,
is_default_constructible<_U2>,
__not_<
__and_<__is_implicitly_default_constructible<_U1>,
__is_implicitly_default_constructible<_U2>>>>
::value, bool>::type = false>
explicit constexpr pair()
: first(), second() { }
#endif
#if __cplusplus < 201103L
pair(const _T1& __a, const _T2& __b)
: first(__a), second(__b) { }
#else
using _PCCP = _PCC<true, _T1, _T2>;
template<typename _U1 = _T1, typename _U2=_T2, typename
enable_if<_PCCP::template
_ConstructiblePair<_U1, _U2>()
&& _PCCP::template
_ImplicitlyConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(const _T1& __a, const _T2& __b)
: first(__a), second(__b) { }
template<typename _U1 = _T1, typename _U2=_T2, typename
enable_if<_PCCP::template
_ConstructiblePair<_U1, _U2>()
&& !_PCCP::template
_ImplicitlyConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(const _T1& __a, const _T2& __b)
: first(__a), second(__b) { }
#endif
#if __cplusplus < 201103L
template<typename _U1, typename _U2>
pair(const pair<_U1, _U2>& __p)
: first(__p.first), second(__p.second) { }
#else
template <typename _U1, typename _U2>
using _PCCFP = _PCC<!is_same<_T1, _U1>::value
|| !is_same<_T2, _U2>::value,
_T1, _T2>;
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_ConstructiblePair<_U1, _U2>()
&& _PCCFP<_U1, _U2>::template
_ImplicitlyConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(const pair<_U1, _U2>& __p)
: first(__p.first), second(__p.second) { }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_ConstructiblePair<_U1, _U2>()
&& !_PCCFP<_U1, _U2>::template
_ImplicitlyConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(const pair<_U1, _U2>& __p)
: first(__p.first), second(__p.second) { }
#endif
#if __cplusplus >= 201103L
constexpr pair(const pair&) = default;
constexpr pair(pair&&) = default;
template<typename _U1, typename
enable_if<_PCCP::template
_MoveCopyPair<true, _U1, _T2>(),
bool>::type=true>
constexpr pair(_U1&& __x, const _T2& __y)
: first(std::forward<_U1>(__x)), second(__y) { }
template<typename _U1, typename
enable_if<_PCCP::template
_MoveCopyPair<false, _U1, _T2>(),
bool>::type=false>
explicit constexpr pair(_U1&& __x, const _T2& __y)
: first(std::forward<_U1>(__x)), second(__y) { }
template<typename _U2, typename
enable_if<_PCCP::template
_CopyMovePair<true, _T1, _U2>(),
bool>::type=true>
constexpr pair(const _T1& __x, _U2&& __y)
: first(__x), second(std::forward<_U2>(__y)) { }
template<typename _U2, typename
enable_if<_PCCP::template
_CopyMovePair<false, _T1, _U2>(),
bool>::type=false>
explicit pair(const _T1& __x, _U2&& __y)
: first(__x), second(std::forward<_U2>(__y)) { }
template<typename _U1, typename _U2, typename
enable_if<_PCCP::template
_MoveConstructiblePair<_U1, _U2>()
&& _PCCP::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(_U1&& __x, _U2&& __y)
: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }
template<typename _U1, typename _U2, typename
enable_if<_PCCP::template
_MoveConstructiblePair<_U1, _U2>()
&& !_PCCP::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(_U1&& __x, _U2&& __y)
: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y)) { }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_MoveConstructiblePair<_U1, _U2>()
&& _PCCFP<_U1, _U2>::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(pair<_U1, _U2>&& __p)
: first(std::forward<_U1>(__p.first)),
second(std::forward<_U2>(__p.second)) { }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_MoveConstructiblePair<_U1, _U2>()
&& !_PCCFP<_U1, _U2>::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(pair<_U1, _U2>&& __p)
: first(std::forward<_U1>(__p.first)),
second(std::forward<_U2>(__p.second)) { }
template<typename... _Args1, typename... _Args2>
_GLIBCXX20_CONSTEXPR
pair(piecewise_construct_t, tuple<_Args1...>, tuple<_Args2...>);
_GLIBCXX20_CONSTEXPR pair&
operator=(typename conditional<
__and_<is_copy_assignable<_T1>,
is_copy_assignable<_T2>>::value,
const pair&, const __nonesuch&>::type __p)
{
first = __p.first;
second = __p.second;
return *this;
}
_GLIBCXX20_CONSTEXPR pair&
operator=(typename conditional<
__and_<is_move_assignable<_T1>,
is_move_assignable<_T2>>::value,
pair&&, __nonesuch&&>::type __p)
noexcept(__and_<is_nothrow_move_assignable<_T1>,
is_nothrow_move_assignable<_T2>>::value)
{
first = std::forward<first_type>(__p.first);
second = std::forward<second_type>(__p.second);
return *this;
}
template<typename _U1, typename _U2>
_GLIBCXX20_CONSTEXPR
typename enable_if<__and_<is_assignable<_T1&, const _U1&>,
is_assignable<_T2&, const _U2&>>::value,
pair&>::type
operator=(const pair<_U1, _U2>& __p)
{
first = __p.first;
second = __p.second;
return *this;
}
template<typename _U1, typename _U2>
_GLIBCXX20_CONSTEXPR
typename enable_if<__and_<is_assignable<_T1&, _U1&&>,
is_assignable<_T2&, _U2&&>>::value,
pair&>::type
operator=(pair<_U1, _U2>&& __p)
{
first = std::forward<_U1>(__p.first);
second = std::forward<_U2>(__p.second);
return *this;
}
_GLIBCXX20_CONSTEXPR void
swap(pair& __p)
noexcept(__and_<__is_nothrow_swappable<_T1>,
__is_nothrow_swappable<_T2>>::value)
{
using std::swap;
swap(first, __p.first);
swap(second, __p.second);
}
private:
template<typename... _Args1, size_t... _Indexes1,
typename... _Args2, size_t... _Indexes2>
_GLIBCXX20_CONSTEXPR
pair(tuple<_Args1...>&, tuple<_Args2...>&,
_Index_tuple<_Indexes1...>, _Index_tuple<_Indexes2...>);
#endif
};
#if __cpp_deduction_guides >= 201606
template<typename _T1, typename _T2> pair(_T1, _T2) -> pair<_T1, _T2>;
#endif
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator==(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return __x.first == __y.first && __x.second == __y.second; }
#if __cpp_lib_three_way_comparison && __cpp_lib_concepts
template<typename _T1, typename _T2>
constexpr common_comparison_category_t<__detail::__synth3way_t<_T1>,
__detail::__synth3way_t<_T2>>
operator<=>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{
if (auto __c = __detail::__synth3way(__x.first, __y.first); __c != 0)
return __c;
return __detail::__synth3way(__x.second, __y.second);
}
#else
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator<(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return __x.first < __y.first
|| (!(__y.first < __x.first) && __x.second < __y.second); }
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator!=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return !(__x == __y); }
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return __y < __x; }
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator<=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return !(__y < __x); }
template<typename _T1, typename _T2>
inline _GLIBCXX_CONSTEXPR bool
operator>=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
{ return !(__x < __y); }
#endif
#if __cplusplus >= 201103L
template<typename _T1, typename _T2>
_GLIBCXX20_CONSTEXPR inline
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
typename enable_if<__and_<__is_swappable<_T1>,
__is_swappable<_T2>>::value>::type
#else
void
#endif
swap(pair<_T1, _T2>& __x, pair<_T1, _T2>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
#if __cplusplus > 201402L || !defined(__STRICT_ANSI__)
template<typename _T1, typename _T2>
typename enable_if<!__and_<__is_swappable<_T1>,
__is_swappable<_T2>>::value>::type
swap(pair<_T1, _T2>&, pair<_T1, _T2>&) = delete;
#endif
#endif
#if __cplusplus >= 201103L
template<typename _T1, typename _T2>
constexpr pair<typename __decay_and_strip<_T1>::__type,
typename __decay_and_strip<_T2>::__type>
make_pair(_T1&& __x, _T2&& __y)
{
typedef typename __decay_and_strip<_T1>::__type __ds_type1;
typedef typename __decay_and_strip<_T2>::__type __ds_type2;
typedef pair<__ds_type1, __ds_type2> __pair_type;
return __pair_type(std::forward<_T1>(__x), std::forward<_T2>(__y));
}
#else
template<typename _T1, typename _T2>
inline pair<_T1, _T2>
make_pair(_T1 __x, _T2 __y)
{ return pair<_T1, _T2>(__x, __y); }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif