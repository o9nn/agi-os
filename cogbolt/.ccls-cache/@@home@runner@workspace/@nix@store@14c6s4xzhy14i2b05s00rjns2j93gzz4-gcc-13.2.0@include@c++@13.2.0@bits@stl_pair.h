#ifndef _STL_PAIR_H
#define _STL_PAIR_H 1
#if __cplusplus >= 201103L
# include <type_traits>
# include <bits/move.h>
# include <bits/utility.h>
#endif
#if __cplusplus >= 202002L
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
#if ! __cpp_lib_concepts
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
#endif
template<typename _U1, typename _U2> class __pair_base
{
#if __cplusplus >= 201103L && ! __cpp_lib_concepts
template<typename _T1, typename _T2> friend struct pair;
__pair_base() = default;
~__pair_base() = default;
__pair_base(const __pair_base&) = default;
__pair_base& operator=(const __pair_base&) = delete;
#endif
};
template<typename _T1, typename _T2>
struct pair
: public __pair_base<_T1, _T2>
{
typedef _T1 first_type;
typedef _T2 second_type;
_T1 first;
_T2 second;
#if __cplusplus >= 201103L
constexpr pair(const pair&) = default;
constexpr pair(pair&&) = default;
template<typename... _Args1, typename... _Args2>
_GLIBCXX20_CONSTEXPR
pair(piecewise_construct_t, tuple<_Args1...>, tuple<_Args2...>);
_GLIBCXX20_CONSTEXPR void
swap(pair& __p)
noexcept(__and_<__is_nothrow_swappable<_T1>,
__is_nothrow_swappable<_T2>>::value)
{
using std::swap;
swap(first, __p.first);
swap(second, __p.second);
}
#if __cplusplus > 202002L
constexpr void
swap(const pair& __p) const
noexcept(__and_v<__is_nothrow_swappable<const _T1>,
__is_nothrow_swappable<const _T2>>)
requires is_swappable_v<const _T1> && is_swappable_v<const _T2>
{
using std::swap;
swap(first, __p.first);
swap(second, __p.second);
}
#endif
private:
template<typename... _Args1, size_t... _Indexes1,
typename... _Args2, size_t... _Indexes2>
_GLIBCXX20_CONSTEXPR
pair(tuple<_Args1...>&, tuple<_Args2...>&,
_Index_tuple<_Indexes1...>, _Index_tuple<_Indexes2...>);
public:
#if __cpp_lib_concepts
constexpr
explicit(__not_<__and_<__is_implicitly_default_constructible<_T1>,
__is_implicitly_default_constructible<_T2>>>())
pair()
requires is_default_constructible_v<_T1>
&& is_default_constructible_v<_T2>
: first(), second()
{ }
private:
template<typename _U1, typename _U2>
static constexpr bool
_S_constructible()
{
if constexpr (is_constructible_v<_T1, _U1>)
return is_constructible_v<_T2, _U2>;
return false;
}
template<typename _U1, typename _U2>
static constexpr bool
_S_nothrow_constructible()
{
if constexpr (is_nothrow_constructible_v<_T1, _U1>)
return is_nothrow_constructible_v<_T2, _U2>;
return false;
}
template<typename _U1, typename _U2>
static constexpr bool
_S_convertible()
{
if constexpr (is_convertible_v<_U1, _T1>)
return is_convertible_v<_U2, _T2>;
return false;
}
template<typename _U1, typename _U2>
static constexpr bool
_S_dangles()
{
#if __has_builtin(__reference_constructs_from_temporary)
if constexpr (__reference_constructs_from_temporary(_T1, _U1&&))
return true;
else
return __reference_constructs_from_temporary(_T2, _U2&&);
#else
return false;
#endif
}
public:
constexpr explicit(!_S_convertible<const _T1&, const _T2&>())
pair(const _T1& __x, const _T2& __y)
noexcept(_S_nothrow_constructible<const _T1&, const _T2&>())
requires (_S_constructible<const _T1&, const _T2&>())
: first(__x), second(__y)
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<_U1, _U2>()) && (!_S_dangles<_U1, _U2>())
constexpr explicit(!_S_convertible<_U1, _U2>())
pair(_U1&& __x, _U2&& __y)
noexcept(_S_nothrow_constructible<_U1, _U2>())
: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y))
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<_U1, _U2>()) && (_S_dangles<_U1, _U2>())
constexpr explicit(!_S_convertible<_U1, _U2>())
pair(_U1&&, _U2&&) = delete;
template<typename _U1, typename _U2>
requires (_S_constructible<const _U1&, const _U2&>())
&& (!_S_dangles<_U1, _U2>())
constexpr explicit(!_S_convertible<const _U1&, const _U2&>())
pair(const pair<_U1, _U2>& __p)
noexcept(_S_nothrow_constructible<const _U1&, const _U2&>())
: first(__p.first), second(__p.second)
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<const _U1&, const _U2&>())
&& (_S_dangles<const _U1&, const _U2&>())
constexpr explicit(!_S_convertible<const _U1&, const _U2&>())
pair(const pair<_U1, _U2>&) = delete;
template<typename _U1, typename _U2>
requires (_S_constructible<_U1, _U2>()) && (!_S_dangles<_U1, _U2>())
constexpr explicit(!_S_convertible<_U1, _U2>())
pair(pair<_U1, _U2>&& __p)
noexcept(_S_nothrow_constructible<_U1, _U2>())
: first(std::forward<_U1>(__p.first)),
second(std::forward<_U2>(__p.second))
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<_U1, _U2>()) && (_S_dangles<_U1, _U2>())
constexpr explicit(!_S_convertible<_U1, _U2>())
pair(pair<_U1, _U2>&&) = delete;
#if __cplusplus > 202002L
template<typename _U1, typename _U2>
requires (_S_constructible<_U1&, _U2&>()) && (!_S_dangles<_U1&, _U2&>())
constexpr explicit(!_S_convertible<_U1&, _U2&>())
pair(pair<_U1, _U2>& __p)
noexcept(_S_nothrow_constructible<_U1&, _U2&>())
: first(__p.first), second(__p.second)
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<_U1&, _U2&>()) && (_S_dangles<_U1&, _U2&>())
constexpr explicit(!_S_convertible<_U1&, _U2&>())
pair(pair<_U1, _U2>&) = delete;
template<typename _U1, typename _U2>
requires (_S_constructible<const _U1, const _U2>())
&& (!_S_dangles<const _U1, const _U2>())
constexpr explicit(!_S_convertible<const _U1, const _U2>())
pair(const pair<_U1, _U2>&& __p)
noexcept(_S_nothrow_constructible<const _U1, const _U2>())
: first(std::forward<const _U1>(__p.first)),
second(std::forward<const _U2>(__p.second))
{ }
template<typename _U1, typename _U2>
requires (_S_constructible<const _U1, const _U2>())
&& (_S_dangles<const _U1, const _U2>())
constexpr explicit(!_S_convertible<const _U1, const _U2>())
pair(const pair<_U1, _U2>&&) = delete;
#endif
private:
template<typename _U1, typename _U2>
static constexpr bool
_S_assignable()
{
if constexpr (is_assignable_v<_T1&, _U1>)
return is_assignable_v<_T2&, _U2>;
return false;
}
template<typename _U1, typename _U2>
static constexpr bool
_S_nothrow_assignable()
{
if constexpr (is_nothrow_assignable_v<_T1&, _U1>)
return is_nothrow_assignable_v<_T2&, _U2>;
return false;
}
public:
pair& operator=(const pair&) = delete;
constexpr pair&
operator=(const pair& __p)
noexcept(_S_nothrow_assignable<const _T1&, const _T2&>())
requires (_S_assignable<const _T1&, const _T2&>())
{
first = __p.first;
second = __p.second;
return *this;
}
constexpr pair&
operator=(pair&& __p)
noexcept(_S_nothrow_assignable<_T1, _T2>())
requires (_S_assignable<_T1, _T2>())
{
first = std::forward<first_type>(__p.first);
second = std::forward<second_type>(__p.second);
return *this;
}
template<typename _U1, typename _U2>
constexpr pair&
operator=(const pair<_U1, _U2>& __p)
noexcept(_S_nothrow_assignable<const _U1&, const _U2&>())
requires (_S_assignable<const _U1&, const _U2&>())
{
first = __p.first;
second = __p.second;
return *this;
}
template<typename _U1, typename _U2>
constexpr pair&
operator=(pair<_U1, _U2>&& __p)
noexcept(_S_nothrow_assignable<_U1, _U2>())
requires (_S_assignable<_U1, _U2>())
{
first = std::forward<_U1>(__p.first);
second = std::forward<_U2>(__p.second);
return *this;
}
#if __cplusplus > 202002L
constexpr const pair&
operator=(const pair& __p) const
requires is_copy_assignable_v<const first_type>
&& is_copy_assignable_v<const second_type>
{
first = __p.first;
second = __p.second;
return *this;
}
constexpr const pair&
operator=(pair&& __p) const
requires is_assignable_v<const first_type&, first_type>
&& is_assignable_v<const second_type&, second_type>
{
first = std::forward<first_type>(__p.first);
second = std::forward<second_type>(__p.second);
return *this;
}
template<typename _U1, typename _U2>
constexpr const pair&
operator=(const pair<_U1, _U2>& __p) const
requires is_assignable_v<const first_type&, const _U1&>
&& is_assignable_v<const second_type&, const _U2&>
{
first = __p.first;
second = __p.second;
return *this;
}
template<typename _U1, typename _U2>
constexpr const pair&
operator=(pair<_U1, _U2>&& __p) const
requires is_assignable_v<const first_type&, _U1>
&& is_assignable_v<const second_type&, _U2>
{
first = std::forward<_U1>(__p.first);
second = std::forward<_U2>(__p.second);
return *this;
}
#endif
#else
#if __has_builtin(__reference_constructs_from_temporary) \
&& defined _GLIBCXX_DEBUG
# define __glibcxx_no_dangling_refs(_U1, _U2) \
static_assert(!__reference_constructs_from_temporary(_T1, _U1) \
&& !__reference_constructs_from_temporary(_T2, _U2), \
"std::pair constructor creates a dangling reference")
#else
# define __glibcxx_no_dangling_refs(_U1, _U2)
#endif
template <typename _U1 = _T1,
typename _U2 = _T2,
typename enable_if<__and_<
__is_implicitly_default_constructible<_U1>,
__is_implicitly_default_constructible<_U2>>
::value, bool>::type = true>
constexpr pair()
: first(), second() { }
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
: first(__p.first), second(__p.second)
{ __glibcxx_no_dangling_refs(const _U1&, const _U2&); }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_ConstructiblePair<_U1, _U2>()
&& !_PCCFP<_U1, _U2>::template
_ImplicitlyConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(const pair<_U1, _U2>& __p)
: first(__p.first), second(__p.second)
{ __glibcxx_no_dangling_refs(const _U1&, const _U2&); }
#if _GLIBCXX_USE_DEPRECATED
#if defined(__DEPRECATED)
# define _GLIBCXX_DEPRECATED_PAIR_CTOR \
__attribute__ ((__deprecated__ ("use 'nullptr' instead of '0' to " \
"initialize std::pair of move-only " \
"type and pointer")))
#else
# define _GLIBCXX_DEPRECATED_PAIR_CTOR
#endif
private:
struct __zero_as_null_pointer_constant
{
__zero_as_null_pointer_constant(int __zero_as_null_pointer_constant::*)
{ }
template<typename _Tp,
typename = __enable_if_t<is_null_pointer<_Tp>::value>>
__zero_as_null_pointer_constant(_Tp) = delete;
};
public:
template<typename _U1,
__enable_if_t<__and_<__not_<is_reference<_U1>>,
is_pointer<_T2>,
is_constructible<_T1, _U1>,
__not_<is_constructible<_T1, const _U1&>>,
is_convertible<_U1, _T1>>::value,
bool> = true>
_GLIBCXX_DEPRECATED_PAIR_CTOR
constexpr
pair(_U1&& __x, __zero_as_null_pointer_constant, ...)
: first(std::forward<_U1>(__x)), second(nullptr)
{ __glibcxx_no_dangling_refs(_U1&&, std::nullptr_t); }
template<typename _U1,
__enable_if_t<__and_<__not_<is_reference<_U1>>,
is_pointer<_T2>,
is_constructible<_T1, _U1>,
__not_<is_constructible<_T1, const _U1&>>,
__not_<is_convertible<_U1, _T1>>>::value,
bool> = false>
_GLIBCXX_DEPRECATED_PAIR_CTOR
explicit constexpr
pair(_U1&& __x, __zero_as_null_pointer_constant, ...)
: first(std::forward<_U1>(__x)), second(nullptr)
{ __glibcxx_no_dangling_refs(_U1&&, std::nullptr_t); }
template<typename _U2,
__enable_if_t<__and_<is_pointer<_T1>,
__not_<is_reference<_U2>>,
is_constructible<_T2, _U2>,
__not_<is_constructible<_T2, const _U2&>>,
is_convertible<_U2, _T2>>::value,
bool> = true>
_GLIBCXX_DEPRECATED_PAIR_CTOR
constexpr
pair(__zero_as_null_pointer_constant, _U2&& __y, ...)
: first(nullptr), second(std::forward<_U2>(__y))
{ __glibcxx_no_dangling_refs(std::nullptr_t, _U2&&); }
template<typename _U2,
__enable_if_t<__and_<is_pointer<_T1>,
__not_<is_reference<_U2>>,
is_constructible<_T2, _U2>,
__not_<is_constructible<_T2, const _U2&>>,
__not_<is_convertible<_U2, _T2>>>::value,
bool> = false>
_GLIBCXX_DEPRECATED_PAIR_CTOR
explicit constexpr
pair(__zero_as_null_pointer_constant, _U2&& __y, ...)
: first(nullptr), second(std::forward<_U2>(__y))
{ __glibcxx_no_dangling_refs(std::nullptr_t, _U2&&); }
#undef _GLIBCXX_DEPRECATED_PAIR_CTOR
#endif
template<typename _U1, typename _U2, typename
enable_if<_PCCP::template
_MoveConstructiblePair<_U1, _U2>()
&& _PCCP::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(_U1&& __x, _U2&& __y)
: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y))
{ __glibcxx_no_dangling_refs(_U1&&, _U2&&); }
template<typename _U1, typename _U2, typename
enable_if<_PCCP::template
_MoveConstructiblePair<_U1, _U2>()
&& !_PCCP::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(_U1&& __x, _U2&& __y)
: first(std::forward<_U1>(__x)), second(std::forward<_U2>(__y))
{ __glibcxx_no_dangling_refs(_U1&&, _U2&&); }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_MoveConstructiblePair<_U1, _U2>()
&& _PCCFP<_U1, _U2>::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=true>
constexpr pair(pair<_U1, _U2>&& __p)
: first(std::forward<_U1>(__p.first)),
second(std::forward<_U2>(__p.second))
{ __glibcxx_no_dangling_refs(_U1&&, _U2&&); }
template<typename _U1, typename _U2, typename
enable_if<_PCCFP<_U1, _U2>::template
_MoveConstructiblePair<_U1, _U2>()
&& !_PCCFP<_U1, _U2>::template
_ImplicitlyMoveConvertiblePair<_U1, _U2>(),
bool>::type=false>
explicit constexpr pair(pair<_U1, _U2>&& __p)
: first(std::forward<_U1>(__p.first)),
second(std::forward<_U2>(__p.second))
{ __glibcxx_no_dangling_refs(_U1&&, _U2&&); }
#undef __glibcxx_no_dangling_refs
pair&
operator=(__conditional_t<__and_<is_copy_assignable<_T1>,
is_copy_assignable<_T2>>::value,
const pair&, const __nonesuch&> __p)
{
first = __p.first;
second = __p.second;
return *this;
}
pair&
operator=(__conditional_t<__and_<is_move_assignable<_T1>,
is_move_assignable<_T2>>::value,
pair&&, __nonesuch&&> __p)
noexcept(__and_<is_nothrow_move_assignable<_T1>,
is_nothrow_move_assignable<_T2>>::value)
{
first = std::forward<first_type>(__p.first);
second = std::forward<second_type>(__p.second);
return *this;
}
template<typename _U1, typename _U2>
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
typename enable_if<__and_<is_assignable<_T1&, _U1&&>,
is_assignable<_T2&, _U2&&>>::value,
pair&>::type
operator=(pair<_U1, _U2>&& __p)
{
first = std::forward<_U1>(__p.first);
second = std::forward<_U2>(__p.second);
return *this;
}
#endif
#else
pair() : first(), second() { }
pair(const _T1& __a, const _T2& __b)
: first(__a), second(__b) { }
template<typename _U1, typename _U2>
pair(const pair<_U1, _U2>& __p)
: first(__p.first), second(__p.second)
{
#if __has_builtin(__reference_constructs_from_temporary)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-local-typedefs"
typedef int _DanglingCheck1[
__reference_constructs_from_temporary(_T1, const _U1&) ? -1 : 1
];
typedef int _DanglingCheck2[
__reference_constructs_from_temporary(_T2, const _U2&) ? -1 : 1
];
#pragma GCC diagnostic pop
#endif
}
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
#if __cplusplus > 202002L
template<typename _T1, typename _T2>
requires is_swappable_v<const _T1> && is_swappable_v<const _T2>
constexpr void
swap(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
noexcept(noexcept(__x.swap(__y)))
{ __x.swap(__y); }
#endif
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
#if __cplusplus >= 201103L
template<typename _T1, typename _T2>
struct __is_tuple_like_impl<pair<_T1, _T2>> : true_type
{ };
template<class _Tp1, class _Tp2>
struct tuple_size<pair<_Tp1, _Tp2>>
: public integral_constant<size_t, 2> { };
template<class _Tp1, class _Tp2>
struct tuple_element<0, pair<_Tp1, _Tp2>>
{ typedef _Tp1 type; };
template<class _Tp1, class _Tp2>
struct tuple_element<1, pair<_Tp1, _Tp2>>
{ typedef _Tp2 type; };
#if __cplusplus >= 201703L
template<typename _Tp1, typename _Tp2>
inline constexpr size_t tuple_size_v<pair<_Tp1, _Tp2>> = 2;
template<typename _Tp1, typename _Tp2>
inline constexpr size_t tuple_size_v<const pair<_Tp1, _Tp2>> = 2;
template<typename _Tp>
inline constexpr bool __is_pair = false;
template<typename _Tp, typename _Up>
inline constexpr bool __is_pair<pair<_Tp, _Up>> = true;
#endif
template<size_t _Int>
struct __pair_get;
template<>
struct __pair_get<0>
{
template<typename _Tp1, typename _Tp2>
static constexpr _Tp1&
__get(pair<_Tp1, _Tp2>& __pair) noexcept
{ return __pair.first; }
template<typename _Tp1, typename _Tp2>
static constexpr _Tp1&&
__move_get(pair<_Tp1, _Tp2>&& __pair) noexcept
{ return std::forward<_Tp1>(__pair.first); }
template<typename _Tp1, typename _Tp2>
static constexpr const _Tp1&
__const_get(const pair<_Tp1, _Tp2>& __pair) noexcept
{ return __pair.first; }
template<typename _Tp1, typename _Tp2>
static constexpr const _Tp1&&
__const_move_get(const pair<_Tp1, _Tp2>&& __pair) noexcept
{ return std::forward<const _Tp1>(__pair.first); }
};
template<>
struct __pair_get<1>
{
template<typename _Tp1, typename _Tp2>
static constexpr _Tp2&
__get(pair<_Tp1, _Tp2>& __pair) noexcept
{ return __pair.second; }
template<typename _Tp1, typename _Tp2>
static constexpr _Tp2&&
__move_get(pair<_Tp1, _Tp2>&& __pair) noexcept
{ return std::forward<_Tp2>(__pair.second); }
template<typename _Tp1, typename _Tp2>
static constexpr const _Tp2&
__const_get(const pair<_Tp1, _Tp2>& __pair) noexcept
{ return __pair.second; }
template<typename _Tp1, typename _Tp2>
static constexpr const _Tp2&&
__const_move_get(const pair<_Tp1, _Tp2>&& __pair) noexcept
{ return std::forward<const _Tp2>(__pair.second); }
};
template<size_t _Int, class _Tp1, class _Tp2>
constexpr typename tuple_element<_Int, pair<_Tp1, _Tp2>>::type&
get(pair<_Tp1, _Tp2>& __in) noexcept
{ return __pair_get<_Int>::__get(__in); }
template<size_t _Int, class _Tp1, class _Tp2>
constexpr typename tuple_element<_Int, pair<_Tp1, _Tp2>>::type&&
get(pair<_Tp1, _Tp2>&& __in) noexcept
{ return __pair_get<_Int>::__move_get(std::move(__in)); }
template<size_t _Int, class _Tp1, class _Tp2>
constexpr const typename tuple_element<_Int, pair<_Tp1, _Tp2>>::type&
get(const pair<_Tp1, _Tp2>& __in) noexcept
{ return __pair_get<_Int>::__const_get(__in); }
template<size_t _Int, class _Tp1, class _Tp2>
constexpr const typename tuple_element<_Int, pair<_Tp1, _Tp2>>::type&&
get(const pair<_Tp1, _Tp2>&& __in) noexcept
{ return __pair_get<_Int>::__const_move_get(std::move(__in)); }
#if __cplusplus >= 201402L
#define __cpp_lib_tuples_by_type 201304L
template <typename _Tp, typename _Up>
constexpr _Tp&
get(pair<_Tp, _Up>& __p) noexcept
{ return __p.first; }
template <typename _Tp, typename _Up>
constexpr const _Tp&
get(const pair<_Tp, _Up>& __p) noexcept
{ return __p.first; }
template <typename _Tp, typename _Up>
constexpr _Tp&&
get(pair<_Tp, _Up>&& __p) noexcept
{ return std::move(__p.first); }
template <typename _Tp, typename _Up>
constexpr const _Tp&&
get(const pair<_Tp, _Up>&& __p) noexcept
{ return std::move(__p.first); }
template <typename _Tp, typename _Up>
constexpr _Tp&
get(pair<_Up, _Tp>& __p) noexcept
{ return __p.second; }
template <typename _Tp, typename _Up>
constexpr const _Tp&
get(const pair<_Up, _Tp>& __p) noexcept
{ return __p.second; }
template <typename _Tp, typename _Up>
constexpr _Tp&&
get(pair<_Up, _Tp>&& __p) noexcept
{ return std::move(__p.second); }
template <typename _Tp, typename _Up>
constexpr const _Tp&&
get(const pair<_Up, _Tp>&& __p) noexcept
{ return std::move(__p.second); }
#if __cplusplus > 202002L
template<typename _T1, typename _T2, typename _U1, typename _U2,
template<typename> class _TQual, template<typename> class _UQual>
requires requires { typename pair<common_reference_t<_TQual<_T1>, _UQual<_U1>>,
common_reference_t<_TQual<_T2>, _UQual<_U2>>>; }
struct basic_common_reference<pair<_T1, _T2>, pair<_U1, _U2>, _TQual, _UQual>
{
using type = pair<common_reference_t<_TQual<_T1>, _UQual<_U1>>,
common_reference_t<_TQual<_T2>, _UQual<_U2>>>;
};
template<typename _T1, typename _T2, typename _U1, typename _U2>
requires requires { typename pair<common_type_t<_T1, _U1>, common_type_t<_T2, _U2>>; }
struct common_type<pair<_T1, _T2>, pair<_U1, _U2>>
{ using type = pair<common_type_t<_T1, _U1>, common_type_t<_T2, _U2>>; };
#endif
#endif
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif