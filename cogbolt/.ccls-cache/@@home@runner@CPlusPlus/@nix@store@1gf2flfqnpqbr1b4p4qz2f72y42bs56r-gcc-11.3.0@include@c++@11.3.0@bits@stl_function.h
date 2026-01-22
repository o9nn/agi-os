#ifndef _STL_FUNCTION_H
#define _STL_FUNCTION_H 1
#if __cplusplus > 201103L
#include <bits/move.h>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Arg, typename _Result>
struct unary_function
{
typedef _Arg 	argument_type;
typedef _Result 	result_type;
};
template<typename _Arg1, typename _Arg2, typename _Result>
struct binary_function
{
typedef _Arg1 	first_argument_type;
typedef _Arg2 	second_argument_type;
typedef _Result 	result_type;
};
#if __cplusplus > 201103L
struct __is_transparent;
template<typename _Tp = void>
struct plus;
template<typename _Tp = void>
struct minus;
template<typename _Tp = void>
struct multiplies;
template<typename _Tp = void>
struct divides;
template<typename _Tp = void>
struct modulus;
template<typename _Tp = void>
struct negate;
#endif
template<typename _Tp>
struct plus : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x + __y; }
};
template<typename _Tp>
struct minus : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x - __y; }
};
template<typename _Tp>
struct multiplies : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x * __y; }
};
template<typename _Tp>
struct divides : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x / __y; }
};
template<typename _Tp>
struct modulus : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x % __y; }
};
template<typename _Tp>
struct negate : public unary_function<_Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x) const
{ return -__x; }
};
#if __cplusplus > 201103L
#define __cpp_lib_transparent_operators 201510
template<>
struct plus<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) + std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) + std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) + std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct minus<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) - std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) - std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) - std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct multiplies<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) * std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) * std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) * std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct divides<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) / std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) / std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) / std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct modulus<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) % std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) % std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) % std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct negate<void>
{
template <typename _Tp>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t) const
noexcept(noexcept(-std::forward<_Tp>(__t)))
-> decltype(-std::forward<_Tp>(__t))
{ return -std::forward<_Tp>(__t); }
typedef __is_transparent is_transparent;
};
#endif
#if __cplusplus > 201103L
template<typename _Tp = void>
struct equal_to;
template<typename _Tp = void>
struct not_equal_to;
template<typename _Tp = void>
struct greater;
template<typename _Tp = void>
struct less;
template<typename _Tp = void>
struct greater_equal;
template<typename _Tp = void>
struct less_equal;
#endif
template<typename _Tp>
struct equal_to : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x == __y; }
};
template<typename _Tp>
struct not_equal_to : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x != __y; }
};
template<typename _Tp>
struct greater : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x > __y; }
};
template<typename _Tp>
struct less : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x < __y; }
};
template<typename _Tp>
struct greater_equal : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x >= __y; }
};
template<typename _Tp>
struct less_equal : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x <= __y; }
};
template<typename _Tp>
struct greater<_Tp*> : public binary_function<_Tp*, _Tp*, bool>
{
_GLIBCXX14_CONSTEXPR bool
operator()(_Tp* __x, _Tp* __y) const _GLIBCXX_NOTHROW
{
#if __cplusplus >= 201402L
#ifdef _GLIBCXX_HAVE_BUILTIN_IS_CONSTANT_EVALUATED
if (__builtin_is_constant_evaluated())
#else
if (__builtin_constant_p(__x > __y))
#endif
return __x > __y;
#endif
return (__UINTPTR_TYPE__)__x > (__UINTPTR_TYPE__)__y;
}
};
template<typename _Tp>
struct less<_Tp*> : public binary_function<_Tp*, _Tp*, bool>
{
_GLIBCXX14_CONSTEXPR bool
operator()(_Tp* __x, _Tp* __y) const _GLIBCXX_NOTHROW
{
#if __cplusplus >= 201402L
#ifdef _GLIBCXX_HAVE_BUILTIN_IS_CONSTANT_EVALUATED
if (__builtin_is_constant_evaluated())
#else
if (__builtin_constant_p(__x < __y))
#endif
return __x < __y;
#endif
return (__UINTPTR_TYPE__)__x < (__UINTPTR_TYPE__)__y;
}
};
template<typename _Tp>
struct greater_equal<_Tp*> : public binary_function<_Tp*, _Tp*, bool>
{
_GLIBCXX14_CONSTEXPR bool
operator()(_Tp* __x, _Tp* __y) const _GLIBCXX_NOTHROW
{
#if __cplusplus >= 201402L
#ifdef _GLIBCXX_HAVE_BUILTIN_IS_CONSTANT_EVALUATED
if (__builtin_is_constant_evaluated())
#else
if (__builtin_constant_p(__x >= __y))
#endif
return __x >= __y;
#endif
return (__UINTPTR_TYPE__)__x >= (__UINTPTR_TYPE__)__y;
}
};
template<typename _Tp>
struct less_equal<_Tp*> : public binary_function<_Tp*, _Tp*, bool>
{
_GLIBCXX14_CONSTEXPR bool
operator()(_Tp* __x, _Tp* __y) const _GLIBCXX_NOTHROW
{
#if __cplusplus >= 201402L
#ifdef _GLIBCXX_HAVE_BUILTIN_IS_CONSTANT_EVALUATED
if (__builtin_is_constant_evaluated())
#else
if (__builtin_constant_p(__x <= __y))
#endif
return __x <= __y;
#endif
return (__UINTPTR_TYPE__)__x <= (__UINTPTR_TYPE__)__y;
}
};
#if __cplusplus >= 201402L
template<>
struct equal_to<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) == std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) == std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) == std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct not_equal_to<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) != std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) != std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) != std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct greater<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) > std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) > std::forward<_Up>(__u))
{
return _S_cmp(std::forward<_Tp>(__t), std::forward<_Up>(__u),
__ptr_cmp<_Tp, _Up>{});
}
template<typename _Tp, typename _Up>
constexpr bool
operator()(_Tp* __t, _Up* __u) const noexcept
{ return greater<common_type_t<_Tp*, _Up*>>{}(__t, __u); }
typedef __is_transparent is_transparent;
private:
template <typename _Tp, typename _Up>
static constexpr decltype(auto)
_S_cmp(_Tp&& __t, _Up&& __u, false_type)
{ return std::forward<_Tp>(__t) > std::forward<_Up>(__u); }
template <typename _Tp, typename _Up>
static constexpr bool
_S_cmp(_Tp&& __t, _Up&& __u, true_type) noexcept
{
return greater<const volatile void*>{}(
static_cast<const volatile void*>(std::forward<_Tp>(__t)),
static_cast<const volatile void*>(std::forward<_Up>(__u)));
}
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded2 : true_type { };
template<typename _Tp, typename _Up>
struct __not_overloaded2<_Tp, _Up, __void_t<
decltype(std::declval<_Tp>().operator>(std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded : __not_overloaded2<_Tp, _Up> { };
template<typename _Tp, typename _Up>
struct __not_overloaded<_Tp, _Up, __void_t<
decltype(operator>(std::declval<_Tp>(), std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up>
using __ptr_cmp = __and_<__not_overloaded<_Tp, _Up>,
is_convertible<_Tp, const volatile void*>,
is_convertible<_Up, const volatile void*>>;
};
template<>
struct less<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) < std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) < std::forward<_Up>(__u))
{
return _S_cmp(std::forward<_Tp>(__t), std::forward<_Up>(__u),
__ptr_cmp<_Tp, _Up>{});
}
template<typename _Tp, typename _Up>
constexpr bool
operator()(_Tp* __t, _Up* __u) const noexcept
{ return less<common_type_t<_Tp*, _Up*>>{}(__t, __u); }
typedef __is_transparent is_transparent;
private:
template <typename _Tp, typename _Up>
static constexpr decltype(auto)
_S_cmp(_Tp&& __t, _Up&& __u, false_type)
{ return std::forward<_Tp>(__t) < std::forward<_Up>(__u); }
template <typename _Tp, typename _Up>
static constexpr bool
_S_cmp(_Tp&& __t, _Up&& __u, true_type) noexcept
{
return less<const volatile void*>{}(
static_cast<const volatile void*>(std::forward<_Tp>(__t)),
static_cast<const volatile void*>(std::forward<_Up>(__u)));
}
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded2 : true_type { };
template<typename _Tp, typename _Up>
struct __not_overloaded2<_Tp, _Up, __void_t<
decltype(std::declval<_Tp>().operator<(std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded : __not_overloaded2<_Tp, _Up> { };
template<typename _Tp, typename _Up>
struct __not_overloaded<_Tp, _Up, __void_t<
decltype(operator<(std::declval<_Tp>(), std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up>
using __ptr_cmp = __and_<__not_overloaded<_Tp, _Up>,
is_convertible<_Tp, const volatile void*>,
is_convertible<_Up, const volatile void*>>;
};
template<>
struct greater_equal<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) >= std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) >= std::forward<_Up>(__u))
{
return _S_cmp(std::forward<_Tp>(__t), std::forward<_Up>(__u),
__ptr_cmp<_Tp, _Up>{});
}
template<typename _Tp, typename _Up>
constexpr bool
operator()(_Tp* __t, _Up* __u) const noexcept
{ return greater_equal<common_type_t<_Tp*, _Up*>>{}(__t, __u); }
typedef __is_transparent is_transparent;
private:
template <typename _Tp, typename _Up>
static constexpr decltype(auto)
_S_cmp(_Tp&& __t, _Up&& __u, false_type)
{ return std::forward<_Tp>(__t) >= std::forward<_Up>(__u); }
template <typename _Tp, typename _Up>
static constexpr bool
_S_cmp(_Tp&& __t, _Up&& __u, true_type) noexcept
{
return greater_equal<const volatile void*>{}(
static_cast<const volatile void*>(std::forward<_Tp>(__t)),
static_cast<const volatile void*>(std::forward<_Up>(__u)));
}
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded2 : true_type { };
template<typename _Tp, typename _Up>
struct __not_overloaded2<_Tp, _Up, __void_t<
decltype(std::declval<_Tp>().operator>=(std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded : __not_overloaded2<_Tp, _Up> { };
template<typename _Tp, typename _Up>
struct __not_overloaded<_Tp, _Up, __void_t<
decltype(operator>=(std::declval<_Tp>(), std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up>
using __ptr_cmp = __and_<__not_overloaded<_Tp, _Up>,
is_convertible<_Tp, const volatile void*>,
is_convertible<_Up, const volatile void*>>;
};
template<>
struct less_equal<void>
{
template <typename _Tp, typename _Up>
constexpr auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) <= std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) <= std::forward<_Up>(__u))
{
return _S_cmp(std::forward<_Tp>(__t), std::forward<_Up>(__u),
__ptr_cmp<_Tp, _Up>{});
}
template<typename _Tp, typename _Up>
constexpr bool
operator()(_Tp* __t, _Up* __u) const noexcept
{ return less_equal<common_type_t<_Tp*, _Up*>>{}(__t, __u); }
typedef __is_transparent is_transparent;
private:
template <typename _Tp, typename _Up>
static constexpr decltype(auto)
_S_cmp(_Tp&& __t, _Up&& __u, false_type)
{ return std::forward<_Tp>(__t) <= std::forward<_Up>(__u); }
template <typename _Tp, typename _Up>
static constexpr bool
_S_cmp(_Tp&& __t, _Up&& __u, true_type) noexcept
{
return less_equal<const volatile void*>{}(
static_cast<const volatile void*>(std::forward<_Tp>(__t)),
static_cast<const volatile void*>(std::forward<_Up>(__u)));
}
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded2 : true_type { };
template<typename _Tp, typename _Up>
struct __not_overloaded2<_Tp, _Up, __void_t<
decltype(std::declval<_Tp>().operator<=(std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up, typename = void>
struct __not_overloaded : __not_overloaded2<_Tp, _Up> { };
template<typename _Tp, typename _Up>
struct __not_overloaded<_Tp, _Up, __void_t<
decltype(operator<=(std::declval<_Tp>(), std::declval<_Up>()))>>
: false_type { };
template<typename _Tp, typename _Up>
using __ptr_cmp = __and_<__not_overloaded<_Tp, _Up>,
is_convertible<_Tp, const volatile void*>,
is_convertible<_Up, const volatile void*>>;
};
#endif
#if __cplusplus > 201103L
template<typename _Tp = void>
struct logical_and;
template<typename _Tp = void>
struct logical_or;
template<typename _Tp = void>
struct logical_not;
#endif
template<typename _Tp>
struct logical_and : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x && __y; }
};
template<typename _Tp>
struct logical_or : public binary_function<_Tp, _Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x || __y; }
};
template<typename _Tp>
struct logical_not : public unary_function<_Tp, bool>
{
_GLIBCXX14_CONSTEXPR
bool
operator()(const _Tp& __x) const
{ return !__x; }
};
#if __cplusplus > 201103L
template<>
struct logical_and<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) && std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) && std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) && std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct logical_or<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) || std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) || std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) || std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template<>
struct logical_not<void>
{
template <typename _Tp>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t) const
noexcept(noexcept(!std::forward<_Tp>(__t)))
-> decltype(!std::forward<_Tp>(__t))
{ return !std::forward<_Tp>(__t); }
typedef __is_transparent is_transparent;
};
#endif
#if __cplusplus > 201103L
template<typename _Tp = void>
struct bit_and;
template<typename _Tp = void>
struct bit_or;
template<typename _Tp = void>
struct bit_xor;
template<typename _Tp = void>
struct bit_not;
#endif
template<typename _Tp>
struct bit_and : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x & __y; }
};
template<typename _Tp>
struct bit_or : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x | __y; }
};
template<typename _Tp>
struct bit_xor : public binary_function<_Tp, _Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x, const _Tp& __y) const
{ return __x ^ __y; }
};
template<typename _Tp>
struct bit_not : public unary_function<_Tp, _Tp>
{
_GLIBCXX14_CONSTEXPR
_Tp
operator()(const _Tp& __x) const
{ return ~__x; }
};
#if __cplusplus > 201103L
template <>
struct bit_and<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) & std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) & std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) & std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template <>
struct bit_or<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) | std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) | std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) | std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template <>
struct bit_xor<void>
{
template <typename _Tp, typename _Up>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t, _Up&& __u) const
noexcept(noexcept(std::forward<_Tp>(__t) ^ std::forward<_Up>(__u)))
-> decltype(std::forward<_Tp>(__t) ^ std::forward<_Up>(__u))
{ return std::forward<_Tp>(__t) ^ std::forward<_Up>(__u); }
typedef __is_transparent is_transparent;
};
template <>
struct bit_not<void>
{
template <typename _Tp>
_GLIBCXX14_CONSTEXPR
auto
operator()(_Tp&& __t) const
noexcept(noexcept(~std::forward<_Tp>(__t)))
-> decltype(~std::forward<_Tp>(__t))
{ return ~std::forward<_Tp>(__t); }
typedef __is_transparent is_transparent;
};
#endif
template<typename _Predicate>
class unary_negate
: public unary_function<typename _Predicate::argument_type, bool>
{
protected:
_Predicate _M_pred;
public:
_GLIBCXX14_CONSTEXPR
explicit
unary_negate(const _Predicate& __x) : _M_pred(__x) { }
_GLIBCXX14_CONSTEXPR
bool
operator()(const typename _Predicate::argument_type& __x) const
{ return !_M_pred(__x); }
};
template<typename _Predicate>
_GLIBCXX14_CONSTEXPR
inline unary_negate<_Predicate>
not1(const _Predicate& __pred)
{ return unary_negate<_Predicate>(__pred); }
template<typename _Predicate>
class binary_negate
: public binary_function<typename _Predicate::first_argument_type,
typename _Predicate::second_argument_type, bool>
{
protected:
_Predicate _M_pred;
public:
_GLIBCXX14_CONSTEXPR
explicit
binary_negate(const _Predicate& __x) : _M_pred(__x) { }
_GLIBCXX14_CONSTEXPR
bool
operator()(const typename _Predicate::first_argument_type& __x,
const typename _Predicate::second_argument_type& __y) const
{ return !_M_pred(__x, __y); }
};
template<typename _Predicate>
_GLIBCXX14_CONSTEXPR
inline binary_negate<_Predicate>
not2(const _Predicate& __pred)
{ return binary_negate<_Predicate>(__pred); }
template<typename _Arg, typename _Result>
class pointer_to_unary_function : public unary_function<_Arg, _Result>
{
protected:
_Result (*_M_ptr)(_Arg);
public:
pointer_to_unary_function() { }
explicit
pointer_to_unary_function(_Result (*__x)(_Arg))
: _M_ptr(__x) { }
_Result
operator()(_Arg __x) const
{ return _M_ptr(__x); }
};
template<typename _Arg, typename _Result>
inline pointer_to_unary_function<_Arg, _Result>
ptr_fun(_Result (*__x)(_Arg))
{ return pointer_to_unary_function<_Arg, _Result>(__x); }
template<typename _Arg1, typename _Arg2, typename _Result>
class pointer_to_binary_function
: public binary_function<_Arg1, _Arg2, _Result>
{
protected:
_Result (*_M_ptr)(_Arg1, _Arg2);
public:
pointer_to_binary_function() { }
explicit
pointer_to_binary_function(_Result (*__x)(_Arg1, _Arg2))
: _M_ptr(__x) { }
_Result
operator()(_Arg1 __x, _Arg2 __y) const
{ return _M_ptr(__x, __y); }
};
template<typename _Arg1, typename _Arg2, typename _Result>
inline pointer_to_binary_function<_Arg1, _Arg2, _Result>
ptr_fun(_Result (*__x)(_Arg1, _Arg2))
{ return pointer_to_binary_function<_Arg1, _Arg2, _Result>(__x); }
template<typename _Tp>
struct _Identity
: public unary_function<_Tp, _Tp>
{
_Tp&
operator()(_Tp& __x) const
{ return __x; }
const _Tp&
operator()(const _Tp& __x) const
{ return __x; }
};
template<typename _Tp> struct _Identity<const _Tp> : _Identity<_Tp> { };
template<typename _Pair>
struct _Select1st
: public unary_function<_Pair, typename _Pair::first_type>
{
typename _Pair::first_type&
operator()(_Pair& __x) const
{ return __x.first; }
const typename _Pair::first_type&
operator()(const _Pair& __x) const
{ return __x.first; }
#if __cplusplus >= 201103L
template<typename _Pair2>
typename _Pair2::first_type&
operator()(_Pair2& __x) const
{ return __x.first; }
template<typename _Pair2>
const typename _Pair2::first_type&
operator()(const _Pair2& __x) const
{ return __x.first; }
#endif
};
template<typename _Pair>
struct _Select2nd
: public unary_function<_Pair, typename _Pair::second_type>
{
typename _Pair::second_type&
operator()(_Pair& __x) const
{ return __x.second; }
const typename _Pair::second_type&
operator()(const _Pair& __x) const
{ return __x.second; }
};
template<typename _Ret, typename _Tp>
class mem_fun_t : public unary_function<_Tp*, _Ret>
{
public:
explicit
mem_fun_t(_Ret (_Tp::*__pf)())
: _M_f(__pf) { }
_Ret
operator()(_Tp* __p) const
{ return (__p->*_M_f)(); }
private:
_Ret (_Tp::*_M_f)();
};
template<typename _Ret, typename _Tp>
class const_mem_fun_t : public unary_function<const _Tp*, _Ret>
{
public:
explicit
const_mem_fun_t(_Ret (_Tp::*__pf)() const)
: _M_f(__pf) { }
_Ret
operator()(const _Tp* __p) const
{ return (__p->*_M_f)(); }
private:
_Ret (_Tp::*_M_f)() const;
};
template<typename _Ret, typename _Tp>
class mem_fun_ref_t : public unary_function<_Tp, _Ret>
{
public:
explicit
mem_fun_ref_t(_Ret (_Tp::*__pf)())
: _M_f(__pf) { }
_Ret
operator()(_Tp& __r) const
{ return (__r.*_M_f)(); }
private:
_Ret (_Tp::*_M_f)();
};
template<typename _Ret, typename _Tp>
class const_mem_fun_ref_t : public unary_function<_Tp, _Ret>
{
public:
explicit
const_mem_fun_ref_t(_Ret (_Tp::*__pf)() const)
: _M_f(__pf) { }
_Ret
operator()(const _Tp& __r) const
{ return (__r.*_M_f)(); }
private:
_Ret (_Tp::*_M_f)() const;
};
template<typename _Ret, typename _Tp, typename _Arg>
class mem_fun1_t : public binary_function<_Tp*, _Arg, _Ret>
{
public:
explicit
mem_fun1_t(_Ret (_Tp::*__pf)(_Arg))
: _M_f(__pf) { }
_Ret
operator()(_Tp* __p, _Arg __x) const
{ return (__p->*_M_f)(__x); }
private:
_Ret (_Tp::*_M_f)(_Arg);
};
template<typename _Ret, typename _Tp, typename _Arg>
class const_mem_fun1_t : public binary_function<const _Tp*, _Arg, _Ret>
{
public:
explicit
const_mem_fun1_t(_Ret (_Tp::*__pf)(_Arg) const)
: _M_f(__pf) { }
_Ret
operator()(const _Tp* __p, _Arg __x) const
{ return (__p->*_M_f)(__x); }
private:
_Ret (_Tp::*_M_f)(_Arg) const;
};
template<typename _Ret, typename _Tp, typename _Arg>
class mem_fun1_ref_t : public binary_function<_Tp, _Arg, _Ret>
{
public:
explicit
mem_fun1_ref_t(_Ret (_Tp::*__pf)(_Arg))
: _M_f(__pf) { }
_Ret
operator()(_Tp& __r, _Arg __x) const
{ return (__r.*_M_f)(__x); }
private:
_Ret (_Tp::*_M_f)(_Arg);
};
template<typename _Ret, typename _Tp, typename _Arg>
class const_mem_fun1_ref_t : public binary_function<_Tp, _Arg, _Ret>
{
public:
explicit
const_mem_fun1_ref_t(_Ret (_Tp::*__pf)(_Arg) const)
: _M_f(__pf) { }
_Ret
operator()(const _Tp& __r, _Arg __x) const
{ return (__r.*_M_f)(__x); }
private:
_Ret (_Tp::*_M_f)(_Arg) const;
};
template<typename _Ret, typename _Tp>
inline mem_fun_t<_Ret, _Tp>
mem_fun(_Ret (_Tp::*__f)())
{ return mem_fun_t<_Ret, _Tp>(__f); }
template<typename _Ret, typename _Tp>
inline const_mem_fun_t<_Ret, _Tp>
mem_fun(_Ret (_Tp::*__f)() const)
{ return const_mem_fun_t<_Ret, _Tp>(__f); }
template<typename _Ret, typename _Tp>
inline mem_fun_ref_t<_Ret, _Tp>
mem_fun_ref(_Ret (_Tp::*__f)())
{ return mem_fun_ref_t<_Ret, _Tp>(__f); }
template<typename _Ret, typename _Tp>
inline const_mem_fun_ref_t<_Ret, _Tp>
mem_fun_ref(_Ret (_Tp::*__f)() const)
{ return const_mem_fun_ref_t<_Ret, _Tp>(__f); }
template<typename _Ret, typename _Tp, typename _Arg>
inline mem_fun1_t<_Ret, _Tp, _Arg>
mem_fun(_Ret (_Tp::*__f)(_Arg))
{ return mem_fun1_t<_Ret, _Tp, _Arg>(__f); }
template<typename _Ret, typename _Tp, typename _Arg>
inline const_mem_fun1_t<_Ret, _Tp, _Arg>
mem_fun(_Ret (_Tp::*__f)(_Arg) const)
{ return const_mem_fun1_t<_Ret, _Tp, _Arg>(__f); }
template<typename _Ret, typename _Tp, typename _Arg>
inline mem_fun1_ref_t<_Ret, _Tp, _Arg>
mem_fun_ref(_Ret (_Tp::*__f)(_Arg))
{ return mem_fun1_ref_t<_Ret, _Tp, _Arg>(__f); }
template<typename _Ret, typename _Tp, typename _Arg>
inline const_mem_fun1_ref_t<_Ret, _Tp, _Arg>
mem_fun_ref(_Ret (_Tp::*__f)(_Arg) const)
{ return const_mem_fun1_ref_t<_Ret, _Tp, _Arg>(__f); }
#if __cplusplus >= 201402L
template<typename _Func, typename _SfinaeType, typename = __void_t<>>
struct __has_is_transparent
{ };
template<typename _Func, typename _SfinaeType>
struct __has_is_transparent<_Func, _SfinaeType,
__void_t<typename _Func::is_transparent>>
{ typedef void type; };
template<typename _Func, typename _SfinaeType>
using __has_is_transparent_t
= typename __has_is_transparent<_Func, _SfinaeType>::type;
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#if (__cplusplus < 201103L) || _GLIBCXX_USE_DEPRECATED
# include <backward/binders.h>
#endif
#endif