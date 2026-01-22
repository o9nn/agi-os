#ifndef _GLIBCXX_CHRONO_H
#define _GLIBCXX_CHRONO_H 1
#pragma GCC system_header
#if __cplusplus >= 201103L
#include <ratio>
#include <type_traits>
#include <limits>
#include <ctime>
#include <bits/parse_numbers.h>
#if __cplusplus >= 202002L
# include <concepts>
# include <compare>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#if __cplusplus >= 201703L
namespace filesystem { struct __file_clock; };
#endif
namespace chrono
{
template<typename _Rep, typename _Period = ratio<1>>
class duration;
template<typename _Clock, typename _Dur = typename _Clock::duration>
class time_point;
}
template<typename _CT, typename _Period1, typename _Period2, typename = void>
struct __duration_common_type
{ };
template<typename _CT, typename _Period1, typename _Period2>
struct __duration_common_type<_CT, _Period1, _Period2,
__void_t<typename _CT::type>>
{
private:
using __gcd_num = __static_gcd<_Period1::num, _Period2::num>;
using __gcd_den = __static_gcd<_Period1::den, _Period2::den>;
using __cr = typename _CT::type;
using __r = ratio<__gcd_num::value,
(_Period1::den / __gcd_den::value) * _Period2::den>;
public:
using type = chrono::duration<__cr, typename __r::type>;
};
template<typename _Rep1, typename _Period1, typename _Rep2, typename _Period2>
struct common_type<chrono::duration<_Rep1, _Period1>,
chrono::duration<_Rep2, _Period2>>
: __duration_common_type<common_type<_Rep1, _Rep2>,
typename _Period1::type,
typename _Period2::type>
{ };
template<typename _Rep, typename _Period>
struct common_type<chrono::duration<_Rep, _Period>,
chrono::duration<_Rep, _Period>>
{
using type = chrono::duration<typename common_type<_Rep>::type,
typename _Period::type>;
};
template<typename _Rep, typename _Period>
struct common_type<chrono::duration<_Rep, _Period>>
{
using type = chrono::duration<typename common_type<_Rep>::type,
typename _Period::type>;
};
template<typename _CT, typename _Clock, typename = void>
struct __timepoint_common_type
{ };
template<typename _CT, typename _Clock>
struct __timepoint_common_type<_CT, _Clock, __void_t<typename _CT::type>>
{
using type = chrono::time_point<_Clock, typename _CT::type>;
};
template<typename _Clock, typename _Duration1, typename _Duration2>
struct common_type<chrono::time_point<_Clock, _Duration1>,
chrono::time_point<_Clock, _Duration2>>
: __timepoint_common_type<common_type<_Duration1, _Duration2>, _Clock>
{ };
template<typename _Clock, typename _Duration>
struct common_type<chrono::time_point<_Clock, _Duration>,
chrono::time_point<_Clock, _Duration>>
{ using type = chrono::time_point<_Clock, _Duration>; };
template<typename _Clock, typename _Duration>
struct common_type<chrono::time_point<_Clock, _Duration>>
{ using type = chrono::time_point<_Clock, _Duration>; };
namespace chrono
{
template<typename _ToDur, typename _CF, typename _CR,
bool _NumIsOne = false, bool _DenIsOne = false>
struct __duration_cast_impl
{
template<typename _Rep, typename _Period>
static constexpr _ToDur
__cast(const duration<_Rep, _Period>& __d)
{
typedef typename _ToDur::rep __to_rep;
return _ToDur(static_cast<__to_rep>(static_cast<_CR>(__d.count())
* static_cast<_CR>(_CF::num)
/ static_cast<_CR>(_CF::den)));
}
};
template<typename _ToDur, typename _CF, typename _CR>
struct __duration_cast_impl<_ToDur, _CF, _CR, true, true>
{
template<typename _Rep, typename _Period>
static constexpr _ToDur
__cast(const duration<_Rep, _Period>& __d)
{
typedef typename _ToDur::rep __to_rep;
return _ToDur(static_cast<__to_rep>(__d.count()));
}
};
template<typename _ToDur, typename _CF, typename _CR>
struct __duration_cast_impl<_ToDur, _CF, _CR, true, false>
{
template<typename _Rep, typename _Period>
static constexpr _ToDur
__cast(const duration<_Rep, _Period>& __d)
{
typedef typename _ToDur::rep __to_rep;
return _ToDur(static_cast<__to_rep>(
static_cast<_CR>(__d.count()) / static_cast<_CR>(_CF::den)));
}
};
template<typename _ToDur, typename _CF, typename _CR>
struct __duration_cast_impl<_ToDur, _CF, _CR, false, true>
{
template<typename _Rep, typename _Period>
static constexpr _ToDur
__cast(const duration<_Rep, _Period>& __d)
{
typedef typename _ToDur::rep __to_rep;
return _ToDur(static_cast<__to_rep>(
static_cast<_CR>(__d.count()) * static_cast<_CR>(_CF::num)));
}
};
template<typename _Tp>
struct __is_duration
: std::false_type
{ };
template<typename _Rep, typename _Period>
struct __is_duration<duration<_Rep, _Period>>
: std::true_type
{ };
template<typename _Tp>
using __enable_if_is_duration
= typename enable_if<__is_duration<_Tp>::value, _Tp>::type;
template<typename _Tp>
using __disable_if_is_duration
= typename enable_if<!__is_duration<_Tp>::value, _Tp>::type;
#if __cplusplus >= 201703L
template<typename _Tp>
inline constexpr bool __is_duration_v = false;
template<typename _Rep, typename _Period>
inline constexpr bool __is_duration_v<duration<_Rep, _Period>> = true;
template<typename _Tp>
inline constexpr bool __is_time_point_v = false;
template<typename _Clock, typename _Dur>
inline constexpr bool __is_time_point_v<time_point<_Clock, _Dur>> = true;
#endif
template<typename _ToDur, typename _Rep, typename _Period>
_GLIBCXX_NODISCARD
constexpr __enable_if_is_duration<_ToDur>
duration_cast(const duration<_Rep, _Period>& __d)
{
#if __cpp_inline_variables && __cpp_if_constexpr
if constexpr (is_same_v<_ToDur, duration<_Rep, _Period>>)
return __d;
else
#endif
{
using __to_period = typename _ToDur::period;
using __to_rep = typename _ToDur::rep;
using __cf = ratio_divide<_Period, __to_period>;
using __cr = typename common_type<__to_rep, _Rep, intmax_t>::type;
using __dc = __duration_cast_impl<_ToDur, __cf, __cr,
__cf::num == 1, __cf::den == 1>;
return __dc::__cast(__d);
}
}
template<typename _Rep>
struct treat_as_floating_point
: is_floating_point<_Rep>
{ };
#if __cplusplus > 201402L
template <typename _Rep>
inline constexpr bool treat_as_floating_point_v =
treat_as_floating_point<_Rep>::value;
template<>
inline constexpr bool treat_as_floating_point_v<int> = false;
template<>
inline constexpr bool treat_as_floating_point_v<long> = false;
template<>
inline constexpr bool treat_as_floating_point_v<long long> = false;
template<>
inline constexpr bool treat_as_floating_point_v<float> = true;
template<>
inline constexpr bool treat_as_floating_point_v<double> = true;
template<>
inline constexpr bool treat_as_floating_point_v<long double> = true;
#endif
#if __cplusplus > 201703L
#if __cpp_lib_concepts
template<typename _Tp>
inline constexpr bool is_clock_v = false;
template<typename _Tp>
requires requires {
typename _Tp::rep;
typename _Tp::period;
typename _Tp::duration;
typename _Tp::time_point::clock;
typename _Tp::time_point::duration;
{ &_Tp::is_steady } -> same_as<const bool*>;
{ _Tp::now() } -> same_as<typename _Tp::time_point>;
requires same_as<typename _Tp::duration,
duration<typename _Tp::rep, typename _Tp::period>>;
requires same_as<typename _Tp::time_point::duration,
typename _Tp::duration>;
}
inline constexpr bool is_clock_v<_Tp> = true;
#else
template<typename _Tp, typename = void>
inline constexpr bool is_clock_v = false;
template<typename _Tp>
inline constexpr bool
is_clock_v<_Tp, void_t<typename _Tp::rep, typename _Tp::period,
typename _Tp::duration,
typename _Tp::time_point::duration,
decltype(_Tp::is_steady),
decltype(_Tp::now())>>
= __and_v<is_same<typename _Tp::duration,
duration<typename _Tp::rep, typename _Tp::period>>,
is_same<typename _Tp::time_point::duration,
typename _Tp::duration>,
is_same<decltype(&_Tp::is_steady), const bool*>,
is_same<decltype(_Tp::now()), typename _Tp::time_point>>;
#endif
template<typename _Tp>
struct is_clock
: bool_constant<is_clock_v<_Tp>>
{ };
#endif
#if __cplusplus >= 201703L
# define __cpp_lib_chrono 201611L
template<typename _ToDur, typename _Rep, typename _Period>
[[nodiscard]] constexpr __enable_if_is_duration<_ToDur>
floor(const duration<_Rep, _Period>& __d)
{
auto __to = chrono::duration_cast<_ToDur>(__d);
if (__to > __d)
return __to - _ToDur{1};
return __to;
}
template<typename _ToDur, typename _Rep, typename _Period>
[[nodiscard]] constexpr __enable_if_is_duration<_ToDur>
ceil(const duration<_Rep, _Period>& __d)
{
auto __to = chrono::duration_cast<_ToDur>(__d);
if (__to < __d)
return __to + _ToDur{1};
return __to;
}
template <typename _ToDur, typename _Rep, typename _Period>
[[nodiscard]] constexpr
enable_if_t<
__and_<__is_duration<_ToDur>,
__not_<treat_as_floating_point<typename _ToDur::rep>>>::value,
_ToDur>
round(const duration<_Rep, _Period>& __d)
{
_ToDur __t0 = chrono::floor<_ToDur>(__d);
_ToDur __t1 = __t0 + _ToDur{1};
auto __diff0 = __d - __t0;
auto __diff1 = __t1 - __d;
if (__diff0 == __diff1)
{
if (__t0.count() & 1)
return __t1;
return __t0;
}
else if (__diff0 < __diff1)
return __t0;
return __t1;
}
template<typename _Rep, typename _Period>
[[nodiscard]] constexpr
enable_if_t<numeric_limits<_Rep>::is_signed, duration<_Rep, _Period>>
abs(duration<_Rep, _Period> __d)
{
if (__d >= __d.zero())
return __d;
return -__d;
}
namespace __detail { using chrono::ceil; }
#else
namespace __detail
{
template<typename _Tp, typename _Up>
constexpr _Tp
__ceil_impl(const _Tp& __t, const _Up& __u)
{
return (__t < __u) ? (__t + _Tp{1}) : __t;
}
template<typename _ToDur, typename _Rep, typename _Period>
constexpr _ToDur
ceil(const duration<_Rep, _Period>& __d)
{
return __detail::__ceil_impl(chrono::duration_cast<_ToDur>(__d), __d);
}
}
#endif
template<typename _Rep>
struct duration_values
{
static constexpr _Rep
zero() noexcept
{ return _Rep(0); }
static constexpr _Rep
max() noexcept
{ return numeric_limits<_Rep>::max(); }
static constexpr _Rep
min() noexcept
{ return numeric_limits<_Rep>::lowest(); }
};
template<typename _Tp>
struct __is_ratio
: std::false_type
{ };
template<intmax_t _Num, intmax_t _Den>
struct __is_ratio<ratio<_Num, _Den>>
: std::true_type
{ };
template<typename _Rep, typename _Period>
class duration
{
static_assert(!__is_duration<_Rep>::value, "rep cannot be a duration");
static_assert(__is_ratio<_Period>::value,
"period must be a specialization of ratio");
static_assert(_Period::num > 0, "period must be positive");
template<typename _Rep2>
using __is_float = treat_as_floating_point<_Rep2>;
static constexpr intmax_t
_S_gcd(intmax_t __m, intmax_t __n) noexcept
{
#if __cplusplus >= 201402L
do
{
intmax_t __rem = __m % __n;
__m = __n;
__n = __rem;
}
while (__n != 0);
return __m;
#else
return (__n == 0) ? __m : _S_gcd(__n, __m % __n);
#endif
}
template<typename _R1, typename _R2,
intmax_t __gcd1 = _S_gcd(_R1::num, _R2::num),
intmax_t __gcd2 = _S_gcd(_R1::den, _R2::den)>
using __divide = ratio<(_R1::num / __gcd1) * (_R2::den / __gcd2),
(_R1::den / __gcd2) * (_R2::num / __gcd1)>;
template<typename _Period2>
using __is_harmonic
= __bool_constant<__divide<_Period2, _Period>::den == 1>;
public:
using rep = _Rep;
using period = typename _Period::type;
constexpr duration() = default;
duration(const duration&) = default;
template<typename _Rep2, typename = _Require<
is_convertible<const _Rep2&, rep>,
__or_<__is_float<rep>, __not_<__is_float<_Rep2>>>>>
constexpr explicit duration(const _Rep2& __rep)
: __r(static_cast<rep>(__rep)) { }
template<typename _Rep2, typename _Period2, typename = _Require<
is_convertible<const _Rep2&, rep>,
__or_<__is_float<rep>,
__and_<__is_harmonic<_Period2>,
__not_<__is_float<_Rep2>>>>>>
constexpr duration(const duration<_Rep2, _Period2>& __d)
: __r(duration_cast<duration>(__d).count()) { }
~duration() = default;
duration& operator=(const duration&) = default;
constexpr rep
count() const
{ return __r; }
constexpr duration<typename common_type<rep>::type, period>
operator+() const
{ return duration<typename common_type<rep>::type, period>(__r); }
constexpr duration<typename common_type<rep>::type, period>
operator-() const
{ return duration<typename common_type<rep>::type, period>(-__r); }
_GLIBCXX17_CONSTEXPR duration&
operator++()
{
++__r;
return *this;
}
_GLIBCXX17_CONSTEXPR duration
operator++(int)
{ return duration(__r++); }
_GLIBCXX17_CONSTEXPR duration&
operator--()
{
--__r;
return *this;
}
_GLIBCXX17_CONSTEXPR duration
operator--(int)
{ return duration(__r--); }
_GLIBCXX17_CONSTEXPR duration&
operator+=(const duration& __d)
{
__r += __d.count();
return *this;
}
_GLIBCXX17_CONSTEXPR duration&
operator-=(const duration& __d)
{
__r -= __d.count();
return *this;
}
_GLIBCXX17_CONSTEXPR duration&
operator*=(const rep& __rhs)
{
__r *= __rhs;
return *this;
}
_GLIBCXX17_CONSTEXPR duration&
operator/=(const rep& __rhs)
{
__r /= __rhs;
return *this;
}
template<typename _Rep2 = rep>
_GLIBCXX17_CONSTEXPR
__enable_if_t<!treat_as_floating_point<_Rep2>::value, duration&>
operator%=(const rep& __rhs)
{
__r %= __rhs;
return *this;
}
template<typename _Rep2 = rep>
_GLIBCXX17_CONSTEXPR
__enable_if_t<!treat_as_floating_point<_Rep2>::value, duration&>
operator%=(const duration& __d)
{
__r %= __d.count();
return *this;
}
static constexpr duration
zero() noexcept
{ return duration(duration_values<rep>::zero()); }
static constexpr duration
min() noexcept
{ return duration(duration_values<rep>::min()); }
static constexpr duration
max() noexcept
{ return duration(duration_values<rep>::max()); }
private:
rep __r;
};
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr typename common_type<duration<_Rep1, _Period1>,
duration<_Rep2, _Period2>>::type
operator+(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __cd;
return __cd(__cd(__lhs).count() + __cd(__rhs).count());
}
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr typename common_type<duration<_Rep1, _Period1>,
duration<_Rep2, _Period2>>::type
operator-(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __cd;
return __cd(__cd(__lhs).count() - __cd(__rhs).count());
}
template<typename _Rep1, typename _Rep2,
typename _CRep = typename common_type<_Rep1, _Rep2>::type>
using __common_rep_t = typename
enable_if<is_convertible<const _Rep2&, _CRep>::value, _CRep>::type;
template<typename _Rep1, typename _Period, typename _Rep2>
constexpr duration<__common_rep_t<_Rep1, _Rep2>, _Period>
operator*(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
{
typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
__cd;
return __cd(__cd(__d).count() * __s);
}
template<typename _Rep1, typename _Rep2, typename _Period>
constexpr duration<__common_rep_t<_Rep2, _Rep1>, _Period>
operator*(const _Rep1& __s, const duration<_Rep2, _Period>& __d)
{ return __d * __s; }
template<typename _Rep1, typename _Period, typename _Rep2>
constexpr
duration<__common_rep_t<_Rep1, __disable_if_is_duration<_Rep2>>, _Period>
operator/(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
{
typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
__cd;
return __cd(__cd(__d).count() / __s);
}
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr typename common_type<_Rep1, _Rep2>::type
operator/(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __cd;
return __cd(__lhs).count() / __cd(__rhs).count();
}
template<typename _Rep1, typename _Period, typename _Rep2>
constexpr
duration<__common_rep_t<_Rep1, __disable_if_is_duration<_Rep2>>, _Period>
operator%(const duration<_Rep1, _Period>& __d, const _Rep2& __s)
{
typedef duration<typename common_type<_Rep1, _Rep2>::type, _Period>
__cd;
return __cd(__cd(__d).count() % __s);
}
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr typename common_type<duration<_Rep1, _Period1>,
duration<_Rep2, _Period2>>::type
operator%(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __cd;
return __cd(__cd(__lhs).count() % __cd(__rhs).count());
}
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator==(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __ct;
return __ct(__lhs).count() == __ct(__rhs).count();
}
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator<(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<__dur1,__dur2>::type __ct;
return __ct(__lhs).count() < __ct(__rhs).count();
}
#if __cpp_lib_three_way_comparison
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
requires three_way_comparable<common_type_t<_Rep1, _Rep2>>
constexpr auto
operator<=>(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
using __ct = common_type_t<duration<_Rep1, _Period1>,
duration<_Rep2, _Period2>>;
return __ct(__lhs).count() <=> __ct(__rhs).count();
}
#else
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator!=(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{ return !(__lhs == __rhs); }
#endif
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator<=(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{ return !(__rhs < __lhs); }
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator>(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{ return __rhs < __lhs; }
template<typename _Rep1, typename _Period1,
typename _Rep2, typename _Period2>
constexpr bool
operator>=(const duration<_Rep1, _Period1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{ return !(__lhs < __rhs); }
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
# define _GLIBCXX_CHRONO_INT64_T int64_t
#elif defined __INT64_TYPE__
# define _GLIBCXX_CHRONO_INT64_T __INT64_TYPE__
#else
static_assert(std::numeric_limits<unsigned long long>::digits >= 64,
"Representation type for nanoseconds must have at least 64 bits");
# define _GLIBCXX_CHRONO_INT64_T long long
#endif
using nanoseconds = duration<_GLIBCXX_CHRONO_INT64_T, nano>;
using microseconds = duration<_GLIBCXX_CHRONO_INT64_T, micro>;
using milliseconds = duration<_GLIBCXX_CHRONO_INT64_T, milli>;
using seconds = duration<_GLIBCXX_CHRONO_INT64_T>;
using minutes = duration<_GLIBCXX_CHRONO_INT64_T, ratio< 60>>;
using hours = duration<_GLIBCXX_CHRONO_INT64_T, ratio<3600>>;
#if __cplusplus > 201703L
using days = duration<_GLIBCXX_CHRONO_INT64_T, ratio<86400>>;
using weeks = duration<_GLIBCXX_CHRONO_INT64_T, ratio<604800>>;
using years = duration<_GLIBCXX_CHRONO_INT64_T, ratio<31556952>>;
using months = duration<_GLIBCXX_CHRONO_INT64_T, ratio<2629746>>;
#endif
#undef _GLIBCXX_CHRONO_INT64_T
template<typename _Clock, typename _Dur>
class time_point
{
static_assert(__is_duration<_Dur>::value,
"duration must be a specialization of std::chrono::duration");
public:
typedef _Clock clock;
typedef _Dur duration;
typedef typename duration::rep rep;
typedef typename duration::period period;
constexpr time_point() : __d(duration::zero())
{ }
constexpr explicit time_point(const duration& __dur)
: __d(__dur)
{ }
template<typename _Dur2,
typename = _Require<is_convertible<_Dur2, _Dur>>>
constexpr time_point(const time_point<clock, _Dur2>& __t)
: __d(__t.time_since_epoch())
{ }
constexpr duration
time_since_epoch() const
{ return __d; }
#if __cplusplus > 201703L
constexpr time_point&
operator++()
{
++__d;
return *this;
}
constexpr time_point
operator++(int)
{ return time_point{__d++}; }
constexpr time_point&
operator--()
{
--__d;
return *this;
}
constexpr time_point
operator--(int)
{ return time_point{__d--}; }
#endif
_GLIBCXX17_CONSTEXPR time_point&
operator+=(const duration& __dur)
{
__d += __dur;
return *this;
}
_GLIBCXX17_CONSTEXPR time_point&
operator-=(const duration& __dur)
{
__d -= __dur;
return *this;
}
static constexpr time_point
min() noexcept
{ return time_point(duration::min()); }
static constexpr time_point
max() noexcept
{ return time_point(duration::max()); }
private:
duration __d;
};
template<typename _ToDur, typename _Clock, typename _Dur>
_GLIBCXX_NODISCARD constexpr
__enable_if_t<__is_duration<_ToDur>::value, time_point<_Clock, _ToDur>>
time_point_cast(const time_point<_Clock, _Dur>& __t)
{
typedef time_point<_Clock, _ToDur> __time_point;
return __time_point(duration_cast<_ToDur>(__t.time_since_epoch()));
}
#if __cplusplus > 201402L
template<typename _ToDur, typename _Clock, typename _Dur>
[[nodiscard]] constexpr
enable_if_t<__is_duration_v<_ToDur>, time_point<_Clock, _ToDur>>
floor(const time_point<_Clock, _Dur>& __tp)
{
return time_point<_Clock, _ToDur>{
chrono::floor<_ToDur>(__tp.time_since_epoch())};
}
template<typename _ToDur, typename _Clock, typename _Dur>
[[nodiscard]] constexpr
enable_if_t<__is_duration_v<_ToDur>, time_point<_Clock, _ToDur>>
ceil(const time_point<_Clock, _Dur>& __tp)
{
return time_point<_Clock, _ToDur>{
chrono::ceil<_ToDur>(__tp.time_since_epoch())};
}
template<typename _ToDur, typename _Clock, typename _Dur>
[[nodiscard]] constexpr
enable_if_t<__is_duration_v<_ToDur>
&& !treat_as_floating_point_v<typename _ToDur::rep>,
time_point<_Clock, _ToDur>>
round(const time_point<_Clock, _Dur>& __tp)
{
return time_point<_Clock, _ToDur>{
chrono::round<_ToDur>(__tp.time_since_epoch())};
}
#endif
template<typename _Clock, typename _Dur1,
typename _Rep2, typename _Period2>
constexpr time_point<_Clock,
typename common_type<_Dur1, duration<_Rep2, _Period2>>::type>
operator+(const time_point<_Clock, _Dur1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<_Dur1,__dur2>::type __ct;
typedef time_point<_Clock, __ct> __time_point;
return __time_point(__lhs.time_since_epoch() + __rhs);
}
template<typename _Rep1, typename _Period1,
typename _Clock, typename _Dur2>
constexpr time_point<_Clock,
typename common_type<duration<_Rep1, _Period1>, _Dur2>::type>
operator+(const duration<_Rep1, _Period1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{
typedef duration<_Rep1, _Period1> __dur1;
typedef typename common_type<__dur1,_Dur2>::type __ct;
typedef time_point<_Clock, __ct> __time_point;
return __time_point(__rhs.time_since_epoch() + __lhs);
}
template<typename _Clock, typename _Dur1,
typename _Rep2, typename _Period2>
constexpr time_point<_Clock,
typename common_type<_Dur1, duration<_Rep2, _Period2>>::type>
operator-(const time_point<_Clock, _Dur1>& __lhs,
const duration<_Rep2, _Period2>& __rhs)
{
typedef duration<_Rep2, _Period2> __dur2;
typedef typename common_type<_Dur1,__dur2>::type __ct;
typedef time_point<_Clock, __ct> __time_point;
return __time_point(__lhs.time_since_epoch() -__rhs);
}
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr typename common_type<_Dur1, _Dur2>::type
operator-(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return __lhs.time_since_epoch() - __rhs.time_since_epoch(); }
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator==(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return __lhs.time_since_epoch() == __rhs.time_since_epoch(); }
#if __cpp_lib_three_way_comparison
template<typename _Clock, typename _Dur1,
three_way_comparable_with<_Dur1> _Dur2>
constexpr auto
operator<=>(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return __lhs.time_since_epoch() <=> __rhs.time_since_epoch(); }
#else
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator!=(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return !(__lhs == __rhs); }
#endif
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator<(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return __lhs.time_since_epoch() < __rhs.time_since_epoch(); }
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator<=(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return !(__rhs < __lhs); }
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator>(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return __rhs < __lhs; }
template<typename _Clock, typename _Dur1, typename _Dur2>
constexpr bool
operator>=(const time_point<_Clock, _Dur1>& __lhs,
const time_point<_Clock, _Dur2>& __rhs)
{ return !(__lhs < __rhs); }
_GLIBCXX_BEGIN_INLINE_ABI_NAMESPACE(_V2)
struct system_clock
{
typedef chrono::nanoseconds duration;
typedef duration::rep rep;
typedef duration::period period;
typedef chrono::time_point<system_clock, duration> time_point;
static_assert(system_clock::duration::min()
< system_clock::duration::zero(),
"a clock's minimum duration cannot be less than its epoch");
static constexpr bool is_steady = false;
static time_point
now() noexcept;
static std::time_t
to_time_t(const time_point& __t) noexcept
{
return std::time_t(duration_cast<chrono::seconds>
(__t.time_since_epoch()).count());
}
static time_point
from_time_t(std::time_t __t) noexcept
{
typedef chrono::time_point<system_clock, seconds> __from;
return time_point_cast<system_clock::duration>
(__from(chrono::seconds(__t)));
}
};
struct steady_clock
{
typedef chrono::nanoseconds duration;
typedef duration::rep rep;
typedef duration::period period;
typedef chrono::time_point<steady_clock, duration> time_point;
static constexpr bool is_steady = true;
static time_point
now() noexcept;
};
using high_resolution_clock = system_clock;
_GLIBCXX_END_INLINE_ABI_NAMESPACE(_V2)
#if __cplusplus >= 202002L
template<typename _Duration>
using sys_time = time_point<system_clock, _Duration>;
using sys_seconds = sys_time<seconds>;
using sys_days = sys_time<days>;
using file_clock = ::std::filesystem::__file_clock;
template<typename _Duration>
using file_time = time_point<file_clock, _Duration>;
template<> struct is_clock<system_clock> : true_type { };
template<> struct is_clock<steady_clock> : true_type { };
template<> struct is_clock<file_clock> : true_type { };
template<> inline constexpr bool is_clock_v<system_clock> = true;
template<> inline constexpr bool is_clock_v<steady_clock> = true;
template<> inline constexpr bool is_clock_v<file_clock> = true;
#endif
}
#if __cplusplus >= 201402L
#define __cpp_lib_chrono_udls 201304L
inline namespace literals
{
inline namespace chrono_literals
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wliteral-suffix"
template<typename _Dur, char... _Digits>
constexpr _Dur __check_overflow()
{
using _Val = __parse_int::_Parse_int<_Digits...>;
constexpr typename _Dur::rep __repval = _Val::value;
static_assert(__repval >= 0 && __repval == _Val::value,
"literal value cannot be represented by duration type");
return _Dur(__repval);
}
constexpr chrono::duration<long double, ratio<3600,1>>
operator""h(long double __hours)
{ return chrono::duration<long double, ratio<3600,1>>{__hours}; }
template <char... _Digits>
constexpr chrono::hours
operator""h()
{ return __check_overflow<chrono::hours, _Digits...>(); }
constexpr chrono::duration<long double, ratio<60,1>>
operator""min(long double __mins)
{ return chrono::duration<long double, ratio<60,1>>{__mins}; }
template <char... _Digits>
constexpr chrono::minutes
operator""min()
{ return __check_overflow<chrono::minutes, _Digits...>(); }
constexpr chrono::duration<long double>
operator""s(long double __secs)
{ return chrono::duration<long double>{__secs}; }
template <char... _Digits>
constexpr chrono::seconds
operator""s()
{ return __check_overflow<chrono::seconds, _Digits...>(); }
constexpr chrono::duration<long double, milli>
operator""ms(long double __msecs)
{ return chrono::duration<long double, milli>{__msecs}; }
template <char... _Digits>
constexpr chrono::milliseconds
operator""ms()
{ return __check_overflow<chrono::milliseconds, _Digits...>(); }
constexpr chrono::duration<long double, micro>
operator""us(long double __usecs)
{ return chrono::duration<long double, micro>{__usecs}; }
template <char... _Digits>
constexpr chrono::microseconds
operator""us()
{ return __check_overflow<chrono::microseconds, _Digits...>(); }
constexpr chrono::duration<long double, nano>
operator""ns(long double __nsecs)
{ return chrono::duration<long double, nano>{__nsecs}; }
template <char... _Digits>
constexpr chrono::nanoseconds
operator""ns()
{ return __check_overflow<chrono::nanoseconds, _Digits...>(); }
#pragma GCC diagnostic pop
}
}
namespace chrono
{
using namespace literals::chrono_literals;
}
#endif
#if __cplusplus >= 201703L
namespace filesystem
{
struct __file_clock
{
using duration = chrono::nanoseconds;
using rep = duration::rep;
using period = duration::period;
using time_point = chrono::time_point<__file_clock>;
static constexpr bool is_steady = false;
static time_point
now() noexcept
{ return _S_from_sys(chrono::system_clock::now()); }
#if __cplusplus > 201703L
template<typename _Dur>
static
chrono::file_time<_Dur>
from_sys(const chrono::sys_time<_Dur>& __t) noexcept
{ return _S_from_sys(__t); }
template<typename _Dur>
static
chrono::sys_time<_Dur>
to_sys(const chrono::file_time<_Dur>& __t) noexcept
{ return _S_to_sys(__t); }
#endif
private:
using __sys_clock = chrono::system_clock;
static constexpr chrono::seconds _S_epoch_diff{6437664000};
protected:
template<typename _Dur>
static
chrono::time_point<__file_clock, _Dur>
_S_from_sys(const chrono::time_point<__sys_clock, _Dur>& __t) noexcept
{
using __file_time = chrono::time_point<__file_clock, _Dur>;
return __file_time{__t.time_since_epoch()} - _S_epoch_diff;
}
template<typename _Dur>
static
chrono::time_point<__sys_clock, _Dur>
_S_to_sys(const chrono::time_point<__file_clock, _Dur>& __t) noexcept
{
using __sys_time = chrono::time_point<__sys_clock, _Dur>;
return __sys_time{__t.time_since_epoch()} + _S_epoch_diff;
}
};
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif