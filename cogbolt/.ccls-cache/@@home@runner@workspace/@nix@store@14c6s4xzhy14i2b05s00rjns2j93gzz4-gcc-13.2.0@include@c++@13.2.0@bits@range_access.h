#ifndef _GLIBCXX_RANGE_ACCESS_H
#define _GLIBCXX_RANGE_ACCESS_H 1
#pragma GCC system_header
#if __cplusplus >= 201103L
#include <initializer_list>
#include <type_traits>
#include <bits/stl_iterator.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
begin(_Container& __cont) -> decltype(__cont.begin())
{ return __cont.begin(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
begin(const _Container& __cont) -> decltype(__cont.begin())
{ return __cont.begin(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
end(_Container& __cont) -> decltype(__cont.end())
{ return __cont.end(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
end(const _Container& __cont) -> decltype(__cont.end())
{ return __cont.end(); }
template<typename _Tp, size_t _Nm>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX14_CONSTEXPR _Tp*
begin(_Tp (&__arr)[_Nm]) noexcept
{ return __arr; }
template<typename _Tp, size_t _Nm>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX14_CONSTEXPR _Tp*
end(_Tp (&__arr)[_Nm]) noexcept
{ return __arr + _Nm; }
#if __cplusplus >= 201402L
template<typename _Tp> class valarray;
template<typename _Tp> _Tp* begin(valarray<_Tp>&) noexcept;
template<typename _Tp> const _Tp* begin(const valarray<_Tp>&) noexcept;
template<typename _Tp> _Tp* end(valarray<_Tp>&) noexcept;
template<typename _Tp> const _Tp* end(const valarray<_Tp>&) noexcept;
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
constexpr auto
cbegin(const _Container& __cont) noexcept(noexcept(std::begin(__cont)))
-> decltype(std::begin(__cont))
{ return std::begin(__cont); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
constexpr auto
cend(const _Container& __cont) noexcept(noexcept(std::end(__cont)))
-> decltype(std::end(__cont))
{ return std::end(__cont); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
rbegin(_Container& __cont) -> decltype(__cont.rbegin())
{ return __cont.rbegin(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
rbegin(const _Container& __cont) -> decltype(__cont.rbegin())
{ return __cont.rbegin(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
rend(_Container& __cont) -> decltype(__cont.rend())
{ return __cont.rend(); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
rend(const _Container& __cont) -> decltype(__cont.rend())
{ return __cont.rend(); }
template<typename _Tp, size_t _Nm>
[[__nodiscard__]]
inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Tp*>
rbegin(_Tp (&__arr)[_Nm]) noexcept
{ return reverse_iterator<_Tp*>(__arr + _Nm); }
template<typename _Tp, size_t _Nm>
[[__nodiscard__]]
inline _GLIBCXX17_CONSTEXPR reverse_iterator<_Tp*>
rend(_Tp (&__arr)[_Nm]) noexcept
{ return reverse_iterator<_Tp*>(__arr); }
template<typename _Tp>
[[__nodiscard__]]
inline _GLIBCXX17_CONSTEXPR reverse_iterator<const _Tp*>
rbegin(initializer_list<_Tp> __il) noexcept
{ return reverse_iterator<const _Tp*>(__il.end()); }
template<typename _Tp>
[[__nodiscard__]]
inline _GLIBCXX17_CONSTEXPR reverse_iterator<const _Tp*>
rend(initializer_list<_Tp> __il) noexcept
{ return reverse_iterator<const _Tp*>(__il.begin()); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
crbegin(const _Container& __cont) -> decltype(std::rbegin(__cont))
{ return std::rbegin(__cont); }
template<typename _Container>
[[__nodiscard__, __gnu__::__always_inline__]]
inline _GLIBCXX17_CONSTEXPR auto
crend(const _Container& __cont) -> decltype(std::rend(__cont))
{ return std::rend(__cont); }
#endif
#if __cplusplus >= 201703L
#define __cpp_lib_nonmember_container_access 201411L
template <typename _Container>
[[nodiscard, __gnu__::__always_inline__]]
constexpr auto
size(const _Container& __cont) noexcept(noexcept(__cont.size()))
-> decltype(__cont.size())
{ return __cont.size(); }
template <typename _Tp, size_t _Nm>
[[nodiscard, __gnu__::__always_inline__]]
constexpr size_t
size(const _Tp (&)[_Nm]) noexcept
{ return _Nm; }
template <typename _Container>
[[nodiscard, __gnu__::__always_inline__]]
constexpr auto
empty(const _Container& __cont) noexcept(noexcept(__cont.empty()))
-> decltype(__cont.empty())
{ return __cont.empty(); }
template <typename _Tp, size_t _Nm>
[[nodiscard, __gnu__::__always_inline__]]
constexpr bool
empty(const _Tp (&)[_Nm]) noexcept
{ return false; }
template <typename _Tp>
[[nodiscard, __gnu__::__always_inline__]]
constexpr bool
empty(initializer_list<_Tp> __il) noexcept
{ return __il.size() == 0;}
template <typename _Container>
[[nodiscard, __gnu__::__always_inline__]]
constexpr auto
data(_Container& __cont) noexcept(noexcept(__cont.data()))
-> decltype(__cont.data())
{ return __cont.data(); }
template <typename _Container>
[[nodiscard, __gnu__::__always_inline__]]
constexpr auto
data(const _Container& __cont) noexcept(noexcept(__cont.data()))
-> decltype(__cont.data())
{ return __cont.data(); }
template <typename _Tp, size_t _Nm>
[[nodiscard, __gnu__::__always_inline__]]
constexpr _Tp*
data(_Tp (&__array)[_Nm]) noexcept
{ return __array; }
template <typename _Tp>
[[nodiscard, __gnu__::__always_inline__]]
constexpr const _Tp*
data(initializer_list<_Tp> __il) noexcept
{ return __il.begin(); }
#if __cplusplus > 201703L
#define __cpp_lib_ssize 201902L
template<typename _Container>
[[nodiscard, __gnu__::__always_inline__]]
constexpr auto
ssize(const _Container& __cont)
noexcept(noexcept(__cont.size()))
-> common_type_t<ptrdiff_t, make_signed_t<decltype(__cont.size())>>
{
using type = make_signed_t<decltype(__cont.size())>;
return static_cast<common_type_t<ptrdiff_t, type>>(__cont.size());
}
template<typename _Tp, ptrdiff_t _Num>
[[nodiscard, __gnu__::__always_inline__]]
constexpr ptrdiff_t
ssize(const _Tp (&)[_Num]) noexcept
{ return _Num; }
#endif
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif
#endif