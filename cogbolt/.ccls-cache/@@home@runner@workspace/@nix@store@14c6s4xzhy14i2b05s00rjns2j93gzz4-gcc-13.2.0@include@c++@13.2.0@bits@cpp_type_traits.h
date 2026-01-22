#ifndef _CPP_TYPE_TRAITS_H
#define _CPP_TYPE_TRAITS_H 1
#pragma GCC system_header
#include <bits/c++config.h>
extern "C++" {
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
struct __true_type { };
struct __false_type { };
template<bool>
struct __truth_type
{ typedef __false_type __type; };
template<>
struct __truth_type<true>
{ typedef __true_type __type; };
template<class _Sp, class _Tp>
struct __traitor
{
enum { __value = bool(_Sp::__value) || bool(_Tp::__value) };
typedef typename __truth_type<__value>::__type __type;
};
template<typename, typename>
struct __are_same
{
enum { __value = 0 };
typedef __false_type __type;
};
template<typename _Tp>
struct __are_same<_Tp, _Tp>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<typename _Tp>
struct __is_void
{
enum { __value = 0 };
typedef __false_type __type;
};
template<>
struct __is_void<void>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<typename _Tp>
struct __is_integer
{
enum { __value = 0 };
typedef __false_type __type;
};
template<>
struct __is_integer<bool>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<char>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<signed char>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<unsigned char>
{
enum { __value = 1 };
typedef __true_type __type;
};
# ifdef __WCHAR_TYPE__
template<>
struct __is_integer<wchar_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
# endif
#ifdef _GLIBCXX_USE_CHAR8_T
template<>
struct __is_integer<char8_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#if __cplusplus >= 201103L
template<>
struct __is_integer<char16_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<char32_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
template<>
struct __is_integer<short>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<unsigned short>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<int>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<unsigned int>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<long>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<unsigned long>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<long long>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_integer<unsigned long long>
{
enum { __value = 1 };
typedef __true_type __type;
};
#define __INT_N(TYPE) 			\
__extension__				\
template<>				\
struct __is_integer<TYPE>		\
{					\
enum { __value = 1 };		\
typedef __true_type __type;	\
};					\
__extension__				\
template<>				\
struct __is_integer<unsigned TYPE>	\
{					\
enum { __value = 1 };		\
typedef __true_type __type;	\
};
#ifdef __GLIBCXX_TYPE_INT_N_0
__INT_N(__GLIBCXX_TYPE_INT_N_0)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_1
__INT_N(__GLIBCXX_TYPE_INT_N_1)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_2
__INT_N(__GLIBCXX_TYPE_INT_N_2)
#endif
#ifdef __GLIBCXX_TYPE_INT_N_3
__INT_N(__GLIBCXX_TYPE_INT_N_3)
#endif
#undef __INT_N
template<typename _Tp>
struct __is_floating
{
enum { __value = 0 };
typedef __false_type __type;
};
template<>
struct __is_floating<float>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_floating<double>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_floating<long double>
{
enum { __value = 1 };
typedef __true_type __type;
};
#ifdef __STDCPP_FLOAT16_T__
template<>
struct __is_floating<_Float16>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#ifdef __STDCPP_FLOAT32_T__
template<>
struct __is_floating<_Float32>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#ifdef __STDCPP_FLOAT64_T__
template<>
struct __is_floating<_Float64>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#ifdef __STDCPP_FLOAT128_T__
template<>
struct __is_floating<_Float128>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#ifdef __STDCPP_BFLOAT16_T__
template<>
struct __is_floating<__gnu_cxx::__bfloat16_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
template<typename _Tp>
struct __is_pointer
{
enum { __value = 0 };
typedef __false_type __type;
};
template<typename _Tp>
struct __is_pointer<_Tp*>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<typename _Tp>
struct __is_arithmetic
: public __traitor<__is_integer<_Tp>, __is_floating<_Tp> >
{ };
template<typename _Tp>
struct __is_scalar
: public __traitor<__is_arithmetic<_Tp>, __is_pointer<_Tp> >
{ };
template<typename _Tp>
struct __is_char
{
enum { __value = 0 };
typedef __false_type __type;
};
template<>
struct __is_char<char>
{
enum { __value = 1 };
typedef __true_type __type;
};
#ifdef __WCHAR_TYPE__
template<>
struct __is_char<wchar_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
template<typename _Tp>
struct __is_byte
{
enum { __value = 0 };
typedef __false_type __type;
};
template<>
struct __is_byte<char>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_byte<signed char>
{
enum { __value = 1 };
typedef __true_type __type;
};
template<>
struct __is_byte<unsigned char>
{
enum { __value = 1 };
typedef __true_type __type;
};
#if __cplusplus >= 201703L
enum class byte : unsigned char;
template<>
struct __is_byte<byte>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
#ifdef _GLIBCXX_USE_CHAR8_T
template<>
struct __is_byte<char8_t>
{
enum { __value = 1 };
typedef __true_type __type;
};
#endif
template<typename> struct iterator_traits;
template<typename _Tp>
struct __is_nonvolatile_trivially_copyable
{
enum { __value = __is_trivially_copyable(_Tp) };
};
template<typename _Tp>
struct __is_nonvolatile_trivially_copyable<volatile _Tp>
{
enum { __value = 0 };
};
template<typename _OutputIter, typename _InputIter>
struct __memcpyable
{
enum { __value = 0 };
};
template<typename _Tp>
struct __memcpyable<_Tp*, _Tp*>
: __is_nonvolatile_trivially_copyable<_Tp>
{ };
template<typename _Tp>
struct __memcpyable<_Tp*, const _Tp*>
: __is_nonvolatile_trivially_copyable<_Tp>
{ };
template<typename _Iter1, typename _Iter2>
struct __memcmpable
{
enum { __value = 0 };
};
template<typename _Tp>
struct __memcmpable<_Tp*, _Tp*>
: __is_nonvolatile_trivially_copyable<_Tp>
{ };
template<typename _Tp>
struct __memcmpable<const _Tp*, _Tp*>
: __is_nonvolatile_trivially_copyable<_Tp>
{ };
template<typename _Tp>
struct __memcmpable<_Tp*, const _Tp*>
: __is_nonvolatile_trivially_copyable<_Tp>
{ };
template<typename _Tp, bool _TreatAsBytes =
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
__is_integer<_Tp>::__value
#else
__is_byte<_Tp>::__value
#endif
>
struct __is_memcmp_ordered
{
static const bool __value = _Tp(-1) > _Tp(1);
};
template<typename _Tp>
struct __is_memcmp_ordered<_Tp, false>
{
static const bool __value = false;
};
template<typename _Tp, typename _Up, bool = sizeof(_Tp) == sizeof(_Up)>
struct __is_memcmp_ordered_with
{
static const bool __value = __is_memcmp_ordered<_Tp>::__value
&& __is_memcmp_ordered<_Up>::__value;
};
template<typename _Tp, typename _Up>
struct __is_memcmp_ordered_with<_Tp, _Up, false>
{
static const bool __value = false;
};
#if __cplusplus >= 201703L
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
template<>
struct __is_memcmp_ordered<std::byte, false>
{ static constexpr bool __value = true; };
#endif
template<>
struct __is_memcmp_ordered_with<std::byte, std::byte, true>
{ static constexpr bool __value = true; };
template<typename _Tp, bool _SameSize>
struct __is_memcmp_ordered_with<_Tp, std::byte, _SameSize>
{ static constexpr bool __value = false; };
template<typename _Up, bool _SameSize>
struct __is_memcmp_ordered_with<std::byte, _Up, _SameSize>
{ static constexpr bool __value = false; };
#endif
template<typename _Tp>
struct __is_move_iterator
{
enum { __value = 0 };
typedef __false_type __type;
};
template<typename _Iterator>
_GLIBCXX20_CONSTEXPR
inline _Iterator
__miter_base(_Iterator __it)
{ return __it; }
_GLIBCXX_END_NAMESPACE_VERSION
}
}
#endif