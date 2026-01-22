#ifndef _EXT_TYPE_TRAITS
#define _EXT_TYPE_TRAITS 1
#pragma GCC system_header
#include <bits/c++config.h>
#include <bits/cpp_type_traits.h>
extern "C++" {
namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<bool, typename>
struct __enable_if
{ };
template<typename _Tp>
struct __enable_if<true, _Tp>
{ typedef _Tp __type; };
template<bool _Cond, typename _Iftrue, typename _Iffalse>
struct __conditional_type
{ typedef _Iftrue __type; };
template<typename _Iftrue, typename _Iffalse>
struct __conditional_type<false, _Iftrue, _Iffalse>
{ typedef _Iffalse __type; };
template<typename _Tp>
struct __add_unsigned
{
private:
typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
public:
typedef typename __if_type::__type __type;
};
template<>
struct __add_unsigned<char>
{ typedef unsigned char __type; };
template<>
struct __add_unsigned<signed char>
{ typedef unsigned char __type; };
template<>
struct __add_unsigned<short>
{ typedef unsigned short __type; };
template<>
struct __add_unsigned<int>
{ typedef unsigned int __type; };
template<>
struct __add_unsigned<long>
{ typedef unsigned long __type; };
template<>
struct __add_unsigned<long long>
{ typedef unsigned long long __type; };
template<>
struct __add_unsigned<bool>;
template<>
struct __add_unsigned<wchar_t>;
template<typename _Tp>
struct __remove_unsigned
{
private:
typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
public:
typedef typename __if_type::__type __type;
};
template<>
struct __remove_unsigned<char>
{ typedef signed char __type; };
template<>
struct __remove_unsigned<unsigned char>
{ typedef signed char __type; };
template<>
struct __remove_unsigned<unsigned short>
{ typedef short __type; };
template<>
struct __remove_unsigned<unsigned int>
{ typedef int __type; };
template<>
struct __remove_unsigned<unsigned long>
{ typedef long __type; };
template<>
struct __remove_unsigned<unsigned long long>
{ typedef long long __type; };
template<>
struct __remove_unsigned<bool>;
template<>
struct __remove_unsigned<wchar_t>;
template<typename _Type>
_GLIBCXX_CONSTEXPR
inline bool
__is_null_pointer(_Type* __ptr)
{ return __ptr == 0; }
template<typename _Type>
_GLIBCXX_CONSTEXPR
inline bool
__is_null_pointer(_Type)
{ return false; }
#if __cplusplus >= 201103L
constexpr bool
__is_null_pointer(std::nullptr_t)
{ return true; }
#endif
template<typename _Tp, bool = std::__is_integer<_Tp>::__value>
struct __promote
{ typedef double __type; };
template<typename _Tp>
struct __promote<_Tp, false>
{ };
template<>
struct __promote<long double>
{ typedef long double __type; };
template<>
struct __promote<double>
{ typedef double __type; };
template<>
struct __promote<float>
{ typedef float __type; };
#ifdef __STDCPP_FLOAT16_T__
template<>
struct __promote<_Float16>
{ typedef _Float16 __type; };
#endif
#ifdef __STDCPP_FLOAT32_T__
template<>
struct __promote<_Float32>
{ typedef _Float32 __type; };
#endif
#ifdef __STDCPP_FLOAT64_T__
template<>
struct __promote<_Float64>
{ typedef _Float64 __type; };
#endif
#ifdef __STDCPP_FLOAT128_T__
template<>
struct __promote<_Float128>
{ typedef _Float128 __type; };
#endif
#ifdef __STDCPP_BFLOAT16_T__
template<>
struct __promote<__gnu_cxx::__bfloat16_t>
{ typedef __gnu_cxx::__bfloat16_t __type; };
#endif
#if __cpp_fold_expressions
template<typename... _Tp>
using __promoted_t = decltype((typename __promote<_Tp>::__type(0) + ...));
template<typename _Tp, typename _Up>
using __promote_2 = __promote<__promoted_t<_Tp, _Up>>;
template<typename _Tp, typename _Up, typename _Vp>
using __promote_3 = __promote<__promoted_t<_Tp, _Up, _Vp>>;
template<typename _Tp, typename _Up, typename _Vp, typename _Wp>
using __promote_4 = __promote<__promoted_t<_Tp, _Up, _Vp, _Wp>>;
#else
template<typename _Tp, typename _Up,
typename _Tp2 = typename __promote<_Tp>::__type,
typename _Up2 = typename __promote<_Up>::__type>
struct __promote_2
{
typedef __typeof__(_Tp2() + _Up2()) __type;
};
template<typename _Tp, typename _Up, typename _Vp,
typename _Tp2 = typename __promote<_Tp>::__type,
typename _Up2 = typename __promote<_Up>::__type,
typename _Vp2 = typename __promote<_Vp>::__type>
struct __promote_3
{
typedef __typeof__(_Tp2() + _Up2() + _Vp2()) __type;
};
template<typename _Tp, typename _Up, typename _Vp, typename _Wp,
typename _Tp2 = typename __promote<_Tp>::__type,
typename _Up2 = typename __promote<_Up>::__type,
typename _Vp2 = typename __promote<_Vp>::__type,
typename _Wp2 = typename __promote<_Wp>::__type>
struct __promote_4
{
typedef __typeof__(_Tp2() + _Up2() + _Vp2() + _Wp2()) __type;
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
}
#endif