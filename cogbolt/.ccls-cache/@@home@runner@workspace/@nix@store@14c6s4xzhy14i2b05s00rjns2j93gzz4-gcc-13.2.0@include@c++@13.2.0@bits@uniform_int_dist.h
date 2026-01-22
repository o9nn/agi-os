#ifndef _GLIBCXX_BITS_UNIFORM_INT_DIST_H
#define _GLIBCXX_BITS_UNIFORM_INT_DIST_H
#include <type_traits>
#include <ext/numeric_traits.h>
#if __cplusplus > 201703L
# include <concepts>
#endif
#include <bits/concept_check.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#ifdef __cpp_lib_concepts
template<typename _Gen>
concept uniform_random_bit_generator
= invocable<_Gen&> && unsigned_integral<invoke_result_t<_Gen&>>
&& requires
{
{ _Gen::min() } -> same_as<invoke_result_t<_Gen&>>;
{ _Gen::max() } -> same_as<invoke_result_t<_Gen&>>;
requires bool_constant<(_Gen::min() < _Gen::max())>::value;
};
#endif
namespace __detail
{
template<typename _Tp>
constexpr bool
_Power_of_2(_Tp __x)
{
return ((__x - 1) & __x) == 0;
}
}
template<typename _IntType = int>
class uniform_int_distribution
{
static_assert(std::is_integral<_IntType>::value,
"template argument must be an integral type");
public:
typedef _IntType result_type;
struct param_type
{
typedef uniform_int_distribution<_IntType> distribution_type;
param_type() : param_type(0) { }
explicit
param_type(_IntType __a,
_IntType __b = __gnu_cxx::__int_traits<_IntType>::__max)
: _M_a(__a), _M_b(__b)
{
__glibcxx_assert(_M_a <= _M_b);
}
result_type
a() const
{ return _M_a; }
result_type
b() const
{ return _M_b; }
friend bool
operator==(const param_type& __p1, const param_type& __p2)
{ return __p1._M_a == __p2._M_a && __p1._M_b == __p2._M_b; }
friend bool
operator!=(const param_type& __p1, const param_type& __p2)
{ return !(__p1 == __p2); }
private:
_IntType _M_a;
_IntType _M_b;
};
public:
uniform_int_distribution() : uniform_int_distribution(0) { }
explicit
uniform_int_distribution(_IntType __a,
_IntType __b
= __gnu_cxx::__int_traits<_IntType>::__max)
: _M_param(__a, __b)
{ }
explicit
uniform_int_distribution(const param_type& __p)
: _M_param(__p)
{ }
void
reset() { }
result_type
a() const
{ return _M_param.a(); }
result_type
b() const
{ return _M_param.b(); }
param_type
param() const
{ return _M_param; }
void
param(const param_type& __param)
{ _M_param = __param; }
result_type
min() const
{ return this->a(); }
result_type
max() const
{ return this->b(); }
template<typename _UniformRandomBitGenerator>
result_type
operator()(_UniformRandomBitGenerator& __urng)
{ return this->operator()(__urng, _M_param); }
template<typename _UniformRandomBitGenerator>
result_type
operator()(_UniformRandomBitGenerator& __urng,
const param_type& __p);
template<typename _ForwardIterator,
typename _UniformRandomBitGenerator>
void
__generate(_ForwardIterator __f, _ForwardIterator __t,
_UniformRandomBitGenerator& __urng)
{ this->__generate(__f, __t, __urng, _M_param); }
template<typename _ForwardIterator,
typename _UniformRandomBitGenerator>
void
__generate(_ForwardIterator __f, _ForwardIterator __t,
_UniformRandomBitGenerator& __urng,
const param_type& __p)
{ this->__generate_impl(__f, __t, __urng, __p); }
template<typename _UniformRandomBitGenerator>
void
__generate(result_type* __f, result_type* __t,
_UniformRandomBitGenerator& __urng,
const param_type& __p)
{ this->__generate_impl(__f, __t, __urng, __p); }
friend bool
operator==(const uniform_int_distribution& __d1,
const uniform_int_distribution& __d2)
{ return __d1._M_param == __d2._M_param; }
private:
template<typename _ForwardIterator,
typename _UniformRandomBitGenerator>
void
__generate_impl(_ForwardIterator __f, _ForwardIterator __t,
_UniformRandomBitGenerator& __urng,
const param_type& __p);
param_type _M_param;
template<typename _Wp, typename _Urbg, typename _Up>
static _Up
_S_nd(_Urbg& __g, _Up __range)
{
using _Up_traits = __gnu_cxx::__int_traits<_Up>;
using _Wp_traits = __gnu_cxx::__int_traits<_Wp>;
static_assert(!_Up_traits::__is_signed, "U must be unsigned");
static_assert(!_Wp_traits::__is_signed, "W must be unsigned");
static_assert(_Wp_traits::__digits == (2 * _Up_traits::__digits),
"W must be twice as wide as U");
_Wp __product = _Wp(__g()) * _Wp(__range);
_Up __low = _Up(__product);
if (__low < __range)
{
_Up __threshold = -__range % __range;
while (__low < __threshold)
{
__product = _Wp(__g()) * _Wp(__range);
__low = _Up(__product);
}
}
return __product >> _Up_traits::__digits;
}
};
template<typename _IntType>
template<typename _UniformRandomBitGenerator>
typename uniform_int_distribution<_IntType>::result_type
uniform_int_distribution<_IntType>::
operator()(_UniformRandomBitGenerator& __urng,
const param_type& __param)
{
typedef typename _UniformRandomBitGenerator::result_type _Gresult_type;
typedef typename make_unsigned<result_type>::type __utype;
typedef typename common_type<_Gresult_type, __utype>::type __uctype;
constexpr __uctype __urngmin = _UniformRandomBitGenerator::min();
constexpr __uctype __urngmax = _UniformRandomBitGenerator::max();
static_assert( __urngmin < __urngmax,
"Uniform random bit generator must define min() < max()");
constexpr __uctype __urngrange = __urngmax - __urngmin;
const __uctype __urange
= __uctype(__param.b()) - __uctype(__param.a());
__uctype __ret;
if (__urngrange > __urange)
{
const __uctype __uerange = __urange + 1;
#if defined __UINT64_TYPE__ && defined __UINT32_TYPE__
#if __SIZEOF_INT128__
if _GLIBCXX17_CONSTEXPR (__urngrange == __UINT64_MAX__)
{
__UINT64_TYPE__ __u64erange = __uerange;
__ret = __extension__ _S_nd<unsigned __int128>(__urng,
__u64erange);
}
else
#endif
if _GLIBCXX17_CONSTEXPR (__urngrange == __UINT32_MAX__)
{
__UINT32_TYPE__ __u32erange = __uerange;
__ret = _S_nd<__UINT64_TYPE__>(__urng, __u32erange);
}
else
#endif
{
const __uctype __scaling = __urngrange / __uerange;
const __uctype __past = __uerange * __scaling;
do
__ret = __uctype(__urng()) - __urngmin;
while (__ret >= __past);
__ret /= __scaling;
}
}
else if (__urngrange < __urange)
{
__uctype __tmp;
do
{
const __uctype __uerngrange = __urngrange + 1;
__tmp = (__uerngrange * operator()
(__urng, param_type(0, __urange / __uerngrange)));
__ret = __tmp + (__uctype(__urng()) - __urngmin);
}
while (__ret > __urange || __ret < __tmp);
}
else
__ret = __uctype(__urng()) - __urngmin;
return __ret + __param.a();
}
template<typename _IntType>
template<typename _ForwardIterator,
typename _UniformRandomBitGenerator>
void
uniform_int_distribution<_IntType>::
__generate_impl(_ForwardIterator __f, _ForwardIterator __t,
_UniformRandomBitGenerator& __urng,
const param_type& __param)
{
__glibcxx_function_requires(_ForwardIteratorConcept<_ForwardIterator>)
typedef typename _UniformRandomBitGenerator::result_type _Gresult_type;
typedef typename make_unsigned<result_type>::type __utype;
typedef typename common_type<_Gresult_type, __utype>::type __uctype;
static_assert( __urng.min() < __urng.max(),
"Uniform random bit generator must define min() < max()");
constexpr __uctype __urngmin = __urng.min();
constexpr __uctype __urngmax = __urng.max();
constexpr __uctype __urngrange = __urngmax - __urngmin;
const __uctype __urange
= __uctype(__param.b()) - __uctype(__param.a());
__uctype __ret;
if (__urngrange > __urange)
{
if (__detail::_Power_of_2(__urngrange + 1)
&& __detail::_Power_of_2(__urange + 1))
{
while (__f != __t)
{
__ret = __uctype(__urng()) - __urngmin;
*__f++ = (__ret & __urange) + __param.a();
}
}
else
{
const __uctype __uerange = __urange + 1;
const __uctype __scaling = __urngrange / __uerange;
const __uctype __past = __uerange * __scaling;
while (__f != __t)
{
do
__ret = __uctype(__urng()) - __urngmin;
while (__ret >= __past);
*__f++ = __ret / __scaling + __param.a();
}
}
}
else if (__urngrange < __urange)
{
__uctype __tmp;
while (__f != __t)
{
do
{
constexpr __uctype __uerngrange = __urngrange + 1;
__tmp = (__uerngrange * operator()
(__urng, param_type(0, __urange / __uerngrange)));
__ret = __tmp + (__uctype(__urng()) - __urngmin);
}
while (__ret > __urange || __ret < __tmp);
*__f++ = __ret;
}
}
else
while (__f != __t)
*__f++ = __uctype(__urng()) - __urngmin + __param.a();
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif