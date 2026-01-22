#ifndef _STREAMBUF_ITERATOR_H
#define _STREAMBUF_ITERATOR_H 1
#pragma GCC system_header
#include <streambuf>
#include <bits/stl_iterator_base_types.h>
#include <debug/debug.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
template<typename _CharT, typename _Traits>
class istreambuf_iterator
: public iterator<input_iterator_tag, _CharT, typename _Traits::off_type,
_CharT*, _CharT>
{
public:
#if __cplusplus < 201103L
typedef _CharT& reference;
#elif __cplusplus > 201703L
using pointer = void;
#endif
typedef _CharT char_type;
typedef _Traits traits_type;
typedef typename _Traits::int_type int_type;
typedef basic_streambuf<_CharT, _Traits> streambuf_type;
typedef basic_istream<_CharT, _Traits> istream_type;
template<typename _CharT2>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
ostreambuf_iterator<_CharT2> >::__type
copy(istreambuf_iterator<_CharT2>, istreambuf_iterator<_CharT2>,
ostreambuf_iterator<_CharT2>);
template<bool _IsMove, typename _CharT2>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
_CharT2*>::__type
__copy_move_a2(istreambuf_iterator<_CharT2>,
istreambuf_iterator<_CharT2>, _CharT2*);
template<typename _CharT2, typename _Size>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
_CharT2*>::__type
__copy_n_a(istreambuf_iterator<_CharT2>, _Size, _CharT2*, bool);
template<typename _CharT2>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
istreambuf_iterator<_CharT2> >::__type
find(istreambuf_iterator<_CharT2>, istreambuf_iterator<_CharT2>,
const _CharT2&);
template<typename _CharT2, typename _Distance>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
void>::__type
advance(istreambuf_iterator<_CharT2>&, _Distance);
private:
mutable streambuf_type* _M_sbuf;
int_type _M_c;
public:
_GLIBCXX_CONSTEXPR istreambuf_iterator() _GLIBCXX_USE_NOEXCEPT
: _M_sbuf(0), _M_c(traits_type::eof()) { }
#if __cplusplus > 201703L && __cpp_lib_concepts
constexpr istreambuf_iterator(default_sentinel_t) noexcept
: istreambuf_iterator() { }
#endif
#if __cplusplus >= 201103L
istreambuf_iterator(const istreambuf_iterator&) noexcept = default;
~istreambuf_iterator() = default;
#endif
istreambuf_iterator(istream_type& __s) _GLIBCXX_USE_NOEXCEPT
: _M_sbuf(__s.rdbuf()), _M_c(traits_type::eof()) { }
istreambuf_iterator(streambuf_type* __s) _GLIBCXX_USE_NOEXCEPT
: _M_sbuf(__s), _M_c(traits_type::eof()) { }
#if __cplusplus >= 201103L
istreambuf_iterator&
operator=(const istreambuf_iterator&) noexcept = default;
#endif
_GLIBCXX_NODISCARD
char_type
operator*() const
{
int_type __c = _M_get();
#ifdef _GLIBCXX_DEBUG_PEDANTIC
__glibcxx_requires_cond(!_S_is_eof(__c),
_M_message(__gnu_debug::__msg_deref_istreambuf)
._M_iterator(*this));
#endif
return traits_type::to_char_type(__c);
}
istreambuf_iterator&
operator++()
{
__glibcxx_requires_cond(_M_sbuf &&
(!_S_is_eof(_M_c) || !_S_is_eof(_M_sbuf->sgetc())),
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(*this));
_M_sbuf->sbumpc();
_M_c = traits_type::eof();
return *this;
}
istreambuf_iterator
operator++(int)
{
__glibcxx_requires_cond(_M_sbuf &&
(!_S_is_eof(_M_c) || !_S_is_eof(_M_sbuf->sgetc())),
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(*this));
istreambuf_iterator __old = *this;
__old._M_c = _M_sbuf->sbumpc();
_M_c = traits_type::eof();
return __old;
}
_GLIBCXX_NODISCARD
bool
equal(const istreambuf_iterator& __b) const
{ return _M_at_eof() == __b._M_at_eof(); }
private:
int_type
_M_get() const
{
int_type __ret = _M_c;
if (_M_sbuf && _S_is_eof(__ret) && _S_is_eof(__ret = _M_sbuf->sgetc()))
_M_sbuf = 0;
return __ret;
}
bool
_M_at_eof() const
{ return _S_is_eof(_M_get()); }
static bool
_S_is_eof(int_type __c)
{
const int_type __eof = traits_type::eof();
return traits_type::eq_int_type(__c, __eof);
}
#if __cplusplus > 201703L && __cpp_lib_concepts
[[nodiscard]]
friend bool
operator==(const istreambuf_iterator& __i, default_sentinel_t __s)
{ return __i._M_at_eof(); }
#endif
};
template<typename _CharT, typename _Traits>
_GLIBCXX_NODISCARD
inline bool
operator==(const istreambuf_iterator<_CharT, _Traits>& __a,
const istreambuf_iterator<_CharT, _Traits>& __b)
{ return __a.equal(__b); }
#if __cpp_impl_three_way_comparison < 201907L
template<typename _CharT, typename _Traits>
_GLIBCXX_NODISCARD
inline bool
operator!=(const istreambuf_iterator<_CharT, _Traits>& __a,
const istreambuf_iterator<_CharT, _Traits>& __b)
{ return !__a.equal(__b); }
#endif
template<typename _CharT, typename _Traits>
class ostreambuf_iterator
: public iterator<output_iterator_tag, void, void, void, void>
{
public:
#if __cplusplus > 201703L
using difference_type = ptrdiff_t;
#endif
typedef _CharT char_type;
typedef _Traits traits_type;
typedef basic_streambuf<_CharT, _Traits> streambuf_type;
typedef basic_ostream<_CharT, _Traits> ostream_type;
template<typename _CharT2>
friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
ostreambuf_iterator<_CharT2> >::__type
copy(istreambuf_iterator<_CharT2>, istreambuf_iterator<_CharT2>,
ostreambuf_iterator<_CharT2>);
private:
streambuf_type* _M_sbuf;
bool _M_failed;
public:
#if __cplusplus > 201703L
constexpr
ostreambuf_iterator() noexcept
: _M_sbuf(nullptr), _M_failed(true) { }
#endif
ostreambuf_iterator(ostream_type& __s) _GLIBCXX_USE_NOEXCEPT
: _M_sbuf(__s.rdbuf()), _M_failed(!_M_sbuf) { }
ostreambuf_iterator(streambuf_type* __s) _GLIBCXX_USE_NOEXCEPT
: _M_sbuf(__s), _M_failed(!_M_sbuf) { }
ostreambuf_iterator&
operator=(_CharT __c)
{
if (!_M_failed &&
_Traits::eq_int_type(_M_sbuf->sputc(__c), _Traits::eof()))
_M_failed = true;
return *this;
}
_GLIBCXX_NODISCARD
ostreambuf_iterator&
operator*()
{ return *this; }
ostreambuf_iterator&
operator++(int)
{ return *this; }
ostreambuf_iterator&
operator++()
{ return *this; }
_GLIBCXX_NODISCARD
bool
failed() const _GLIBCXX_USE_NOEXCEPT
{ return _M_failed; }
ostreambuf_iterator&
_M_put(const _CharT* __ws, streamsize __len)
{
if (__builtin_expect(!_M_failed, true)
&& __builtin_expect(this->_M_sbuf->sputn(__ws, __len) != __len,
false))
_M_failed = true;
return *this;
}
};
#pragma GCC diagnostic pop
template<typename _CharT>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
ostreambuf_iterator<_CharT> >::__type
copy(istreambuf_iterator<_CharT> __first,
istreambuf_iterator<_CharT> __last,
ostreambuf_iterator<_CharT> __result)
{
if (__first._M_sbuf && !__last._M_sbuf && !__result._M_failed)
{
bool __ineof;
__copy_streambufs_eof(__first._M_sbuf, __result._M_sbuf, __ineof);
if (!__ineof)
__result._M_failed = true;
}
return __result;
}
template<bool _IsMove, typename _CharT>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
ostreambuf_iterator<_CharT> >::__type
__copy_move_a2(_CharT* __first, _CharT* __last,
ostreambuf_iterator<_CharT> __result)
{
const streamsize __num = __last - __first;
if (__num > 0)
__result._M_put(__first, __num);
return __result;
}
template<bool _IsMove, typename _CharT>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
ostreambuf_iterator<_CharT> >::__type
__copy_move_a2(const _CharT* __first, const _CharT* __last,
ostreambuf_iterator<_CharT> __result)
{
const streamsize __num = __last - __first;
if (__num > 0)
__result._M_put(__first, __num);
return __result;
}
template<bool _IsMove, typename _CharT>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
_CharT*>::__type
__copy_move_a2(istreambuf_iterator<_CharT> __first,
istreambuf_iterator<_CharT> __last, _CharT* __result)
{
typedef istreambuf_iterator<_CharT> __is_iterator_type;
typedef typename __is_iterator_type::traits_type traits_type;
typedef typename __is_iterator_type::streambuf_type streambuf_type;
typedef typename traits_type::int_type int_type;
if (__first._M_sbuf && !__last._M_sbuf)
{
streambuf_type* __sb = __first._M_sbuf;
int_type __c = __sb->sgetc();
while (!traits_type::eq_int_type(__c, traits_type::eof()))
{
const streamsize __n = __sb->egptr() - __sb->gptr();
if (__n > 1)
{
traits_type::copy(__result, __sb->gptr(), __n);
__sb->__safe_gbump(__n);
__result += __n;
__c = __sb->underflow();
}
else
{
*__result++ = traits_type::to_char_type(__c);
__c = __sb->snextc();
}
}
}
return __result;
}
template<typename _CharT, typename _Size>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
_CharT*>::__type
__copy_n_a(istreambuf_iterator<_CharT> __it, _Size __n, _CharT* __result,
bool __strict __attribute__((__unused__)))
{
if (__n == 0)
return __result;
__glibcxx_requires_cond(__it._M_sbuf,
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(__it));
_CharT* __beg = __result;
__result += __it._M_sbuf->sgetn(__beg, __n);
__glibcxx_requires_cond(!__strict || __result - __beg == __n,
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(__it));
return __result;
}
template<typename _CharT>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
istreambuf_iterator<_CharT> >::__type
find(istreambuf_iterator<_CharT> __first,
istreambuf_iterator<_CharT> __last, const _CharT& __val)
{
typedef istreambuf_iterator<_CharT> __is_iterator_type;
typedef typename __is_iterator_type::traits_type traits_type;
typedef typename __is_iterator_type::streambuf_type streambuf_type;
typedef typename traits_type::int_type int_type;
const int_type __eof = traits_type::eof();
if (__first._M_sbuf && !__last._M_sbuf)
{
const int_type __ival = traits_type::to_int_type(__val);
streambuf_type* __sb = __first._M_sbuf;
int_type __c = __sb->sgetc();
while (!traits_type::eq_int_type(__c, __eof)
&& !traits_type::eq_int_type(__c, __ival))
{
streamsize __n = __sb->egptr() - __sb->gptr();
if (__n > 1)
{
const _CharT* __p = traits_type::find(__sb->gptr(),
__n, __val);
if (__p)
__n = __p - __sb->gptr();
__sb->__safe_gbump(__n);
__c = __sb->sgetc();
}
else
__c = __sb->snextc();
}
__first._M_c = __eof;
}
return __first;
}
template<typename _CharT, typename _Distance>
typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
void>::__type
advance(istreambuf_iterator<_CharT>& __i, _Distance __n)
{
if (__n == 0)
return;
__glibcxx_assert(__n > 0);
__glibcxx_requires_cond(!__i._M_at_eof(),
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(__i));
typedef istreambuf_iterator<_CharT> __is_iterator_type;
typedef typename __is_iterator_type::traits_type traits_type;
typedef typename __is_iterator_type::streambuf_type streambuf_type;
typedef typename traits_type::int_type int_type;
const int_type __eof = traits_type::eof();
streambuf_type* __sb = __i._M_sbuf;
while (__n > 0)
{
streamsize __size = __sb->egptr() - __sb->gptr();
if (__size > __n)
{
__sb->__safe_gbump(__n);
break;
}
__sb->__safe_gbump(__size);
__n -= __size;
if (traits_type::eq_int_type(__sb->underflow(), __eof))
{
__glibcxx_requires_cond(__n == 0,
_M_message(__gnu_debug::__msg_inc_istreambuf)
._M_iterator(__i));
break;
}
}
__i._M_c = __eof;
}
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif