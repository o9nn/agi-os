#ifndef _BASIC_IOS_H
#define _BASIC_IOS_H 1
#pragma GCC system_header
#include <bits/localefwd.h>
#include <bits/locale_classes.h>
#include <bits/locale_facets.h>
#include <bits/streambuf_iterator.h>
#include <bits/move.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
template<typename _Facet>
inline const _Facet&
__check_facet(const _Facet* __f)
{
if (!__f)
__throw_bad_cast();
return *__f;
}
template<typename _CharT, typename _Traits>
class basic_ios : public ios_base
{
public:
typedef _CharT                                 char_type;
typedef typename _Traits::int_type             int_type;
typedef typename _Traits::pos_type             pos_type;
typedef typename _Traits::off_type             off_type;
typedef _Traits                                traits_type;
typedef ctype<_CharT>                          __ctype_type;
typedef num_put<_CharT, ostreambuf_iterator<_CharT, _Traits> >
__num_put_type;
typedef num_get<_CharT, istreambuf_iterator<_CharT, _Traits> >
__num_get_type;
protected:
basic_ostream<_CharT, _Traits>*                _M_tie;
mutable char_type                              _M_fill;
mutable bool                                   _M_fill_init;
basic_streambuf<_CharT, _Traits>*              _M_streambuf;
const __ctype_type*                            _M_ctype;
const __num_put_type*                          _M_num_put;
const __num_get_type*                          _M_num_get;
public:
#if __cplusplus >= 201103L
explicit operator bool() const
{ return !this->fail(); }
#else
operator void*() const
{ return this->fail() ? 0 : const_cast<basic_ios*>(this); }
#endif
bool
operator!() const
{ return this->fail(); }
iostate
rdstate() const
{ return _M_streambuf_state; }
void
clear(iostate __state = goodbit);
void
setstate(iostate __state)
{ this->clear(this->rdstate() | __state); }
void
_M_setstate(iostate __state)
{
_M_streambuf_state |= __state;
if (this->exceptions() & __state)
__throw_exception_again;
}
bool
good() const
{ return this->rdstate() == 0; }
bool
eof() const
{ return (this->rdstate() & eofbit) != 0; }
bool
fail() const
{ return (this->rdstate() & (badbit | failbit)) != 0; }
bool
bad() const
{ return (this->rdstate() & badbit) != 0; }
iostate
exceptions() const
{ return _M_exception; }
void
exceptions(iostate __except)
{
_M_exception = __except;
this->clear(_M_streambuf_state);
}
explicit
basic_ios(basic_streambuf<_CharT, _Traits>* __sb)
: ios_base(), _M_tie(0), _M_fill(), _M_fill_init(false), _M_streambuf(0),
_M_ctype(0), _M_num_put(0), _M_num_get(0)
{ this->init(__sb); }
virtual
~basic_ios() { }
basic_ostream<_CharT, _Traits>*
tie() const
{ return _M_tie; }
basic_ostream<_CharT, _Traits>*
tie(basic_ostream<_CharT, _Traits>* __tiestr)
{
basic_ostream<_CharT, _Traits>* __old = _M_tie;
_M_tie = __tiestr;
return __old;
}
basic_streambuf<_CharT, _Traits>*
rdbuf() const
{ return _M_streambuf; }
basic_streambuf<_CharT, _Traits>*
rdbuf(basic_streambuf<_CharT, _Traits>* __sb);
basic_ios&
copyfmt(const basic_ios& __rhs);
char_type
fill() const
{
if (!_M_fill_init)
{
_M_fill = this->widen(' ');
_M_fill_init = true;
}
return _M_fill;
}
char_type
fill(char_type __ch)
{
char_type __old = this->fill();
_M_fill = __ch;
return __old;
}
locale
imbue(const locale& __loc);
char
narrow(char_type __c, char __dfault) const
{ return __check_facet(_M_ctype).narrow(__c, __dfault); }
char_type
widen(char __c) const
{ return __check_facet(_M_ctype).widen(__c); }
protected:
basic_ios()
: ios_base(), _M_tie(0), _M_fill(char_type()), _M_fill_init(false),
_M_streambuf(0), _M_ctype(0), _M_num_put(0), _M_num_get(0)
{ }
void
init(basic_streambuf<_CharT, _Traits>* __sb);
#if __cplusplus >= 201103L
basic_ios(const basic_ios&) = delete;
basic_ios& operator=(const basic_ios&) = delete;
void
move(basic_ios& __rhs)
{
ios_base::_M_move(__rhs);
_M_cache_locale(_M_ios_locale);
this->tie(__rhs.tie(nullptr));
_M_fill = __rhs._M_fill;
_M_fill_init = __rhs._M_fill_init;
_M_streambuf = nullptr;
}
void
move(basic_ios&& __rhs)
{ this->move(__rhs); }
void
swap(basic_ios& __rhs) noexcept
{
ios_base::_M_swap(__rhs);
_M_cache_locale(_M_ios_locale);
__rhs._M_cache_locale(__rhs._M_ios_locale);
std::swap(_M_tie, __rhs._M_tie);
std::swap(_M_fill, __rhs._M_fill);
std::swap(_M_fill_init, __rhs._M_fill_init);
}
void
set_rdbuf(basic_streambuf<_CharT, _Traits>* __sb)
{ _M_streambuf = __sb; }
#endif
void
_M_cache_locale(const locale& __loc);
};
_GLIBCXX_END_NAMESPACE_VERSION
}
#include <bits/basic_ios.tcc>
#endif