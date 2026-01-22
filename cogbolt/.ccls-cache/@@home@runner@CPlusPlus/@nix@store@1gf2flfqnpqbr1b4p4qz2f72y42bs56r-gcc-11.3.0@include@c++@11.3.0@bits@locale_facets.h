#ifndef _LOCALE_FACETS_H
#define _LOCALE_FACETS_H 1
#pragma GCC system_header
#include <cwctype>
#include <cctype>
#include <bits/ctype_base.h>
#include <iosfwd>
#include <bits/ios_base.h>
#include <streambuf>
#include <bits/cpp_type_traits.h>
#include <ext/type_traits.h>
#include <ext/numeric_traits.h>
#include <bits/streambuf_iterator.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#define _GLIBCXX_NUM_FACETS 14
#define _GLIBCXX_NUM_CXX11_FACETS (_GLIBCXX_USE_DUAL_ABI ? 8 : 0)
#ifdef _GLIBCXX_USE_CHAR8_T
# define _GLIBCXX_NUM_UNICODE_FACETS 4
#else
# define _GLIBCXX_NUM_UNICODE_FACETS 2
#endif
#define _GLIBCXX_NUM_LBDL_ALT128_FACETS (4 + (_GLIBCXX_USE_DUAL_ABI ? 2 : 0))
template<typename _Tp>
void
__convert_to_v(const char*, _Tp&, ios_base::iostate&,
const __c_locale&) throw();
template<>
void
__convert_to_v(const char*, float&, ios_base::iostate&,
const __c_locale&) throw();
template<>
void
__convert_to_v(const char*, double&, ios_base::iostate&,
const __c_locale&) throw();
template<>
void
__convert_to_v(const char*, long double&, ios_base::iostate&,
const __c_locale&) throw();
template<typename _CharT, typename _Traits>
struct __pad
{
static void
_S_pad(ios_base& __io, _CharT __fill, _CharT* __news,
const _CharT* __olds, streamsize __newlen, streamsize __oldlen);
};
template<typename _CharT>
_CharT*
__add_grouping(_CharT* __s, _CharT __sep,
const char* __gbeg, size_t __gsize,
const _CharT* __first, const _CharT* __last);
template<typename _CharT>
inline
ostreambuf_iterator<_CharT>
__write(ostreambuf_iterator<_CharT> __s, const _CharT* __ws, int __len)
{
__s._M_put(__ws, __len);
return __s;
}
template<typename _CharT, typename _OutIter>
inline
_OutIter
__write(_OutIter __s, const _CharT* __ws, int __len)
{
for (int __j = 0; __j < __len; __j++, ++__s)
*__s = __ws[__j];
return __s;
}
template<typename _CharT>
class __ctype_abstract_base : public locale::facet, public ctype_base
{
public:
typedef _CharT char_type;
bool
is(mask __m, char_type __c) const
{ return this->do_is(__m, __c); }
const char_type*
is(const char_type *__lo, const char_type *__hi, mask *__vec) const
{ return this->do_is(__lo, __hi, __vec); }
const char_type*
scan_is(mask __m, const char_type* __lo, const char_type* __hi) const
{ return this->do_scan_is(__m, __lo, __hi); }
const char_type*
scan_not(mask __m, const char_type* __lo, const char_type* __hi) const
{ return this->do_scan_not(__m, __lo, __hi); }
char_type
toupper(char_type __c) const
{ return this->do_toupper(__c); }
const char_type*
toupper(char_type *__lo, const char_type* __hi) const
{ return this->do_toupper(__lo, __hi); }
char_type
tolower(char_type __c) const
{ return this->do_tolower(__c); }
const char_type*
tolower(char_type* __lo, const char_type* __hi) const
{ return this->do_tolower(__lo, __hi); }
char_type
widen(char __c) const
{ return this->do_widen(__c); }
const char*
widen(const char* __lo, const char* __hi, char_type* __to) const
{ return this->do_widen(__lo, __hi, __to); }
char
narrow(char_type __c, char __dfault) const
{ return this->do_narrow(__c, __dfault); }
const char_type*
narrow(const char_type* __lo, const char_type* __hi,
char __dfault, char* __to) const
{ return this->do_narrow(__lo, __hi, __dfault, __to); }
protected:
explicit
__ctype_abstract_base(size_t __refs = 0): facet(__refs) { }
virtual
~__ctype_abstract_base() { }
virtual bool
do_is(mask __m, char_type __c) const = 0;
virtual const char_type*
do_is(const char_type* __lo, const char_type* __hi,
mask* __vec) const = 0;
virtual const char_type*
do_scan_is(mask __m, const char_type* __lo,
const char_type* __hi) const = 0;
virtual const char_type*
do_scan_not(mask __m, const char_type* __lo,
const char_type* __hi) const = 0;
virtual char_type
do_toupper(char_type __c) const = 0;
virtual const char_type*
do_toupper(char_type* __lo, const char_type* __hi) const = 0;
virtual char_type
do_tolower(char_type __c) const = 0;
virtual const char_type*
do_tolower(char_type* __lo, const char_type* __hi) const = 0;
virtual char_type
do_widen(char __c) const = 0;
virtual const char*
do_widen(const char* __lo, const char* __hi, char_type* __to) const = 0;
virtual char
do_narrow(char_type __c, char __dfault) const = 0;
virtual const char_type*
do_narrow(const char_type* __lo, const char_type* __hi,
char __dfault, char* __to) const = 0;
};
template<typename _CharT>
class ctype : public __ctype_abstract_base<_CharT>
{
public:
typedef _CharT char_type;
typedef typename __ctype_abstract_base<_CharT>::mask mask;
static locale::id id;
explicit
ctype(size_t __refs = 0) : __ctype_abstract_base<_CharT>(__refs) { }
protected:
virtual
~ctype();
virtual bool
do_is(mask __m, char_type __c) const;
virtual const char_type*
do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const;
virtual const char_type*
do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const;
virtual const char_type*
do_scan_not(mask __m, const char_type* __lo,
const char_type* __hi) const;
virtual char_type
do_toupper(char_type __c) const;
virtual const char_type*
do_toupper(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_tolower(char_type __c) const;
virtual const char_type*
do_tolower(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_widen(char __c) const;
virtual const char*
do_widen(const char* __lo, const char* __hi, char_type* __dest) const;
virtual char
do_narrow(char_type, char __dfault) const;
virtual const char_type*
do_narrow(const char_type* __lo, const char_type* __hi,
char __dfault, char* __to) const;
};
template<typename _CharT>
locale::id ctype<_CharT>::id;
template<>
class ctype<char> : public locale::facet, public ctype_base
{
public:
typedef char char_type;
protected:
__c_locale _M_c_locale_ctype;
bool _M_del;
__to_type _M_toupper;
__to_type _M_tolower;
const mask* _M_table;
mutable char _M_widen_ok;
mutable char _M_widen[1 + static_cast<unsigned char>(-1)];
mutable char _M_narrow[1 + static_cast<unsigned char>(-1)];
mutable char _M_narrow_ok;
public:
static locale::id id;
static const size_t table_size = 1 + static_cast<unsigned char>(-1);
explicit
ctype(const mask* __table = 0, bool __del = false, size_t __refs = 0);
explicit
ctype(__c_locale __cloc, const mask* __table = 0, bool __del = false,
size_t __refs = 0);
inline bool
is(mask __m, char __c) const;
inline const char*
is(const char* __lo, const char* __hi, mask* __vec) const;
inline const char*
scan_is(mask __m, const char* __lo, const char* __hi) const;
inline const char*
scan_not(mask __m, const char* __lo, const char* __hi) const;
char_type
toupper(char_type __c) const
{ return this->do_toupper(__c); }
const char_type*
toupper(char_type *__lo, const char_type* __hi) const
{ return this->do_toupper(__lo, __hi); }
char_type
tolower(char_type __c) const
{ return this->do_tolower(__c); }
const char_type*
tolower(char_type* __lo, const char_type* __hi) const
{ return this->do_tolower(__lo, __hi); }
char_type
widen(char __c) const
{
if (_M_widen_ok)
return _M_widen[static_cast<unsigned char>(__c)];
this->_M_widen_init();
return this->do_widen(__c);
}
const char*
widen(const char* __lo, const char* __hi, char_type* __to) const
{
if (_M_widen_ok == 1)
{
if (__builtin_expect(__hi != __lo, true))
__builtin_memcpy(__to, __lo, __hi - __lo);
return __hi;
}
if (!_M_widen_ok)
_M_widen_init();
return this->do_widen(__lo, __hi, __to);
}
char
narrow(char_type __c, char __dfault) const
{
if (_M_narrow[static_cast<unsigned char>(__c)])
return _M_narrow[static_cast<unsigned char>(__c)];
const char __t = do_narrow(__c, __dfault);
if (__t != __dfault)
_M_narrow[static_cast<unsigned char>(__c)] = __t;
return __t;
}
const char_type*
narrow(const char_type* __lo, const char_type* __hi,
char __dfault, char* __to) const
{
if (__builtin_expect(_M_narrow_ok == 1, true))
{
if (__builtin_expect(__hi != __lo, true))
__builtin_memcpy(__to, __lo, __hi - __lo);
return __hi;
}
if (!_M_narrow_ok)
_M_narrow_init();
return this->do_narrow(__lo, __hi, __dfault, __to);
}
const mask*
table() const throw()
{ return _M_table; }
static const mask*
classic_table() throw();
protected:
virtual
~ctype();
virtual char_type
do_toupper(char_type __c) const;
virtual const char_type*
do_toupper(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_tolower(char_type __c) const;
virtual const char_type*
do_tolower(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_widen(char __c) const
{ return __c; }
virtual const char*
do_widen(const char* __lo, const char* __hi, char_type* __to) const
{
if (__builtin_expect(__hi != __lo, true))
__builtin_memcpy(__to, __lo, __hi - __lo);
return __hi;
}
virtual char
do_narrow(char_type __c, char __dfault __attribute__((__unused__))) const
{ return __c; }
virtual const char_type*
do_narrow(const char_type* __lo, const char_type* __hi,
char __dfault __attribute__((__unused__)), char* __to) const
{
if (__builtin_expect(__hi != __lo, true))
__builtin_memcpy(__to, __lo, __hi - __lo);
return __hi;
}
private:
void _M_narrow_init() const;
void _M_widen_init() const;
};
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
class ctype<wchar_t> : public __ctype_abstract_base<wchar_t>
{
public:
typedef wchar_t char_type;
typedef wctype_t __wmask_type;
protected:
__c_locale _M_c_locale_ctype;
bool _M_narrow_ok;
char _M_narrow[128];
wint_t _M_widen[1 + static_cast<unsigned char>(-1)];
mask _M_bit[16];
__wmask_type _M_wmask[16];
public:
static locale::id id;
explicit
ctype(size_t __refs = 0);
explicit
ctype(__c_locale __cloc, size_t __refs = 0);
protected:
__wmask_type
_M_convert_to_wmask(const mask __m) const throw();
virtual
~ctype();
virtual bool
do_is(mask __m, char_type __c) const;
virtual const char_type*
do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const;
virtual const char_type*
do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const;
virtual const char_type*
do_scan_not(mask __m, const char_type* __lo,
const char_type* __hi) const;
virtual char_type
do_toupper(char_type __c) const;
virtual const char_type*
do_toupper(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_tolower(char_type __c) const;
virtual const char_type*
do_tolower(char_type* __lo, const char_type* __hi) const;
virtual char_type
do_widen(char __c) const;
virtual const char*
do_widen(const char* __lo, const char* __hi, char_type* __to) const;
virtual char
do_narrow(char_type __c, char __dfault) const;
virtual const char_type*
do_narrow(const char_type* __lo, const char_type* __hi,
char __dfault, char* __to) const;
void
_M_initialize_ctype() throw();
};
#endif
template<typename _CharT>
class ctype_byname : public ctype<_CharT>
{
public:
typedef typename ctype<_CharT>::mask mask;
explicit
ctype_byname(const char* __s, size_t __refs = 0);
#if __cplusplus >= 201103L
explicit
ctype_byname(const string& __s, size_t __refs = 0)
: ctype_byname(__s.c_str(), __refs) { }
#endif
protected:
virtual
~ctype_byname() { }
};
template<>
class ctype_byname<char> : public ctype<char>
{
public:
explicit
ctype_byname(const char* __s, size_t __refs = 0);
#if __cplusplus >= 201103L
explicit
ctype_byname(const string& __s, size_t __refs = 0);
#endif
protected:
virtual
~ctype_byname();
};
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
class ctype_byname<wchar_t> : public ctype<wchar_t>
{
public:
explicit
ctype_byname(const char* __s, size_t __refs = 0);
#if __cplusplus >= 201103L
explicit
ctype_byname(const string& __s, size_t __refs = 0);
#endif
protected:
virtual
~ctype_byname();
};
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#include <bits/ctype_inline.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class __num_base
{
public:
enum
{
_S_ominus,
_S_oplus,
_S_ox,
_S_oX,
_S_odigits,
_S_odigits_end = _S_odigits + 16,
_S_oudigits = _S_odigits_end,
_S_oudigits_end = _S_oudigits + 16,
_S_oe = _S_odigits + 14,
_S_oE = _S_oudigits + 14,
_S_oend = _S_oudigits_end
};
static const char* _S_atoms_out;
static const char* _S_atoms_in;
enum
{
_S_iminus,
_S_iplus,
_S_ix,
_S_iX,
_S_izero,
_S_ie = _S_izero + 14,
_S_iE = _S_izero + 20,
_S_iend = 26
};
static void
_S_format_float(const ios_base& __io, char* __fptr, char __mod) throw();
};
template<typename _CharT>
struct __numpunct_cache : public locale::facet
{
const char* _M_grouping;
size_t _M_grouping_size;
bool _M_use_grouping;
const _CharT* _M_truename;
size_t _M_truename_size;
const _CharT* _M_falsename;
size_t _M_falsename_size;
_CharT _M_decimal_point;
_CharT _M_thousands_sep;
_CharT _M_atoms_out[__num_base::_S_oend];
_CharT _M_atoms_in[__num_base::_S_iend];
bool _M_allocated;
__numpunct_cache(size_t __refs = 0)
: facet(__refs), _M_grouping(0), _M_grouping_size(0),
_M_use_grouping(false),
_M_truename(0), _M_truename_size(0), _M_falsename(0),
_M_falsename_size(0), _M_decimal_point(_CharT()),
_M_thousands_sep(_CharT()), _M_allocated(false)
{ }
~__numpunct_cache();
void
_M_cache(const locale& __loc);
private:
__numpunct_cache&
operator=(const __numpunct_cache&);
explicit
__numpunct_cache(const __numpunct_cache&);
};
template<typename _CharT>
__numpunct_cache<_CharT>::~__numpunct_cache()
{
if (_M_allocated)
{
delete [] _M_grouping;
delete [] _M_truename;
delete [] _M_falsename;
}
}
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT>
class numpunct : public locale::facet
{
public:
typedef _CharT char_type;
typedef basic_string<_CharT> string_type;
typedef __numpunct_cache<_CharT> __cache_type;
protected:
__cache_type* _M_data;
public:
static locale::id id;
explicit
numpunct(size_t __refs = 0)
: facet(__refs), _M_data(0)
{ _M_initialize_numpunct(); }
explicit
numpunct(__cache_type* __cache, size_t __refs = 0)
: facet(__refs), _M_data(__cache)
{ _M_initialize_numpunct(); }
explicit
numpunct(__c_locale __cloc, size_t __refs = 0)
: facet(__refs), _M_data(0)
{ _M_initialize_numpunct(__cloc); }
char_type
decimal_point() const
{ return this->do_decimal_point(); }
char_type
thousands_sep() const
{ return this->do_thousands_sep(); }
string
grouping() const
{ return this->do_grouping(); }
string_type
truename() const
{ return this->do_truename(); }
string_type
falsename() const
{ return this->do_falsename(); }
protected:
virtual
~numpunct();
virtual char_type
do_decimal_point() const
{ return _M_data->_M_decimal_point; }
virtual char_type
do_thousands_sep() const
{ return _M_data->_M_thousands_sep; }
virtual string
do_grouping() const
{ return _M_data->_M_grouping; }
virtual string_type
do_truename() const
{ return _M_data->_M_truename; }
virtual string_type
do_falsename() const
{ return _M_data->_M_falsename; }
void
_M_initialize_numpunct(__c_locale __cloc = 0);
};
template<typename _CharT>
locale::id numpunct<_CharT>::id;
template<>
numpunct<char>::~numpunct();
template<>
void
numpunct<char>::_M_initialize_numpunct(__c_locale __cloc);
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
numpunct<wchar_t>::~numpunct();
template<>
void
numpunct<wchar_t>::_M_initialize_numpunct(__c_locale __cloc);
#endif
template<typename _CharT>
class numpunct_byname : public numpunct<_CharT>
{
public:
typedef _CharT char_type;
typedef basic_string<_CharT> string_type;
explicit
numpunct_byname(const char* __s, size_t __refs = 0)
: numpunct<_CharT>(__refs)
{
if (__builtin_strcmp(__s, "C") != 0
&& __builtin_strcmp(__s, "POSIX") != 0)
{
__c_locale __tmp;
this->_S_create_c_locale(__tmp, __s);
this->_M_initialize_numpunct(__tmp);
this->_S_destroy_c_locale(__tmp);
}
}
#if __cplusplus >= 201103L
explicit
numpunct_byname(const string& __s, size_t __refs = 0)
: numpunct_byname(__s.c_str(), __refs) { }
#endif
protected:
virtual
~numpunct_byname() { }
};
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_BEGIN_NAMESPACE_LDBL
template<typename _CharT, typename _InIter>
class num_get : public locale::facet
{
public:
typedef _CharT char_type;
typedef _InIter iter_type;
static locale::id id;
explicit
num_get(size_t __refs = 0) : facet(__refs) { }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, bool& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, long& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned short& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned int& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned long& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
#ifdef _GLIBCXX_USE_LONG_LONG
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, long long& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned long long& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
#endif
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, float& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, double& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, long double& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
iter_type
get(iter_type __in, iter_type __end, ios_base& __io,
ios_base::iostate& __err, void*& __v) const
{ return this->do_get(__in, __end, __io, __err, __v); }
protected:
virtual ~num_get() { }
_GLIBCXX_DEFAULT_ABI_TAG
iter_type
_M_extract_float(iter_type, iter_type, ios_base&, ios_base::iostate&,
string&) const;
template<typename _ValueT>
_GLIBCXX_DEFAULT_ABI_TAG
iter_type
_M_extract_int(iter_type, iter_type, ios_base&, ios_base::iostate&,
_ValueT&) const;
template<typename _CharT2>
typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value, int>::__type
_M_find(const _CharT2*, size_t __len, _CharT2 __c) const
{
int __ret = -1;
if (__len <= 10)
{
if (__c >= _CharT2('0') && __c < _CharT2(_CharT2('0') + __len))
__ret = __c - _CharT2('0');
}
else
{
if (__c >= _CharT2('0') && __c <= _CharT2('9'))
__ret = __c - _CharT2('0');
else if (__c >= _CharT2('a') && __c <= _CharT2('f'))
__ret = 10 + (__c - _CharT2('a'));
else if (__c >= _CharT2('A') && __c <= _CharT2('F'))
__ret = 10 + (__c - _CharT2('A'));
}
return __ret;
}
template<typename _CharT2>
typename __gnu_cxx::__enable_if<!__is_char<_CharT2>::__value,
int>::__type
_M_find(const _CharT2* __zero, size_t __len, _CharT2 __c) const
{
int __ret = -1;
const char_type* __q = char_traits<_CharT2>::find(__zero, __len, __c);
if (__q)
{
__ret = __q - __zero;
if (__ret > 15)
__ret -= 6;
}
return __ret;
}
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, bool&) const;
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, long& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned short& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned int& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned long& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
#ifdef _GLIBCXX_USE_LONG_LONG
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, long long& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
virtual iter_type
do_get(iter_type __beg, iter_type __end, ios_base& __io,
ios_base::iostate& __err, unsigned long long& __v) const
{ return _M_extract_int(__beg, __end, __io, __err, __v); }
#endif
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, float&) const;
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
double&) const;
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined __LONG_DOUBLE_128__
virtual iter_type
__do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
double&) const;
#else
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
long double&) const;
#endif
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, void*&) const;
#if defined _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT \
&& defined __LONG_DOUBLE_IEEE128__
virtual iter_type
__do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
__ibm128&) const;
#endif
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined __LONG_DOUBLE_128__
virtual iter_type
do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
long double&) const;
#endif
};
template<typename _CharT, typename _InIter>
locale::id num_get<_CharT, _InIter>::id;
template<typename _CharT, typename _OutIter>
class num_put : public locale::facet
{
public:
typedef _CharT char_type;
typedef _OutIter iter_type;
static locale::id id;
explicit
num_put(size_t __refs = 0) : facet(__refs) { }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill, bool __v) const
{ return this->do_put(__s, __io, __fill, __v); }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill, long __v) const
{ return this->do_put(__s, __io, __fill, __v); }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill,
unsigned long __v) const
{ return this->do_put(__s, __io, __fill, __v); }
#ifdef _GLIBCXX_USE_LONG_LONG
iter_type
put(iter_type __s, ios_base& __io, char_type __fill, long long __v) const
{ return this->do_put(__s, __io, __fill, __v); }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill,
unsigned long long __v) const
{ return this->do_put(__s, __io, __fill, __v); }
#endif
iter_type
put(iter_type __s, ios_base& __io, char_type __fill, double __v) const
{ return this->do_put(__s, __io, __fill, __v); }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill,
long double __v) const
{ return this->do_put(__s, __io, __fill, __v); }
iter_type
put(iter_type __s, ios_base& __io, char_type __fill,
const void* __v) const
{ return this->do_put(__s, __io, __fill, __v); }
protected:
template<typename _ValueT>
iter_type
_M_insert_float(iter_type, ios_base& __io, char_type __fill,
char __mod, _ValueT __v) const;
void
_M_group_float(const char* __grouping, size_t __grouping_size,
char_type __sep, const char_type* __p, char_type* __new,
char_type* __cs, int& __len) const;
template<typename _ValueT>
iter_type
_M_insert_int(iter_type, ios_base& __io, char_type __fill,
_ValueT __v) const;
void
_M_group_int(const char* __grouping, size_t __grouping_size,
char_type __sep, ios_base& __io, char_type* __new,
char_type* __cs, int& __len) const;
void
_M_pad(char_type __fill, streamsize __w, ios_base& __io,
char_type* __new, const char_type* __cs, int& __len) const;
virtual
~num_put() { }
virtual iter_type
do_put(iter_type __s, ios_base& __io, char_type __fill, bool __v) const;
virtual iter_type
do_put(iter_type __s, ios_base& __io, char_type __fill, long __v) const
{ return _M_insert_int(__s, __io, __fill, __v); }
virtual iter_type
do_put(iter_type __s, ios_base& __io, char_type __fill,
unsigned long __v) const
{ return _M_insert_int(__s, __io, __fill, __v); }
#ifdef _GLIBCXX_USE_LONG_LONG
virtual iter_type
do_put(iter_type __s, ios_base& __io, char_type __fill,
long long __v) const
{ return _M_insert_int(__s, __io, __fill, __v); }
virtual iter_type
do_put(iter_type __s, ios_base& __io, char_type __fill,
unsigned long long __v) const
{ return _M_insert_int(__s, __io, __fill, __v); }
#endif
virtual iter_type
do_put(iter_type, ios_base&, char_type, double) const;
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined __LONG_DOUBLE_128__
virtual iter_type
__do_put(iter_type, ios_base&, char_type, double) const;
#else
virtual iter_type
do_put(iter_type, ios_base&, char_type, long double) const;
#endif
virtual iter_type
do_put(iter_type, ios_base&, char_type, const void*) const;
#if defined _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT \
&& defined __LONG_DOUBLE_IEEE128__
virtual iter_type
__do_put(iter_type, ios_base&, char_type, __ibm128) const;
#endif
#if defined _GLIBCXX_LONG_DOUBLE_COMPAT && defined __LONG_DOUBLE_128__
virtual iter_type
do_put(iter_type, ios_base&, char_type, long double) const;
#endif
};
template <typename _CharT, typename _OutIter>
locale::id num_put<_CharT, _OutIter>::id;
_GLIBCXX_END_NAMESPACE_LDBL
template<typename _CharT>
inline bool
isspace(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::space, __c); }
template<typename _CharT>
inline bool
isprint(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::print, __c); }
template<typename _CharT>
inline bool
iscntrl(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::cntrl, __c); }
template<typename _CharT>
inline bool
isupper(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::upper, __c); }
template<typename _CharT>
inline bool
islower(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::lower, __c); }
template<typename _CharT>
inline bool
isalpha(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::alpha, __c); }
template<typename _CharT>
inline bool
isdigit(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::digit, __c); }
template<typename _CharT>
inline bool
ispunct(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::punct, __c); }
template<typename _CharT>
inline bool
isxdigit(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::xdigit, __c); }
template<typename _CharT>
inline bool
isalnum(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::alnum, __c); }
template<typename _CharT>
inline bool
isgraph(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::graph, __c); }
#if __cplusplus >= 201103L
template<typename _CharT>
inline bool
isblank(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).is(ctype_base::blank, __c); }
#endif
template<typename _CharT>
inline _CharT
toupper(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).toupper(__c); }
template<typename _CharT>
inline _CharT
tolower(_CharT __c, const locale& __loc)
{ return use_facet<ctype<_CharT> >(__loc).tolower(__c); }
_GLIBCXX_END_NAMESPACE_VERSION
}
# include <bits/locale_facets.tcc>
#endif