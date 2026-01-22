#ifndef _CODECVT_H
#define _CODECVT_H 1
#pragma GCC system_header
#include <bits/c++config.h>
#include <bits/locale_classes.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class codecvt_base
{
public:
enum result
{
ok,
partial,
error,
noconv
};
};
template<typename _InternT, typename _ExternT, typename _StateT>
class __codecvt_abstract_base
: public locale::facet, public codecvt_base
{
public:
typedef codecvt_base::result	result;
typedef _InternT			intern_type;
typedef _ExternT			extern_type;
typedef _StateT			state_type;
result
out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const
{
return this->do_out(__state, __from, __from_end, __from_next,
__to, __to_end, __to_next);
}
result
unshift(state_type& __state, extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const
{ return this->do_unshift(__state, __to,__to_end,__to_next); }
result
in(state_type& __state, const extern_type* __from,
const extern_type* __from_end, const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const
{
return this->do_in(__state, __from, __from_end, __from_next,
__to, __to_end, __to_next);
}
int
encoding() const throw()
{ return this->do_encoding(); }
bool
always_noconv() const throw()
{ return this->do_always_noconv(); }
int
length(state_type& __state, const extern_type* __from,
const extern_type* __end, size_t __max) const
{ return this->do_length(__state, __from, __end, __max); }
int
max_length() const throw()
{ return this->do_max_length(); }
protected:
explicit
__codecvt_abstract_base(size_t __refs = 0) : locale::facet(__refs) { }
virtual
~__codecvt_abstract_base() { }
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const = 0;
virtual result
do_unshift(state_type& __state, extern_type* __to,
extern_type* __to_end, extern_type*& __to_next) const = 0;
virtual result
do_in(state_type& __state, const extern_type* __from,
const extern_type* __from_end, const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const = 0;
virtual int
do_encoding() const throw() = 0;
virtual bool
do_always_noconv() const throw() = 0;
virtual int
do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const = 0;
virtual int
do_max_length() const throw() = 0;
};
template<typename _InternT, typename _ExternT, typename _StateT>
class codecvt
: public __codecvt_abstract_base<_InternT, _ExternT, _StateT>
{
public:
typedef codecvt_base::result	result;
typedef _InternT			intern_type;
typedef _ExternT			extern_type;
typedef _StateT			state_type;
protected:
__c_locale			_M_c_locale_codecvt;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0)
: __codecvt_abstract_base<_InternT, _ExternT, _StateT> (__refs),
_M_c_locale_codecvt(0)
{ }
explicit
codecvt(__c_locale __cloc, size_t __refs = 0);
protected:
virtual
~codecvt() { }
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state, extern_type* __to,
extern_type* __to_end, extern_type*& __to_next) const;
virtual result
do_in(state_type& __state, const extern_type* __from,
const extern_type* __from_end, const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual int
do_encoding() const throw();
virtual bool
do_always_noconv() const throw();
virtual int
do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
template<typename _InternT, typename _ExternT, typename _StateT>
locale::id codecvt<_InternT, _ExternT, _StateT>::id;
template<>
class codecvt<char, char, mbstate_t>
: public __codecvt_abstract_base<char, char, mbstate_t>
{
friend class messages<char>;
public:
typedef char			intern_type;
typedef char			extern_type;
typedef mbstate_t			state_type;
protected:
__c_locale			_M_c_locale_codecvt;
public:
static locale::id id;
explicit
codecvt(size_t __refs = 0);
explicit
codecvt(__c_locale __cloc, size_t __refs = 0);
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state, extern_type* __to,
extern_type* __to_end, extern_type*& __to_next) const;
virtual result
do_in(state_type& __state, const extern_type* __from,
const extern_type* __from_end, const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual int
do_encoding() const throw();
virtual bool
do_always_noconv() const throw();
virtual int
do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
class codecvt<wchar_t, char, mbstate_t>
: public __codecvt_abstract_base<wchar_t, char, mbstate_t>
{
friend class messages<wchar_t>;
public:
typedef wchar_t			intern_type;
typedef char			extern_type;
typedef mbstate_t			state_type;
protected:
__c_locale			_M_c_locale_codecvt;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0);
explicit
codecvt(__c_locale __cloc, size_t __refs = 0);
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_in(state_type& __state,
const extern_type* __from, const extern_type* __from_end,
const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual
int do_encoding() const throw();
virtual
bool do_always_noconv() const throw();
virtual
int do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
#endif
#if __cplusplus >= 201103L
template<>
class codecvt<char16_t, char, mbstate_t>
: public __codecvt_abstract_base<char16_t, char, mbstate_t>
{
public:
typedef char16_t			intern_type;
typedef char			extern_type;
typedef mbstate_t			state_type;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0)
: __codecvt_abstract_base<char16_t, char, mbstate_t>(__refs) { }
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_in(state_type& __state,
const extern_type* __from, const extern_type* __from_end,
const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual
int do_encoding() const throw();
virtual
bool do_always_noconv() const throw();
virtual
int do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
template<>
class codecvt<char32_t, char, mbstate_t>
: public __codecvt_abstract_base<char32_t, char, mbstate_t>
{
public:
typedef char32_t			intern_type;
typedef char			extern_type;
typedef mbstate_t			state_type;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0)
: __codecvt_abstract_base<char32_t, char, mbstate_t>(__refs) { }
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_in(state_type& __state,
const extern_type* __from, const extern_type* __from_end,
const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual
int do_encoding() const throw();
virtual
bool do_always_noconv() const throw();
virtual
int do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
#ifdef _GLIBCXX_USE_CHAR8_T
template<>
class codecvt<char16_t, char8_t, mbstate_t>
: public __codecvt_abstract_base<char16_t, char8_t, mbstate_t>
{
public:
typedef char16_t			intern_type;
typedef char8_t			extern_type;
typedef mbstate_t			state_type;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0)
: __codecvt_abstract_base<char16_t, char8_t, mbstate_t>(__refs) { }
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_in(state_type& __state,
const extern_type* __from, const extern_type* __from_end,
const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual
int do_encoding() const throw();
virtual
bool do_always_noconv() const throw();
virtual
int do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
template<>
class codecvt<char32_t, char8_t, mbstate_t>
: public __codecvt_abstract_base<char32_t, char8_t, mbstate_t>
{
public:
typedef char32_t			intern_type;
typedef char8_t			extern_type;
typedef mbstate_t			state_type;
public:
static locale::id			id;
explicit
codecvt(size_t __refs = 0)
: __codecvt_abstract_base<char32_t, char8_t, mbstate_t>(__refs) { }
protected:
virtual
~codecvt();
virtual result
do_out(state_type& __state, const intern_type* __from,
const intern_type* __from_end, const intern_type*& __from_next,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_unshift(state_type& __state,
extern_type* __to, extern_type* __to_end,
extern_type*& __to_next) const;
virtual result
do_in(state_type& __state,
const extern_type* __from, const extern_type* __from_end,
const extern_type*& __from_next,
intern_type* __to, intern_type* __to_end,
intern_type*& __to_next) const;
virtual
int do_encoding() const throw();
virtual
bool do_always_noconv() const throw();
virtual
int do_length(state_type&, const extern_type* __from,
const extern_type* __end, size_t __max) const;
virtual int
do_max_length() const throw();
};
#endif
#endif
template<typename _InternT, typename _ExternT, typename _StateT>
class codecvt_byname : public codecvt<_InternT, _ExternT, _StateT>
{
public:
explicit
codecvt_byname(const char* __s, size_t __refs = 0)
: codecvt<_InternT, _ExternT, _StateT>(__refs)
{
if (__builtin_strcmp(__s, "C") != 0
&& __builtin_strcmp(__s, "POSIX") != 0)
{
this->_S_destroy_c_locale(this->_M_c_locale_codecvt);
this->_S_create_c_locale(this->_M_c_locale_codecvt, __s);
}
}
#if __cplusplus >= 201103L
explicit
codecvt_byname(const string& __s, size_t __refs = 0)
: codecvt_byname(__s.c_str(), __refs) { }
#endif
protected:
virtual
~codecvt_byname() { }
};
#if __cplusplus >= 201103L
template<>
class codecvt_byname<char16_t, char, mbstate_t>
: public codecvt<char16_t, char, mbstate_t>
{
public:
explicit
codecvt_byname(const char*, size_t __refs = 0)
: codecvt<char16_t, char, mbstate_t>(__refs) { }
explicit
codecvt_byname(const string& __s, size_t __refs = 0)
: codecvt_byname(__s.c_str(), __refs) { }
protected:
virtual
~codecvt_byname() { }
};
template<>
class codecvt_byname<char32_t, char, mbstate_t>
: public codecvt<char32_t, char, mbstate_t>
{
public:
explicit
codecvt_byname(const char*, size_t __refs = 0)
: codecvt<char32_t, char, mbstate_t>(__refs) { }
explicit
codecvt_byname(const string& __s, size_t __refs = 0)
: codecvt_byname(__s.c_str(), __refs) { }
protected:
virtual
~codecvt_byname() { }
};
#if defined(_GLIBCXX_USE_CHAR8_T)
template<>
class codecvt_byname<char16_t, char8_t, mbstate_t>
: public codecvt<char16_t, char8_t, mbstate_t>
{
public:
explicit
codecvt_byname(const char*, size_t __refs = 0)
: codecvt<char16_t, char8_t, mbstate_t>(__refs) { }
explicit
codecvt_byname(const string& __s, size_t __refs = 0)
: codecvt_byname(__s.c_str(), __refs) { }
protected:
virtual
~codecvt_byname() { }
};
template<>
class codecvt_byname<char32_t, char8_t, mbstate_t>
: public codecvt<char32_t, char8_t, mbstate_t>
{
public:
explicit
codecvt_byname(const char*, size_t __refs = 0)
: codecvt<char32_t, char8_t, mbstate_t>(__refs) { }
explicit
codecvt_byname(const string& __s, size_t __refs = 0)
: codecvt_byname(__s.c_str(), __refs) { }
protected:
virtual
~codecvt_byname() { }
};
#endif
#endif
#if _GLIBCXX_EXTERN_TEMPLATE
extern template class codecvt_byname<char, char, mbstate_t>;
extern template
const codecvt<char, char, mbstate_t>&
use_facet<codecvt<char, char, mbstate_t> >(const locale&);
extern template
bool
has_facet<codecvt<char, char, mbstate_t> >(const locale&);
#ifdef _GLIBCXX_USE_WCHAR_T
extern template class codecvt_byname<wchar_t, char, mbstate_t>;
extern template
const codecvt<wchar_t, char, mbstate_t>&
use_facet<codecvt<wchar_t, char, mbstate_t> >(const locale&);
extern template
bool
has_facet<codecvt<wchar_t, char, mbstate_t> >(const locale&);
#endif
#if __cplusplus >= 201103L
extern template class codecvt_byname<char16_t, char, mbstate_t>;
extern template class codecvt_byname<char32_t, char, mbstate_t>;
#if defined(_GLIBCXX_USE_CHAR8_T)
extern template class codecvt_byname<char16_t, char8_t, mbstate_t>;
extern template class codecvt_byname<char32_t, char8_t, mbstate_t>;
#endif
#endif
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif