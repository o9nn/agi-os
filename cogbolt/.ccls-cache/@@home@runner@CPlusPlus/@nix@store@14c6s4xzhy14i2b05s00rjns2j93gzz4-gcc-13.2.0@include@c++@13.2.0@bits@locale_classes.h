#ifndef _LOCALE_CLASSES_H
#define _LOCALE_CLASSES_H 1
#pragma GCC system_header
#include <bits/localefwd.h>
#include <string>
#include <ext/atomicity.h>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class locale
{
public:
typedef int	category;
class facet;
class id;
class _Impl;
friend class facet;
friend class _Impl;
template<typename _Facet>
friend bool
has_facet(const locale&) throw();
template<typename _Facet>
friend const _Facet&
use_facet(const locale&);
template<typename _Facet>
friend const _Facet*
__try_use_facet(const locale&) _GLIBCXX_NOTHROW;
template<typename _Cache>
friend struct __use_cache;
static const category none		= 0;
static const category ctype		= 1L << 0;
static const category numeric	= 1L << 1;
static const category collate	= 1L << 2;
static const category time		= 1L << 3;
static const category monetary	= 1L << 4;
static const category messages	= 1L << 5;
static const category all		= (ctype | numeric | collate |
time  | monetary | messages);
locale() throw();
locale(const locale& __other) throw();
explicit
locale(const char* __s);
locale(const locale& __base, const char* __s, category __cat);
#if __cplusplus >= 201103L
explicit
locale(const std::string& __s) : locale(__s.c_str()) { }
locale(const locale& __base, const std::string& __s, category __cat)
: locale(__base, __s.c_str(), __cat) { }
#endif
locale(const locale& __base, const locale& __add, category __cat);
template<typename _Facet>
locale(const locale& __other, _Facet* __f);
~locale() throw();
const locale&
operator=(const locale& __other) throw();
template<typename _Facet>
locale
combine(const locale& __other) const;
_GLIBCXX_DEFAULT_ABI_TAG
string
name() const;
bool
operator==(const locale& __other) const throw();
#if __cpp_impl_three_way_comparison < 201907L
bool
operator!=(const locale& __other) const throw()
{ return !(this->operator==(__other)); }
#endif
template<typename _Char, typename _Traits, typename _Alloc>
bool
operator()(const basic_string<_Char, _Traits, _Alloc>& __s1,
const basic_string<_Char, _Traits, _Alloc>& __s2) const;
static locale
global(const locale& __loc);
static const locale&
classic();
private:
_Impl*		_M_impl;
static _Impl*       _S_classic;
static _Impl*	_S_global;
static const char* const* const _S_categories;
enum { _S_categories_size = 6 + _GLIBCXX_NUM_CATEGORIES };
#ifdef __GTHREADS
static __gthread_once_t _S_once;
#endif
explicit
locale(_Impl*) throw();
static void
_S_initialize();
static void
_S_initialize_once() throw();
static category
_S_normalize_category(category);
void
_M_coalesce(const locale& __base, const locale& __add, category __cat);
#if _GLIBCXX_USE_CXX11_ABI
static const id* const _S_twinned_facets[];
#endif
};
class locale::facet
{
private:
friend class locale;
friend class locale::_Impl;
mutable _Atomic_word		_M_refcount;
static __c_locale                   _S_c_locale;
static const char			_S_c_name[2];
#ifdef __GTHREADS
static __gthread_once_t		_S_once;
#endif
static void
_S_initialize_once();
protected:
explicit
facet(size_t __refs = 0) throw() : _M_refcount(__refs ? 1 : 0)
{ }
virtual
~facet();
static void
_S_create_c_locale(__c_locale& __cloc, const char* __s,
__c_locale __old = 0);
static __c_locale
_S_clone_c_locale(__c_locale& __cloc) throw();
static void
_S_destroy_c_locale(__c_locale& __cloc);
static __c_locale
_S_lc_ctype_c_locale(__c_locale __cloc, const char* __s);
static __c_locale
_S_get_c_locale();
_GLIBCXX_CONST static const char*
_S_get_c_name() throw();
#if __cplusplus < 201103L
private:
facet(const facet&);
facet&
operator=(const facet&);
#else
facet(const facet&) = delete;
facet&
operator=(const facet&) = delete;
#endif
private:
void
_M_add_reference() const throw()
{ __gnu_cxx::__atomic_add_dispatch(&_M_refcount, 1); }
void
_M_remove_reference() const throw()
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_refcount);
if (__gnu_cxx::__exchange_and_add_dispatch(&_M_refcount, -1) == 1)
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_refcount);
__try
{ delete this; }
__catch(...)
{ }
}
}
const facet* _M_sso_shim(const id*) const;
const facet* _M_cow_shim(const id*) const;
protected:
class __shim;
};
class locale::id
{
private:
friend class locale;
friend class locale::_Impl;
template<typename _Facet>
friend const _Facet&
use_facet(const locale&);
template<typename _Facet>
friend bool
has_facet(const locale&) throw();
template<typename _Facet>
friend const _Facet*
__try_use_facet(const locale&) _GLIBCXX_NOTHROW;
mutable size_t		_M_index;
static _Atomic_word		_S_refcount;
void
operator=(const id&);
id(const id&);
public:
id() { }
size_t
_M_id() const throw();
};
class locale::_Impl
{
public:
friend class locale;
friend class locale::facet;
template<typename _Facet>
friend bool
has_facet(const locale&) throw();
template<typename _Facet>
friend const _Facet&
use_facet(const locale&);
template<typename _Facet>
friend const _Facet*
__try_use_facet(const locale&) _GLIBCXX_NOTHROW;
template<typename _Cache>
friend struct __use_cache;
private:
_Atomic_word			_M_refcount;
const facet**			_M_facets;
size_t				_M_facets_size;
const facet**			_M_caches;
char**				_M_names;
static const locale::id* const	_S_id_ctype[];
static const locale::id* const	_S_id_numeric[];
static const locale::id* const	_S_id_collate[];
static const locale::id* const	_S_id_time[];
static const locale::id* const	_S_id_monetary[];
static const locale::id* const	_S_id_messages[];
static const locale::id* const* const _S_facet_categories[];
void
_M_add_reference() throw()
{ __gnu_cxx::__atomic_add_dispatch(&_M_refcount, 1); }
void
_M_remove_reference() throw()
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_refcount);
if (__gnu_cxx::__exchange_and_add_dispatch(&_M_refcount, -1) == 1)
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_refcount);
__try
{ delete this; }
__catch(...)
{ }
}
}
_Impl(const _Impl&, size_t);
_Impl(const char*, size_t);
_Impl(size_t) throw();
~_Impl() throw();
_Impl(const _Impl&);
void
operator=(const _Impl&);
bool
_M_check_same_name()
{
bool __ret = true;
if (_M_names[1])
for (size_t __i = 0; __ret && __i < _S_categories_size - 1; ++__i)
__ret = __builtin_strcmp(_M_names[__i], _M_names[__i + 1]) == 0;
return __ret;
}
void
_M_replace_categories(const _Impl*, category);
void
_M_replace_category(const _Impl*, const locale::id* const*);
void
_M_replace_facet(const _Impl*, const locale::id*);
void
_M_install_facet(const locale::id*, const facet*);
template<typename _Facet>
void
_M_init_facet(_Facet* __facet)
{ _M_install_facet(&_Facet::id, __facet); }
template<typename _Facet>
void
_M_init_facet_unchecked(_Facet* __facet)
{
__facet->_M_add_reference();
_M_facets[_Facet::id._M_id()] = __facet;
}
void
_M_install_cache(const facet*, size_t);
void _M_init_extra(facet**);
void _M_init_extra(void*, void*, const char*, const char*);
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
void _M_init_extra_ldbl128(bool);
#endif
};
template<typename _CharT>
class _GLIBCXX_NAMESPACE_CXX11 collate : public locale::facet
{
public:
typedef _CharT			char_type;
typedef basic_string<_CharT>	string_type;
protected:
__c_locale			_M_c_locale_collate;
public:
static locale::id			id;
explicit
collate(size_t __refs = 0)
: facet(__refs), _M_c_locale_collate(_S_get_c_locale())
{ }
explicit
collate(__c_locale __cloc, size_t __refs = 0)
: facet(__refs), _M_c_locale_collate(_S_clone_c_locale(__cloc))
{ }
int
compare(const _CharT* __lo1, const _CharT* __hi1,
const _CharT* __lo2, const _CharT* __hi2) const
{ return this->do_compare(__lo1, __hi1, __lo2, __hi2); }
string_type
transform(const _CharT* __lo, const _CharT* __hi) const
{ return this->do_transform(__lo, __hi); }
long
hash(const _CharT* __lo, const _CharT* __hi) const
{ return this->do_hash(__lo, __hi); }
int
_M_compare(const _CharT*, const _CharT*) const throw();
size_t
_M_transform(_CharT*, const _CharT*, size_t) const throw();
protected:
virtual
~collate()
{ _S_destroy_c_locale(_M_c_locale_collate); }
virtual int
do_compare(const _CharT* __lo1, const _CharT* __hi1,
const _CharT* __lo2, const _CharT* __hi2) const;
virtual string_type
do_transform(const _CharT* __lo, const _CharT* __hi) const;
virtual long
do_hash(const _CharT* __lo, const _CharT* __hi) const;
};
template<typename _CharT>
locale::id collate<_CharT>::id;
template<>
int
collate<char>::_M_compare(const char*, const char*) const throw();
template<>
size_t
collate<char>::_M_transform(char*, const char*, size_t) const throw();
#ifdef _GLIBCXX_USE_WCHAR_T
template<>
int
collate<wchar_t>::_M_compare(const wchar_t*, const wchar_t*) const throw();
template<>
size_t
collate<wchar_t>::_M_transform(wchar_t*, const wchar_t*, size_t) const throw();
#endif
template<typename _CharT>
class _GLIBCXX_NAMESPACE_CXX11 collate_byname : public collate<_CharT>
{
public:
typedef _CharT               char_type;
typedef basic_string<_CharT> string_type;
explicit
collate_byname(const char* __s, size_t __refs = 0)
: collate<_CharT>(__refs)
{
if (__builtin_strcmp(__s, "C") != 0
&& __builtin_strcmp(__s, "POSIX") != 0)
{
this->_S_destroy_c_locale(this->_M_c_locale_collate);
this->_S_create_c_locale(this->_M_c_locale_collate, __s);
}
}
#if __cplusplus >= 201103L
explicit
collate_byname(const string& __s, size_t __refs = 0)
: collate_byname(__s.c_str(), __refs) { }
#endif
protected:
virtual
~collate_byname() { }
};
_GLIBCXX_END_NAMESPACE_VERSION
}
# include <bits/locale_classes.tcc>
#endif