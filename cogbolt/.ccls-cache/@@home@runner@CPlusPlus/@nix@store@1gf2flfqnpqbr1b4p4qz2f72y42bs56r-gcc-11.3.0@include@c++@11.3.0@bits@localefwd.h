#ifndef _LOCALE_FWD_H
#define _LOCALE_FWD_H 1
#pragma GCC system_header
#include <bits/c++config.h>
#include <bits/c++locale.h>
#include <iosfwd>
#include <cctype>
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
class locale;
template<typename _Facet>
bool
has_facet(const locale&) throw();
template<typename _Facet>
const _Facet&
use_facet(const locale&);
template<typename _CharT>
bool
isspace(_CharT, const locale&);
template<typename _CharT>
bool
isprint(_CharT, const locale&);
template<typename _CharT>
bool
iscntrl(_CharT, const locale&);
template<typename _CharT>
bool
isupper(_CharT, const locale&);
template<typename _CharT>
bool
islower(_CharT, const locale&);
template<typename _CharT>
bool
isalpha(_CharT, const locale&);
template<typename _CharT>
bool
isdigit(_CharT, const locale&);
template<typename _CharT>
bool
ispunct(_CharT, const locale&);
template<typename _CharT>
bool
isxdigit(_CharT, const locale&);
template<typename _CharT>
bool
isalnum(_CharT, const locale&);
template<typename _CharT>
bool
isgraph(_CharT, const locale&);
#if __cplusplus >= 201103L
template<typename _CharT>
bool
isblank(_CharT, const locale&);
#endif
template<typename _CharT>
_CharT
toupper(_CharT, const locale&);
template<typename _CharT>
_CharT
tolower(_CharT, const locale&);
struct ctype_base;
template<typename _CharT>
class ctype;
template<> class ctype<char>;
#ifdef _GLIBCXX_USE_WCHAR_T
template<> class ctype<wchar_t>;
#endif
template<typename _CharT>
class ctype_byname;
class codecvt_base;
template<typename _InternT, typename _ExternT, typename _StateT>
class codecvt;
template<> class codecvt<char, char, mbstate_t>;
#ifdef _GLIBCXX_USE_WCHAR_T
template<> class codecvt<wchar_t, char, mbstate_t>;
#endif
#if __cplusplus >= 201103L
template<> class codecvt<char16_t, char, mbstate_t>;
template<> class codecvt<char32_t, char, mbstate_t>;
#ifdef _GLIBCXX_USE_CHAR8_T
template<> class codecvt<char16_t, char8_t, mbstate_t>;
template<> class codecvt<char32_t, char8_t, mbstate_t>;
#endif
#endif
template<typename _InternT, typename _ExternT, typename _StateT>
class codecvt_byname;
_GLIBCXX_BEGIN_NAMESPACE_LDBL
template<typename _CharT, typename _InIter = istreambuf_iterator<_CharT> >
class num_get;
template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
class num_put;
_GLIBCXX_END_NAMESPACE_LDBL
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT> class numpunct;
template<typename _CharT> class numpunct_byname;
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT>
class collate;
template<typename _CharT>
class collate_byname;
_GLIBCXX_END_NAMESPACE_CXX11
class time_base;
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
class time_get;
template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
class time_get_byname;
_GLIBCXX_END_NAMESPACE_CXX11
template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
class time_put;
template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
class time_put_byname;
class money_base;
_GLIBCXX_BEGIN_NAMESPACE_LDBL_OR_CXX11
template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
class money_get;
template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
class money_put;
_GLIBCXX_END_NAMESPACE_LDBL_OR_CXX11
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT, bool _Intl = false>
class moneypunct;
template<typename _CharT, bool _Intl = false>
class moneypunct_byname;
_GLIBCXX_END_NAMESPACE_CXX11
struct messages_base;
_GLIBCXX_BEGIN_NAMESPACE_CXX11
template<typename _CharT>
class messages;
template<typename _CharT>
class messages_byname;
_GLIBCXX_END_NAMESPACE_CXX11
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif