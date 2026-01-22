#ifndef _IOS_BASE_H
#define _IOS_BASE_H 1
#pragma GCC system_header
#include <ext/atomicity.h>
#include <bits/localefwd.h>
#include <bits/locale_classes.h>
#if __cplusplus < 201103L
# include <stdexcept>
#else
# include <system_error>
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
enum _Ios_Fmtflags
{
_S_boolalpha = 1L << 0,
_S_dec = 1L << 1,
_S_fixed = 1L << 2,
_S_hex = 1L << 3,
_S_internal = 1L << 4,
_S_left = 1L << 5,
_S_oct = 1L << 6,
_S_right = 1L << 7,
_S_scientific = 1L << 8,
_S_showbase = 1L << 9,
_S_showpoint = 1L << 10,
_S_showpos = 1L << 11,
_S_skipws = 1L << 12,
_S_unitbuf = 1L << 13,
_S_uppercase = 1L << 14,
_S_adjustfield = _S_left | _S_right | _S_internal,
_S_basefield = _S_dec | _S_oct | _S_hex,
_S_floatfield = _S_scientific | _S_fixed,
_S_ios_fmtflags_end = 1L << 16,
_S_ios_fmtflags_max = __INT_MAX__,
_S_ios_fmtflags_min = ~__INT_MAX__
};
inline _GLIBCXX_CONSTEXPR _Ios_Fmtflags
operator&(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
{ return _Ios_Fmtflags(static_cast<int>(__a) & static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Fmtflags
operator|(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
{ return _Ios_Fmtflags(static_cast<int>(__a) | static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Fmtflags
operator^(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
{ return _Ios_Fmtflags(static_cast<int>(__a) ^ static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Fmtflags
operator~(_Ios_Fmtflags __a)
{ return _Ios_Fmtflags(~static_cast<int>(__a)); }
inline const _Ios_Fmtflags&
operator|=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
{ return __a = __a | __b; }
inline const _Ios_Fmtflags&
operator&=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
{ return __a = __a & __b; }
inline const _Ios_Fmtflags&
operator^=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
{ return __a = __a ^ __b; }
enum _Ios_Openmode
{
_S_app = 1L << 0,
_S_ate = 1L << 1,
_S_bin = 1L << 2,
_S_in = 1L << 3,
_S_out = 1L << 4,
_S_trunc = 1L << 5,
_S_noreplace = 1L << 6,
_S_ios_openmode_end = 1L << 16,
_S_ios_openmode_max = __INT_MAX__,
_S_ios_openmode_min = ~__INT_MAX__
};
inline _GLIBCXX_CONSTEXPR _Ios_Openmode
operator&(_Ios_Openmode __a, _Ios_Openmode __b)
{ return _Ios_Openmode(static_cast<int>(__a) & static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Openmode
operator|(_Ios_Openmode __a, _Ios_Openmode __b)
{ return _Ios_Openmode(static_cast<int>(__a) | static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Openmode
operator^(_Ios_Openmode __a, _Ios_Openmode __b)
{ return _Ios_Openmode(static_cast<int>(__a) ^ static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Openmode
operator~(_Ios_Openmode __a)
{ return _Ios_Openmode(~static_cast<int>(__a)); }
inline const _Ios_Openmode&
operator|=(_Ios_Openmode& __a, _Ios_Openmode __b)
{ return __a = __a | __b; }
inline const _Ios_Openmode&
operator&=(_Ios_Openmode& __a, _Ios_Openmode __b)
{ return __a = __a & __b; }
inline const _Ios_Openmode&
operator^=(_Ios_Openmode& __a, _Ios_Openmode __b)
{ return __a = __a ^ __b; }
enum _Ios_Iostate
{
_S_goodbit = 0,
_S_badbit = 1L << 0,
_S_eofbit = 1L << 1,
_S_failbit = 1L << 2,
_S_ios_iostate_end = 1L << 16,
_S_ios_iostate_max = __INT_MAX__,
_S_ios_iostate_min = ~__INT_MAX__
};
inline _GLIBCXX_CONSTEXPR _Ios_Iostate
operator&(_Ios_Iostate __a, _Ios_Iostate __b)
{ return _Ios_Iostate(static_cast<int>(__a) & static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Iostate
operator|(_Ios_Iostate __a, _Ios_Iostate __b)
{ return _Ios_Iostate(static_cast<int>(__a) | static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Iostate
operator^(_Ios_Iostate __a, _Ios_Iostate __b)
{ return _Ios_Iostate(static_cast<int>(__a) ^ static_cast<int>(__b)); }
inline _GLIBCXX_CONSTEXPR _Ios_Iostate
operator~(_Ios_Iostate __a)
{ return _Ios_Iostate(~static_cast<int>(__a)); }
inline const _Ios_Iostate&
operator|=(_Ios_Iostate& __a, _Ios_Iostate __b)
{ return __a = __a | __b; }
inline const _Ios_Iostate&
operator&=(_Ios_Iostate& __a, _Ios_Iostate __b)
{ return __a = __a & __b; }
inline const _Ios_Iostate&
operator^=(_Ios_Iostate& __a, _Ios_Iostate __b)
{ return __a = __a ^ __b; }
enum _Ios_Seekdir
{
_S_beg = 0,
_S_cur = _GLIBCXX_STDIO_SEEK_CUR,
_S_end = _GLIBCXX_STDIO_SEEK_END,
_S_ios_seekdir_end = 1L << 16
};
#if __cplusplus >= 201103L
enum class io_errc { stream = 1 };
template <> struct is_error_code_enum<io_errc> : public true_type { };
[[__nodiscard__, __gnu__::__const__]]
const error_category&
iostream_category() noexcept;
[[__nodiscard__]]
inline error_code
make_error_code(io_errc __e) noexcept
{ return error_code(static_cast<int>(__e), iostream_category()); }
[[__nodiscard__]]
inline error_condition
make_error_condition(io_errc __e) noexcept
{ return error_condition(static_cast<int>(__e), iostream_category()); }
#endif
class ios_base
{
#if _GLIBCXX_USE_CXX11_ABI
#if __cplusplus < 201103L
struct system_error : std::runtime_error
{
struct error_code
{
error_code() { }
private:
int _M_value;
const void* _M_cat;
} _M_code;
};
#endif
#endif
public:
#if _GLIBCXX_USE_CXX11_ABI
class _GLIBCXX_ABI_TAG_CXX11 failure : public system_error
{
public:
explicit
failure(const string& __str);
#if __cplusplus >= 201103L
explicit
failure(const string&, const error_code&);
explicit
failure(const char*, const error_code& = io_errc::stream);
#endif
virtual
~failure() throw();
virtual const char*
what() const throw();
};
#else
class failure : public exception
{
public:
explicit
failure(const string& __str) throw();
virtual
~failure() throw();
virtual const char*
what() const throw();
#if __cplusplus >= 201103L
explicit
failure(const string& __s, const error_code&) noexcept
: failure(__s)
{ }
explicit
failure(const char* __s, const error_code& = error_code{})
: failure(string(__s))
{ }
error_code code() const noexcept { return error_code{}; }
#endif
private:
string _M_msg;
};
#endif
typedef _Ios_Fmtflags fmtflags;
static const fmtflags boolalpha = _S_boolalpha;
static const fmtflags dec = _S_dec;
static const fmtflags fixed = _S_fixed;
static const fmtflags hex = _S_hex;
static const fmtflags internal = _S_internal;
static const fmtflags left = _S_left;
static const fmtflags oct = _S_oct;
static const fmtflags right = _S_right;
static const fmtflags scientific = _S_scientific;
static const fmtflags showbase = _S_showbase;
static const fmtflags showpoint = _S_showpoint;
static const fmtflags showpos = _S_showpos;
static const fmtflags skipws = _S_skipws;
static const fmtflags unitbuf = _S_unitbuf;
static const fmtflags uppercase = _S_uppercase;
static const fmtflags adjustfield = _S_adjustfield;
static const fmtflags basefield = _S_basefield;
static const fmtflags floatfield = _S_floatfield;
typedef _Ios_Iostate iostate;
static const iostate badbit = _S_badbit;
static const iostate eofbit = _S_eofbit;
static const iostate failbit = _S_failbit;
static const iostate goodbit = _S_goodbit;
typedef _Ios_Openmode openmode;
static const openmode app = _S_app;
static const openmode ate = _S_ate;
static const openmode binary = _S_bin;
static const openmode in = _S_in;
static const openmode out = _S_out;
static const openmode trunc = _S_trunc;
static const openmode __noreplace = _S_noreplace;
#if __cplusplus >= 202100L
#define __cpp_lib_ios_noreplace 202207L
static const openmode noreplace = _S_noreplace;
#endif
typedef _Ios_Seekdir seekdir;
static const seekdir beg = _S_beg;
static const seekdir cur = _S_cur;
static const seekdir end = _S_end;
#if __cplusplus <= 201402L
typedef int io_state
_GLIBCXX_DEPRECATED_SUGGEST("std::iostate");
typedef int open_mode
_GLIBCXX_DEPRECATED_SUGGEST("std::openmode");
typedef int seek_dir
_GLIBCXX_DEPRECATED_SUGGEST("std::seekdir");
typedef std::streampos streampos
_GLIBCXX_DEPRECATED_SUGGEST("std::streampos");
typedef std::streamoff streamoff
_GLIBCXX_DEPRECATED_SUGGEST("std::streamoff");
#endif
enum event
{
erase_event,
imbue_event,
copyfmt_event
};
typedef void (*event_callback) (event __e, ios_base& __b, int __i);
void
register_callback(event_callback __fn, int __index);
protected:
streamsize _M_precision;
streamsize _M_width;
fmtflags _M_flags;
iostate _M_exception;
iostate _M_streambuf_state;
struct _Callback_list
{
_Callback_list* _M_next;
ios_base::event_callback _M_fn;
int _M_index;
_Atomic_word _M_refcount;
_Callback_list(ios_base::event_callback __fn, int __index,
_Callback_list* __cb)
: _M_next(__cb), _M_fn(__fn), _M_index(__index), _M_refcount(0) { }
void
_M_add_reference() { __gnu_cxx::__atomic_add_dispatch(&_M_refcount, 1); }
int
_M_remove_reference()
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_BEFORE(&_M_refcount);
int __res = __gnu_cxx::__exchange_and_add_dispatch(&_M_refcount, -1);
if (__res == 0)
{
_GLIBCXX_SYNCHRONIZATION_HAPPENS_AFTER(&_M_refcount);
}
return __res;
}
};
_Callback_list* _M_callbacks;
void
_M_call_callbacks(event __ev) throw();
void
_M_dispose_callbacks(void) throw();
struct _Words
{
void* _M_pword;
long _M_iword;
_Words() : _M_pword(0), _M_iword(0) { }
};
_Words _M_word_zero;
enum { _S_local_word_size = 8 };
_Words _M_local_word[_S_local_word_size];
int _M_word_size;
_Words* _M_word;
_Words&
_M_grow_words(int __index, bool __iword);
locale _M_ios_locale;
void
_M_init() throw();
public:
class Init
{
friend class ios_base;
public:
Init();
~Init();
#if __cplusplus >= 201103L
Init(const Init&) = default;
Init& operator=(const Init&) = default;
#endif
private:
static _Atomic_word _S_refcount;
static bool _S_synced_with_stdio;
};
fmtflags
flags() const
{ return _M_flags; }
fmtflags
flags(fmtflags __fmtfl)
{
fmtflags __old = _M_flags;
_M_flags = __fmtfl;
return __old;
}
fmtflags
setf(fmtflags __fmtfl)
{
fmtflags __old = _M_flags;
_M_flags |= __fmtfl;
return __old;
}
fmtflags
setf(fmtflags __fmtfl, fmtflags __mask)
{
fmtflags __old = _M_flags;
_M_flags &= ~__mask;
_M_flags |= (__fmtfl & __mask);
return __old;
}
void
unsetf(fmtflags __mask)
{ _M_flags &= ~__mask; }
streamsize
precision() const
{ return _M_precision; }
streamsize
precision(streamsize __prec)
{
streamsize __old = _M_precision;
_M_precision = __prec;
return __old;
}
streamsize
width() const
{ return _M_width; }
streamsize
width(streamsize __wide)
{
streamsize __old = _M_width;
_M_width = __wide;
return __old;
}
static bool
sync_with_stdio(bool __sync = true);
locale
imbue(const locale& __loc) throw();
locale
getloc() const
{ return _M_ios_locale; }
const locale&
_M_getloc() const
{ return _M_ios_locale; }
static int
xalloc() throw();
long&
iword(int __ix)
{
_Words& __word = ((unsigned)__ix < (unsigned)_M_word_size)
? _M_word[__ix] : _M_grow_words(__ix, true);
return __word._M_iword;
}
void*&
pword(int __ix)
{
_Words& __word = ((unsigned)__ix < (unsigned)_M_word_size)
? _M_word[__ix] : _M_grow_words(__ix, false);
return __word._M_pword;
}
virtual ~ios_base();
protected:
ios_base() throw ();
#if __cplusplus < 201103L
private:
ios_base(const ios_base&);
ios_base&
operator=(const ios_base&);
#else
public:
ios_base(const ios_base&) = delete;
ios_base&
operator=(const ios_base&) = delete;
protected:
void
_M_move(ios_base&) noexcept;
void
_M_swap(ios_base& __rhs) noexcept;
#endif
};
inline ios_base&
boolalpha(ios_base& __base)
{
__base.setf(ios_base::boolalpha);
return __base;
}
inline ios_base&
noboolalpha(ios_base& __base)
{
__base.unsetf(ios_base::boolalpha);
return __base;
}
inline ios_base&
showbase(ios_base& __base)
{
__base.setf(ios_base::showbase);
return __base;
}
inline ios_base&
noshowbase(ios_base& __base)
{
__base.unsetf(ios_base::showbase);
return __base;
}
inline ios_base&
showpoint(ios_base& __base)
{
__base.setf(ios_base::showpoint);
return __base;
}
inline ios_base&
noshowpoint(ios_base& __base)
{
__base.unsetf(ios_base::showpoint);
return __base;
}
inline ios_base&
showpos(ios_base& __base)
{
__base.setf(ios_base::showpos);
return __base;
}
inline ios_base&
noshowpos(ios_base& __base)
{
__base.unsetf(ios_base::showpos);
return __base;
}
inline ios_base&
skipws(ios_base& __base)
{
__base.setf(ios_base::skipws);
return __base;
}
inline ios_base&
noskipws(ios_base& __base)
{
__base.unsetf(ios_base::skipws);
return __base;
}
inline ios_base&
uppercase(ios_base& __base)
{
__base.setf(ios_base::uppercase);
return __base;
}
inline ios_base&
nouppercase(ios_base& __base)
{
__base.unsetf(ios_base::uppercase);
return __base;
}
inline ios_base&
unitbuf(ios_base& __base)
{
__base.setf(ios_base::unitbuf);
return __base;
}
inline ios_base&
nounitbuf(ios_base& __base)
{
__base.unsetf(ios_base::unitbuf);
return __base;
}
inline ios_base&
internal(ios_base& __base)
{
__base.setf(ios_base::internal, ios_base::adjustfield);
return __base;
}
inline ios_base&
left(ios_base& __base)
{
__base.setf(ios_base::left, ios_base::adjustfield);
return __base;
}
inline ios_base&
right(ios_base& __base)
{
__base.setf(ios_base::right, ios_base::adjustfield);
return __base;
}
inline ios_base&
dec(ios_base& __base)
{
__base.setf(ios_base::dec, ios_base::basefield);
return __base;
}
inline ios_base&
hex(ios_base& __base)
{
__base.setf(ios_base::hex, ios_base::basefield);
return __base;
}
inline ios_base&
oct(ios_base& __base)
{
__base.setf(ios_base::oct, ios_base::basefield);
return __base;
}
inline ios_base&
fixed(ios_base& __base)
{
__base.setf(ios_base::fixed, ios_base::floatfield);
return __base;
}
inline ios_base&
scientific(ios_base& __base)
{
__base.setf(ios_base::scientific, ios_base::floatfield);
return __base;
}
#if __cplusplus >= 201103L
inline ios_base&
hexfloat(ios_base& __base)
{
__base.setf(ios_base::fixed | ios_base::scientific, ios_base::floatfield);
return __base;
}
inline ios_base&
defaultfloat(ios_base& __base)
{
__base.unsetf(ios_base::floatfield);
return __base;
}
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif