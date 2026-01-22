#ifndef _GLIBCXX_POSTYPES_H
#define _GLIBCXX_POSTYPES_H 1
#pragma GCC system_header
#include <cwchar>
#if (defined(_GLIBCXX_HAVE_INT64_T) && !defined(_GLIBCXX_HAVE_INT64_T_LONG) \
&& !defined(_GLIBCXX_HAVE_INT64_T_LONG_LONG))
#ifndef __STDC_LIMIT_MACROS
# define _UNDEF__STDC_LIMIT_MACROS
# define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
# define _UNDEF__STDC_CONSTANT_MACROS
# define __STDC_CONSTANT_MACROS
#endif
#include <stdint.h>
#ifdef _UNDEF__STDC_LIMIT_MACROS
# undef __STDC_LIMIT_MACROS
# undef _UNDEF__STDC_LIMIT_MACROS
#endif
#ifdef _UNDEF__STDC_CONSTANT_MACROS
# undef __STDC_CONSTANT_MACROS
# undef _UNDEF__STDC_CONSTANT_MACROS
#endif
#endif
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
#ifdef _GLIBCXX_HAVE_INT64_T_LONG
typedef long          streamoff;
#elif defined(_GLIBCXX_HAVE_INT64_T_LONG_LONG)
typedef long long     streamoff;
#elif defined(_GLIBCXX_HAVE_INT64_T)
typedef int64_t       streamoff;
#else
typedef long long     streamoff;
#endif
typedef ptrdiff_t	streamsize;
template<typename _StateT>
class fpos
{
private:
streamoff	                _M_off;
_StateT			_M_state;
public:
fpos()
: _M_off(0), _M_state() { }
fpos(streamoff __off)
: _M_off(__off), _M_state() { }
#if __cplusplus >= 201103L
fpos(const fpos&) = default;
fpos& operator=(const fpos&) = default;
~fpos() = default;
#endif
operator streamoff() const { return _M_off; }
void
state(_StateT __st)
{ _M_state = __st; }
_StateT
state() const
{ return _M_state; }
fpos&
operator+=(streamoff __off)
{
_M_off += __off;
return *this;
}
fpos&
operator-=(streamoff __off)
{
_M_off -= __off;
return *this;
}
fpos
operator+(streamoff __off) const
{
fpos __pos(*this);
__pos += __off;
return __pos;
}
fpos
operator-(streamoff __off) const
{
fpos __pos(*this);
__pos -= __off;
return __pos;
}
streamoff
operator-(const fpos& __other) const
{ return _M_off - __other._M_off; }
};
template<typename _StateT>
inline bool
operator==(const fpos<_StateT>& __lhs, const fpos<_StateT>& __rhs)
{ return streamoff(__lhs) == streamoff(__rhs); }
template<typename _StateT>
inline bool
operator!=(const fpos<_StateT>& __lhs, const fpos<_StateT>& __rhs)
{ return streamoff(__lhs) != streamoff(__rhs); }
typedef fpos<mbstate_t> streampos;
typedef fpos<mbstate_t> wstreampos;
#ifdef _GLIBCXX_USE_CHAR8_T
typedef fpos<mbstate_t> u8streampos;
#endif
#if __cplusplus >= 201103L
typedef fpos<mbstate_t> u16streampos;
typedef fpos<mbstate_t> u32streampos;
#endif
_GLIBCXX_END_NAMESPACE_VERSION
}
#endif