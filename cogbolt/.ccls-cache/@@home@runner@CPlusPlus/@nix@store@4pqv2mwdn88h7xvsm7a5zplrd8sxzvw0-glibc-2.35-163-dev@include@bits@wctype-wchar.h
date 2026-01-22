#ifndef _BITS_WCTYPE_WCHAR_H
#define _BITS_WCTYPE_WCHAR_H 1
#if !defined _WCTYPE_H && !defined _WCHAR_H
#error "Never include <bits/wctype-wchar.h> directly; include <wctype.h> or <wchar.h> instead."
#endif
#include <bits/types.h>
#include <bits/types/wint_t.h>
typedef unsigned long int wctype_t;
# ifndef _ISwbit
#  include <bits/endian.h>
#  if __BYTE_ORDER == __BIG_ENDIAN
#   define _ISwbit(bit)	(1 << (bit))
#  else
#   define _ISwbit(bit)	\
((bit) < 8 ? (int) ((1UL << (bit)) << 24)			      \
: ((bit) < 16 ? (int) ((1UL << (bit)) << 8)			      \
: ((bit) < 24 ? (int) ((1UL << (bit)) >> 8)			      \
: (int) ((1UL << (bit)) >> 24))))
#  endif
enum
{
__ISwupper = 0,
__ISwlower = 1,
__ISwalpha = 2,
__ISwdigit = 3,
__ISwxdigit = 4,
__ISwspace = 5,
__ISwprint = 6,
__ISwgraph = 7,
__ISwblank = 8,
__ISwcntrl = 9,
__ISwpunct = 10,
__ISwalnum = 11,
_ISwupper = _ISwbit (__ISwupper),
_ISwlower = _ISwbit (__ISwlower),
_ISwalpha = _ISwbit (__ISwalpha),
_ISwdigit = _ISwbit (__ISwdigit),
_ISwxdigit = _ISwbit (__ISwxdigit),
_ISwspace = _ISwbit (__ISwspace),
_ISwprint = _ISwbit (__ISwprint),
_ISwgraph = _ISwbit (__ISwgraph),
_ISwblank = _ISwbit (__ISwblank),
_ISwcntrl = _ISwbit (__ISwcntrl),
_ISwpunct = _ISwbit (__ISwpunct),
_ISwalnum = _ISwbit (__ISwalnum)
};
# endif
__BEGIN_DECLS
extern int iswalnum (wint_t __wc) __THROW;
extern int iswalpha (wint_t __wc) __THROW;
extern int iswcntrl (wint_t __wc) __THROW;
extern int iswdigit (wint_t __wc) __THROW;
extern int iswgraph (wint_t __wc) __THROW;
extern int iswlower (wint_t __wc) __THROW;
extern int iswprint (wint_t __wc) __THROW;
extern int iswpunct (wint_t __wc) __THROW;
extern int iswspace (wint_t __wc) __THROW;
extern int iswupper (wint_t __wc) __THROW;
extern int iswxdigit (wint_t __wc) __THROW;
# ifdef __USE_ISOC99
extern int iswblank (wint_t __wc) __THROW;
# endif
extern wctype_t wctype (const char *__property) __THROW;
extern int iswctype (wint_t __wc, wctype_t __desc) __THROW;
extern wint_t towlower (wint_t __wc) __THROW;
extern wint_t towupper (wint_t __wc) __THROW;
__END_DECLS
#endif