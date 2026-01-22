#ifndef _LINK_GRAMMAR_UTILITIES_H_
#define _LINK_GRAMMAR_UTILITIES_H_
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>
#include <locale.h>
#ifdef HAVE_LOCALE_T_IN_XLOCALE_H
#include <xlocale.h>
#endif
#include "link-includes.h"
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#elif defined __GNUC__
#ifndef alloca
# define alloca __builtin_alloca
#endif
#elif defined _AIX
# define alloca __alloca
#elif defined _MSC_VER
# include <malloc.h>
# define alloca _alloca
#else
# include <stddef.h>
# ifdef  __cplusplus
extern "C"
# endif
void *alloca (size_t);
#endif
#ifndef TLS
#ifdef _MSC_VER
#define TLS __declspec(thread)
#else
#define TLS
#endif
#endif
#ifdef _MSC_VER
#if 0
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#endif
#define HAVE__ALIGNED_MALLOC 1
#define free(x) free((void *)x)
#define realloc(x, s) realloc((void *)x, s)
#define memcpy(x, y, s) memcpy((void *)x, (void *)y, s)
#define qsort(x, y, z, w) qsort((void *)x, y, z, w)
#endif
#if defined(HAVE_LOCALE_T_IN_LOCALE_H) || defined(HAVE_LOCALE_T_IN_XLOCALE_H)
#define HAVE_LOCALE_T 1
#endif
#if defined _MSC_VER || defined __cplusplus
#define restrict __restrict
#endif
#ifdef _WIN32
#include <windows.h>
#include <mbctype.h>
#ifndef strcasecmp
#define strcasecmp _stricmp
#endif
#ifndef strncasecmp
#define strncasecmp _strnicmp
#endif
#undef rand_r
int rand_r(unsigned int *);
#ifndef __MINGW32__
#if _WINVER != 0x501  && _WINVER != 0x502
#define strtok_r strtok_s
#define HAVE_STRTOK_R
#endif
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#define HAVE_LOCALE_T
#endif
#ifdef HAVE_LOCALE_T
#define locale_t _locale_t
#define iswupper_l  _iswupper_l
#define iswalpha_l  _iswalpha_l
#define iswdigit_l  _iswdigit_l
#define iswspace_l  _iswspace_l
#define towlower_l  _towlower_l
#define towupper_l  _towupper_l
#define strtof_l    _strtof_l
#define freelocale _free_locale
#endif
char * strndup (const char *str, size_t size);
#ifdef mbrtowc
#undef mbrtowc
#endif
size_t lg_mbrtowc(wchar_t *, const char *, size_t n, mbstate_t *ps);
#define mbrtowc(w,s,n,x) lg_mbrtowc(w,s,n,x)
#endif
#define lg_isspace(c) ((0 < c) && (c < 127) && isspace(c))
void lg_strerror(int err_no, char *buf, size_t len);
#if defined(__sun__)
int strncasecmp(const char *s1, const char *s2, size_t n);
#endif
#ifdef HAVE_LOCALE_T
locale_t newlocale_LC_CTYPE(const char *);
#else
typedef void *locale_t;
#define iswupper_l(c, l) iswupper(c)
#define iswalpha_l(c, l) iswalpha(c)
#define iswdigit_l(c, l) iswdigit(c)
#define iswspace_l(c, l) iswspace(c)
#define towlower_l(c, l) towlower(c)
#define towupper_l(c, l) towupper(c)
#define freelocale(l)
#endif
#if HAVE__ALIGNED_MALLOC
#define aligned_alloc(alignment, size) _aligned_malloc (size, alignment)
#define aligned_free(p) _aligned_free(p)
#undef HAVE_POSIX_MEMALIGN
#elif HAVE_ALIGNED_ALLOC
#define aligned_free(p) free(p)
#undef HAVE_POSIX_MEMALIGN
#elif HAVE_POSIX_MEMALIGN
void *aligned_alloc(size_t alignment, size_t size);
#define aligned_free(p) free(p)
#else
#define NO_ALIGNED_MALLOC
#define aligned_alloc(alignment, size) malloc(size)
#define aligned_free(p) free(p)
#endif
#define ALIGN(size, alignment) (((size)+(alignment-1))&~(alignment-1))
#define STR(x) #x
#define STRINGIFY(x) STR(x)
#if !defined(MIN)
#define MIN(X,Y)  ( ((X) < (Y)) ? (X) : (Y))
#endif
#if !defined(MAX)
#define MAX(X,Y)  ( ((X) > (Y)) ? (X) : (Y))
#endif
#ifndef strdupa
#define strdupa(s) strcpy(alloca(strlen(s)+1), s)
#endif
#ifndef strndupa
#define strndupa(s, n) _strndupa3(alloca((n)+1), s, n)
static inline char *_strndupa3(char *new_s, const char *s, size_t n)
{
memcpy(new_s, s, n);
new_s[n] = '\0';
return new_s;
}
#endif
#if !defined(ARRAY_SIZE)
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]) + _array_size_chk(arr))
#if HAVE___BUILTIN_TYPES_COMPATIBLE_P && HAVE_TYPEOF
#define BUILD_ASSERT_OR_ZERO(cond) (sizeof(char [1 - 2*!(cond)]) - 1)
#define _array_size_chk(arr) \
BUILD_ASSERT_OR_ZERO(!__builtin_types_compatible_p(typeof(arr), \
typeof(&(arr)[0])))
#else
#define _array_size_chk(arr) 0
#endif
#endif
#if __GNUC__
#define GCC_DIAGNOSTIC
#define UNREACHABLE(x) (__extension__ ({if (x) __builtin_unreachable();}))
#define GNUC_MALLOC __attribute__ ((__malloc__))
#define GNUC_UNUSED __attribute__ ((__unused__))
#define NORETURN __attribute__ ((__noreturn__))
#define ATTR_PURE __attribute__ ((__pure__))
#define NO_SAN __attribute__ ((no_sanitize_address, no_sanitize_undefined))
#ifdef NO_SAN_DICT
#undef NO_SAN_DICT
#define NO_SAN_DICT NO_SAN
#else
#define NO_SAN_DICT
#endif
#ifndef DONT_EXPECT
#define likely(x)      __builtin_expect(!!(x), 1)
#define unlikely(x)    __builtin_expect(!!(x), 0)
#endif
#else
#define UNREACHABLE(x)
#define GNUC_MALLOC
#define GNUC_UNUSED
#define NORETURN
#define ATTR_PURE
#define NO_SAN_DICT
#define likely(x) x
#define unlikely(x) x
#endif
#ifdef _MSC_VER
#undef NORETURN
#define NORETURN __declspec(noreturn)
#endif
#ifdef GCC_DIAGNOSTIC
#ifdef HAVE_MAYBE_UNINITIALIZED
#define PRAGMA_MAYBE_UNINITIALIZED \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
#else
#define PRAGMA_MAYBE_UNINITIALIZED \
_Pragma("GCC diagnostic push")
#endif
#define PRAGMA_START(x) \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wunknown-pragmas\"") \
_Pragma(#x)
#define PRAGMA_END _Pragma("GCC diagnostic pop")
#else
#define PRAGMA_START(x)
#define PRAGMA_END
#define PRAGMA_MAYBE_UNINITIALIZED
#endif
static inline size_t utf8_strlen(const char *s)
{
mbstate_t mbss;
memset(&mbss, 0, sizeof(mbss));
#if _WIN32
return MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0)-1;
#else
return mbsrtowcs(NULL, &s, 0, &mbss);
#endif
}
static inline int utf8_charlen(const char *xc)
{
unsigned char c;
c = (unsigned char) *xc;
if (c == 0) return 0;
if (c < 0x80) return 1;
if ((c >= 0xc2) && (c < 0xe0)) return 2;
if ((c >= 0xe0) && (c < 0xf0)) return 3;
if ((c >= 0xf0) && (c <= 0xf4)) return 4;
return -1;
}
static inline size_t utf8_strncpy(char *dest, const char *src, size_t n)
{
size_t b = 0;
while (0 < n)
{
size_t k = utf8_charlen(src);
if (0 > (ssize_t)k) return 0;
b += k;
while (0 < k) { *dest = *src; dest++; src++; k--; }
n--;
if (0x0 == *src) break;
}
return b;
}
static inline int is_utf8_upper(const char *s, locale_t dict_locale)
{
mbstate_t mbs;
wchar_t c;
int nbytes;
memset(&mbs, 0, sizeof(mbs));
nbytes = mbrtowc(&c, s, MB_CUR_MAX, &mbs);
if (nbytes < 0) return 0;
if (iswupper_l(c, dict_locale)) return nbytes;
return 0;
}
static inline int is_utf8_alpha(const char *s, locale_t dict_locale)
{
mbstate_t mbs;
wchar_t c;
int nbytes;
memset(&mbs, 0, sizeof(mbs));
nbytes = mbrtowc(&c, s, MB_CUR_MAX, &mbs);
if (nbytes < 0) return 0;
if (iswalpha_l(c, dict_locale)) return nbytes;
return 0;
}
static inline int is_utf8_digit(const char *s, locale_t dict_locale)
{
mbstate_t mbs;
wchar_t c;
int nbytes;
memset(&mbs, 0, sizeof(mbs));
nbytes = mbrtowc(&c, s, MB_CUR_MAX, &mbs);
if (nbytes < 0) return 0;
if (iswdigit_l(c, dict_locale)) return nbytes;
return 0;
}
static inline int is_utf8_space(const char *s, locale_t dict_locale)
{
mbstate_t mbs;
wchar_t c;
int nbytes;
memset(&mbs, 0, sizeof(mbs));
nbytes = mbrtowc(&c, s, MB_CUR_MAX, &mbs);
if (nbytes < 0) return 0;
if (iswspace_l(c, dict_locale)) return nbytes;
if ((2==nbytes) && ((0xff & s[0]) == 0xc2) && ((0xff & s[1]) == 0xa0)) return 2;
if ((2==nbytes) && (c == 0xa0)) return 2;
return 0;
}
#if 0
static inline const char * skip_utf8_upper(const char * s, locale_t dict_locale)
{
int nb = is_utf8_upper(s, dict_locale);
while (nb)
{
s += nb;
nb = is_utf8_upper(s, dict_locale);
}
return s;
}
static inline bool utf8_upper_match(const char * s, const char * t,
locale_t dict_locale)
{
mbstate_t mbs, mbt;
wchar_t ws, wt;
int ns, nt;
memset(&mbs, 0, sizeof(mbs));
memset(&mbt, 0, sizeof(mbt));
ns = mbrtowc(&ws, s, MB_CUR_MAX, &mbs);
nt = mbrtowc(&wt, t, MB_CUR_MAX, &mbt);
if (ns < 0 || nt < 0) return false;
while (iswupper_l(ws, dict_locale) || iswupper_l(wt, dict_locale))
{
if (ws != wt) return false;
s += ns;
t += nt;
ns = mbrtowc(&ws, s, MB_CUR_MAX, &mbs);
nt = mbrtowc(&wt, t, MB_CUR_MAX, &mbt);
if (ns < 0 || nt < 0) return false;
}
return true;
}
#endif
void downcase_utf8_str(char *to, const char * from, size_t usize, locale_t);
#if 0
void upcase_utf8_str(char *to, const char * from, size_t usize, locale_t);
#endif
size_t lg_strlcpy(char * restrict dst, const char * restrict src, size_t dsize);
void safe_strcat(char *u, const char *v, size_t usize);
char *safe_strdup(const char *u);
typedef struct
{
char *str;
size_t end;
size_t len;
} dyn_str;
dyn_str* dyn_str_new(void);
void dyn_str_delete(dyn_str*);
static inline void dyn_str_release(char * mem) { free(mem); }
void dyn_strcat(dyn_str*, const char*);
void dyn_trimback(dyn_str*);
char * dyn_str_take(dyn_str*);
const char * dyn_str_value(dyn_str*);
size_t dyn_strlen(dyn_str*);
size_t altlen(const char **);
void init_memusage(void);
void * xalloc(size_t) GNUC_MALLOC;
void * exalloc(size_t) GNUC_MALLOC;
#ifdef TRACK_SPACE_USAGE
void xfree(void *, size_t);
void exfree(void *, size_t);
#else
static inline void xfree(void *p, size_t sz) { free(p); }
static inline void exfree(void *p, size_t sz) { free(p); };
#endif
size_t get_space_in_use(void);
size_t get_max_space_used(void);
char * get_default_locale(void);
void set_utf8_program_locale(void);
bool try_locale(const char *);
bool strtofC(const char *, float *);
static inline size_t next_power_of_two_up(size_t i)
{
size_t j=1;
while (j<i) j <<= 1;
return j;
}
static inline unsigned int power_of_2_log2(size_t i)
{
unsigned int n = 0;
while (i >>= 1)
n++;
return n;
}
#endif