#ifndef _WCHAR_H
#define _WCHAR_H 1
#define __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION
#include <bits/libc-header-start.h>
#include <bits/floatn.h>
#define __need_size_t
#define __need_wchar_t
#define __need_NULL
#include <stddef.h>
#define __need___va_list
#include <stdarg.h>
#include <bits/wchar.h>
#include <bits/types/wint_t.h>
#include <bits/types/mbstate_t.h>
#include <bits/types/__FILE.h>
#if defined __USE_UNIX98 || defined __USE_XOPEN2K
# include <bits/types/FILE.h>
#endif
#ifdef __USE_XOPEN2K8
# include <bits/types/locale_t.h>
#endif
#if defined __cplusplus && __GNUC_PREREQ (4, 4)
# define __CORRECT_ISO_CPP_WCHAR_H_PROTO
#endif
#ifndef WCHAR_MIN
# define WCHAR_MIN __WCHAR_MIN
# define WCHAR_MAX __WCHAR_MAX
#endif
#ifndef WEOF
# define WEOF (0xffffffffu)
#endif
#if (defined __USE_XOPEN && !defined __USE_GNU \
&& !(defined __USE_XOPEN2K && !defined __USE_XOPEN2KXSI))
# include <bits/wctype-wchar.h>
#endif
__BEGIN_DECLS
struct tm;
extern wchar_t *wcscpy (wchar_t *__restrict __dest,
const wchar_t *__restrict __src)
__THROW __nonnull ((1, 2));
extern wchar_t *wcsncpy (wchar_t *__restrict __dest,
const wchar_t *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
extern wchar_t *wcscat (wchar_t *__restrict __dest,
const wchar_t *__restrict __src)
__THROW __nonnull ((1, 2));
extern wchar_t *wcsncat (wchar_t *__restrict __dest,
const wchar_t *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
extern int wcscmp (const wchar_t *__s1, const wchar_t *__s2)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern int wcsncmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
#ifdef __USE_XOPEN2K8
extern int wcscasecmp (const wchar_t *__s1, const wchar_t *__s2) __THROW;
extern int wcsncasecmp (const wchar_t *__s1, const wchar_t *__s2,
size_t __n) __THROW;
extern int wcscasecmp_l (const wchar_t *__s1, const wchar_t *__s2,
locale_t __loc) __THROW;
extern int wcsncasecmp_l (const wchar_t *__s1, const wchar_t *__s2,
size_t __n, locale_t __loc) __THROW;
#endif
extern int wcscoll (const wchar_t *__s1, const wchar_t *__s2) __THROW;
extern size_t wcsxfrm (wchar_t *__restrict __s1,
const wchar_t *__restrict __s2, size_t __n) __THROW;
#ifdef __USE_XOPEN2K8
extern int wcscoll_l (const wchar_t *__s1, const wchar_t *__s2,
locale_t __loc) __THROW;
extern size_t wcsxfrm_l (wchar_t *__s1, const wchar_t *__s2,
size_t __n, locale_t __loc) __THROW;
extern wchar_t *wcsdup (const wchar_t *__s) __THROW
__attribute_malloc__ __attr_dealloc_free;
#endif
#ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wcschr (wchar_t *__wcs, wchar_t __wc)
__THROW __asm ("wcschr") __attribute_pure__;
extern "C++" const wchar_t *wcschr (const wchar_t *__wcs, wchar_t __wc)
__THROW __asm ("wcschr") __attribute_pure__;
#else
extern wchar_t *wcschr (const wchar_t *__wcs, wchar_t __wc)
__THROW __attribute_pure__;
#endif
#ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wcsrchr (wchar_t *__wcs, wchar_t __wc)
__THROW __asm ("wcsrchr") __attribute_pure__;
extern "C++" const wchar_t *wcsrchr (const wchar_t *__wcs, wchar_t __wc)
__THROW __asm ("wcsrchr") __attribute_pure__;
#else
extern wchar_t *wcsrchr (const wchar_t *__wcs, wchar_t __wc)
__THROW __attribute_pure__;
#endif
#ifdef __USE_GNU
extern wchar_t *wcschrnul (const wchar_t *__s, wchar_t __wc)
__THROW __attribute_pure__;
#endif
extern size_t wcscspn (const wchar_t *__wcs, const wchar_t *__reject)
__THROW __attribute_pure__;
extern size_t wcsspn (const wchar_t *__wcs, const wchar_t *__accept)
__THROW __attribute_pure__;
#ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wcspbrk (wchar_t *__wcs, const wchar_t *__accept)
__THROW __asm ("wcspbrk") __attribute_pure__;
extern "C++" const wchar_t *wcspbrk (const wchar_t *__wcs,
const wchar_t *__accept)
__THROW __asm ("wcspbrk") __attribute_pure__;
#else
extern wchar_t *wcspbrk (const wchar_t *__wcs, const wchar_t *__accept)
__THROW __attribute_pure__;
#endif
#ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wcsstr (wchar_t *__haystack, const wchar_t *__needle)
__THROW __asm ("wcsstr") __attribute_pure__;
extern "C++" const wchar_t *wcsstr (const wchar_t *__haystack,
const wchar_t *__needle)
__THROW __asm ("wcsstr") __attribute_pure__;
#else
extern wchar_t *wcsstr (const wchar_t *__haystack, const wchar_t *__needle)
__THROW __attribute_pure__;
#endif
extern wchar_t *wcstok (wchar_t *__restrict __s,
const wchar_t *__restrict __delim,
wchar_t **__restrict __ptr) __THROW;
extern size_t wcslen (const wchar_t *__s) __THROW __attribute_pure__;
#ifdef __USE_XOPEN
# ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wcswcs (wchar_t *__haystack, const wchar_t *__needle)
__THROW __asm ("wcswcs") __attribute_pure__;
extern "C++" const wchar_t *wcswcs (const wchar_t *__haystack,
const wchar_t *__needle)
__THROW __asm ("wcswcs") __attribute_pure__;
# else
extern wchar_t *wcswcs (const wchar_t *__haystack, const wchar_t *__needle)
__THROW __attribute_pure__;
# endif
#endif
#ifdef __USE_XOPEN2K8
extern size_t wcsnlen (const wchar_t *__s, size_t __maxlen)
__THROW __attribute_pure__;
#endif
#ifdef __CORRECT_ISO_CPP_WCHAR_H_PROTO
extern "C++" wchar_t *wmemchr (wchar_t *__s, wchar_t __c, size_t __n)
__THROW __asm ("wmemchr") __attribute_pure__;
extern "C++" const wchar_t *wmemchr (const wchar_t *__s, wchar_t __c,
size_t __n)
__THROW __asm ("wmemchr") __attribute_pure__;
#else
extern wchar_t *wmemchr (const wchar_t *__s, wchar_t __c, size_t __n)
__THROW __attribute_pure__;
#endif
extern int wmemcmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n)
__THROW __attribute_pure__;
extern wchar_t *wmemcpy (wchar_t *__restrict __s1,
const wchar_t *__restrict __s2, size_t __n) __THROW;
extern wchar_t *wmemmove (wchar_t *__s1, const wchar_t *__s2, size_t __n)
__THROW;
extern wchar_t *wmemset (wchar_t *__s, wchar_t __c, size_t __n) __THROW;
#ifdef __USE_GNU
extern wchar_t *wmempcpy (wchar_t *__restrict __s1,
const wchar_t *__restrict __s2, size_t __n)
__THROW;
#endif
extern wint_t btowc (int __c) __THROW;
extern int wctob (wint_t __c) __THROW;
extern int mbsinit (const mbstate_t *__ps) __THROW __attribute_pure__;
extern size_t mbrtowc (wchar_t *__restrict __pwc,
const char *__restrict __s, size_t __n,
mbstate_t *__restrict __p) __THROW;
extern size_t wcrtomb (char *__restrict __s, wchar_t __wc,
mbstate_t *__restrict __ps) __THROW;
extern size_t __mbrlen (const char *__restrict __s, size_t __n,
mbstate_t *__restrict __ps) __THROW;
extern size_t mbrlen (const char *__restrict __s, size_t __n,
mbstate_t *__restrict __ps) __THROW;
#ifdef __USE_EXTERN_INLINES
extern wint_t __btowc_alias (int __c) __asm ("btowc");
__extern_inline wint_t
__NTH (btowc (int __c))
{ return (__builtin_constant_p (__c) && __c >= '\0' && __c <= '\x7f'
? (wint_t) __c : __btowc_alias (__c)); }
extern int __wctob_alias (wint_t __c) __asm ("wctob");
__extern_inline int
__NTH (wctob (wint_t __wc))
{ return (__builtin_constant_p (__wc) && __wc >= L'\0' && __wc <= L'\x7f'
? (int) __wc : __wctob_alias (__wc)); }
__extern_inline size_t
__NTH (mbrlen (const char *__restrict __s, size_t __n,
mbstate_t *__restrict __ps))
{ return (__ps != NULL
? mbrtowc (NULL, __s, __n, __ps) : __mbrlen (__s, __n, NULL)); }
#endif
extern size_t mbsrtowcs (wchar_t *__restrict __dst,
const char **__restrict __src, size_t __len,
mbstate_t *__restrict __ps) __THROW;
extern size_t wcsrtombs (char *__restrict __dst,
const wchar_t **__restrict __src, size_t __len,
mbstate_t *__restrict __ps) __THROW;
#ifdef	__USE_XOPEN2K8
extern size_t mbsnrtowcs (wchar_t *__restrict __dst,
const char **__restrict __src, size_t __nmc,
size_t __len, mbstate_t *__restrict __ps) __THROW;
extern size_t wcsnrtombs (char *__restrict __dst,
const wchar_t **__restrict __src,
size_t __nwc, size_t __len,
mbstate_t *__restrict __ps) __THROW;
#endif
#ifdef __USE_XOPEN
extern int wcwidth (wchar_t __c) __THROW;
extern int wcswidth (const wchar_t *__s, size_t __n) __THROW;
#endif
extern double wcstod (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#ifdef __USE_ISOC99
extern float wcstof (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
extern long double wcstold (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT16 && defined __USE_GNU
extern _Float16 wcstof16 (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT32 && defined __USE_GNU
extern _Float32 wcstof32 (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT64 && defined __USE_GNU
extern _Float64 wcstof64 (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT128 && defined __USE_GNU
extern _Float128 wcstof128 (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT32X && defined __USE_GNU
extern _Float32x wcstof32x (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT64X && defined __USE_GNU
extern _Float64x wcstof64x (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
#if __HAVE_FLOAT128X && defined __USE_GNU
extern _Float128x wcstof128x (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr) __THROW;
#endif
extern long int wcstol (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, int __base) __THROW;
extern unsigned long int wcstoul (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, int __base)
__THROW;
#ifdef __USE_ISOC99
__extension__
extern long long int wcstoll (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, int __base)
__THROW;
__extension__
extern unsigned long long int wcstoull (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
int __base) __THROW;
#endif
#ifdef __USE_GNU
__extension__
extern long long int wcstoq (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, int __base)
__THROW;
__extension__
extern unsigned long long int wcstouq (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
int __base) __THROW;
#endif
#ifdef __USE_GNU
extern long int wcstol_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, int __base,
locale_t __loc) __THROW;
extern unsigned long int wcstoul_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
int __base, locale_t __loc) __THROW;
__extension__
extern long long int wcstoll_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
int __base, locale_t __loc) __THROW;
__extension__
extern unsigned long long int wcstoull_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
int __base, locale_t __loc)
__THROW;
extern double wcstod_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, locale_t __loc)
__THROW;
extern float wcstof_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr, locale_t __loc)
__THROW;
extern long double wcstold_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# if __HAVE_FLOAT16
extern _Float16 wcstof16_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT32
extern _Float32 wcstof32_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT64
extern _Float64 wcstof64_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT128
extern _Float128 wcstof128_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT32X
extern _Float32x wcstof32x_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT64X
extern _Float64x wcstof64x_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
# if __HAVE_FLOAT128X
extern _Float128x wcstof128x_l (const wchar_t *__restrict __nptr,
wchar_t **__restrict __endptr,
locale_t __loc) __THROW;
# endif
#endif
#ifdef __USE_XOPEN2K8
extern wchar_t *wcpcpy (wchar_t *__restrict __dest,
const wchar_t *__restrict __src) __THROW;
extern wchar_t *wcpncpy (wchar_t *__restrict __dest,
const wchar_t *__restrict __src, size_t __n)
__THROW;
#endif
#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2)
# ifndef __attr_dealloc_fclose
#   if defined __has_builtin
#     if __has_builtin (__builtin_fclose)
#      define __attr_dealloc_fclose __attr_dealloc (__builtin_fclose, 1)
#     endif
#   endif
# endif
# ifndef __attr_dealloc_fclose
#  define __attr_dealloc_fclose
# endif
extern __FILE *open_wmemstream (wchar_t **__bufloc, size_t *__sizeloc) __THROW
__attribute_malloc__ __attr_dealloc_fclose;
#endif
#if defined __USE_ISOC95 || defined __USE_UNIX98
extern int fwide (__FILE *__fp, int __mode) __THROW;
extern int fwprintf (__FILE *__restrict __stream,
const wchar_t *__restrict __format, ...)
;
extern int wprintf (const wchar_t *__restrict __format, ...)
;
extern int swprintf (wchar_t *__restrict __s, size_t __n,
const wchar_t *__restrict __format, ...)
__THROW ;
extern int vfwprintf (__FILE *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg)
;
extern int vwprintf (const wchar_t *__restrict __format,
__gnuc_va_list __arg)
;
extern int vswprintf (wchar_t *__restrict __s, size_t __n,
const wchar_t *__restrict __format,
__gnuc_va_list __arg)
__THROW ;
extern int fwscanf (__FILE *__restrict __stream,
const wchar_t *__restrict __format, ...)
;
extern int wscanf (const wchar_t *__restrict __format, ...)
;
extern int swscanf (const wchar_t *__restrict __s,
const wchar_t *__restrict __format, ...)
__THROW ;
#if !__GLIBC_USE (DEPRECATED_SCANF) && !defined __LDBL_COMPAT \
&& __LDOUBLE_REDIRECTS_TO_FLOAT128_ABI == 0
#  ifdef __REDIRECT
extern int __REDIRECT (fwscanf, (__FILE *__restrict __stream,
const wchar_t *__restrict __format, ...),
__isoc99_fwscanf)
;
extern int __REDIRECT (wscanf, (const wchar_t *__restrict __format, ...),
__isoc99_wscanf)
;
extern int __REDIRECT_NTH (swscanf, (const wchar_t *__restrict __s,
const wchar_t *__restrict __format,
...), __isoc99_swscanf)
;
#  else
extern int __isoc99_fwscanf (__FILE *__restrict __stream,
const wchar_t *__restrict __format, ...);
extern int __isoc99_wscanf (const wchar_t *__restrict __format, ...);
extern int __isoc99_swscanf (const wchar_t *__restrict __s,
const wchar_t *__restrict __format, ...)
__THROW;
#   define fwscanf __isoc99_fwscanf
#   define wscanf __isoc99_wscanf
#   define swscanf __isoc99_swscanf
#  endif
# endif
#endif
#ifdef __USE_ISOC99
extern int vfwscanf (__FILE *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg)
;
extern int vwscanf (const wchar_t *__restrict __format,
__gnuc_va_list __arg)
;
extern int vswscanf (const wchar_t *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg)
__THROW ;
# if !__GLIBC_USE (DEPRECATED_SCANF) \
&& (!defined __LDBL_COMPAT || !defined __REDIRECT) \
&& (defined __STRICT_ANSI__ || defined __USE_XOPEN2K) \
&& __LDOUBLE_REDIRECTS_TO_FLOAT128_ABI == 0
#  ifdef __REDIRECT
extern int __REDIRECT (vfwscanf, (__FILE *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg), __isoc99_vfwscanf)
;
extern int __REDIRECT (vwscanf, (const wchar_t *__restrict __format,
__gnuc_va_list __arg), __isoc99_vwscanf)
;
extern int __REDIRECT_NTH (vswscanf, (const wchar_t *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg), __isoc99_vswscanf)
;
#  else
extern int __isoc99_vfwscanf (__FILE *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg);
extern int __isoc99_vwscanf (const wchar_t *__restrict __format,
__gnuc_va_list __arg);
extern int __isoc99_vswscanf (const wchar_t *__restrict __s,
const wchar_t *__restrict __format,
__gnuc_va_list __arg) __THROW;
#   define vfwscanf __isoc99_vfwscanf
#   define vwscanf __isoc99_vwscanf
#   define vswscanf __isoc99_vswscanf
#  endif
# endif
#endif
extern wint_t fgetwc (__FILE *__stream);
extern wint_t getwc (__FILE *__stream);
extern wint_t getwchar (void);
extern wint_t fputwc (wchar_t __wc, __FILE *__stream);
extern wint_t putwc (wchar_t __wc, __FILE *__stream);
extern wint_t putwchar (wchar_t __wc);
extern wchar_t *fgetws (wchar_t *__restrict __ws, int __n,
__FILE *__restrict __stream);
extern int fputws (const wchar_t *__restrict __ws,
__FILE *__restrict __stream);
extern wint_t ungetwc (wint_t __wc, __FILE *__stream);
#ifdef __USE_GNU
extern wint_t getwc_unlocked (__FILE *__stream);
extern wint_t getwchar_unlocked (void);
extern wint_t fgetwc_unlocked (__FILE *__stream);
extern wint_t fputwc_unlocked (wchar_t __wc, __FILE *__stream);
extern wint_t putwc_unlocked (wchar_t __wc, __FILE *__stream);
extern wint_t putwchar_unlocked (wchar_t __wc);
extern wchar_t *fgetws_unlocked (wchar_t *__restrict __ws, int __n,
__FILE *__restrict __stream);
extern int fputws_unlocked (const wchar_t *__restrict __ws,
__FILE *__restrict __stream);
#endif
extern size_t wcsftime (wchar_t *__restrict __s, size_t __maxsize,
const wchar_t *__restrict __format,
const struct tm *__restrict __tp) __THROW;
# ifdef __USE_GNU
extern size_t wcsftime_l (wchar_t *__restrict __s, size_t __maxsize,
const wchar_t *__restrict __format,
const struct tm *__restrict __tp,
locale_t __loc) __THROW;
# endif
#if __USE_FORTIFY_LEVEL > 0 && defined __fortify_function
# include <bits/wchar2.h>
#endif
#include <bits/floatn.h>
#if defined __LDBL_COMPAT || __LDOUBLE_REDIRECTS_TO_FLOAT128_ABI == 1
# include <bits/wchar-ldbl.h>
#endif
__END_DECLS
#endif