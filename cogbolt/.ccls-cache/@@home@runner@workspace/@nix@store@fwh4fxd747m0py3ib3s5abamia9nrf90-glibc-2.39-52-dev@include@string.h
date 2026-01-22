#ifndef _STRING_H
#define _STRING_H 1
#define __GLIBC_INTERNAL_STARTING_HEADER_IMPLEMENTATION
#include <bits/libc-header-start.h>
__BEGIN_DECLS
#define __need_size_t
#define __need_NULL
#include <stddef.h>
#if defined __cplusplus && (__GNUC_PREREQ (4, 4) \
|| __glibc_clang_prereq (3, 5))
# define __CORRECT_ISO_CPP_STRING_H_PROTO
#endif
extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
size_t __n) __THROW __nonnull ((1, 2));
extern void *memmove (void *__dest, const void *__src, size_t __n)
__THROW __nonnull ((1, 2));
#if defined __USE_MISC || defined __USE_XOPEN || __GLIBC_USE (ISOC2X)
extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
int __c, size_t __n)
__THROW __nonnull ((1, 2)) __attr_access ((__write_only__, 1, 4));
#endif
extern void *memset (void *__s, int __c, size_t __n) __THROW __nonnull ((1));
extern int memcmp (const void *__s1, const void *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern int __memcmpeq (const void *__s1, const void *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
#ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++"
{
extern void *memchr (void *__s, int __c, size_t __n)
__THROW __asm ("memchr") __attribute_pure__ __nonnull ((1));
extern const void *memchr (const void *__s, int __c, size_t __n)
__THROW __asm ("memchr") __attribute_pure__ __nonnull ((1));
# ifdef __OPTIMIZE__
__extern_always_inline void *
memchr (void *__s, int __c, size_t __n) __THROW
{
return __builtin_memchr (__s, __c, __n);
}
__extern_always_inline const void *
memchr (const void *__s, int __c, size_t __n) __THROW
{
return __builtin_memchr (__s, __c, __n);
}
# endif
}
#else
extern void *memchr (const void *__s, int __c, size_t __n)
__THROW __attribute_pure__ __nonnull ((1));
#endif
#ifdef __USE_GNU
# ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++" void *rawmemchr (void *__s, int __c)
__THROW __asm ("rawmemchr") __attribute_pure__ __nonnull ((1));
extern "C++" const void *rawmemchr (const void *__s, int __c)
__THROW __asm ("rawmemchr") __attribute_pure__ __nonnull ((1));
# else
extern void *rawmemchr (const void *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
# endif
# ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++" void *memrchr (void *__s, int __c, size_t __n)
__THROW __asm ("memrchr") __attribute_pure__ __nonnull ((1))
__attr_access ((__read_only__, 1, 3));
extern "C++" const void *memrchr (const void *__s, int __c, size_t __n)
__THROW __asm ("memrchr") __attribute_pure__ __nonnull ((1))
__attr_access ((__read_only__, 1, 3));
# else
extern void *memrchr (const void *__s, int __c, size_t __n)
__THROW __attribute_pure__ __nonnull ((1))
__attr_access ((__read_only__, 1, 3));
# endif
#endif
extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
__THROW __nonnull ((1, 2));
extern char *strncpy (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
extern char *strcat (char *__restrict __dest, const char *__restrict __src)
__THROW __nonnull ((1, 2));
extern char *strncat (char *__restrict __dest, const char *__restrict __src,
size_t __n) __THROW __nonnull ((1, 2));
extern int strcmp (const char *__s1, const char *__s2)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern int strncmp (const char *__s1, const char *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern int strcoll (const char *__s1, const char *__s2)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern size_t strxfrm (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((2)) __attr_access ((__write_only__, 1, 3));
#ifdef __USE_XOPEN2K8
# include <bits/types/locale_t.h>
extern int strcoll_l (const char *__s1, const char *__s2, locale_t __l)
__THROW __attribute_pure__ __nonnull ((1, 2, 3));
extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
locale_t __l) __THROW __nonnull ((2, 4))
__attr_access ((__write_only__, 1, 3));
#endif
#if (defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8 \
|| __GLIBC_USE (LIB_EXT2) || __GLIBC_USE (ISOC2X))
extern char *strdup (const char *__s)
__THROW __attribute_malloc__ __nonnull ((1));
#endif
#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2) || __GLIBC_USE (ISOC2X)
extern char *strndup (const char *__string, size_t __n)
__THROW __attribute_malloc__ __nonnull ((1));
#endif
#if defined __USE_GNU && defined __GNUC__
# define strdupa(s) \
(__extension__ \
({ \
const char *__old = (s); \
size_t __len = strlen (__old) + 1; \
char *__new = (char *) __builtin_alloca (__len); \
(char *) memcpy (__new, __old, __len); \
}))
# define strndupa(s, n) \
(__extension__ \
({ \
const char *__old = (s); \
size_t __len = strnlen (__old, (n)); \
char *__new = (char *) __builtin_alloca (__len + 1); \
__new[__len] = '\0'; \
(char *) memcpy (__new, __old, __len); \
}))
#endif
#ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++"
{
extern char *strchr (char *__s, int __c)
__THROW __asm ("strchr") __attribute_pure__ __nonnull ((1));
extern const char *strchr (const char *__s, int __c)
__THROW __asm ("strchr") __attribute_pure__ __nonnull ((1));
# ifdef __OPTIMIZE__
__extern_always_inline char *
strchr (char *__s, int __c) __THROW
{
return __builtin_strchr (__s, __c);
}
__extern_always_inline const char *
strchr (const char *__s, int __c) __THROW
{
return __builtin_strchr (__s, __c);
}
# endif
}
#else
extern char *strchr (const char *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
#endif
#ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++"
{
extern char *strrchr (char *__s, int __c)
__THROW __asm ("strrchr") __attribute_pure__ __nonnull ((1));
extern const char *strrchr (const char *__s, int __c)
__THROW __asm ("strrchr") __attribute_pure__ __nonnull ((1));
# ifdef __OPTIMIZE__
__extern_always_inline char *
strrchr (char *__s, int __c) __THROW
{
return __builtin_strrchr (__s, __c);
}
__extern_always_inline const char *
strrchr (const char *__s, int __c) __THROW
{
return __builtin_strrchr (__s, __c);
}
# endif
}
#else
extern char *strrchr (const char *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
#endif
#ifdef __USE_MISC
# ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++" char *strchrnul (char *__s, int __c)
__THROW __asm ("strchrnul") __attribute_pure__ __nonnull ((1));
extern "C++" const char *strchrnul (const char *__s, int __c)
__THROW __asm ("strchrnul") __attribute_pure__ __nonnull ((1));
# else
extern char *strchrnul (const char *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
# endif
#endif
extern size_t strcspn (const char *__s, const char *__reject)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern size_t strspn (const char *__s, const char *__accept)
__THROW __attribute_pure__ __nonnull ((1, 2));
#ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++"
{
extern char *strpbrk (char *__s, const char *__accept)
__THROW __asm ("strpbrk") __attribute_pure__ __nonnull ((1, 2));
extern const char *strpbrk (const char *__s, const char *__accept)
__THROW __asm ("strpbrk") __attribute_pure__ __nonnull ((1, 2));
# ifdef __OPTIMIZE__
__extern_always_inline char *
strpbrk (char *__s, const char *__accept) __THROW
{
return __builtin_strpbrk (__s, __accept);
}
__extern_always_inline const char *
strpbrk (const char *__s, const char *__accept) __THROW
{
return __builtin_strpbrk (__s, __accept);
}
# endif
}
#else
extern char *strpbrk (const char *__s, const char *__accept)
__THROW __attribute_pure__ __nonnull ((1, 2));
#endif
#ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++"
{
extern char *strstr (char *__haystack, const char *__needle)
__THROW __asm ("strstr") __attribute_pure__ __nonnull ((1, 2));
extern const char *strstr (const char *__haystack, const char *__needle)
__THROW __asm ("strstr") __attribute_pure__ __nonnull ((1, 2));
# ifdef __OPTIMIZE__
__extern_always_inline char *
strstr (char *__haystack, const char *__needle) __THROW
{
return __builtin_strstr (__haystack, __needle);
}
__extern_always_inline const char *
strstr (const char *__haystack, const char *__needle) __THROW
{
return __builtin_strstr (__haystack, __needle);
}
# endif
}
#else
extern char *strstr (const char *__haystack, const char *__needle)
__THROW __attribute_pure__ __nonnull ((1, 2));
#endif
extern char *strtok (char *__restrict __s, const char *__restrict __delim)
__THROW __nonnull ((2));
extern char *__strtok_r (char *__restrict __s,
const char *__restrict __delim,
char **__restrict __save_ptr)
__THROW __nonnull ((2, 3));
#ifdef __USE_POSIX
extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
char **__restrict __save_ptr)
__THROW __nonnull ((2, 3));
#endif
#ifdef __USE_MISC
# ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++" char *strcasestr (char *__haystack, const char *__needle)
__THROW __asm ("strcasestr") __attribute_pure__ __nonnull ((1, 2));
extern "C++" const char *strcasestr (const char *__haystack,
const char *__needle)
__THROW __asm ("strcasestr") __attribute_pure__ __nonnull ((1, 2));
# else
extern char *strcasestr (const char *__haystack, const char *__needle)
__THROW __attribute_pure__ __nonnull ((1, 2));
# endif
#endif
#ifdef __USE_MISC
extern void *memmem (const void *__haystack, size_t __haystacklen,
const void *__needle, size_t __needlelen)
__THROW __attribute_pure__ __nonnull ((1, 3))
__attr_access ((__read_only__, 1, 2))
__attr_access ((__read_only__, 3, 4));
extern void *__mempcpy (void *__restrict __dest,
const void *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
extern void *mempcpy (void *__restrict __dest,
const void *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
#endif
extern size_t strlen (const char *__s)
__THROW __attribute_pure__ __nonnull ((1));
#ifdef __USE_XOPEN2K8
extern size_t strnlen (const char *__string, size_t __maxlen)
__THROW __attribute_pure__ __nonnull ((1));
#endif
extern char *strerror (int __errnum) __THROW;
#ifdef __USE_XOPEN2K
# if defined __USE_XOPEN2K && !defined __USE_GNU
# ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (strerror_r,
(int __errnum, char *__buf, size_t __buflen),
__xpg_strerror_r) __nonnull ((2))
__attr_access ((__write_only__, 2, 3));
# else
extern int __xpg_strerror_r (int __errnum, char *__buf, size_t __buflen)
__THROW __nonnull ((2)) __attr_access ((__write_only__, 2, 3));
# define strerror_r __xpg_strerror_r
# endif
# else
extern char *strerror_r (int __errnum, char *__buf, size_t __buflen)
__THROW __nonnull ((2)) __wur __attr_access ((__write_only__, 2, 3));
# endif
# ifdef __USE_GNU
extern const char *strerrordesc_np (int __err) __THROW;
extern const char *strerrorname_np (int __err) __THROW;
# endif
#endif
#ifdef __USE_XOPEN2K8
extern char *strerror_l (int __errnum, locale_t __l) __THROW;
#endif
#ifdef __USE_MISC
# include <strings.h>
extern void explicit_bzero (void *__s, size_t __n) __THROW __nonnull ((1))
__fortified_attr_access (__write_only__, 1, 2);
extern char *strsep (char **__restrict __stringp,
const char *__restrict __delim)
__THROW __nonnull ((1, 2));
#endif
#ifdef __USE_XOPEN2K8
extern char *strsignal (int __sig) __THROW;
# ifdef __USE_GNU
extern const char *sigabbrev_np (int __sig) __THROW;
extern const char *sigdescr_np (int __sig) __THROW;
# endif
extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
__THROW __nonnull ((1, 2));
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
__THROW __nonnull ((1, 2));
extern char *__stpncpy (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
extern char *stpncpy (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2));
#endif
#ifdef __USE_MISC
extern size_t strlcpy (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2)) __attr_access ((__write_only__, 1, 3));
extern size_t strlcat (char *__restrict __dest,
const char *__restrict __src, size_t __n)
__THROW __nonnull ((1, 2)) __attr_access ((__read_write__, 1, 3));
#endif
#ifdef __USE_GNU
extern int strverscmp (const char *__s1, const char *__s2)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern char *strfry (char *__string) __THROW __nonnull ((1));
extern void *memfrob (void *__s, size_t __n) __THROW __nonnull ((1))
__attr_access ((__read_write__, 1, 2));
# ifndef basename
# ifdef __CORRECT_ISO_CPP_STRING_H_PROTO
extern "C++" char *basename (char *__filename)
__THROW __asm ("basename") __nonnull ((1));
extern "C++" const char *basename (const char *__filename)
__THROW __asm ("basename") __nonnull ((1));
# else
extern char *basename (const char *__filename) __THROW __nonnull ((1));
# endif
# endif
#endif
#if __GNUC_PREREQ (3,4)
# if __USE_FORTIFY_LEVEL > 0 && defined __fortify_function
# include <bits/string_fortified.h>
# endif
#endif
__END_DECLS
#endif