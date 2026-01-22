#ifndef _STRINGS_H
#define _STRINGS_H 1
#include <features.h>
#define __need_size_t
#include <stddef.h>
#if defined __cplusplus && __GNUC_PREREQ (4, 4)
# define __CORRECT_ISO_CPP_STRINGS_H_PROTO
#endif
__BEGIN_DECLS
#if defined __USE_MISC || !defined __USE_XOPEN2K8
extern int bcmp (const void *__s1, const void *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern void bcopy (const void *__src, void *__dest, size_t __n)
__THROW __nonnull ((1, 2));
extern void bzero (void *__s, size_t __n) __THROW __nonnull ((1));
# ifdef __CORRECT_ISO_CPP_STRINGS_H_PROTO
extern "C++"
{
extern char *index (char *__s, int __c)
__THROW __asm ("index") __attribute_pure__ __nonnull ((1));
extern const char *index (const char *__s, int __c)
__THROW __asm ("index") __attribute_pure__ __nonnull ((1));
# if defined __OPTIMIZE__
__extern_always_inline char *
index (char *__s, int __c) __THROW
{
return __builtin_index (__s, __c);
}
__extern_always_inline const char *
index (const char *__s, int __c) __THROW
{
return __builtin_index (__s, __c);
}
# endif
}
# else
extern char *index (const char *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
# endif
# ifdef __CORRECT_ISO_CPP_STRINGS_H_PROTO
extern "C++"
{
extern char *rindex (char *__s, int __c)
__THROW __asm ("rindex") __attribute_pure__ __nonnull ((1));
extern const char *rindex (const char *__s, int __c)
__THROW __asm ("rindex") __attribute_pure__ __nonnull ((1));
# if defined __OPTIMIZE__
__extern_always_inline char *
rindex (char *__s, int __c) __THROW
{
return __builtin_rindex (__s, __c);
}
__extern_always_inline const char *
rindex (const char *__s, int __c) __THROW
{
return __builtin_rindex (__s, __c);
}
# endif
}
# else
extern char *rindex (const char *__s, int __c)
__THROW __attribute_pure__ __nonnull ((1));
# endif
#endif
#if defined __USE_MISC || !defined __USE_XOPEN2K8 || defined __USE_XOPEN2K8XSI
extern int ffs (int __i) __THROW __attribute_const__;
#endif
# ifdef __USE_MISC
extern int ffsl (long int __l) __THROW __attribute_const__;
__extension__ extern int ffsll (long long int __ll)
__THROW __attribute_const__;
# endif
extern int strcasecmp (const char *__s1, const char *__s2)
__THROW __attribute_pure__ __nonnull ((1, 2));
extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
__THROW __attribute_pure__ __nonnull ((1, 2));
#ifdef __USE_XOPEN2K8
# include <bits/types/locale_t.h>
extern int strcasecmp_l (const char *__s1, const char *__s2, locale_t __loc)
__THROW __attribute_pure__ __nonnull ((1, 2, 3));
extern int strncasecmp_l (const char *__s1, const char *__s2,
size_t __n, locale_t __loc)
__THROW __attribute_pure__ __nonnull ((1, 2, 4));
#endif
__END_DECLS
#if __GNUC_PREREQ (3,4) && __USE_FORTIFY_LEVEL > 0 \
&& defined __fortify_function
# if defined __USE_MISC || !defined __USE_XOPEN2K8
# include <bits/strings_fortified.h>
# endif
#endif
#endif