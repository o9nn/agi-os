#ifndef _GETOPT_POSIX_H
#define _GETOPT_POSIX_H 1
#if !defined _UNISTD_H && !defined _STDIO_H
#error "Never include getopt_posix.h directly; use unistd.h instead."
#endif
#include <bits/getopt_core.h>
__BEGIN_DECLS
#if defined __USE_POSIX2 && !defined __USE_POSIX_IMPLICITLY \
&& !defined __USE_GNU && !defined _GETOPT_H
# ifdef __REDIRECT
extern int __REDIRECT_NTH (getopt, (int ___argc, char *const *___argv,
const char *__shortopts),
__posix_getopt);
# else
extern int __posix_getopt (int ___argc, char *const *___argv,
const char *__shortopts)
__THROW __nonnull ((2, 3));
#  define getopt __posix_getopt
# endif
#endif
__END_DECLS
#endif