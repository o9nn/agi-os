#ifndef _GETOPT_H_
#define _GETOPT_H_
#define GETOPT_API
#ifdef _WIN32
# ifdef  __cplusplus
#  define __BEGIN_DECLS  extern "C" {
#  define __END_DECLS    }
# else
#  define __BEGIN_DECLS
#  define __END_DECLS
# endif
# define __P(args)      args
#endif
#if !defined(_POSIX_SOURCE) && !defined(_XOPEN_SOURCE)
#define no_argument        0
#define required_argument  1
#define optional_argument  2
struct option {
const char *name;
int has_arg;
int *flag;
int val;
};
__BEGIN_DECLS
GETOPT_API int getopt_long __P((int, char * const *, const char *,
const struct option *, int *));
__END_DECLS
#endif
#ifdef _WIN32
__BEGIN_DECLS
GETOPT_API extern int   opterr,
optind,
optopt,
optreset;
GETOPT_API extern char* optarg;
GETOPT_API int getopt __P((int, char * const *, const char *));
__END_DECLS
#endif
#endif