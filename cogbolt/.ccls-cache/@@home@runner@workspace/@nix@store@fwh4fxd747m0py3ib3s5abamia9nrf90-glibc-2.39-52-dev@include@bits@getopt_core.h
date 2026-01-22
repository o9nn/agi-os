#ifndef _GETOPT_CORE_H
#define _GETOPT_CORE_H 1
__BEGIN_DECLS
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;
extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
__THROW __nonnull ((2, 3));
__END_DECLS
#endif