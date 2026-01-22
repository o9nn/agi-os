#include <sys/stat.h>
#ifndef HAVE_LSTAT
# define lstat(path, buf)	stat(path, buf)
#endif
#ifdef STAT_MACROS_BROKEN
# undef S_ISREG
# undef S_ISDIR
# undef S_ISCHR
# undef S_ISBLK
# undef S_ISFIFO
# undef S_ISSOCK
# undef S_ISLNK
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
# define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISDIR) && defined(S_IFDIR)
# define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined(S_ISCHR) && defined(S_IFCHR)
# define S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#endif
#if !defined(S_ISBLK) && defined(S_IFBLK)
# define S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#endif
#if !defined(S_ISFIFO) && defined(S_IFIFO)
# define S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)
#endif
#if !defined(S_ISLNK) && defined(S_IFLNK)
# define S_ISLNK(m)	(((m) & S_IFMT) == S_IFLNK)
#endif
#if !defined(S_ISSOCK) && defined(S_IFSOCK)
# define S_ISSOCK(m)	(((m) & S_IFMT) == S_IFSOCK)
#endif
#if !defined(S_ISCDF) && defined(S_CDF)
# define S_ISCDF(m)	(S_ISDIR(m) && ((m) & S_CDF))
#endif
#ifndef S_ISVTX
# define S_ISVTX	01000
#endif
#ifndef S_IXUSR
# define S_IXUSR	00100
#endif
#ifndef S_IXGRP
# define S_IXGRP	00010
#endif
#ifndef S_IXOTH
# define S_IXOTH	00001
#endif