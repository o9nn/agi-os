#ifndef stat__INCLUDED
# define stat__INCLUDED
#include "std.h"
#ifdef __MWERKS__
#include <stat.h>
#else
#include <sys/stat.h>
#endif
#if defined(__SVR3) || defined(__EMX__) || defined(__DVX__) || defined(OSK) || defined(__MSDOS__) || defined(__QNX__) || defined(VMS) || defined(__WIN32__) || defined(__IBMC__) || defined(__BEOS__) || defined(Plan9) || defined(__WATCOMC__)
# define stat_blocks(psbuf) (((psbuf)->st_size + 1023) >> 10)
#else
# define stat_blocks(psbuf) ((psbuf)->st_blocks)
#endif
#ifdef _MSC_VER
# define stat _stat
#endif
#if defined(OSK) || !defined(S_ISDIR)
# ifdef S_IFDIR
# define stat_is_dir(stbuf) ((stbuf).st_mode & S_IFDIR)
# else
# ifdef _S_IFDIR
# define stat_is_dir(stbuf) ((stbuf).st_mode & _S_IFDIR)
# endif
# endif
#else
# define stat_is_dir(stbuf) S_ISDIR((stbuf).st_mode)
#endif
#if !defined(S_ISCHR) || !defined(S_ISREG)
# ifndef S_IFMT
# ifdef _S_IFMT
# define S_IFMT _S_IFMT
# define S_IFCHR _S_IFCHR
# define S_IFREG _S_IFREG
# else
# ifdef __S_IFMT
# define S_IFMT __S_IFMT
# define S_IFCHR __S_IFCHR
# define S_IFREG __S_IFREG
# endif
# endif
# endif
# define S_ISCHR(mode) (((mode) & S_IFMT) == S_IFCHR)
# define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif
#ifndef S_IRUSR
# ifndef S_IREAD
# define S_IRUSR _S_IREAD
# else
# define S_IRUSR S_IREAD
# endif
#endif
#ifndef S_IWUSR
# ifndef S_IWRITE
# define S_IWUSR _S_IWRITE
# else
# define S_IWUSR S_IWRITE
# endif
#endif
#endif