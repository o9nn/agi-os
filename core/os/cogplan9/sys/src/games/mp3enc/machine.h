#ifndef LAME_MACHINE_H
#define LAME_MACHINE_H
#include <stdio.h>
#include <memory.h>
#ifdef STDC_HEADERS
# include <stdlib.h>
# include <string.h>
#else
# ifndef HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
char *strchr (), *strrchr ();
# ifndef HAVE_MEMCPY
#  define memcpy(d, s, n) bcopy ((s), (d), (n))
#  define memmove(d, s, n) bcopy ((s), (d), (n))
# endif
#endif
#if  defined(__riscos__)  &&  defined(FPA10)
# include "ymath.h"
#else
# include <math.h>
#endif
#include <ctype.h>
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if defined(macintosh)
# include <types.h>
# include <stat.h>
#else
# include <sys/types.h>
# include <sys/stat.h>
#endif
#define POW20(x)  pow20[x]
#define IPOW20(x)  ipow20[x]
#ifndef inline
# define inline
#endif
#define INLINE inline
#if defined(_MSC_VER)
# undef inline
# define inline _inline
#elif defined(__SASC) || defined(__GNUC__)
# undef inline
# define inline __inline
#endif
#if    defined(_MSC_VER)
# pragma warning( disable : 4244 )
#endif
#if ( defined(_MSC_VER) || defined(__BORLANDC__) || defined(__MINGW32__) )
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
#else
# ifndef FLOAT
typedef float   FLOAT;
# endif
#endif
#ifndef FLOAT8
typedef double  FLOAT8;
#endif
#if   defined _WIN32 && !defined __CYGWIN__
typedef unsigned char	u_char;
#elif defined __DECALPHA__
#elif defined OS_AMIGAOS
#elif defined __DJGPP__
typedef unsigned char	u_char;
#elif !defined __GNUC__  ||  defined __STRICT_ANSI__
typedef unsigned char	u_char;
#else
#endif
typedef FLOAT     sample_t;
typedef sample_t  stereo_t [2];
#endif