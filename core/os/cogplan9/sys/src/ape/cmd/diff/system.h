#if !__STDC__
#ifndef volatile
#define volatile
#endif
#endif
#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#if __STDC__
#define PARAMS(args) args
#define VOID void
#else
#define PARAMS(args) ()
#define VOID char
#endif
#if STAT_MACROS_BROKEN
#undef S_ISBLK
#undef S_ISCHR
#undef S_ISDIR
#undef S_ISFIFO
#undef S_ISREG
#undef S_ISSOCK
#endif
#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif
#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISBLK) && defined(S_IFBLK)
#define S_ISBLK(mode) (((mode) & S_IFMT) == S_IFBLK)
#endif
#if !defined(S_ISCHR) && defined(S_IFCHR)
#define S_ISCHR(mode) (((mode) & S_IFMT) == S_IFCHR)
#endif
#if !defined(S_ISFIFO) && defined(S_IFFIFO)
#define S_ISFIFO(mode) (((mode) & S_IFMT) == S_IFFIFO)
#endif
#if !defined(S_ISSOCK) && defined(S_IFSOCK)
#define S_ISSOCK(mode) (((mode) & S_IFMT) == S_IFSOCK)
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#if HAVE_TIME_H
#include <time.h>
#else
#include <sys/time.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#endif
#if !HAVE_DUP2
#define dup2(f,t)	(close (t),  fcntl (f,F_DUPFD,t))
#endif
#ifndef O_RDONLY
#define O_RDONLY 0
#endif
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) ((unsigned) (stat_val) >> 8)
#endif
#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif
#ifndef STAT_BLOCKSIZE
#if HAVE_ST_BLKSIZE
#define STAT_BLOCKSIZE(s) (s).st_blksize
#else
#define STAT_BLOCKSIZE(s) (8 * 1024)
#endif
#endif
#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) ((dirent)->d_namlen)
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#if HAVE_STDLIB_H
#include <stdlib.h>
#else
VOID *malloc ();
VOID *realloc ();
#endif
#ifndef getenv
char *getenv ();
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif
#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif
#if STDC_HEADERS || HAVE_STRING_H
# include <string.h>
# ifndef bzero
#  define bzero(s, n) memset (s, 0, n)
# endif
#else
# if !HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif
char *strchr (), *strrchr ();
# if !HAVE_MEMCHR
#  define memcmp(s1, s2, n) bcmp (s1, s2, n)
#  define memcpy(d, s, n) bcopy (s, d, n)
void *memchr ();
# endif
#endif
#include <ctype.h>
#if STDC_HEADERS
#define CTYPE_DOMAIN(c) 1
#else
#define CTYPE_DOMAIN(c) ((unsigned) (c) <= 0177)
#endif
#ifndef ISPRINT
#define ISPRINT(c) (CTYPE_DOMAIN (c) && isprint (c))
#endif
#ifndef ISSPACE
#define ISSPACE(c) (CTYPE_DOMAIN (c) && isspace (c))
#endif
#ifndef ISUPPER
#define ISUPPER(c) (CTYPE_DOMAIN (c) && isupper (c))
#endif
#ifndef ISDIGIT
#define ISDIGIT(c) ((unsigned) (c) - '0' <= 9)
#endif
#include <errno.h>
#if !STDC_HEADERS
extern int errno;
#endif
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#ifndef filename_cmp
#define filename_cmp(a, b) strcmp (a, b)
#endif
#ifndef filename_lastdirchar
#define filename_lastdirchar(filename) strrchr (filename, '/')
#endif
#ifndef HAVE_FORK
#define HAVE_FORK 1
#endif
#ifndef HAVE_SETMODE
#define HAVE_SETMODE 0
#endif
#ifndef initialize_main
#define initialize_main(argcp, argvp)
#endif
#ifndef same_file
#define same_file(s,t) 0
#endif
#ifndef SYSTEM_QUOTE_ARG
#define SYSTEM_QUOTE_ARG(q, a) \
{ \
*(q)++ = '\''; \
for (;  *(a);  *(q)++ = *(a)++) \
if (*(a) == '\'') \
{ \
*(q)++ = '\''; \
*(q)++ = '\\'; \
*(q)++ = '\''; \
} \
*(q)++ = '\''; \
}
#endif
#ifndef FOLD_FN_CHAR
#define FOLD_FN_CHAR(c) (c)
#define fnfold(filename) (filename)
#define fncmp strcmp
#endif