#ifndef _PAX_PORT_H
#define _PAX_PORT_H
#define TERM_SIGNAL(status) ((status) & 0x7F)
#define TERM_COREDUMP(status) (((status) & 0x80) != 0)
#define TERM_VALUE(status) ((status) >> 8)
#if defined(USG)
# include <string.h>
#ifndef _POSIX_SOURCE
# include <memory.h>
#endif
#else
# ifdef __STDC__
extern char *rindex(char *, char);
extern char *index(char *, char);
extern char *bcopy(char *, char *, unsigned int);
extern char *bzero(char *, unsigned int);
extern char *strcat(char *, char *);
extern char *strcpy(char *, char *);
# else
extern char *rindex();
extern char *index();
extern char *bcopy();
extern char *bzero();
extern char *strcat();
extern char *strcpy();
# endif
# define memcpy(a,b,n) bcopy((b),(a),(n))
# define memset(a,b,n) bzero((a),(n))
# define strrchr(s,c) rindex(s,c)
# define strchr(s,c) index(s,c)
#endif
#endif