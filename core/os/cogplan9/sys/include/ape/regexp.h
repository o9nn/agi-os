#ifndef __REGEXP_H
#define __REGEXP_H
#ifndef _REGEXP_EXTENSION
This header file is an extension to ANSI/POSIX
#endif
#pragma lib "/$M/lib/ape/libregexp.a"
#ifdef	UTF
#define	Runeself	0xA0
#else
#define	Runeself	0
#endif
typedef struct Resub		Resub;
typedef struct Reclass		Reclass;
typedef struct Reinst		Reinst;
typedef struct Reprog		Reprog;
struct Resub{
union
{
char *sp;
wchar_t *rsp;
} s;
union
{
char *ep;
wchar_t *rep;
} e;
};
struct Reclass{
wchar_t	*end;
wchar_t	spans[64];
};
struct Reinst{
int	type;
union	{
Reclass	*cp;
wchar_t	r;
int	subid;
Reinst	*right;
} r;
union {
Reinst *left;
Reinst *next;
} l;
};
struct Reprog{
Reinst	*startinst;
Reclass	class[16];
Reinst	firstinst[5];
};
extern Reprog	*regcomp(char*);
extern Reprog	*regcomplit(char*);
extern Reprog	*regcompnl(char*);
extern void	regerror(char*);
extern int	regexec(Reprog*, char*, Resub*, int);
extern void	regsub(char*, char*, int, Resub*, int);
extern int	rregexec(Reprog*, wchar_t*, Resub*, int);
extern void	rregsub(wchar_t*, wchar_t*, int, Resub*, int);
#endif