#define _XOPEN_SOURCE 500
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#ifdef sgi
#define _BSD_TYPES	1
#include <sys/select.h>
#define _BSD_SOURCE	1
#ifdef IRIX5X
#define __inttypes_INCLUDED
typedef unsigned int            uint32_t;
typedef signed long long int    int64_t;
typedef unsigned long long int  uint64_t;
#endif
#endif
#ifdef sun
#define __EXTENSIONS__	1
#endif
#include <inttypes.h>
#include <stdlib.h>
#include <stdarg.h>
#ifndef va_copy
#ifdef __va_copy
#define va_copy	__va_copy
#else
#define va_copy(d, s)	memmove(&(d), &(s), sizeof(va_list))
#endif
#endif
#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#define ulong p9ulong
#define ushort p9ushort
#define uchar p9uchar
#define uint p9uint
#define vlong p9vlong
#define uvlong p9uvlong
#define u32int p9u32int
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned long ulong;
typedef unsigned int uint;
typedef int64_t vlong;
typedef uint64_t uvlong;
typedef uint32_t u32int;
typedef uint64_t u64int;
typedef ushort Rune;
#define nil ((void*)0)
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#ifndef offsetof
#define	offsetof(s, m)	(ulong)(&(((s*)0)->m))
#endif
#define	assert(x)	if(x);else _assert("x")
extern char *argv0;
#define	ARGBEGIN	for((void)(argv0||(argv0=*argv)),argv++,argc--;\
argv[0] && argv[0][0]=='-' && argv[0][1];\
argc--, argv++) {\
char *_args, *_argt;\
Rune _argc;\
_args = &argv[0][1];\
if(_args[0]=='-' && _args[1]==0){\
argc--; argv++; break;\
}\
_argc = 0;\
while(*_args && (_args += chartorune(&_argc, _args)))\
switch(_argc)
#define	ARGEND		SET(_argt);USED(_argt);USED(_argc);USED(_args);}\
USED(argv);USED(argc);
#define	ARGF()		(_argt=_args, _args="",\
(*_argt? _argt: argv[1]? (argc--, *++argv): 0))
#define	EARGF(x)		(_argt=_args, _args="",\
(*_argt? _argt: argv[1]? (argc--, *++argv): ((x), abort(), (char*)0)))
#define	ARGC()		_argc
#define	SET(x)	(x) = 0
#define	USED(x)	(void)(x)
enum
{
UTFmax		= 3,
Runesync	= 0x80,
Runeself	= 0x80,
Runeerror	= 0x80
};
extern	int	runetochar(char*, Rune*);
extern	int	chartorune(Rune*, char*);
extern	int	runelen(long);
extern	int	utflen(char*);
extern	char*	strecpy(char*, char*, char*);
extern	int	tokenize(char*, char**, int);
extern	int	getfields(char*, char**, int, int, char*);
typedef	struct	Fconv	Fconv;
struct	Fconv
{
char*	out;
char*	eout;
int	f1;
int	f2;
int	f3;
int	chr;
};
extern	char*	doprint(char*, char*, char*, va_list *argp);
extern	int	print(char*, ...);
extern	char*	seprint(char*, char*, char*, ...);
extern	int	snprint(char*, int, char*, ...);
extern	int	sprint(char*, char*, ...);
extern	int	fprint(int, char*, ...);
extern	int	fmtinstall(int, int (*)(va_list*, Fconv*));
extern	int	numbconv(va_list*, Fconv*);
extern	void	strconv(char*, Fconv*);
extern	int	fltconv(va_list*, Fconv*);
#define	OREAD	0
#define	OWRITE	1
#define	ORDWR	2
#define	OEXEC	3
#define	OTRUNC	16
#define	OCEXEC	32
#define	ORCLOSE	64
#define	OEXCL	0x1000
#define QTDIR		0x80
#define QTAPPEND	0x40
#define QTEXCL		0x20
#define QTMOUNT		0x10
#define QTAUTH		0x08
#define QTFILE		0x00
#define DMDIR		0x80000000
#define DMAPPEND	0x40000000
#define DMEXCL		0x20000000
#define DMMOUNT		0x10000000
#define DMREAD		0x4
#define DMWRITE		0x2
#define DMEXEC		0x1
typedef
struct Qid
{
vlong	path;
ulong	vers;
uchar	type;
} Qid;
typedef
struct Dir {
ushort	type;
uint	dev;
Qid	qid;
ulong	mode;
ulong	atime;
ulong	mtime;
vlong	length;
char	*name;
char	*uid;
char	*gid;
char	*muid;
} Dir;
long readn(int, void*, long);
void remotehost(char*, int);
enum {
NAMELEN = 28,
ERRLEN = 64
};
#define DESKEYLEN 7
void	key_setup(char key[DESKEYLEN], char expandedkey[128]);
void	block_cipher(char expandedkey[128], char buf[8], int decrypting);