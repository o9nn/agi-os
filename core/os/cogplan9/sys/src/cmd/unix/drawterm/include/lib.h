#define accept	pm_accept
#define listen  pm_listen
#define sleep	ksleep
#define wakeup	kwakeup
#ifdef strtod
#undef strtod
#endif
#define strtod		fmtstrtod
#define encrypt	libencrypt
#define decrypt libdecrypt
#define oserror	liboserror
#define clone	libclone
#define atexit	libatexit
#define log2	liblog2
#define log	liblog
#define reboot	libreboot
#define strtoll libstrtoll
#undef timeradd
#define timeradd	xtimeradd
#define	nil	((void*)0)
typedef unsigned char	p9_uchar;
typedef unsigned int	p9_uint;
typedef unsigned int	p9_ulong;
typedef int		p9_long;
typedef signed char	p9_schar;
typedef unsigned short	p9_ushort;
typedef unsigned int	Rune;
typedef unsigned int	p9_u32int;
typedef p9_u32int mpdigit;
#define schar	p9_schar
#define uchar	p9_uchar
#define ushort	p9_ushort
#define uint	p9_uint
#define u32int	p9_u32int
#define long	int
#define ulong	p9_ulong
#define vlong	p9_vlong
#define uvlong	p9_uvlong
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#define SET(x)		((x)=0)
#define	USED(x)		if(x);else
enum
{
UTFmax		= 4,
Runesync	= 0x80,
Runeself	= 0x80,
Runeerror	= 0xFFFD,
Runemax		= 0x10FFFF,
Runemask	= 0x1FFFFF,
};
extern	int	runetochar(char*, Rune*);
extern	int	chartorune(Rune*, char*);
extern	int	runelen(long);
extern	int	fullrune(char*, int);
extern  int	wstrtoutf(char*, Rune*, int);
extern  int	wstrutflen(Rune*);
extern	long	utflen(char*);
extern	char*	utfrune(char*, long);
extern	char*	utfrrune(char*, long);
#define	MORDER	0x0003
#define	MREPL	0x0000
#define	MBEFORE	0x0001
#define	MAFTER	0x0002
#define	MCREATE	0x0004
#define	MCACHE	0x0010
#define	MMASK	0x0017
#define	OREAD	0
#define	OWRITE	1
#define	ORDWR	2
#define	OEXEC	3
#define	OTRUNC	16
#define	OCEXEC	32
#define	ORCLOSE	64
#define	OEXCL   0x1000
#define	NCONT	0
#define	NDFLT	1
#define	NSAVE	2
#define	NRSTR	3
#define	ERRMAX			128
#define	KNAMELEN		28
#define QTDIR		0x80
#define QTAPPEND	0x40
#define QTEXCL		0x20
#define QTMOUNT		0x10
#define QTAUTH		0x08
#define QTFILE		0x00
#define DMDIR		0x80000000
#define DMAPPEND		0x40000000
#define DMEXCL		0x20000000
#define DMMOUNT		0x10000000
#define DMAUTH		0x08000000
#define DMREAD		0x4
#define DMWRITE		0x2
#define DMEXEC		0x1
typedef struct Lock
{
#ifdef PTHREAD
int init;
pthread_mutex_t mutex;
#else
long	key;
#endif
} Lock;
typedef struct QLock
{
Lock	lk;
struct Proc	*hold;
struct Proc	*first;
struct Proc	*last;
} QLock;
typedef
struct Qid
{
uvlong	path;
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
typedef
struct Waitmsg
{
int pid;
ulong time[3];
char	*msg;
} Waitmsg;
typedef struct Fmt	Fmt;
struct Fmt{
uchar	runes;
void	*start;
void	*to;
void	*stop;
int	(*flush)(Fmt *);
void	*farg;
int	nfmt;
va_list	args;
int	r;
int	width;
int	prec;
ulong	flags;
};
enum{
FmtWidth	= 1,
FmtLeft		= FmtWidth << 1,
FmtPrec		= FmtLeft << 1,
FmtSharp	= FmtPrec << 1,
FmtSpace	= FmtSharp << 1,
FmtSign		= FmtSpace << 1,
FmtZero		= FmtSign << 1,
FmtUnsigned	= FmtZero << 1,
FmtShort	= FmtUnsigned << 1,
FmtLong		= FmtShort << 1,
FmtVLong	= FmtLong << 1,
FmtComma	= FmtVLong << 1,
FmtByte	= FmtComma << 1,
FmtFlag		= FmtByte << 1,
FmtLDouble	= FmtFlag << 1
};
extern	int	print(char*, ...);
extern	char*	seprint(char*, char*, char*, ...);
extern	char*	vseprint(char*, char*, char*, va_list);
extern	int	snprint(char*, int, char*, ...);
extern	int	vsnprint(char*, int, char*, va_list);
extern	char*	smprint(char*, ...);
extern	char*	vsmprint(char*, va_list);
extern	int	sprint(char*, char*, ...);
extern	int	fprint(int, char*, ...);
extern	int	vfprint(int, char*, va_list);
extern	int	(*doquote)(int);
extern	int	runesprint(Rune*, char*, ...);
extern	int	runesnprint(Rune*, int, char*, ...);
extern	int	runevsnprint(Rune*, int, char*, va_list);
extern	Rune*	runeseprint(Rune*, Rune*, char*, ...);
extern	Rune*	runevseprint(Rune*, Rune*, char*, va_list);
extern	Rune*	runesmprint(char*, ...);
extern	Rune*	runevsmprint(char*, va_list);
extern       Rune*	runestrchr(Rune*, Rune);
extern       long	runestrlen(Rune*);
extern       Rune*	runestrstr(Rune*, Rune*);
extern	int	fmtfdinit(Fmt*, int, char*, int);
extern	int	fmtfdflush(Fmt*);
extern	int	fmtstrinit(Fmt*);
extern	int	fmtinstall(int, int (*)(Fmt*));
extern	char*	fmtstrflush(Fmt*);
extern	int	runefmtstrinit(Fmt*);
extern	Rune*	runefmtstrflush(Fmt*);
extern	int	encodefmt(Fmt*);
extern	int	fmtstrcpy(Fmt*, char*);
extern	int	fmtprint(Fmt*, char*, ...);
extern	int	fmtvprint(Fmt*, char*, va_list);
extern	void*	mallocz(ulong, int);
extern	uintptr	getcallerpc(void*);
extern	char*	cleanname(char*);
extern	void	sysfatal(char*, ...);
extern	char*	strecpy(char*, char*, char*);
extern	int	tokenize(char*, char**, int);
extern	int	getfields(char*, char**, int, int, char*);
extern	char*	utfecpy(char*, char*, char*);
extern	long	tas(long*);
extern	void	quotefmtinstall(void);
extern	int	dec64(uchar*, int, char*, int);
extern	int	enc64(char*, int, uchar*, int);
extern	int	dec32(uchar*, int, char*, int);
extern	int	enc32(char*, int, uchar*, int);
extern	int	enc16(char*, int, uchar*, int);
void		hnputs(void *p, unsigned short v);
extern	int	dofmt(Fmt*, char*);
extern	double	__NaN(void);
extern	int	__isNaN(double);
extern	double	strtod(const char*, char**);
extern	int	utfnlen(char*, long);
extern	double	__Inf(int);
extern	int	__isInf(double, int);
extern int (*fmtdoquote)(int);