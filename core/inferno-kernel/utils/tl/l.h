#include	<lib9.h>
#include	<bio.h>
#include	"../5c/5.out.h"
#ifndef	EXTERN
#define	EXTERN	extern
#endif
#define	CALLEEBX
typedef	struct	Adr	Adr;
typedef	struct	Sym	Sym;
typedef	struct	Autom	Auto;
typedef	struct	Prog	Prog;
typedef	struct	Optab	Optab;
typedef	struct	Oprang	Oprang;
typedef	uchar	Opcross[32][2][32];
typedef	struct	Count	Count;
typedef	struct	Use	Use;
#define	P		((Prog*)0)
#define	S		((Sym*)0)
#define	U		((Use*)0)
#define	TNAME		(curtext&&curtext->from.sym?curtext->from.sym->name:noname)
struct	Adr
{
union
{
long	u0offset;
char*	u0sval;
Ieee*	u0ieee;
} u0;
union
{
Auto*	u1autom;
Sym*	u1sym;
} u1;
char	type;
char	reg;
char	name;
char	class;
};
#define	offset	u0.u0offset
#define	sval	u0.u0sval
#define	ieee	u0.u0ieee
#define	autom	u1.u1autom
#define	sym	u1.u1sym
struct	Prog
{
Adr	from;
Adr	to;
union
{
long	u0regused;
Prog*	u0forwd;
} u0;
Prog*	cond;
Prog*	link;
long	pc;
long	line;
uchar	mark;
uchar	optab;
uchar	as;
uchar	scond;
uchar	reg;
uchar	align;
};
#define	regused	u0.u0regused
#define	forwd	u0.u0forwd
struct	Sym
{
char	*name;
short	type;
short	version;
short	become;
short	frame;
uchar	subtype;
ushort	file;
long	value;
long	sig;
uchar	used;
uchar	thumb;
uchar	foreign;
uchar	fnptr;
Use*		use;
Sym*	link;
long	base;
};
#define SIGNINTERN	(1729*325*1729)
struct	Autom
{
Sym*	asym;
Auto*	link;
long	aoffset;
short	type;
};
struct	Optab
{
char	as;
char	a1;
char	a2;
char	a3;
char	type;
char	size;
char	param;
char	flag;
};
struct	Oprang
{
Optab*	start;
Optab*	stop;
};
struct	Count
{
long	count;
long	outof;
};
struct	Use
{
Prog*	p;
Prog*	ct;
Use*		link;
};
enum
{
STEXT		= 1,
SDATA,
SBSS,
SDATA1,
SXREF,
SLEAF,
SFILE,
SCONST,
SSTRING,
SUNDEF,
SREMOVED,
SIMPORT,
SEXPORT,
LFROM		= 1<<0,
LTO		= 1<<1,
LPOOL		= 1<<2,
V4		= 1<<3,
C_NONE		= 0,
C_REG,
C_REGREG,
C_SHIFT,
C_FREG,
C_PSR,
C_FCR,
C_RCON,
C_NCON,
C_SCON,
C_BCON,
C_LCON,
C_FCON,
C_GCON,
C_RACON,
C_SACON,
C_LACON,
C_GACON,
C_RECON,
C_LECON,
C_SBRA,
C_LBRA,
C_GBRA,
C_HAUTO,
C_FAUTO,
C_HFAUTO,
C_SAUTO,
C_LAUTO,
C_HEXT,
C_FEXT,
C_HFEXT,
C_SEXT,
C_LEXT,
C_HOREG,
C_FOREG,
C_HFOREG,
C_SOREG,
C_ROREG,
C_SROREG,
C_LOREG,
C_GOREG,
C_PC,
C_SP,
C_HREG,
C_OFFPC,
C_ADDR,
C_GOK,
FOLL		= 1<<0,
LABEL		= 1<<1,
LEAF		= 1<<2,
BIG		= (1<<12)-4,
STRINGSZ	= 200,
NHASH		= 10007,
NHUNK		= 100000,
MINSIZ		= 64,
NENT		= 100,
MAXIO		= 8192,
MAXHIST		= 20,
Roffset	= 22,
Rindex	= 10,
};
EXTERN union
{
struct
{
uchar	obuf[MAXIO];
uchar	ibuf[MAXIO];
} u;
char	dbuf[1];
} buf;
#define	cbuf	u.obuf
#define	xbuf	u.ibuf
#define	setarch(p)		if((p)->as==ATEXT) thumb=(p)->reg&ALLTHUMBS
#define	setthumb(p)	if((p)->as==ATEXT) seenthumb|=(p)->reg&ALLTHUMBS
#ifndef COFFCVT
EXTERN	long	HEADR;
EXTERN	int	HEADTYPE;
EXTERN	long	INITDAT;
EXTERN	long	INITRODAT;
EXTERN	long	INITRND;
EXTERN	long	INITTEXT;
EXTERN	char*	INITENTRY;
EXTERN	long	autosize;
EXTERN	Biobuf	bso;
EXTERN	long	bsssize;
EXTERN	int	cbc;
EXTERN	uchar*	cbp;
EXTERN	int	cout;
EXTERN	Auto*	curauto;
EXTERN	Auto*	curhist;
EXTERN	Prog*	curp;
EXTERN	Prog*	curtext;
EXTERN	Prog*	datap;
EXTERN	long	datsize;
EXTERN	char	debug[128];
EXTERN	Prog*	etextp;
EXTERN	Prog*	firstp;
EXTERN	char	fnuxi4[4];
EXTERN	char	fnuxi8[8];
EXTERN	char*	noname;
EXTERN	Sym*	hash[NHASH];
EXTERN	Sym*	histfrog[MAXHIST];
EXTERN	int	histfrogp;
EXTERN	int	histgen;
EXTERN	char*	library[50];
EXTERN	char*	libraryobj[50];
EXTERN	int	libraryp;
EXTERN	int	xrefresolv;
EXTERN	char*	hunk;
EXTERN	char	inuxi1[1];
EXTERN	char	inuxi2[2];
EXTERN	char	inuxi4[4];
EXTERN	Prog*	lastp;
EXTERN	long	lcsize;
EXTERN	char	literal[32];
EXTERN	int	nerrors;
EXTERN	long	nhunk;
EXTERN	long	instoffset;
EXTERN	Opcross	opcross[8];
EXTERN	Oprang	oprange[ALAST];
EXTERN	Oprang	thumboprange[ALAST];
EXTERN	char*	outfile;
EXTERN	long	pc;
EXTERN	uchar	repop[ALAST];
EXTERN	long	symsize;
EXTERN	Prog*	textp;
EXTERN	long	textsize;
EXTERN	long	thunk;
EXTERN	int	version;
EXTERN	char	xcmp[C_GOK+1][C_GOK+1];
EXTERN	Prog	zprg;
EXTERN	int	dtype;
EXTERN	int	armv4;
EXTERN	int	thumb;
EXTERN	int	seenthumb;
EXTERN	int	armsize;
EXTERN	int	doexp, dlm;
EXTERN	int	imports, nimports;
EXTERN	int	exports, nexports;
EXTERN	char*	EXPTAB;
EXTERN	Prog	undefp;
#define	UP	(&undefp)
extern	char*	anames[];
extern	Optab	optab[];
extern	Optab	thumboptab[];
void	addpool(Prog*, Adr*);
EXTERN	Prog*	blitrl;
EXTERN	Prog*	elitrl;
#pragma	varargck	type	"A"	int
#pragma	varargck	type	"C"	int
#pragma	varargck	type	"D"	Adr*
#pragma	varargck	type	"N"	Adr*
#pragma	varargck	type	"P"	Prog*
#pragma	varargck	type	"S"	char*
int	Aconv(Fmt*);
int	Cconv(Fmt*);
int	Dconv(Fmt*);
int	Nconv(Fmt*);
int	Pconv(Fmt*);
int	Sconv(Fmt*);
int	aclass(Adr*);
int	thumbaclass(Adr*, Prog*);
void	addhist(long, int);
void	append(Prog*, Prog*);
void	asmb(void);
void	asmdyn(void);
void	asmlc(void);
void	asmthumbmap(void);
void	asmout(Prog*, Optab*);
void	thumbasmout(Prog*, Optab*);
void	asmsym(void);
long	atolwhex(char*);
Prog*	brloop(Prog*);
void	buildop(void);
void	thumbbuildop(void);
void	buildrep(int, int);
void	cflush(void);
void	ckoff(Sym*, long);
int	chipfloat(Ieee*);
int	cmp(int, int);
int	compound(Prog*);
double	cputime(void);
void	datblk(long, long, int);
void	diag(char*, ...);
void	dodata(void);
void	doprof1(void);
void	doprof2(void);
void	dynreloc(Sym*, long, int);
long	entryvalue(void);
void	errorexit(void);
void	exchange(Prog*);
void	export(void);
int	find1(long, int);
void	follow(void);
void	gethunk(void);
void	histtoauto(void);
void	hputl(int);
double	ieeedtod(Ieee*);
long	ieeedtof(Ieee*);
void	import(void);
int	isnop(Prog*);
void	ldobj(int, long, char*);
void	loadlib(void);
void	listinit(void);
Sym*	lookup(char*, int);
void	cput(int);
void	hput(long);
void	lput(long);
void	lputl(long);
void	mkfwd(void);
void*	mysbrk(ulong);
void	names(void);
void	nocache(Prog*);
void	nuxiinit(void);
void	objfile(char*);
int	ocmp(void*, void*);
long	opirr(int);
Optab*	oplook(Prog*);
long	oprrr(int, int);
long	olr(long, int, int, int);
long	olhr(long, int, int, int);
long	olrr(int, int, int, int);
long	olhrr(int, int, int, int);
long	osr(int, int, long, int, int);
long	oshr(int, long, int, int);
long	ofsr(int, int, long, int, int, Prog*);
long	osrr(int, int, int, int);
long	oshrr(int, int, int, int);
long	omvl(Prog*, Adr*, int);
void	patch(void);
void	prasm(Prog*);
void	prepend(Prog*, Prog*);
Prog*	prg(void);
int	pseudo(Prog*);
void	putsymb(char*, int, long, int);
void	readundefs(char*, int);
long	regoff(Adr*);
int	relinv(int);
long	rnd(long, long);
void	span(void);
void	strnput(char*, int);
void	undef(void);
void	undefsym(Sym*);
void	wput(long);
void	xdefine(char*, int, long);
void	xfol(Prog*);
void	zerosig(char*);
void	noops(void);
long	immrot(ulong);
long	immaddr(long);
long	opbra(int, int);
int	brextra(Prog*);
int	isbranch(Prog*);
int	fnpinc(Sym *);
int	fninc(Sym *);
void	thumbcount(void);
void reachable(void);
void fnptrs(void);
#endif