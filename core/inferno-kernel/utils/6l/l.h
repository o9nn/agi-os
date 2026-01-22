#include	<lib9.h>
#include	<bio.h>
#include	"../6c/6.out.h"
#include	"../ld/elf.h"
#ifndef	EXTERN
#define	EXTERN	extern
#endif
#define	P		((Prog*)0)
#define	S		((Sym*)0)
#define	TNAME		(curtext?curtext->from.sym->name:noname)
#define	cput(c)\
{ *cbp++ = c;\
if(--cbc <= 0)\
cflush(); }
#define	LIBNAMELEN	300
typedef	struct	Adr	Adr;
typedef	struct	Prog	Prog;
typedef	struct	Sym	Sym;
typedef	struct	Auto	Auto;
typedef	struct	Optab	Optab;
typedef	struct	Movtab	Movtab;
struct	Adr
{
union
{
vlong	u0offset;
char	u0scon[8];
Prog	*u0cond;
Ieee	u0ieee;
} u0;
union
{
Auto*	u1autom;
Sym*	u1sym;
} u1;
short	type;
char	index;
char	scale;
};
#define	offset	u0.u0offset
#define	scon	u0.u0scon
#define	cond	u0.u0cond
#define	ieee	u0.u0ieee
#define	autom	u1.u1autom
#define	sym	u1.u1sym
struct	Prog
{
Adr	from;
Adr	to;
Prog	*forwd;
Prog*	link;
Prog*	pcond;
vlong	pc;
long	line;
uchar	mark;
uchar	back;
short	as;
char	width;
char	mode;
};
struct	Auto
{
Sym*	asym;
Auto*	link;
long	aoffset;
short	type;
};
struct	Sym
{
char	*name;
short	type;
short	version;
short	become;
short	frame;
uchar	subtype;
ushort	file;
vlong	value;
long	sig;
Sym*	link;
};
struct	Optab
{
short	as;
uchar*	ytab;
uchar	prefix;
uchar	op[20];
};
struct	Movtab
{
short	as;
uchar	ft;
uchar	tt;
uchar	code;
uchar	op[4];
};
enum
{
STEXT		= 1,
SDATA,
SBSS,
SDATA1,
SXREF,
SFILE,
SCONST,
SUNDEF,
SIMPORT,
SEXPORT,
NHASH		= 10007,
NHUNK		= 100000,
MINSIZ		= 8,
STRINGSZ	= 200,
MINLC		= 1,
MAXIO		= 8192,
MAXHIST		= 20,
Yxxx		= 0,
Ynone,
Yi0,
Yi1,
Yi8,
Ys32,
Yi32,
Yi64,
Yiauto,
Yal,
Ycl,
Yax,
Ycx,
Yrb,
Yrl,
Yrf,
Yf0,
Yrx,
Ymb,
Yml,
Ym,
Ybr,
Ycol,
Ycs,	Yss,	Yds,	Yes,	Yfs,	Ygs,
Ygdtr,	Yidtr,	Yldtr,	Ymsw,	Ytask,
Ycr0,	Ycr1,	Ycr2,	Ycr3,	Ycr4,	Ycr5,	Ycr6,	Ycr7,	Ycr8,
Ydr0,	Ydr1,	Ydr2,	Ydr3,	Ydr4,	Ydr5,	Ydr6,	Ydr7,
Ytr0,	Ytr1,	Ytr2,	Ytr3,	Ytr4,	Ytr5,	Ytr6,	Ytr7,	Yrl32,	Yrl64,
Ymr, Ymm,
Yxr, Yxm,
Ymax,
Zxxx		= 0,
Zlit,
Z_rp,
Zbr,
Zcall,
Zib_,
Zib_rp,
Zibo_m,
Zibo_m_xm,
Zil_,
Zil_rp,
Ziq_rp,
Zilo_m,
Ziqo_m,
Zjmp,
Zloop,
Zo_iw,
Zm_o,
Zm_r,
Zm_r_xm,
Zm_r_i_xm,
Zm_r_3d,
Zm_r_xm_nr,
Zr_m_xm_nr,
Zibm_r,
Zmb_r,
Zaut_r,
Zo_m,
Zo_m64,
Zpseudo,
Zr_m,
Zr_m_xm,
Zr_m_i_xm,
Zrp_,
Z_ib,
Z_il,
Zm_ibo,
Zm_ilo,
Zib_rr,
Zil_rr,
Zclr,
Zbyte,
Zmax,
Px		= 0,
P32		= 0x32,
Pe		= 0x66,
Pm		= 0x0f,
Pq		= 0xff,
Pb		= 0xfe,
Pf2		= 0xf2,
Pf3		= 0xf3,
Pw		= 0x48,
Py		= 0x80,
Rxf		= 1<<9,
Rxt		= 1<<8,
Rxw		= 1<<3,
Rxr		= 1<<2,
Rxx		= 1<<1,
Rxb		= 1<<0,
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
#pragma	varargck	type	"A"	int
#pragma	varargck	type	"A"	uint
#pragma	varargck	type	"D"	Adr*
#pragma	varargck	type	"P"	Prog*
#pragma	varargck	type	"R"	int
#pragma	varargck	type	"S"	char*
#pragma	varargck	argpos	diag 1
EXTERN	long	HEADR;
EXTERN	long	HEADTYPE;
EXTERN	vlong	INITDAT;
EXTERN	long	INITRND;
EXTERN	vlong	INITTEXT;
EXTERN	vlong	INITTEXTP;
EXTERN	char*	INITENTRY;
EXTERN	Biobuf	bso;
EXTERN	long	bsssize;
EXTERN	int	cbc;
EXTERN	uchar*	cbp;
EXTERN	char*	pcstr;
EXTERN	int	cout;
EXTERN	Auto*	curauto;
EXTERN	Auto*	curhist;
EXTERN	Prog*	curp;
EXTERN	Prog*	curtext;
EXTERN	Prog*	datap;
EXTERN	Prog*	edatap;
EXTERN	vlong	datsize;
EXTERN	char	debug[128];
EXTERN	char	literal[32];
EXTERN	Prog*	etextp;
EXTERN	Prog*	firstp;
EXTERN	uchar	fnuxi8[8];
EXTERN	uchar	fnuxi4[4];
EXTERN	Sym*	hash[NHASH];
EXTERN	Sym*	histfrog[MAXHIST];
EXTERN	int	histfrogp;
EXTERN	int	histgen;
EXTERN	char*	library[50];
EXTERN	char*	libraryobj[50];
EXTERN	int	libraryp;
EXTERN	int	xrefresolv;
EXTERN	char*	hunk;
EXTERN	uchar	inuxi1[1];
EXTERN	uchar	inuxi2[2];
EXTERN	uchar	inuxi4[4];
EXTERN	uchar	inuxi8[8];
EXTERN	char	ycover[Ymax*Ymax];
EXTERN	uchar*	andptr;
EXTERN	uchar*	rexptr;
EXTERN	uchar	and[30];
EXTERN	int	reg[D_NONE];
EXTERN	int	regrex[D_NONE+1];
EXTERN	Prog*	lastp;
EXTERN	long	lcsize;
EXTERN	int	nerrors;
EXTERN	long	nhunk;
EXTERN	long	nsymbol;
EXTERN	char*	noname;
EXTERN	char*	outfile;
EXTERN	vlong	pc;
EXTERN	long	spsize;
EXTERN	Sym*	symlist;
EXTERN	long	symsize;
EXTERN	Prog*	textp;
EXTERN	vlong	textsize;
EXTERN	long	thunk;
EXTERN	int	version;
EXTERN	Prog	zprg;
EXTERN	int	dtype;
EXTERN	char*	paramspace;
EXTERN	Adr*	reloca;
EXTERN	int	doexp, dlm;
EXTERN	int	imports, nimports;
EXTERN	int	exports, nexports;
EXTERN	char*	EXPTAB;
EXTERN	Prog	undefp;
#define	UP	(&undefp)
extern	Optab	optab[];
extern	Optab*	opindex[];
extern	char*	anames[];
int	Aconv(Fmt*);
int	Dconv(Fmt*);
int	Pconv(Fmt*);
int	Rconv(Fmt*);
int	Sconv(Fmt*);
void	addhist(long, int);
void	addlibpath(char*);
Prog*	appendp(Prog*);
void	asmb(void);
void	asmdyn(void);
void	asmins(Prog*);
void	asmlc(void);
void	asmsp(void);
void	asmsym(void);
vlong	atolwhex(char*);
Prog*	brchain(Prog*);
Prog*	brloop(Prog*);
void	buildop(void);
void	cflush(void);
void	ckoff(Sym*, long);
Prog*	copyp(Prog*);
double	cputime(void);
void	datblk(long, long);
void	diag(char*, ...);
void	dodata(void);
void	doinit(void);
void	doprof1(void);
void	doprof2(void);
void	dostkoff(void);
void	dynreloc(Sym*, ulong, int);
vlong	entryvalue(void);
void	errorexit(void);
void	export(void);
int	fileexists(char*);
int	find1(long, int);
int	find2(long, int);
char*	findlib(char*);
void	follow(void);
void	gethunk(void);
void	histtoauto(void);
double	ieeedtod(Ieee*);
long	ieeedtof(Ieee*);
void	import(void);
void	ldobj(int, long, char*);
void	loadlib(void);
void	listinit(void);
Sym*	lookup(char*, int);
void	llput(vlong v);
void	llputl(vlong v);
void	lput(long);
void	lputl(long);
void	main(int, char*[]);
void	mkfwd(void);
void*	mysbrk(ulong);
void	nuxiinit(void);
void	objfile(char*);
int	opsize(Prog*);
void	patch(void);
Prog*	prg(void);
void	readundefs(char*, int);
int	relinv(int);
long	reuse(Prog*, Sym*);
vlong	rnd(vlong, vlong);
void	span(void);
void	strnput(char*, int);
void	undef(void);
void	undefsym(Sym*);
vlong	vaddr(Adr*);
void	wput(long);
void	wputl(long);
void	xdefine(char*, int, vlong);
void	xfol(Prog*);
int	zaddr(uchar*, Adr*, Sym*[]);
void	zerosig(char*);