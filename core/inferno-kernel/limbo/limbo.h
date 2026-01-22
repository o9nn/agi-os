#include "lib9.h"
#include "bio.h"
#include "isa.h"
#include "mathi.h"
#define IEXC	MAXDIS
#define IEXC0	(MAXDIS+1)
#define INOOP	(MAXDIS+2)
#define	LDT	1
#ifndef Extern
#define Extern extern
#endif
#define YYMAXDEPTH	200
typedef	struct Addr	Addr;
typedef	struct Case	Case;
typedef	struct Decl	Decl;
typedef	struct Desc	Desc;
typedef	struct Dlist	Dlist;
typedef	struct Except	Except;
typedef struct File	File;
typedef struct Fline	Fline;
typedef	struct Inst	Inst;
typedef	struct Label	Label;
typedef	struct Line	Line;
typedef	struct Node	Node;
typedef struct Ok	Ok;
typedef	struct Src	Src;
typedef	struct Sym	Sym;
typedef struct Szal	Szal;
typedef	struct Tattr	Tattr;
typedef	struct Teq	Teq;
typedef	struct Tpair	Tpair;
typedef	struct Type	Type;
typedef	struct Typelist	Typelist;
typedef	double		Real;
typedef	vlong		Long;
enum
{
STemp		= NREG * IBY2WD,
RTemp		= STemp+IBY2WD,
DTemp		= RTemp+IBY2WD,
MaxTemp		= DTemp+IBY2WD,
MaxReg		= 1<<16,
MaxAlign	= IBY2LG,
StrSize		= 256,
NumSize		= 32,
MaxIncPath	= 32,
MaxScope	= 64,
MaxInclude	= 32,
ScopeBuiltin	= 0,
ScopeNils	= 1,
ScopeGlobal	= 2
};
struct Ok
{
int	ok;
int	allok;
};
struct Szal
{
int	size;
int	align;
};
struct Fline
{
File	*file;
int	line;
};
struct File
{
char	*name;
int	abs;
int	off;
int	in;
char	*act;
int	actoff;
int	sbl;
};
struct Line
{
int	line;
int	pos;
};
struct Src
{
Line	start;
Line	stop;
};
enum
{
Aimm,
Amp,
Ampind,
Afp,
Afpind,
Apc,
Adesc,
Aoff,
Anoff,
Aerr,
Anone,
Aldt,
Aend
};
struct Addr
{
long	reg;
long	offset;
Decl	*decl;
};
struct Inst
{
Src	src;
ushort	op;
long	pc;
uchar	reach;
uchar	sm;
uchar	mm;
uchar	dm;
Addr	s;
Addr	m;
Addr	d;
Inst	*branch;
Inst	*next;
int	block;
};
struct Case
{
int	nlab;
int	nsnd;
long	offset;
Label	*labs;
Node	*wild;
Inst	*iwild;
};
struct Label
{
Node	*node;
char	isptr;
Node	*start;
Node	*stop;
Inst	*inst;
};
enum
{
Dtype,
Dfn,
Dglobal,
Darg,
Dlocal,
Dconst,
Dfield,
Dtag,
Dimport,
Dunbound,
Dundef,
Dwundef,
Dend
};
struct Decl
{
Src	src;
Sym	*sym;
uchar	store;
uchar	nid;
schar	caninline;
uchar	das;
Decl	*dot;
Type	*ty;
int	refs;
long	offset;
int	tag;
uchar	scope;
uchar	handler;
Decl	*next;
Decl	*old;
Node	*eimport;
Decl	*importid;
Decl	*timport;
Node	*init;
int	tref;
char	cycle;
char	cyc;
char	cycerr;
char	implicit;
Decl	*iface;
Decl	*locals;
Decl *link;
Inst	*pc;
Desc	*desc;
};
struct Desc
{
int	id;
uchar	used;
uchar	*map;
long	size;
long	nmap;
Desc	*next;
};
struct Dlist
{
Decl *d;
Dlist *next;
};
struct Except
{
Inst *p1;
Inst *p2;
Case *c;
Decl *d;
Node *zn;
Desc *desc;
int ne;
Except *next;
};
struct Sym
{
ushort	token;
char	*name;
int	len;
int	hash;
Sym	*next;
Decl	*decl;
Decl	*unbound;
};
enum
{
Oadd = 1,
Oaddas,
Oadr,
Oadtdecl,
Oalt,
Oand,
Oandand,
Oandas,
Oarray,
Oas,
Obreak,
Ocall,
Ocase,
Ocast,
Ochan,
Ocomma,
Ocomp,
Ocondecl,
Ocons,
Oconst,
Ocont,
Odas,
Odec,
Odiv,
Odivas,
Odo,
Odot,
Oelem,
Oeq,
Oexcept,
Oexdecl,
Oexit,
Oexp,
Oexpas,
Oexstmt,
Ofielddecl,
Ofnptr,
Ofor,
Ofunc,
Ogeq,
Ogt,
Ohd,
Oif,
Oimport,
Oinc,
Oind,
Oindex,
Oinds,
Oindx,
Oinv,
Ojmp,
Olabel,
Olen,
Oleq,
Oload,
Olsh,
Olshas,
Olt,
Omdot,
Omod,
Omodas,
Omoddecl,
Omul,
Omulas,
Oname,
Oneg,
Oneq,
Onot,
Onothing,
Oor,
Ooras,
Ooror,
Opick,
Opickdecl,
Opredec,
Opreinc,
Oraise,
Orange,
Orcv,
Oref,
Oret,
Orsh,
Orshas,
Oscope,
Oself,
Oseq,
Oslice,
Osnd,
Ospawn,
Osub,
Osubas,
Otagof,
Otl,
Otuple,
Otype,
Otypedecl,
Oused,
Ovardecl,
Ovardecli,
Owild,
Oxor,
Oxoras,
Oend
};
enum
{
Mas,
Mcons,
Mhd,
Mtl,
Mend
};
enum
{
Rreg,
Rmreg,
Roff,
Rnoff,
Rdesc,
Rdescp,
Rconst,
Ralways,
Radr,
Rmadr,
Rcant,
Rpc,
Rmpc,
Rareg,
Ramreg,
Raadr,
Ramadr,
Rldt,
Rend
};
#define PARENS	1
#define TEMP		2
#define FNPTRA	4
#define FNPTR2	8
#define FNPTRN	16
#define FNPTR		(FNPTRA|FNPTR2|FNPTRN)
struct Node
{
Src	src;
uchar	op;
uchar	addable;
uchar	flags;
uchar	temps;
Node	*left;
Node	*right;
Type	*ty;
Decl	*decl;
Long	val;
Real	rval;
};
enum
{
Tnone	= 0,
Tadt,
Tadtpick,
Tarray,
Tbig,
Tbyte,
Tchan,
Treal,
Tfn,
Tint,
Tlist,
Tmodule,
Tref,
Tstring,
Ttuple,
Texception,
Tfix,
Tpoly,
Tainit,
Talt,
Tany,
Tarrow,
Tcase,
Tcasel,
Tcasec,
Tdot,
Terror,
Tgoto,
Tid,
Tiface,
Texcept,
Tinst,
Tend
};
enum
{
OKbind		= 1 << 0,
OKverify	= 1 << 1,
OKsized		= 1 << 2,
OKref		= 1 << 3,
OKclass		= 1 << 4,
OKcyc		= 1 << 5,
OKcycsize	= 1 << 6,
OKmodref	= 1 << 7,
OKmask		= 0xff,
TReq		= 1 << 0,
TRcom		= 1 << 1,
TRcyc		= 1 << 2,
TRvis		= 1 << 3,
};
#define	FULLARGS	1
#define	INST	2
#define	CYCLIC	4
#define	POLY	8
#define	NOPOLY	16
struct Type
{
Src	src;
uchar	kind;
uchar	varargs;
uchar	ok;
uchar	linkall;
uchar	rec;
uchar	cons;
uchar	align;
uchar	flags;
int	sbl;
long	sig;
long	size;
Decl	*decl;
Type	*tof;
Decl	*ids;
Decl	*tags;
Decl *polys;
Case	*cse;
Type	*teq;
Type	*tcom;
Teq	*eq;
Node *val;
union {
Node *eraises;
Typelist *tlist;
Tpair *tmap;
} u;
};
struct Teq
{
int	id;
Type	*ty;
Teq	*eq;
};
struct Tattr
{
char	isptr;
char	refable;
char	conable;
char	big;
char	vis;
};
enum {
Sother,
Sloop,
Sscope
};
struct Tpair
{
Type *t1;
Type *t2;
Tpair *nxt;
};
struct Typelist
{
Type *t;
Typelist *nxt;
};
Extern	Decl	**adts;
Extern	Sym	*anontupsym;
Extern	int	arrayz;
Extern	int	asmsym;
Extern	Biobuf	*bins[MaxInclude];
Extern	int	blocks;
Extern	Biobuf	*bout;
Extern	Biobuf	*bsym;
Extern	double	canonnan;
Extern	uchar	casttab[Tend][Tend];
Extern	long	constval;
Extern	Decl	*curfn;
Extern	char	debug[256];
Extern	Desc	*descriptors;
Extern	int	dontcompile;
Extern	int	dowarn;
Extern	char	*emitcode;
Extern	int	emitdyn;
Extern	int	emitstub;
Extern	char	*emittab;
Extern	int	errors;
Extern	char	escmap[256];
Extern	Inst	*firstinst;
Extern	long	fixss;
Extern	Decl	*fndecls;
Extern	Decl	**fns;
Extern	int	gendis;
Extern	Decl	*impdecl;
Extern	Dlist	*impdecls;
Extern	Decl	*impmods;
Extern	Decl	*iota;
Extern	uchar	isbyteinst[256];
Extern	int	isfatal;
Extern	int	isrelop[Oend];
Extern	uchar	isused[Oend];
Extern	Inst	*lastinst;
Extern	int	lenadts;
Extern	int	maxerr;
Extern	int	maxlabdep;
Extern	long	maxstack;
Extern	int	mustcompile;
Extern	int	oldcycles;
Extern	int	nadts;
Extern	int	newfnptr;
Extern	int	nfns;
Extern	Decl	*nildecl;
Extern	int	nlabel;
Extern	int	dontinline;
Extern	Line	noline;
Extern	Src	nosrc;
Extern	uchar	opcommute[Oend];
Extern	int	opind[Tend];
Extern	uchar	oprelinvert[Oend];
Extern	int	optims;
Extern	char	*outfile;
Extern	Type	*precasttab[Tend][Tend];
Extern	int	scope;
Extern	Decl	*selfdecl;
Extern	uchar	sideeffect[Oend];
Extern	char	*signdump;
Extern	int	superwarn;
Extern	char	*symfile;
Extern	Type	*tany;
Extern	Type	*tbig;
Extern	Type	*tbyte;
Extern	Type	*terror;
Extern	Type	*tint;
Extern	Type	*tnone;
Extern	Type	*treal;
Extern	Node	*tree;
Extern	Type	*tstring;
Extern	Type *texception;
Extern	Type	*tunknown;
Extern	Type *tfnptr;
Extern	Type	*rtexception;
Extern	char	unescmap[256];
Extern	Src	unifysrc;
Extern	Node	znode;
extern	int	*blockstack;
extern	int	blockdep;
extern	int	nblocks;
extern	File	**files;
extern	int	nfiles;
extern	uchar	chantab[Tend];
extern	uchar	disoptab[Oend+1][7];
extern	char	*instname[];
extern	char	*kindname[Tend];
extern	uchar	movetab[Mend][Tend];
extern	char	*opname[];
extern	int	setisbyteinst[];
extern	int	setisused[];
extern	int	setsideeffect[];
extern	char	*storename[Dend];
extern	int	storespace[Dend];
extern	Tattr	tattr[Tend];
#include "fns.h"
#pragma varargck	type	"D"	Decl*
#pragma varargck	type	"I"	Inst*
#pragma varargck	type	"K"	Decl*
#pragma varargck	type	"k"	Decl*
#pragma varargck	type	"L"	Line
#pragma varargck	type	"M"	Desc*
#pragma varargck	type	"n"	Node*
#pragma varargck	type	"O"	int
#pragma varargck	type	"O"	uint
#pragma varargck	type	"g"	double
#pragma varargck	type	"Q"	Node*
#pragma varargck	type	"R"	Type*
#pragma varargck	type	"T"	Type*
#pragma varargck	type	"t"	Type*
#pragma varargck	type	"U"	Src
#pragma varargck	type	"v"	Node*
#pragma	varargck	type	"V"	Node*