#include "a.out.h"
#pragma	src	"/sys/src/libmach"
#pragma	lib	"libmach.a"
enum
{
MMIPS,
MSPARC,
M68020,
MI386,
MI960,
M3210,
MMIPS2,
NMIPS2,
M29000,
MARM,
MPOWER,
MALPHA,
NMIPS,
MSPARC64,
MAMD64,
MPOWER64,
MARM64,
FNONE = 0,
FMIPS,
FMIPSB,
FSPARC,
FSPARCB,
F68020,
F68020B,
FNEXTB,
FI386,
FI386B,
FI960,
FI960B,
F3210,
FMIPS2BE,
F29000,
FARM,
FARMB,
FPOWER,
FPOWERB,
FMIPS2LE,
FALPHA,
FALPHAB,
FMIPSLE,
FSPARC64,
FAMD64,
FAMD64B,
FPOWER64,
FPOWER64B,
FARM64,
FARM64B,
ANONE = 0,
AMIPS,
AMIPSCO,
ASPARC,
ASUNSPARC,
A68020,
AI386,
AI8086,
AI960,
A29000,
AARM,
APOWER,
AALPHA,
ASPARC64,
AAMD64,
APOWER64,
AARM64,
Obj68020 = 0,
ObjSparc,
ObjMips,
Obj386,
Obj960,
Obj3210,
ObjMips2,
Obj29000,
ObjArm,
ObjPower,
ObjMips2le,
ObjAlpha,
ObjSparc64,
ObjAmd64,
ObjSpim,
ObjPower64,
ObjArm64,
Maxobjtype,
CNONE  = 0,
CAUTO,
CPARAM,
CSTAB,
CTEXT,
CDATA,
CANY,
};
typedef	struct	Map	Map;
typedef struct	Symbol	Symbol;
typedef	struct	Reglist	Reglist;
typedef	struct	Mach	Mach;
typedef	struct	Machdata Machdata;
struct Map {
int	nsegs;
struct segment {
char	*name;
int	fd;
int	inuse;
int	cache;
uvlong	b;
uvlong	e;
vlong	f;
} seg[1];
};
struct Symbol {
void 	*handle;
struct {
char	*name;
vlong	value;
char	type;
char	class;
int	index;
};
};
struct Reglist {
char	*rname;
short	roffs;
char	rflags;
char	rformat;
};
enum {
RINT	= (0<<0),
RFLT	= (1<<0),
RRDONLY	= (1<<1),
};
struct Mach{
char	*name;
int	mtype;
Reglist *reglist;
long	regsize;
long	fpregsize;
char	*pc;
char	*sp;
char	*link;
char	*sbreg;
uvlong	sb;
int	pgsize;
uvlong	kbase;
uvlong	ktmask;
uvlong	utop;
int	pcquant;
int	szaddr;
int	szreg;
int	szfloat;
int	szdouble;
};
extern	Mach	*mach;
typedef uvlong	(*Rgetter)(Map*, char*);
typedef	void	(*Tracer)(Map*, uvlong, uvlong, Symbol*);
struct	Machdata {
uchar	bpinst[4];
short	bpsize;
ushort	(*swab)(ushort);
ulong	(*swal)(ulong);
uvlong	(*swav)(uvlong);
int	(*ctrace)(Map*, uvlong, uvlong, uvlong, Tracer);
uvlong	(*findframe)(Map*, uvlong, uvlong, uvlong, uvlong);
char*	(*excep)(Map*, Rgetter);
ulong	(*bpfix)(uvlong);
int	(*sftos)(char*, int, void*);
int	(*dftos)(char*, int, void*);
int	(*foll)(Map*, uvlong, Rgetter, uvlong*);
int	(*das)(Map*, uvlong, char, char*, int);
int	(*hexinst)(Map*, uvlong, char*, int);
int	(*instsize)(Map*, uvlong);
};
typedef struct Fhdr
{
char	*name;
uchar	type;
uchar	hdrsz;
uchar	_magic;
uchar	spare;
long	magic;
uvlong	txtaddr;
vlong	txtoff;
uvlong	dataddr;
vlong	datoff;
vlong	symoff;
uvlong	entry;
vlong	sppcoff;
vlong	lnpcoff;
long	txtsz;
long	datsz;
long	bsssz;
long	symsz;
long	sppcsz;
long	lnpcsz;
} Fhdr;
extern	int	asstype;
extern	Machdata *machdata;
Map*		attachproc(int, int, int, Fhdr*);
int		beieee80ftos(char*, int, void*);
int		beieeesftos(char*, int, void*);
int		beieeedftos(char*, int, void*);
ushort		beswab(ushort);
ulong		beswal(ulong);
uvlong		beswav(uvlong);
uvlong		ciscframe(Map*, uvlong, uvlong, uvlong, uvlong);
int		cisctrace(Map*, uvlong, uvlong, uvlong, Tracer);
int		crackhdr(int fd, Fhdr*);
uvlong		file2pc(char*, long);
int		fileelem(Sym**, uchar *, char*, int);
long		fileline(char*, int, uvlong);
int		filesym(int, char*, int);
int		findlocal(Symbol*, char*, Symbol*);
int		findseg(Map*, char*);
int		findsym(uvlong, int, Symbol *);
int		fnbound(uvlong, uvlong*);
int		fpformat(Map*, Reglist*, char*, int, int);
int		get1(Map*, uvlong, uchar*, int);
int		get2(Map*, uvlong, ushort*);
int		get4(Map*, uvlong, ulong*);
int		get8(Map*, uvlong, uvlong*);
int		geta(Map*, uvlong, uvlong*);
int		getauto(Symbol*, int, int, Symbol*);
Sym*		getsym(int);
int		globalsym(Symbol *, int);
char*		_hexify(char*, ulong, int);
int		ieeesftos(char*, int, ulong);
int		ieeedftos(char*, int, ulong, ulong);
int		isar(Biobuf*);
int		leieee80ftos(char*, int, void*);
int		leieeesftos(char*, int, void*);
int		leieeedftos(char*, int, void*);
ushort		leswab(ushort);
ulong		leswal(ulong);
uvlong		leswav(uvlong);
uvlong		line2addr(long, uvlong, uvlong);
Map*		loadmap(Map*, int, Fhdr*);
int		localaddr(Map*, char*, char*, uvlong*, Rgetter);
int		localsym(Symbol*, int);
int		lookup(char*, char*, Symbol*);
void		machbytype(int);
int		machbyname(char*);
int		nextar(Biobuf*, int, char*);
Map*		newmap(Map*, int);
void		objtraverse(void(*)(Sym*, void*), void*);
int		objtype(Biobuf*, char**);
uvlong		pc2sp(uvlong);
long		pc2line(uvlong);
int		put1(Map*, uvlong, uchar*, int);
int		put2(Map*, uvlong, ushort);
int		put4(Map*, uvlong, ulong);
int		put8(Map*, uvlong, uvlong);
int		puta(Map*, uvlong, uvlong);
int		readar(Biobuf*, int, vlong, int);
int		readobj(Biobuf*, int);
uvlong		riscframe(Map*, uvlong, uvlong, uvlong, uvlong);
int		risctrace(Map*, uvlong, uvlong, uvlong, Tracer);
int		setmap(Map*, int, uvlong, uvlong, vlong, char*);
Sym*		symbase(long*);
int		syminit(int, Fhdr*);
int		symoff(char*, int, uvlong, int);
void		textseg(uvlong, Fhdr*);
int		textsym(Symbol*, int);
void		unusemap(Map*, int);