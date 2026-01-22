enum {
MiscW		= 0x03C2,
MiscR		= 0x03CC,
Status0		= 0x03C2,
Status1		= 0x03DA,
FeatureR	= 0x03CA,
FeatureW	= 0x03DA,
Seqx		= 0x03C4,
Crtx		= 0x03D4,
Grx		= 0x03CE,
Attrx		= 0x03C0,
PaddrW		= 0x03C8,
Pdata		= 0x03C9,
Pixmask		= 0x03C6,
PaddrR		= 0x03C7,
Pstatus		= 0x03C7,
Pcolours	= 256,
Red		= 0,
Green		= 1,
Blue		= 2,
Pblack		= 0x00,
Pwhite		= 0xFF,
};
enum {
RefFreq		= 14318180,
VgaFreq0	= 25175000,
VgaFreq1	= 28322000,
};
enum {
Namelen		= 32,
};
typedef struct Ctlr Ctlr;
typedef struct Vga Vga;
typedef struct Ctlr {
char	name[Namelen+1];
void	(*snarf)(Vga*, Ctlr*);
void	(*options)(Vga*, Ctlr*);
void	(*init)(Vga*, Ctlr*);
void	(*load)(Vga*, Ctlr*);
void	(*dump)(Vga*, Ctlr*);
char*	type;
ulong	flag;
Ctlr*	link;
} Ctlr;
enum {
Fsnarf		= 0x00000001,
Foptions	= 0x00000002,
Finit		= 0x00000004,
Fload		= 0x00000008,
Fdump		= 0x00000010,
Ferror		= 0x00000020,
Hpclk2x8	= 0x00000100,
Upclk2x8	= 0x00000200,
Henhanced	= 0x00000400,
Uenhanced	= 0x00000800,
Hpvram		= 0x00001000,
Upvram		= 0x00002000,
Hextsid		= 0x00004000,
Uextsid		= 0x00008000,
Hclk2		= 0x00010000,
Uclk2		= 0x00020000,
Hlinear		= 0x00040000,
Ulinear		= 0x00080000,
Hclkdiv		= 0x00100000,
Uclkdiv		= 0x00200000,
Hsid32		= 0x00400000,
};
typedef struct Attr Attr;
typedef struct Attr {
char*	attr;
char*	val;
Attr*	next;
} Attr;
typedef struct Mode {
char	type[Namelen+1];
char	size[Namelen+1];
char	chan[Namelen+1];
char name[Namelen+1];
int	frequency;
int	deffrequency;
int	x;
int	y;
int	z;
int	ht;
int	shb;
int	ehb;
int	shs;
int	ehs;
int	vt;
int	vrs;
int	vre;
int		vbs;
int		vbe;
ulong	videobw;
char	hsync;
char	vsync;
char	interlace;
Attr*	attr;
} Mode;
typedef struct Vga {
uchar	misc;
uchar	feature;
uchar	sequencer[256];
ushort	crt[256];
uchar	graphics[256];
uchar	attribute[256];
uchar	pixmask;
uchar	pstatus;
uchar	palette[Pcolours][3];
ulong	f[2];
ulong	d[2];
ulong	i[2];
ulong	m[2];
ulong	n[2];
ulong	p[2];
ulong	q[2];
ulong	r[2];
ulong	vma;
ulong	vmb;
ulong	apz;
ulong	vmz;
ulong	membw;
long	offset;
char*	bios;
Pcidev*	pci;
Mode*	mode;
ulong	virtx;
ulong	virty;
int	panning;
Ctlr*	ctlr;
Ctlr*	ramdac;
Ctlr*	clock;
Ctlr*	hwgc;
Ctlr* vesa;
Ctlr*	link;
int	linear;
Attr*	attr;
void*	private;
} Vga;
extern Ctlr tdfx;
extern Ctlr tdfxhwgc;
extern Ctlr ark2000pv;
extern Ctlr ark2000pvhwgc;
extern Ctlr att20c490;
extern Ctlr att20c491;
extern Ctlr att20c492;
extern uchar attdaci(uchar);
extern void attdaco(uchar, uchar);
extern Ctlr att21c498;
extern uchar bt485i(uchar);
extern void bt485o(uchar, uchar);
extern Ctlr bt485;
extern Ctlr ch9294;
extern void clgd54xxclock(Vga*, Ctlr*);
extern Ctlr clgd542x;
extern Ctlr clgd542xhwgc;
extern Ctlr clgd546x;
extern Ctlr clgd546xhwgc;
extern Ctlr ct65540;
extern Ctlr ct65545;
extern Ctlr ct65545hwgc;
extern Ctlr cyber938x;
extern Ctlr cyber938xhwgc;
extern int cflag;
extern int dflag;
extern Ctlr *ctlrs[];
extern ushort dacxreg[4];
extern char* dbattr(Attr*, char*);
extern int dbctlr(char*, Vga*);
extern Mode* dbmode(char*, char*, char*);
extern void dbdumpmode(Mode*);
extern void error(char*, ...);
extern void trace(char*, ...);
extern int vflag, Vflag;
extern Ctlr et4000;
extern Ctlr et4000hwgc;
extern Ctlr hiqvideo;
extern Ctlr hiqvideohwgc;
extern Ctlr i81x;
extern Ctlr i81xhwgc;
extern Ctlr ibm8514;
extern Ctlr icd2061a;
extern Ctlr ics2494;
extern Ctlr ics2494a;
extern Ctlr ics534x;
extern uchar inportb(long);
extern void outportb(long, uchar);
extern ushort inportw(long);
extern void outportw(long, ushort);
extern ulong inportl(long);
extern void outportl(long, ulong);
extern char* vgactlr(char*, char*);
extern void vgactlw(char*, char*);
extern char* readbios(long, long);
extern void dumpbios(long);
extern void error(char*, ...);
extern void* alloc(ulong);
extern void printitem(char*, char*);
extern void printreg(ulong);
extern void printflag(ulong);
extern void setpalette(int, int, int, int);
extern int curprintindex;
extern Ctlr mach32;
extern Ctlr mach64;
extern Ctlr mach64xx;
extern Ctlr mach64xxhwgc;
extern char* chanstr[];
extern void resyncinit(Vga*, Ctlr*, ulong, ulong);
extern void sequencer(Vga*, int);
extern void main(int, char*[]);
Biobuf stdout;
extern Ctlr mga2164w;
extern Ctlr mga2164whwgc;
extern Ctlr neomagic;
extern Ctlr neomagichwgc;
extern Ctlr nvidia;
extern Ctlr nvidiahwgc;
extern Ctlr radeon;
extern Ctlr radeonhwgc;
extern Ctlr palette;
typedef struct Pcidev Pcidev;
extern int pcicfgr8(Pcidev*, int);
extern int pcicfgr16(Pcidev*, int);
extern int pcicfgr32(Pcidev*, int);
extern void pcicfgw8(Pcidev*, int, int);
extern void pcicfgw16(Pcidev*, int, int);
extern void pcicfgw32(Pcidev*, int, int);
extern void pcihinv(Pcidev*);
extern Pcidev* pcimatch(Pcidev*, int, int);
extern Ctlr rgb524;
extern uchar (*rgb524mnxi)(Vga*, int);
extern void (*rgb524mnxo)(Vga*, int, uchar);
extern Ctlr rgb524mn;
extern Ctlr s3801;
extern Ctlr s3805;
extern Ctlr s3928;
extern Ctlr s3clock;
extern Ctlr s3generic;
extern Ctlr bt485hwgc;
extern Ctlr rgb524hwgc;
extern Ctlr s3hwgc;
extern Ctlr tvp3020hwgc;
extern Ctlr tvp3026hwgc;
extern Ctlr sc15025;
extern Ctlr stg1702;
extern Ctlr t2r4;
extern Ctlr t2r4hwgc;
extern void trio64clock(Vga*, Ctlr*);
extern Ctlr trio64;
extern uchar tvp3020i(uchar);
extern uchar tvp3020xi(uchar);
extern void tvp3020o(uchar, uchar);
extern void tvp3020xo(uchar, uchar);
extern Ctlr tvp3020;
extern Ctlr tvp3025;
extern Ctlr tvp3025clock;
extern uchar tvp3026xi(uchar);
extern void tvp3026xo(uchar, uchar);
extern Ctlr tvp3026;
extern Ctlr tvp3026clock;
extern uchar vgai(long);
extern uchar vgaxi(long, uchar);
extern void vgao(long, uchar);
extern void vgaxo(long, uchar, uchar);
extern Ctlr generic;
extern Ctlr vesa;
extern Ctlr softhwgc;
extern int dbvesa(Vga*);
extern Mode *dbvesamode(char*);
extern void vesatextmode(void);
extern Ctlr virge;
extern Ctlr vision864;
extern Ctlr vision964;
extern Ctlr vision968;
extern Ctlr vmware;
extern Ctlr vmwarehwgc;
extern Ctlr w30c516;
extern Ctlr mga4xx;
extern Ctlr mga4xxhwgc;
#pragma	varargck	argpos	error	1
#pragma	varargck	argpos	trace	1