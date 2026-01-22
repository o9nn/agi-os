#pragma	src	"/sys/src/libmemdraw"
#pragma	lib	"libmemdraw.a"
typedef struct	Memimage Memimage;
typedef struct	Memdata Memdata;
typedef struct	Memsubfont Memsubfont;
typedef struct	Memlayer Memlayer;
typedef struct	Memcmap Memcmap;
typedef struct	Memdrawparam	Memdrawparam;
#pragma incomplete Memlayer
struct Memdata
{
ulong	*base;
uchar	*bdata;
int		ref;
void*	imref;
int		allocd;
};
enum {
Frepl		= 1<<0,
Fsimple	= 1<<1,
Fgrey	= 1<<2,
Falpha	= 1<<3,
Fcmap	= 1<<4,
Fbytes	= 1<<5,
};
struct Memimage
{
Rectangle	r;
Rectangle	clipr;
int		depth;
int		nchan;
ulong	chan;
Memcmap	*cmap;
Memdata	*data;
int		zero;
ulong	width;
Memlayer	*layer;
ulong	flags;
int		shift[NChan];
int		mask[NChan];
int		nbits[NChan];
};
struct Memcmap
{
uchar	cmap2rgb[3*256];
uchar	rgb2cmap[16*16*16];
};
struct	Memsubfont
{
char		*name;
short	n;
uchar	height;
char	ascent;
Fontchar *info;
Memimage	*bits;
};
enum {
Simplesrc=1<<0,
Simplemask=1<<1,
Replsrc=1<<2,
Replmask=1<<3,
Fullmask=1<<4,
};
struct	Memdrawparam
{
Memimage *dst;
Rectangle	r;
Memimage *src;
Rectangle sr;
Memimage *mask;
Rectangle mr;
int op;
ulong state;
ulong mval;
ulong mrgba;
ulong sval;
ulong srgba;
ulong sdval;
};
extern Memimage*	allocmemimage(Rectangle, ulong);
extern Memimage*	allocmemimaged(Rectangle, ulong, Memdata*);
extern Memimage*	readmemimage(int);
extern Memimage*	creadmemimage(int);
extern int	writememimage(int, Memimage*);
extern void	freememimage(Memimage*);
extern int		loadmemimage(Memimage*, Rectangle, uchar*, int);
extern int		cloadmemimage(Memimage*, Rectangle, uchar*, int);
extern int		unloadmemimage(Memimage*, Rectangle, uchar*, int);
extern ulong*	wordaddr(Memimage*, Point);
extern uchar*	byteaddr(Memimage*, Point);
extern int		drawclip(Memimage*, Rectangle*, Memimage*, Point*, Memimage*, Point*, Rectangle*, Rectangle*);
extern void	memfillcolor(Memimage*, ulong);
extern int		memsetchan(Memimage*, ulong);
extern void	memdraw(Memimage*, Rectangle, Memimage*, Point, Memimage*, Point, int);
extern void	memline(Memimage*, Point, Point, int, int, int, Memimage*, Point, int);
extern void	mempoly(Memimage*, Point*, int, int, int, int, Memimage*, Point, int);
extern void	memfillpoly(Memimage*, Point*, int, int, Memimage*, Point, int);
extern void	_memfillpolysc(Memimage*, Point*, int, int, Memimage*, Point, int, int, int, int);
extern void	memimagedraw(Memimage*, Rectangle, Memimage*, Point, Memimage*, Point, int);
extern int	hwdraw(Memdrawparam*);
extern void	memimageline(Memimage*, Point, Point, int, int, int, Memimage*, Point, int);
extern void	_memimageline(Memimage*, Point, Point, int, int, int, Memimage*, Point, Rectangle, int);
extern Point	memimagestring(Memimage*, Point, Memimage*, Point, Memsubfont*, char*);
extern void	memellipse(Memimage*, Point, int, int, int, Memimage*, Point, int);
extern void	memarc(Memimage*, Point, int, int, int, Memimage*, Point, int, int, int);
extern Rectangle	memlinebbox(Point, Point, int, int, int);
extern int	memlineendsize(int);
extern void	_memmkcmap(void);
extern void	memimageinit(void);
extern Memsubfont*	allocmemsubfont(char*, int, int, int, Fontchar*, Memimage*);
extern Memsubfont*	openmemsubfont(char*);
extern void	freememsubfont(Memsubfont*);
extern Point	memsubfontwidth(Memsubfont*, char*);
extern Memsubfont*	getmemdefont(void);
extern	Memimage*	memwhite;
extern	Memimage*	memblack;
extern	Memimage*	memopaque;
extern	Memimage*	memtransparent;
extern	Memcmap	*memdefcmap;
void		memimagemove(void*, void*);
extern void	rdb(void);
extern int		iprint(char*, ...);
#pragma varargck argpos iprint 1
extern int		drawdebug;
#pragma varargck type "llb" vlong
#pragma varargck type "llb" uvlong
#pragma varargck type "lb" long
#pragma varargck type "lb" ulong
#pragma varargck type "b" int
#pragma varargck type "b" uint