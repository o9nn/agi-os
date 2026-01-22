typedef struct Cursor Cursor;
enum {
CURSWID = 16,
CURSHGT = 16,
};
struct	Cursor
{
Point	offset;
uchar	clr[CURSWID/BI2BY*CURSHGT];
uchar	set[CURSWID/BI2BY*CURSHGT];
};
typedef struct Cursorinfo Cursorinfo;
struct Cursorinfo {
Cursor;
Lock;
};
extern void mousetrack(int, int, int, int);
extern Point mousexy(void);
extern void mouseaccelerate(int);
extern int m3mouseputc(Queue*, int);
extern int m5mouseputc(Queue*, int);
extern int mouseputc(Queue*, int);
extern Cursorinfo cursor;
extern Cursor arrow;
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
Pred		= 0,
Pgreen		= 1,
Pblue		= 2,
Pblack		= 0x00,
Pwhite		= 0xFF,
};
#define VGAMEM()	0xA0000
#define vgai(port)		inb(port)
#define vgao(port, data)	outb(port, data)
extern int vgaxi(long, uchar);
extern int vgaxo(long, uchar, uchar);
typedef struct VGAdev VGAdev;
typedef struct VGAcur VGAcur;
typedef struct VGAscr VGAscr;
struct VGAdev {
char*	name;
void	(*enable)(VGAscr*);
void	(*disable)(VGAscr*);
void	(*page)(VGAscr*, int);
ulong	(*linear)(VGAscr*, int*, int*);
void	(*drawinit)(VGAscr*);
int	(*fill)(VGAscr*, Rectangle, ulong);
void	(*ovlctl)(VGAscr*, Chan*, void*, int);
int	(*ovlwrite)(VGAscr*, void*, int, vlong);
void (*flush)(VGAscr*, Rectangle);
};
struct VGAcur {
char*	name;
void	(*enable)(VGAscr*);
void	(*disable)(VGAscr*);
void	(*load)(VGAscr*, Cursor*);
int	(*move)(VGAscr*, Point);
int	doespanning;
};
struct VGAscr {
Lock	devlock;
VGAdev*	dev;
VGAcur*	cur;
ulong	storage;
Cursor;
int	useflush;
ulong	aperture;
int	isupamem;
int	apsize;
ulong	io;
ulong	colormap[Pcolours][3];
int	palettedepth;
ulong	*mmio;
Memimage* gscreen;
Memdata* gscreendata;
Memsubfont* memdefont;
int	(*fill)(VGAscr*, Rectangle, ulong);
int	(*scroll)(VGAscr*, Rectangle, Rectangle);
void	(*blank)(VGAscr*, int);
ulong	id;
int isblank;
int overlayinit;
};
extern VGAscr vgascreen[];
enum {
Backgnd		= 0,
};
extern void mousectl(Cmdbuf*);
extern int		hwaccel;
extern int		hwblank;
extern int		panning;
extern void addvgaseg(char*, ulong, ulong);
extern uchar* attachscreen(Rectangle*, ulong*, int*, int*, int*);
extern void	flushmemscreen(Rectangle);
extern int	cursoron(int);
extern void	cursoroff(int);
extern void	setcursor(Cursor*);
extern int	screensize(int, int, int, ulong);
extern int	screenaperture(int, int);
extern Rectangle physgscreenr;
extern void	blankscreen(int);
extern void	deletescreenimage(void);
extern int		drawhasclients(void);
extern ulong	blanktime;
extern void	setscreenimageclipr(Rectangle);
extern void	drawflush(void);
extern int drawidletime(void);
extern void	vgascreenwin(VGAscr*);
extern void	vgaimageinit(ulong);
extern ulong	vgapcilinear(VGAscr*, int*, int*, int, int);
extern void	drawblankscreen(int);
extern void	vgablank(VGAscr*, int);