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
#define vgai(port)		inb(port)
#define vgao(port, data)	outb(port, data)
extern int vgaxi(long, uchar);
extern int vgaxo(long, uchar, uchar);
typedef struct Cursor Cursor;
struct	Cursor
{
Point	offset;
uchar	clr[2*16];
uchar	set[2*16];
};
typedef struct Mode {
int	x;
int	y;
int	d;
ulong	aperture;
int	apsize;
int	apshift;
} Mode;
typedef struct Vgac Vgac;
struct Vgac {
char*	name;
void	(*page)(int);
void	(*init)(Mode*);
int	(*ident)(void);
void	(*enable)(void);
void	(*disable)(void);
void	(*move)(int, int);
void	(*load)(Cursor*);
Vgac*	link;
};
extern void addvgaclink(Vgac*);