enum {
Pcolours	= 256,
Pred		= 0,
Pgreen		= 1,
Pblue		= 2,
Pblack		= 0x00,
Pwhite		= 0xFF,
};
typedef struct Cursor Cursor;
struct	Cursor
{
Point	offset;
uchar	clr[2*16];
uchar	set[2*16];
};
typedef struct LCDconfig {
long	freq;
int	wbl;
int	vpw;
int	wbf;
int	ac;
ulong	flags;
ulong	notpdpar;
} LCDconfig;
enum {
ClockLow = 1<<11,
OELow = 1<<10,
HsyncLow = 1<<9,
VsyncLow = 1<<8,
DataLow = 1<<7,
Passive8 = 1<<4,
DualScan = 1<<3,
IsColour = 1<<2,
IsTFT = 1<<1,
};
typedef struct Mode {
int	x;
int	y;
int	d;
uchar*	aperture;
int	apsize;
LCDconfig	lcd;
} Mode;
int	archlcdmode(Mode*);
extern	Point	mousexy(void);
extern void	blankscreen(int);