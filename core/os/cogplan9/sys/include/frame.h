#pragma	src	"/sys/src/libframe"
#pragma	lib	"libframe.a"
typedef struct Frbox Frbox;
typedef struct Frame Frame;
enum{
BACK,
HIGH,
BORD,
TEXT,
HTEXT,
NCOL
};
#define	FRTICKW	3
struct Frbox
{
long		wid;
long		nrune;
union{
uchar	*ptr;
struct{
short	bc;
short	minwid;
};
};
};
struct Frame
{
Font		*font;
Display		*display;
Image		*b;
Image		*cols[NCOL];
Rectangle	r;
Rectangle	entire;
void			(*scroll)(Frame*, int);
Frbox		*box;
ulong		p0, p1;
ushort		nbox, nalloc;
ushort		maxtab;
ushort		nchars;
ushort		nlines;
ushort		maxlines;
ushort		lastlinefull;
ushort		modified;
Image		*tick;
Image		*tickback;
int			ticked;
};
ulong	frcharofpt(Frame*, Point);
Point	frptofchar(Frame*, ulong);
int	frdelete(Frame*, ulong, ulong);
void	frinsert(Frame*, Rune*, Rune*, ulong);
void	frselect(Frame*, Mousectl*);
void	frselectpaint(Frame*, Point, Point, Image*);
void	frdrawsel(Frame*, Point, ulong, ulong, int);
Point frdrawsel0(Frame*, Point, ulong, ulong, Image*, Image*);
void	frinit(Frame*, Rectangle, Font*, Image*, Image**);
void	frsetrects(Frame*, Rectangle, Image*);
void	frclear(Frame*, int);
uchar	*_frallocstr(Frame*, unsigned);
void	_frinsure(Frame*, int, unsigned);
Point	_frdraw(Frame*, Point);
void	_frgrowbox(Frame*, int);
void	_frfreebox(Frame*, int, int);
void	_frmergebox(Frame*, int);
void	_frdelbox(Frame*, int, int);
void	_frsplitbox(Frame*, int, int);
int	_frfindbox(Frame*, int, ulong, ulong);
void	_frclosebox(Frame*, int, int);
int	_frcanfit(Frame*, Point, Frbox*);
void	_frcklinewrap(Frame*, Point*, Frbox*);
void	_frcklinewrap0(Frame*, Point*, Frbox*);
void	_fradvance(Frame*, Point*, Frbox*);
int	_frnewwid(Frame*, Point, Frbox*);
int	_frnewwid0(Frame*, Point, Frbox*);
void	_frclean(Frame*, Point, int, int);
void	_frdrawtext(Frame*, Point, Image*, Image*);
void	_fraddbox(Frame*, int, int);
Point	_frptofcharptb(Frame*, ulong, Point, int);
Point	_frptofcharnb(Frame*, ulong, int);
int	_frstrlen(Frame*, int);
void	frtick(Frame*, Point, int);
void	frinittick(Frame*);
void	frredraw(Frame*);
#define	NRUNE(b)	((b)->nrune<0? 1 : (b)->nrune)
#define	NBYTE(b)	strlen((char*)(b)->ptr)