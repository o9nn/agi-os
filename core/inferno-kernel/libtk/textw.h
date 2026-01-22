typedef struct TkText TkText;
typedef struct TkTitem TkTitem;
typedef struct TkTline TkTline;
typedef struct TkTindex TkTindex;
typedef struct TkTmarkinfo TkTmarkinfo;
typedef struct TkTtaginfo TkTtaginfo;
typedef struct TkTwind TkTwind;
enum
{
TkTascii,
TkTrune,
TkTtab,
TkTnewline,
TkTcontline,
TkTwin,
TkTmark,
TkTbyitem = 0,
TkTbyitemback,
TkTbytline,
TkTbytlineback,
TkTbychar,
TkTbycharback,
TkTbycharstart,
TkTbyline,
TkTbylineback,
TkTbylinestart,
TkTbylineend,
TkTbywordstart,
TkTbywordend,
TkTbywrapstart,
TkTbywrapend,
TkTselid	= 0,
TkTmaxtag	= 32,
Textwidth	= 40,
Textheight	= 10,
TkTfirst	= (1<<0),
TkTlast		= (1<<1),
TkTdrawn	= (1<<2),
TkTdlocked	= (1<<3),
TkTjustfoc	= (1<<4),
TkTnodrag		= (1<<5),
TkTunset 	= (1<<31),
TkTborderwidth	= 0,
TkTjustify,
TkTlmargin1,
TkTlmargin2,
TkTlmargin3,
TkTrmargin,
TkTspacing1,
TkTspacing2,
TkTspacing3,
TkToffset,
TkTunderline,
TkToverstrike,
TkTrelief,
TkTwrap,
TkTlineheight,
TkTnumopts
};
struct TkTline
{
Point		orig;
int		width;
int		height;
int		ascent;
int		flags;
TkTitem*	items;
TkTline*	next;
TkTline*	prev;
};
struct TkText
{
TkTline		start;
TkTline		end;
Tk*			tagshare;
TkTtabstop*	tabs;
TkTtaginfo*	tags;
TkTmarkinfo*	marks;
char*		xscroll;
char*		yscroll;
uchar		selunit;
uchar		tflag;
int			nlines;
TkTitem*	selfirst;
TkTitem*	sellast;
Point		deltatv;
Point		deltaiv;
Point		current;
Point		track;
int		nexttag;
TkTitem*	mouse;
int		inswidth;
int		sborderwidth;
int		opts[TkTnumopts];
int		propagate;
int		scrolltop[2];
int		scrollbot[2];
Image*		image;
uchar		cur_flag;
Rectangle	cur_rec;
};
struct TkTwind
{
Tk*		sub;
Tk*		focus;
int		width;
int		height;
int		owned;
int		align;
char*		create;
int		padx;
int		pady;
int		ascent;
int		stretch;
};
struct TkTitem
{
uchar		kind;
uchar		tagextra;
short		width;
TkTitem		*next;
union	{
char*		string;
TkTwind*	win;
TkTmarkinfo*	mark;
TkTline*	line;
} u;
ulong		tags[1];
};
struct TkTmarkinfo
{
char*		name;
int		gravity;
TkTitem*	cur;
TkTmarkinfo*	next;
};
struct TkTtaginfo
{
int		id;
char*		name;
TkEnv*		env;
TkTtabstop*	tabs;
TkTtaginfo*	next;
TkAction*	binds;
int		opts[TkTnumopts];
};
struct TkTindex
{
TkTitem*	item;
TkTline*	line;
int		pos;
};
extern	TkCmdtab	tkttagcmd[];
extern	TkCmdtab	tktmarkcmd[];
extern	TkCmdtab	tktwincmd[];
extern	void		tkfreetext(Tk*);
extern	char*		tktaddmarkinfo(TkText*, char*, TkTmarkinfo**);
extern	char*		tktaddtaginfo(Tk*, char*, TkTtaginfo**);
extern	int		tktadjustind(TkText*, int, TkTindex*);
extern	int		tktanytags(TkTitem*);
extern	Rectangle	tktbbox(Tk*, TkTindex*);
extern	void		tktdirty(Tk*);
extern	int		tktdispwidth(Tk*, TkTtabstop *tabs, TkTitem*, Font*, int, int, int);
extern	void		tktendind(TkText*, TkTindex*);
extern	char*	tktextcursor(Tk*, char*, char **);
extern	Tk*		tktextevent(Tk*, int, void*);
extern	Tk*		tktinwindow(Tk*, Point*);
extern	char*		tktextselection(Tk*, char*, char**);
extern	void		tktextsize(Tk*, int);
extern	TkTmarkinfo*	tktfindmark(TkTmarkinfo*, char*);
extern	int		tktfindsubitem(Tk*, TkTindex*);
extern	TkTtaginfo*	tktfindtag(TkTtaginfo*, char*);
extern	char*		tktfixgeom(Tk*, TkTline*, TkTline*, int);
extern	void		tktfreeitems(TkText*, TkTitem*, int);
extern	void		tktfreelines(TkText*, TkTline*, int);
extern	void		tktfreemarks(TkTmarkinfo*);
extern	void		tktfreetabs(TkTtabstop*);
extern	void		tktfreetags(TkTtaginfo*);
extern	int		tktindcompare(TkText*, TkTindex*, int, TkTindex*);
extern	int		tktindbefore(TkTindex*, TkTindex*);
extern	int		tktindrune(TkTindex*);
extern	char*		tktinsert(Tk*, TkTindex*, char*, TkTitem*);
extern	int	tktisbreak(int);
extern	void		tktitemind(TkTitem*, TkTindex*);
extern	char*		tktiteminsert(TkText*, TkTindex*, TkTitem*);
extern	TkTline*	tktitemline(TkTitem*);
extern	char*		tktindparse(Tk*, char**, TkTindex*);
extern	TkTitem*	tktlastitem(TkTitem*);
extern	int		tktlinenum(TkText*, TkTindex*);
extern	int		tktlinepos(TkText*, TkTindex*);
extern	int		tktmarkind(Tk*, char*, TkTindex*);
extern	char*		tktmarkmove(Tk*, TkTmarkinfo*, TkTindex*);
extern	char*		tktmarkparse(Tk*, char**, TkTmarkinfo**);
extern	int		tktmaxwid(TkTline*);
extern	char*		tktnewitem(int, int, TkTitem**);
extern	char*		tktnewline(int, TkTitem*, TkTline*, TkTline*, TkTline**);
extern	int		tktposcount(TkTitem*);
extern	TkTline*	tktprevwrapline(Tk*, TkTline*);
extern	void		tktremitem(TkText*, TkTindex*);
extern	int		tktsametags(TkTitem*, TkTitem*);
extern	char*		tktsplititem(TkTindex*);
extern	void		tktstartind(TkText*, TkTindex*);
extern	char*		tkttagchange(Tk*, int, TkTindex*, TkTindex*, int);
extern	int		tkttagbit(TkTitem*, int, int);
extern	void		tkttagcomb(TkTitem*, TkTitem*, int);
extern	int		tkttagind(Tk*, char*, int, TkTindex*);
extern	char*		tkttagname(TkText*, int);
extern	int		tkttagnrange(TkText*, int, TkTindex*, TkTindex*, TkTindex*, TkTindex*);
extern	void		tkttagopts(Tk*, TkTitem*, int*, TkEnv*, TkTtabstop **, int);
extern	char*		tkttagparse(Tk*, char**, TkTtaginfo**);
extern	int		tkttagset(TkTitem*, int);
extern	int		tktxyind(Tk*, int, int, TkTindex*);
extern	void		tktxtforgetsub(Tk*, Tk*);