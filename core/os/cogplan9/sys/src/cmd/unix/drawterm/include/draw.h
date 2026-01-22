#ifdef PLAN9
#pragma src "/sys/src/libdraw"
#pragma lib "libdraw.a"
#endif
typedef struct Cachefont Cachefont;
typedef struct Cacheinfo Cacheinfo;
typedef struct Cachesubf Cachesubf;
typedef struct Display Display;
typedef struct Font Font;
typedef struct Fontchar Fontchar;
typedef struct Image Image;
typedef struct Mouse Mouse;
typedef struct Point Point;
typedef struct Rectangle Rectangle;
typedef struct RGB RGB;
typedef struct Screen Screen;
typedef struct Subfont Subfont;
#ifdef VARARGCK
#pragma varargck type "R" Rectangle
#pragma varargck type "P" Point
#endif
extern int Rfmt(Fmt*);
extern int Pfmt(Fmt*);
enum
{
DOpaque = 0xFFFFFFFF,
DTransparent = 0x00000000,
DBlack = 0x000000FF,
DWhite = 0xFFFFFFFF,
DRed = 0xFF0000FF,
DGreen = 0x00FF00FF,
DBlue = 0x0000FFFF,
DCyan = 0x00FFFFFF,
DMagenta = 0xFF00FFFF,
DYellow = 0xFFFF00FF,
DPaleyellow = 0xFFFFAAFF,
DDarkyellow = 0xEEEE9EFF,
DDarkgreen = 0x448844FF,
DPalegreen = 0xAAFFAAFF,
DMedgreen = 0x88CC88FF,
DDarkblue = 0x000055FF,
DPalebluegreen= 0xAAFFFFFF,
DPaleblue = 0x0000BBFF,
DBluegreen = 0x008888FF,
DGreygreen = 0x55AAAAFF,
DPalegreygreen = 0x9EEEEEFF,
DYellowgreen = 0x99994CFF,
DMedblue = 0x000099FF,
DGreyblue = 0x005DBBFF,
DPalegreyblue = 0x4993DDFF,
DPurpleblue = 0x8888CCFF,
DNotacolor = 0xFFFFFF00,
DNofill = DNotacolor,
};
enum
{
Displaybufsize = 8000,
ICOSSCALE = 1024,
Borderwidth = 4,
};
enum
{
Refbackup = 0,
Refnone = 1,
Refmesg = 2
};
#define NOREFRESH ((void*)-1)
enum
{
Endsquare = 0,
Enddisc = 1,
Endarrow = 2,
Endmask = 0x1F
};
#define ARROW(a, b, c) (Endarrow|((a)<<5)|((b)<<14)|((c)<<23))
typedef enum
{
Clear = 0,
SinD = 8,
DinS = 4,
SoutD = 2,
DoutS = 1,
S = SinD|SoutD,
SoverD = SinD|SoutD|DoutS,
SatopD = SinD|DoutS,
SxorD = SoutD|DoutS,
D = DinS|DoutS,
DoverS = DinS|DoutS|SoutD,
DatopS = DinS|SoutD,
DxorS = DoutS|SoutD,
Ncomp = 12,
} Drawop;
enum {
CRed = 0,
CGreen,
CBlue,
CGrey,
CAlpha,
CMap,
CIgnore,
NChan,
};
#define __DC(type, nbits) ((((type)&15)<<4)|((nbits)&15))
#define CHAN1(a,b) __DC(a,b)
#define CHAN2(a,b,c,d) (CHAN1((a),(b))<<8|__DC((c),(d)))
#define CHAN3(a,b,c,d,e,f) (CHAN2((a),(b),(c),(d))<<8|__DC((e),(f)))
#define CHAN4(a,b,c,d,e,f,g,h) (CHAN3((a),(b),(c),(d),(e),(f))<<8|__DC((g),(h)))
#define NBITS(c) ((c)&15)
#define TYPE(c) (((c)>>4)&15)
enum {
GREY1 = CHAN1(CGrey, 1),
GREY2 = CHAN1(CGrey, 2),
GREY4 = CHAN1(CGrey, 4),
GREY8 = CHAN1(CGrey, 8),
CMAP8 = CHAN1(CMap, 8),
RGB15 = CHAN4(CIgnore, 1, CRed, 5, CGreen, 5, CBlue, 5),
RGB16 = CHAN3(CRed, 5, CGreen, 6, CBlue, 5),
RGB24 = CHAN3(CRed, 8, CGreen, 8, CBlue, 8),
BGR24 = CHAN3(CBlue, 8, CGreen, 8, CRed, 8),
RGBA32 = CHAN4(CRed, 8, CGreen, 8, CBlue, 8, CAlpha, 8),
ARGB32 = CHAN4(CAlpha, 8, CRed, 8, CGreen, 8, CBlue, 8),
XRGB32 = CHAN4(CIgnore, 8, CRed, 8, CGreen, 8, CBlue, 8),
XBGR32 = CHAN4(CIgnore, 8, CBlue, 8, CGreen, 8, CRed, 8),
};
extern char* chantostr(char*, ulong);
extern ulong strtochan(char*);
extern int chantodepth(ulong);
struct Point
{
int x;
int y;
};
struct Rectangle
{
Point min;
Point max;
};
typedef void (*Reffn)(Image*, Rectangle, void*);
struct Screen
{
Display *display;
int id;
Image *image;
Image *fill;
};
struct Display
{
QLock qlock;
int locking;
int dirno;
int fd;
int reffd;
int ctlfd;
int imageid;
int local;
void (*error)(Display*, char*);
char *devdir;
char *windir;
char oldlabel[64];
ulong dataqid;
Image *white;
Image *black;
Image *opaque;
Image *transparent;
Image *image;
uchar *buf;
int bufsize;
uchar *bufp;
Font *defaultfont;
Subfont *defaultsubfont;
Image *windows;
Image *screenimage;
int _isnewdisplay;
};
struct Image
{
Display *display;
int id;
Rectangle r;
Rectangle clipr;
int depth;
ulong chan;
int repl;
Screen *screen;
Image *next;
};
struct RGB
{
ulong red;
ulong green;
ulong blue;
};
struct Fontchar
{
int x;
uchar top;
uchar bottom;
char left;
uchar width;
};
struct Subfont
{
char *name;
short n;
uchar height;
char ascent;
Fontchar *info;
Image *bits;
int ref;
};
enum
{
LOG2NFCACHE = 6,
NFCACHE = (1<<LOG2NFCACHE),
NFLOOK = 5,
NFSUBF = 2,
MAXFCACHE = 1024+NFLOOK,
MAXSUBF = 50,
DSUBF = 4,
SUBFAGE = 10000,
CACHEAGE = 10000
};
struct Cachefont
{
Rune min;
Rune max;
int offset;
char *name;
char *subfontname;
};
struct Cacheinfo
{
ushort x;
uchar width;
schar left;
Rune value;
ushort age;
};
struct Cachesubf
{
ulong age;
Cachefont *cf;
Subfont *f;
};
struct Font
{
char *name;
Display *display;
short height;
short ascent;
short width;
short nsub;
ulong age;
int maxdepth;
int ncache;
int nsubf;
Cacheinfo *cache;
Cachesubf *subf;
Cachefont **sub;
Image *cacheimage;
};
#define Dx(r) ((r).max.x-(r).min.x)
#define Dy(r) ((r).max.y-(r).min.y)
extern Image* _allocimage(Image*, Display*, Rectangle, ulong, int, ulong, int, int);
extern Image* allocimage(Display*, Rectangle, ulong, int, ulong);
extern uchar* bufimage(Display*, int);
extern int bytesperline(Rectangle, int);
extern void closedisplay(Display*);
extern void drawerror(Display*, char*);
extern int flushimage(Display*, int);
extern int freeimage(Image*);
extern int _freeimage1(Image*);
extern int geninitdraw(char*, void(*)(Display*, char*), char*, char*, char*, int);
extern int initdraw(void(*)(Display*, char*), char*, char*);
extern int newwindow(char*);
extern Display* initdisplay(char*, char*, void(*)(Display*, char*));
extern int loadimage(Image*, Rectangle, uchar*, int);
extern int cloadimage(Image*, Rectangle, uchar*, int);
extern int getwindow(Display*, int);
extern int gengetwindow(Display*, char*, Image**, Screen**, int);
extern Image* readimage(Display*, int, int);
extern Image* creadimage(Display*, int, int);
extern int unloadimage(Image*, Rectangle, uchar*, int);
extern int wordsperline(Rectangle, int);
extern int writeimage(int, Image*, int);
extern Image* namedimage(Display*, char*);
extern int nameimage(Image*, char*, int);
extern Image* allocimagemix(Display*, ulong, ulong);
extern void readcolmap(Display*, RGB*);
extern void writecolmap(Display*, RGB*);
extern ulong setalpha(ulong, uchar);
extern Screen* allocscreen(Image*, Image*, int);
extern Image* _allocwindow(Image*, Screen*, Rectangle, int, ulong);
extern Image* allocwindow(Screen*, Rectangle, int, ulong);
extern void bottomnwindows(Image**, int);
extern void bottomwindow(Image*);
extern int freescreen(Screen*);
extern Screen* publicscreen(Display*, int, ulong);
extern void topnwindows(Image**, int);
extern void topwindow(Image*);
extern int originwindow(Image*, Point, Point);
extern Point Pt(int, int);
extern Rectangle Rect(int, int, int, int);
extern Rectangle Rpt(Point, Point);
extern Point addpt(Point, Point);
extern Point subpt(Point, Point);
extern Point divpt(Point, int);
extern Point mulpt(Point, int);
extern int eqpt(Point, Point);
extern int eqrect(Rectangle, Rectangle);
extern Rectangle insetrect(Rectangle, int);
extern Rectangle rectaddpt(Rectangle, Point);
extern Rectangle rectsubpt(Rectangle, Point);
extern Rectangle canonrect(Rectangle);
extern int rectXrect(Rectangle, Rectangle);
extern int rectinrect(Rectangle, Rectangle);
extern void combinerect(Rectangle*, Rectangle);
extern int rectclip(Rectangle*, Rectangle);
extern int ptinrect(Point, Rectangle);
extern void replclipr(Image*, int, Rectangle);
extern int drawreplxy(int, int, int);
extern Point drawrepl(Rectangle, Point);
extern int rgb2cmap(int, int, int);
extern int cmap2rgb(int);
extern int cmap2rgba(int);
extern void icossin(int, int*, int*);
extern void icossin2(int, int, int*, int*);
extern void draw(Image*, Rectangle, Image*, Image*, Point);
extern void drawop(Image*, Rectangle, Image*, Image*, Point, Drawop);
extern void gendraw(Image*, Rectangle, Image*, Point, Image*, Point);
extern void gendrawop(Image*, Rectangle, Image*, Point, Image*, Point, Drawop);
extern void line(Image*, Point, Point, int, int, int, Image*, Point);
extern void lineop(Image*, Point, Point, int, int, int, Image*, Point, Drawop);
extern void poly(Image*, Point*, int, int, int, int, Image*, Point);
extern void polyop(Image*, Point*, int, int, int, int, Image*, Point, Drawop);
extern void fillpoly(Image*, Point*, int, int, Image*, Point);
extern void fillpolyop(Image*, Point*, int, int, Image*, Point, Drawop);
extern Point string(Image*, Point, Image*, Point, Font*, char*);
extern Point stringop(Image*, Point, Image*, Point, Font*, char*, Drawop);
extern Point stringn(Image*, Point, Image*, Point, Font*, char*, int);
extern Point stringnop(Image*, Point, Image*, Point, Font*, char*, int, Drawop);
extern Point runestring(Image*, Point, Image*, Point, Font*, Rune*);
extern Point runestringop(Image*, Point, Image*, Point, Font*, Rune*, Drawop);
extern Point runestringn(Image*, Point, Image*, Point, Font*, Rune*, int);
extern Point runestringnop(Image*, Point, Image*, Point, Font*, Rune*, int, Drawop);
extern Point stringbg(Image*, Point, Image*, Point, Font*, char*, Image*, Point);
extern Point stringbgop(Image*, Point, Image*, Point, Font*, char*, Image*, Point, Drawop);
extern Point stringnbg(Image*, Point, Image*, Point, Font*, char*, int, Image*, Point);
extern Point stringnbgop(Image*, Point, Image*, Point, Font*, char*, int, Image*, Point, Drawop);
extern Point runestringbg(Image*, Point, Image*, Point, Font*, Rune*, Image*, Point);
extern Point runestringbgop(Image*, Point, Image*, Point, Font*, Rune*, Image*, Point, Drawop);
extern Point runestringnbg(Image*, Point, Image*, Point, Font*, Rune*, int, Image*, Point);
extern Point runestringnbgop(Image*, Point, Image*, Point, Font*, Rune*, int, Image*, Point, Drawop);
extern Point _string(Image*, Point, Image*, Point, Font*, char*, Rune*, int, Rectangle, Image*, Point, Drawop);
extern Point stringsubfont(Image*, Point, Image*, Subfont*, char*);
extern int bezier(Image*, Point, Point, Point, Point, int, int, int, Image*, Point);
extern int bezierop(Image*, Point, Point, Point, Point, int, int, int, Image*, Point, Drawop);
extern int bezspline(Image*, Point*, int, int, int, int, Image*, Point);
extern int bezsplineop(Image*, Point*, int, int, int, int, Image*, Point, Drawop);
extern int bezsplinepts(Point*, int, Point**);
extern int fillbezier(Image*, Point, Point, Point, Point, int, Image*, Point);
extern int fillbezierop(Image*, Point, Point, Point, Point, int, Image*, Point, Drawop);
extern int fillbezspline(Image*, Point*, int, int, Image*, Point);
extern int fillbezsplineop(Image*, Point*, int, int, Image*, Point, Drawop);
extern void ellipse(Image*, Point, int, int, int, Image*, Point);
extern void ellipseop(Image*, Point, int, int, int, Image*, Point, Drawop);
extern void fillellipse(Image*, Point, int, int, Image*, Point);
extern void fillellipseop(Image*, Point, int, int, Image*, Point, Drawop);
extern void arc(Image*, Point, int, int, int, Image*, Point, int, int);
extern void arcop(Image*, Point, int, int, int, Image*, Point, int, int, Drawop);
extern void fillarc(Image*, Point, int, int, Image*, Point, int, int);
extern void fillarcop(Image*, Point, int, int, Image*, Point, int, int, Drawop);
extern void border(Image*, Rectangle, int, Image*, Point);
extern void borderop(Image*, Rectangle, int, Image*, Point, Drawop);
extern Font* openfont(Display*, char*);
extern Font* buildfont(Display*, char*, char*);
extern void freefont(Font*);
extern Font* mkfont(Subfont*, Rune);
extern int cachechars(Font*, char**, Rune**, ushort*, int, int*, char**);
extern void agefont(Font*);
extern Subfont* allocsubfont(char*, int, int, int, Fontchar*, Image*);
extern Subfont* lookupsubfont(Display*, char*);
extern void installsubfont(char*, Subfont*);
extern void uninstallsubfont(Subfont*);
extern void freesubfont(Subfont*);
extern Subfont* readsubfont(Display*, char*, int, int);
extern Subfont* readsubfonti(Display*, char*, int, Image*, int);
extern int writesubfont(int, Subfont*);
extern void _unpackinfo(Fontchar*, uchar*, int);
extern Point stringsize(Font*, char*);
extern int stringwidth(Font*, char*);
extern int stringnwidth(Font*, char*, int);
extern Point runestringsize(Font*, Rune*);
extern int runestringwidth(Font*, Rune*);
extern int runestringnwidth(Font*, Rune*, int);
extern Point strsubfontwidth(Subfont*, char*);
extern int loadchar(Font*, Rune, Cacheinfo*, int, int, char**);
extern char* subfontname(char*, char*, int);
extern Subfont* _getsubfont(Display*, char*);
extern Subfont* getdefont(Display*);
extern void lockdisplay(Display*);
extern void unlockdisplay(Display*);
extern int drawlsetrefresh(ulong, int, void*, void*);
extern uchar defontdata[];
extern int sizeofdefont;
extern Point ZP;
extern Rectangle ZR;
extern Display *display;
extern Font *font;
extern Screen *_screen;
extern int _cursorfd;
extern int _drawdebug;
extern void _setdrawop(Display*, Drawop);
#define BGSHORT(p) (((p)[0]<<0) | ((p)[1]<<8))
#define BGLONG(p) ((BGSHORT(p)<<0) | (BGSHORT(p+2)<<16))
#define BPSHORT(p, v) ((p)[0]=(v), (p)[1]=((v)>>8))
#define BPLONG(p, v) (BPSHORT(p, (v)), BPSHORT(p+2, (v)>>16))
#define NMATCH 3
#define NRUN (NMATCH+31)
#define NMEM 1024
#define NDUMP 128
#define NCBLOCK 6000
extern void _twiddlecompressed(uchar*, int);
extern int _compblocksize(Rectangle, int);
extern int log2[];
extern ulong drawld2chan[];
extern void drawsetdebug(int);