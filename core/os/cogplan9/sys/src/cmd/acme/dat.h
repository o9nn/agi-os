enum
{
Qdir,
Qacme,
Qcons,
Qconsctl,
Qdraw,
Qeditout,
Qindex,
Qlabel,
Qnew,
QWaddr,
QWbody,
QWctl,
QWdata,
QWeditout,
QWerrors,
QWevent,
QWrdsel,
QWwrsel,
QWtag,
QWxdata,
QMAX,
};
enum
{
Blockincr = 256,
Maxblock = 8*1024,
NRange = 10,
Infinity = 0x7FFFFFFF,
};
typedef struct Block Block;
typedef struct Buffer Buffer;
typedef struct Command Command;
typedef struct Column Column;
typedef struct Dirlist Dirlist;
typedef struct Dirtab Dirtab;
typedef struct Disk Disk;
typedef struct Expand Expand;
typedef struct Fid Fid;
typedef struct File File;
typedef struct Elog Elog;
typedef struct Mntdir Mntdir;
typedef struct Range Range;
typedef struct Rangeset Rangeset;
typedef struct Reffont Reffont;
typedef struct Row Row;
typedef struct Runestr Runestr;
typedef struct Text Text;
typedef struct Timer Timer;
typedef struct Window Window;
typedef struct Xfid Xfid;
struct Runestr
{
Rune *r;
int nr;
};
struct Range
{
int q0;
int q1;
};
struct Block
{
uint addr;
union
{
uint n;
Block *next;
};
};
struct Disk
{
int fd;
uint addr;
Block *free[Maxblock/Blockincr+1];
};
Disk* diskinit(void);
Block* disknewblock(Disk*, uint);
void diskrelease(Disk*, Block*);
void diskread(Disk*, Block*, Rune*, uint);
void diskwrite(Disk*, Block**, Rune*, uint);
struct Buffer
{
uint nc;
Rune *c;
uint cnc;
uint cmax;
uint cq;
int cdirty;
uint cbi;
Block **bl;
uint nbl;
};
void bufinsert(Buffer*, uint, Rune*, uint);
void bufdelete(Buffer*, uint, uint);
uint bufload(Buffer*, uint, int, int*);
void bufread(Buffer*, uint, Rune*, uint);
void bufclose(Buffer*);
void bufreset(Buffer*);
struct Elog
{
short type;
uint q0;
uint nd;
uint nr;
Rune *r;
};
void elogterm(File*);
void elogclose(File*);
void eloginsert(File*, int, Rune*, int);
void elogdelete(File*, int, int);
void elogreplace(File*, int, int, Rune*, int);
void elogapply(File*);
struct File
{
Buffer;
Buffer delta;
Buffer epsilon;
Buffer *elogbuf;
Elog elog;
Rune *name;
int nname;
uvlong qidpath;
uint mtime;
int dev;
int unread;
int editclean;
int seq;
int mod;
Text *curtext;
Text **text;
int ntext;
int dumpid;
};
File* fileaddtext(File*, Text*);
void fileclose(File*);
void filedelete(File*, uint, uint);
void filedeltext(File*, Text*);
void fileinsert(File*, uint, Rune*, uint);
uint fileload(File*, uint, int, int*);
void filemark(File*);
void filereset(File*);
void filesetname(File*, Rune*, int);
void fileundelete(File*, Buffer*, uint, uint);
void fileuninsert(File*, Buffer*, uint, uint);
void fileunsetname(File*, Buffer*);
void fileundo(File*, int, uint*, uint*);
uint fileredoseq(File*);
enum
{
Columntag,
Rowtag,
Tag,
Body,
};
struct Text
{
File *file;
Frame;
Reffont *reffont;
uint org;
uint q0;
uint q1;
int what;
int tabstop;
Window *w;
Rectangle scrollr;
Rectangle lastsr;
Rectangle all;
Row *row;
Column *col;
uint eq0;
uint cq0;
int ncache;
int ncachealloc;
Rune *cache;
int nofill;
};
uint textbacknl(Text*, uint, uint);
uint textbsinsert(Text*, uint, Rune*, uint, int, int*);
int textbswidth(Text*, Rune);
int textclickmatch(Text*, int, int, int, uint*);
void textclose(Text*);
void textcolumnate(Text*, Dirlist**, int);
void textcommit(Text*, int);
void textconstrain(Text*, uint, uint, uint*, uint*);
void textdelete(Text*, uint, uint, int);
void textdoubleclick(Text*, uint*, uint*);
void textfill(Text*);
void textframescroll(Text*, int);
void textinit(Text*, File*, Rectangle, Reffont*, Image**);
void textinsert(Text*, uint, Rune*, uint, int);
uint textload(Text*, uint, char*, int);
Rune textreadc(Text*, uint);
void textredraw(Text*, Rectangle, Font*, Image*, int);
void textreset(Text*);
int textresize(Text*, Rectangle);
void textscrdraw(Text*);
void textscroll(Text*, int);
void textselect(Text*);
int textselect2(Text*, uint*, uint*, Text**);
int textselect23(Text*, uint*, uint*, Image*, int);
int textselect3(Text*, uint*, uint*);
void textsetorigin(Text*, uint, int);
void textsetselect(Text*, uint, uint);
void textshow(Text*, uint, uint, int);
void texttype(Text*, Rune);
struct Window
{
QLock;
Ref;
Text tag;
Text body;
Rectangle r;
uchar isdir;
uchar isscratch;
uchar filemenu;
uchar dirty;
uchar autoindent;
int id;
Range addr;
Range limit;
uchar nopen[QMAX];
uchar nomark;
uchar noscroll;
Range wrselrange;
int rdselfd;
Column *col;
Xfid *eventx;
char *events;
int nevents;
int owner;
int maxlines;
Dirlist **dlp;
int ndl;
int putseq;
int nincl;
Rune **incl;
Reffont *reffont;
QLock ctllock;
uint ctlfid;
char *dumpstr;
char *dumpdir;
int dumpid;
int utflastqid;
int utflastboff;
int utflastq;
};
void wininit(Window*, Window*, Rectangle);
void winlock(Window*, int);
void winlock1(Window*, int);
void winunlock(Window*);
void wintype(Window*, Text*, Rune);
void winundo(Window*, int);
void winsetname(Window*, Rune*, int);
void winsettag(Window*);
void winsettag1(Window*);
void wincommit(Window*, Text*);
int winresize(Window*, Rectangle, int);
void winclose(Window*);
void windelete(Window*);
int winclean(Window*, int);
void windirfree(Window*);
void winevent(Window*, char*, ...);
void winmousebut(Window*);
void winaddincl(Window*, Rune*, int);
void wincleartag(Window*);
char *winctlprint(Window*, char*, int);
struct Column
{
Rectangle r;
Text tag;
Row *row;
Window **w;
int nw;
int safe;
};
void colinit(Column*, Rectangle);
Window* coladd(Column*, Window*, Window*, int);
void colclose(Column*, Window*, int);
void colcloseall(Column*);
void colresize(Column*, Rectangle);
Text* colwhich(Column*, Point);
void coldragwin(Column*, Window*, int);
void colgrow(Column*, Window*, int);
int colclean(Column*);
void colsort(Column*);
void colmousebut(Column*);
struct Row
{
QLock;
Rectangle r;
Text tag;
Column **col;
int ncol;
};
void rowinit(Row*, Rectangle);
Column* rowadd(Row*, Column *c, int);
void rowclose(Row*, Column*, int);
Text* rowwhich(Row*, Point);
Column* rowwhichcol(Row*, Point);
void rowresize(Row*, Rectangle);
Text* rowtype(Row*, Rune, Point);
void rowdragcol(Row*, Column*, int but);
int rowclean(Row*);
void rowdump(Row*, char*);
int rowload(Row*, char*, int);
void rowloadfonts(char*);
struct Timer
{
int dt;
int cancel;
Channel *c;
Timer *next;
};
struct Command
{
int pid;
Rune *name;
int nname;
char *text;
char **av;
int iseditcmd;
Mntdir *md;
Command *next;
};
struct Dirtab
{
char *name;
uchar type;
uint qid;
uint perm;
};
struct Mntdir
{
int id;
int ref;
Rune *dir;
int ndir;
Mntdir *next;
int nincl;
Rune **incl;
};
struct Fid
{
int fid;
int busy;
int open;
Qid qid;
Window *w;
Dirtab *dir;
Fid *next;
Mntdir *mntdir;
int nrpart;
uchar rpart[UTFmax];
};
struct Xfid
{
void *arg;
Fcall;
Xfid *next;
Channel *c;
Fid *f;
uchar *buf;
int flushed;
};
void xfidctl(void *);
void xfidflush(Xfid*);
void xfidopen(Xfid*);
void xfidclose(Xfid*);
void xfidread(Xfid*);
void xfidwrite(Xfid*);
void xfidctlwrite(Xfid*, Window*);
void xfideventread(Xfid*, Window*);
void xfideventwrite(Xfid*, Window*);
void xfidindexread(Xfid*);
void xfidutfread(Xfid*, Text*, uint, int);
int xfidruneread(Xfid*, Text*, uint, uint);
struct Reffont
{
Ref;
Font *f;
};
Reffont *rfget(int, int, int, char*);
void rfclose(Reffont*);
struct Rangeset
{
Range r[NRange];
};
struct Dirlist
{
Rune *r;
int nr;
int wid;
};
struct Expand
{
uint q0;
uint q1;
Rune *name;
int nname;
char *bname;
int jump;
union{
Text *at;
Rune *ar;
};
int (*agetc)(void*, uint);
int a0;
int a1;
};
enum
{
BUFSIZE = Maxblock+IOHDRSZ,
RBUFSIZE = BUFSIZE/sizeof(Rune),
EVENTSIZE = 256,
Scrollwid = 12,
Scrollgap = 4,
Margin = 4,
Border = 2,
};
#define QID(w,q) ((w<<8)|(q))
#define WIN(q) ((((ulong)(q).path)>>8) & 0xFFFFFF)
#define FILE(q) ((q).path & 0xFF)
enum
{
FALSE,
TRUE,
XXX,
};
enum
{
Empty = 0,
Null = '-',
Delete = 'd',
Insert = 'i',
Replace = 'r',
Filename = 'f',
};
enum
{
Inactive = 0,
Inserting,
Collecting,
};
uint globalincref;
uint seq;
uint maxtab;
Display *display;
Image *screen;
Font *font;
Mouse *mouse;
Mousectl *mousectl;
Keyboardctl *keyboardctl;
Reffont reffont;
Image *modbutton;
Image *colbutton;
Image *button;
Image *but2col;
Image *but3col;
Cursor boxcursor;
Row row;
int timerpid;
Disk *disk;
Text *seltext;
Text *argtext;
Text *mousetext;
Text *typetext;
Text *barttext;
int bartflag;
Window *activewin;
Column *activecol;
Buffer snarfbuf;
Rectangle nullrect;
int fsyspid;
char *cputype;
char *objtype;
char *home;
char *fontnames[2];
char acmeerrorfile[128];
Image *tagcols[NCOL];
Image *textcols[NCOL];
int plumbsendfd;
int plumbeditfd;
char wdir[];
int editing;
int messagesize;
int globalautoindent;
enum
{
Kscrolloneup = KF|0x20,
Kscrollonedown = KF|0x21,
};
Channel *cplumb;
Channel *cwait;
Channel *ccommand;
Channel *ckill;
Channel *cxfidalloc;
Channel *cxfidfree;
Channel *cnewwindow;
Channel *mouseexit0;
Channel *mouseexit1;
Channel *cexit;
Channel *cerr;
Channel *cedit;
Channel *cwarn;
#define STACK 8192