typedef struct TkCimeth TkCimeth;
typedef struct TkCitem TkCitem;
typedef struct TkCanvas TkCanvas;
typedef struct TkCline TkCline;
typedef struct TkCtag TkCtag;
typedef struct TkCpoints TkCpoints;
typedef struct TkCwind TkCwind;
struct TkCline
{
int arrow;
int shape[3];
int width;
Image* stipple;
Image* pen;
int arrowf;
int arrowl;
int capstyle;
int smooth;
int steps;
};
struct TkCwind
{
Tk* sub;
Tk* focus;
int width;
int height;
int flags;
};
struct TkCpoints
{
int npoint;
Point* parampt;
Point* drawpt;
Rectangle bb;
};
struct TkCitem
{
int id;
int type;
TkCpoints p;
TkEnv* env;
TkCitem* next;
TkName* tags;
TkCtag* stag;
};
struct TkCtag
{
TkCitem* item;
TkName* name;
TkCtag* taglist;
TkCtag* itemlist;
};
enum
{
TkCVline,
TkCVtext,
TkCVrect,
TkCVoval,
TkCVbitmap,
TkCVpoly,
TkCVwindow,
TkCVimage,
TkCVarc,
TkCselto = 0,
TkCselfrom,
TkCseladjust,
TkCbufauto = 0,
TkCbufnone,
TkCbufvisible,
TkCbufall,
TkCadd = 0,
TkCfind,
TkChash = 32,
TkCarrowf = (1<<0),
TkCarrowl = (1<<1),
Tknarrow = 6
};
struct TkCanvas
{
int close;
int confine;
int cleanup;
int scrollr[4];
Rectangle region;
Rectangle update;
Point view;
TkCitem* selection;
int width;
int height;
int sborderwidth;
int xscrolli;
int yscrolli;
char* xscroll;
char* yscroll;
int id;
TkCitem* head;
TkCitem* tail;
TkCitem* focus;
TkCitem* mouse;
TkCitem* grab;
TkName* current;
TkCtag curtag;
Image* image;
int ialloc;
Image* mask;
TkName* thash[TkChash];
int actions;
int actlim;
int buffer;
};
struct TkCimeth
{
char* name;
char* (*create)(Tk*, char *arg, char **val);
void (*draw)(Image*, TkCitem*, TkEnv*);
void (*free)(TkCitem*);
char* (*coord)(TkCitem*, char*, int, int);
char* (*cget)(TkCitem*, char*, char**);
char* (*conf)(Tk*, TkCitem*, char*);
int (*hit)(TkCitem*, Point);
};
extern TkCimeth tkcimethod[];
extern int cvslshape[];
extern Rectangle bbnil;
extern Rectangle huger;
extern char* tkcaddtag(Tk*, TkCitem*, int);
extern TkCtag* tkcfirsttag(TkCitem*, TkCtag*);
extern TkCtag* tkclasttag(TkCitem*, TkCtag*);
extern void tkcvsappend(TkCanvas*, TkCitem*);
extern TkCitem* tkcnewitem(Tk*, int, int);
extern void tkcvsfreeitem(TkCitem*);
extern Point tkcvsrelpos(Tk*);
extern Tk* tkcvsinwindow(Tk*, Point*);
extern char* tkcvstextdchar(Tk*, TkCitem*, char*);
extern char* tkcvstextindex(Tk*, TkCitem*, char*, char **val);
extern char* tkcvstextinsert(Tk*, TkCitem*, char*);
extern char* tkcvstexticursor(Tk*, TkCitem*, char*);
extern void tkmkpen(Image**, TkEnv*, Image*);
extern void tkcvstextfocus(Tk*, TkCitem*, int);
extern char* tkcvstextselect(Tk*, TkCitem*, char*, int);
extern void tkcvstextclr(Tk*);
extern Tk* tkcvsevent(Tk*, int, void*);
extern Point tkcvsanchor(Point, int, int, int);
extern void tkcvsdirty(Tk*);
extern void tkfreectag(TkCtag*);
extern char* tkparsepts(TkTop*, TkCpoints*, char**, int);
extern void tkfreepoint(TkCpoints*);
extern void tkxlatepts(Point*, int, int, int);
extern void tkpolybound(Point*, int, Rectangle*);
extern TkName* tkctaglook(Tk*, TkName*, char*);
extern void tkbbmax(Rectangle*, Rectangle*);
extern void tkcvssetdirty(Tk*);
extern char* tkcvslinecreat(Tk*, char *arg, char **val);
extern void tkcvslinedraw(Image*, TkCitem*, TkEnv*);
extern void tkcvslinefree(TkCitem*);
extern char* tkcvslinecoord(TkCitem*, char*, int, int);
extern char* tkcvslinecget(TkCitem*, char*, char**);
extern char* tkcvslineconf(Tk*, TkCitem*, char*);
extern int tkcvslinehit(TkCitem*, Point);
extern char* tkcvstextcreat(Tk*, char *arg, char **val);
extern void tkcvstextdraw(Image*, TkCitem*, TkEnv*);
extern void tkcvstextfree(TkCitem*);
extern char* tkcvstextcoord(TkCitem*, char*, int, int);
extern char* tkcvstextcget(TkCitem*, char*, char**);
extern char* tkcvstextconf(Tk*, TkCitem*, char*);
extern char* tkcvsrectcreat(Tk*, char *arg, char **val);
extern void tkcvsrectdraw(Image*, TkCitem*, TkEnv*);
extern void tkcvsrectfree(TkCitem*);
extern char* tkcvsrectcoord(TkCitem*, char*, int, int);
extern char* tkcvsrectcget(TkCitem*, char*, char**);
extern char* tkcvsrectconf(Tk*, TkCitem*, char*);
extern char* tkcvsovalcreat(Tk*, char *arg, char **val);
extern void tkcvsovaldraw(Image*, TkCitem*, TkEnv*);
extern void tkcvsovalfree(TkCitem*);
extern char* tkcvsovalcoord(TkCitem*, char*, int, int);
extern char* tkcvsovalcget(TkCitem*, char*, char**);
extern char* tkcvsovalconf(Tk*, TkCitem*, char*);
extern int tkcvsovalhit(TkCitem*, Point);
extern char* tkcvsarccreat(Tk*, char *arg, char **val);
extern void tkcvsarcdraw(Image*, TkCitem*, TkEnv*);
extern void tkcvsarcfree(TkCitem*);
extern char* tkcvsarccoord(TkCitem*, char*, int, int);
extern char* tkcvsarccget(TkCitem*, char*, char**);
extern char* tkcvsarcconf(Tk*, TkCitem*, char*);
extern char* tkcvsbitcreat(Tk*, char *arg, char **val);
extern void tkcvsbitdraw(Image*, TkCitem*, TkEnv*);
extern void tkcvsbitfree(TkCitem*);
extern char* tkcvsbitcoord(TkCitem*, char*, int, int);
extern char* tkcvsbitcget(TkCitem*, char*, char**);
extern char* tkcvsbitconf(Tk*, TkCitem*, char*);
extern char* tkcvswindcreat(Tk*, char *arg, char **val);
extern void tkcvswinddraw(Image*, TkCitem*, TkEnv*);
extern void tkcvswindfree(TkCitem*);
extern char* tkcvswindcoord(TkCitem*, char*, int, int);
extern char* tkcvswindcget(TkCitem*, char*, char**);
extern char* tkcvswindconf(Tk*, TkCitem*, char*);
extern char* tkcvspolycreat(Tk*, char *arg, char **val);
extern void tkcvspolydraw(Image*, TkCitem*, TkEnv*);
extern void tkcvspolyfree(TkCitem*);
extern char* tkcvspolycoord(TkCitem*, char*, int, int);
extern char* tkcvspolycget(TkCitem*, char*, char**);
extern char* tkcvspolyconf(Tk*, TkCitem*, char*);
extern int tkcvspolyhit(TkCitem*, Point);
extern char* tkcvsimgcreat(Tk*, char *arg, char **val);
extern void tkcvsimgdraw(Image*, TkCitem*, TkEnv*);
extern void tkcvsimgfree(TkCitem*);
extern char* tkcvsimgcoord(TkCitem*, char*, int, int);
extern char* tkcvsimgcget(TkCitem*, char*, char**);
extern char* tkcvsimgconf(Tk*, TkCitem*, char*);
extern TkCitem* tkcvsfindwin(Tk*);
extern void tkcvsforgetsub(Tk*, Tk*);