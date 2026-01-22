#pragma src "/sys/src/libcontrol"
#pragma lib "libcontrol.a"
#pragma varargck argpos ctlprint 2
#pragma varargck argpos _ctlprint 2
typedef struct Control Control;
typedef struct Controlset Controlset;
typedef struct CParse CParse;
typedef struct CCache CCache;
typedef struct CCache CImage;
typedef struct CCache CFont;
enum
{
Ctlunknown,
Ctlbox,
Ctlbutton,
Ctlentry,
Ctlkeyboard,
Ctllabel,
Ctlmenu,
Ctlradio,
Ctlscribble,
Ctlslider,
Ctltabs,
Ctltext,
Ctltextbutton,
Ctltextbutton3,
Ctlgroup,
Ctlboxbox,
Ctlcolumn,
Ctlrow,
Ctlstack,
Ctltab,
Ntypes,
};
struct Controlset
{
Control *controls;
Image *screen;
Control *actives;
Control *focus;
Channel *ctl;
Channel *data;
Channel *kbdc;
Channel *mousec;
Channel *resizec;
Channel *resizeexitc;
Channel *csexitc;
Keyboardctl *keyboardctl;
Mousectl *mousectl;
int clicktotype;
};
struct Control
{
char *name;
Rectangle rect;
Rectangle size;
Channel *event;
Channel *data;
int type;
int hidden;
Controlset *controlset;
Image *screen;
char *format;
char wevent;
char wdata;
void (*ctl)(Control*, CParse*);
void (*mouse)(Control*, Mouse*);
void (*key)(Control*, Rune*);
void (*exit)(Control*);
void (*setsize)(Control*);
void (*activate)(Control*, int);
Control *nextactive;
Control *next;
};
struct CCache
{
union{
Image *image;
Font *font;
};
char *name;
int index;
int ref;
};
struct CParse
{
char str[256];
char *sender;
char *receiver;
int cmd;
char *pargs[32];
int iargs[32];
char **args;
int nargs;
};
enum
{
Aupperleft = 0,
Auppercenter,
Aupperright,
Acenterleft,
Acenter,
Acenterright,
Alowerleft,
Alowercenter,
Alowerright,
Nalignments
};
enum
{
_Ctlmaxsize = 10000,
};
extern char *ctltypenames[];
void _ctladdgroup(Control*, Control*);
void _ctlargcount(Control*, CParse*, int);
Control* _createctl(Controlset*, char*, uint, char*);
Rune* _ctlrunestr(char*);
char* _ctlstrrune(Rune*);
void _ctlputsnarf(Rune*);
Rune* _ctlgetsnarf(void);
int _ctlalignment(char*);
Point _ctlalignpoint(Rectangle, int, int, int);
void _ctlfocus(Control*, int);
void _activategroup(Control*);
void _deactivategroup(Control*);
int _ctllookup(char *s, char *tab[], int ntab);
void _ctlprint(Control *c, char *fmt, ...);
CImage* _getctlimage(char*);
void _setctlimage(Control*, CImage**, char*);
void _putctlimage(CImage*);
CFont* _getctlfont(char*);
void _putctlfont(CFont*);
CImage* _getctlfont(char*);
void _setctlfont(Control*, CImage**, char*);
void _putctlfont(CImage*);
CFont* _getctlfont(char*);
void _putctlfont(CFont*);
int namectlimage(Image*, char*);
int freectlimage(char*);
int namectlfont(Font*, char*);
int freectlfont(char*);
int ctlprint(Control*, char*, ...);
void initcontrols(void);
Controlset* newcontrolset(Image*, Channel*, Channel*, Channel*);
void closecontrolset(Controlset*);
void closecontrol(Control*);
void ctlerror(char*, ...);
Control* controlcalled(char*);
void* ctlmalloc(uint);
void* ctlrealloc(void*, uint);
char* ctlstrdup(char*);
void controlwire(Control*, char*, Channel*);
void activate(Control*);
void deactivate(Control*);
Control* createbox(Controlset*, char*);
Control* createbutton(Controlset*, char*);
Control* createcolumn(Controlset*, char*);
Control* createboxbox(Controlset*, char*);
Control* createentry(Controlset*, char*);
Control* createkeyboard(Controlset*, char*);
Control* createlabel(Controlset*, char*);
Control* createmenu(Controlset*, char*);
Control* createradiobutton(Controlset*, char*);
Control* createrow(Controlset*, char*);
Control* createscribble(Controlset*, char*);
Control* createslider(Controlset*, char*);
Control* createstack(Controlset*, char*);
Control* createtab(Controlset*, char*);
Control* createtext(Controlset*, char*);
Control* createtextbutton(Controlset*, char*);
Control* createtextbutton3(Controlset*, char*);
void resizecontrolset(Controlset*);
int _ctlsnarffd;
char *alignnames[];
int ctldeletequits;