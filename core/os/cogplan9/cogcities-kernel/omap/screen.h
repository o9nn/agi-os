typedef struct Cursor Cursor;
typedef struct Cursorinfo Cursorinfo;
typedef struct OScreen OScreen;
typedef struct Omap3fb Omap3fb;
typedef struct Settings Settings;
struct Cursorinfo
{
Cursor;
Lock;
};
extern Cursor arrow;
extern Cursorinfo cursor;
extern void mousetrack(int, int, int, int);
extern Point mousexy(void);
extern void mouseaccelerate(int);
extern void mouseresize(void);
extern uchar* attachscreen(Rectangle*, ulong*, int*, int*, int*);
extern void flushmemscreen(Rectangle);
extern int cursoron(int);
extern void cursoroff(int);
extern void setcursor(Cursor*);
extern int screensize(int, int, int, ulong);
extern int screenaperture(int, int);
extern Rectangle physgscreenr;
extern void blankscreen(int);
extern void swcursorinit(void);
extern void swcursorhide(void);
extern void swcursoravoid(Rectangle);
extern void swcursorunhide(void);
extern void deletescreenimage(void);
extern void resetscreenimage(void);
extern int drawhasclients(void);
extern ulong blanktime;
extern void setscreenimageclipr(Rectangle);
extern void drawflush(void);
extern int drawidletime(void);
extern QLock drawlock;
#define ishwimage(i) 0
enum {
Wid = 1280,
Ht = 1024,
Depth = 16,
Pcolours = 256,
Pred = 0,
Pgreen = 1,
Pblue = 2,
Pblack = 0x00,
Pwhite = 0xFF,
Res800x600 = 0,
Res1024x768,
Res1280x1024,
Res1400x1050,
};
struct Settings {
uint wid;
uint ht;
uint freq;
uint chan;
uint pixelclock;
uint hbp;
uint hfp;
uint hsw;
uint vbp;
uint vfp;
uint vsw;
};
struct OScreen {
Lock;
Cursor;
Settings *settings;
int open;
};
struct Omap3fb {
ushort pixel[Wid*Ht];
};