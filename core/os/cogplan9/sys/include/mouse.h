#pragma src "/sys/src/libdraw"
typedef struct Channel Channel;
typedef struct Cursor Cursor;
typedef struct Menu Menu;
typedef struct Mousectl Mousectl;
struct Mouse
{
int buttons;
Point xy;
ulong msec;
};
struct Mousectl
{
Mouse;
Channel *c;
Channel *resizec;
char *file;
int mfd;
int cfd;
int pid;
Image* image;
};
struct Menu
{
char **item;
char *(*gen)(int);
int lasthit;
};
extern Mousectl* initmouse(char*, Image*);
extern void moveto(Mousectl*, Point);
extern int readmouse(Mousectl*);
extern void closemouse(Mousectl*);
extern void setcursor(Mousectl*, Cursor*);
extern void drawgetrect(Rectangle, int);
extern Rectangle getrect(int, Mousectl*);
extern int menuhit(int, Mousectl*, Menu*, Screen*);