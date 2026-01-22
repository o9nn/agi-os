typedef struct TkLabel TkLabel;
struct TkLabel
{
char* text;
Image* bitmap;
TkImg* img;
int justify;
int anchor;
int w;
int h;
int textheight;
char* command;
char* value;
char* offvalue;
char* variable;
int ul;
int check;
int indicator;
char* menu;
char** values;
int nvalues;
};
enum {
Textpadx = 3,
Textpady = 0,
Bitpadx = 0,
Bitpady = 0,
CheckButton = 10,
CheckButtonBW = 1,
ButtonBorder = 4,
CheckSpace = CheckButton + 2*CheckButtonBW + 2*ButtonBorder,
};
extern TkOption tkbutopts[];
extern TkOption tkradopts[];
extern TkOption tkcbopts[];
extern void tksizelabel(Tk*);
extern char* tkdrawlabel(Tk*, Point);
extern void tkfreelabel(Tk*);
extern void tklabelgetimgs(Tk*, Image**, Image**);
extern char* tksetvar(TkTop*, char*, char*);
extern Tk* tkmkbutton(TkTop*, int);
extern void tksizebutton(Tk*);
extern char* tkdrawbutton(Tk*, Point);
extern char* tkbuttoninvoke(Tk*, char*, char**);
extern char* tkradioinvoke(Tk*, char*, char**);
extern void tkfreebutton(Tk*);
extern int tklabelmargin(Tk*);
extern int tkbuttonmargin(Tk*);