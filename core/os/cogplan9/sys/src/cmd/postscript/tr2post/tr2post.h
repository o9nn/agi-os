#define MAXSPECHARS 512
#define MAXTOKENSIZE 128
#define CHARLIB "/sys/lib/troff/font/devutf/charlib"
extern int debug;
extern int fontsize;
extern int fontpos;
extern int resolution;
extern int minx;
extern int miny;
extern char devname[];
extern int devres;
extern int unitwidth;
extern char *printdesclang;
extern char *encoding;
extern int fontmnt;
extern char **fontmtab;
extern int curtrofffontid;
extern int troffontcnt;
extern BOOLEAN drawflag;
struct specname {
char *str;
struct specname *next;
};
struct charent {
char postfontid;
char postcharid;
short troffcharwidth;
char *name;
struct charent *next;
};
extern struct charent **build_char_list;
extern int build_char_cnt;
struct pfnament {
char *str;
int used;
};
struct psfent {
int start;
int end;
int offset;
int psftid;
};
struct troffont {
char *trfontid;
BOOLEAN special;
int spacewidth;
int psfmapsize;
struct psfent *psfmap;
struct charent *charent[NUMOFONTS][FONTSIZE];
};
extern struct troffont *troffontab;
extern struct charent spechars[];
void initialize(void);
void mountfont(int, char*);
int findtfn(char *, int);
void runeout(Rune);
void specialout(char *);
long nametorune(char *);
void conv(Biobufhdr *);
void hgoto(int);
void vgoto(int);
void hmot(int);
void vmot(int);
void draw(Biobufhdr *);
void devcntl(Biobufhdr *);
void notavail(char *);
void error(int, char *, ...);
void loadfont(int, char *);
void flushtext(void);
void t_charht(int);
void t_slant(int);
void startstring(void);
void endstring(void);
BOOLEAN pageon(void);
void setpsfont(int, int);
void settrfont(void);
int hash(char *, int);
BOOLEAN readDESC(void);
void finish(void);
void ps_include(Biobufhdr *, Biobufhdr *, int, int,
int, int, double, double, double, double,
double, double, double);
void picture(Biobufhdr *, char *);
void beginpath(char*, int);
void drawpath(char*, int);