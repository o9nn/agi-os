#include <u.h>
#include <libc.h>
#include <bio.h>
# include <ctype.h>
# define MAXLIN 250
# define MAXHEAD 44
# define MAXCOL 30
# define MAXCHS 2000
#define MAXLINLEN 300
# define MAXRPT 100
# define CLLEN 10
# define SHORTLINE 4
extern int nlin, ncol, iline, nclin, nslin;
extern int (*style)[MAXHEAD];
extern char (*font)[MAXHEAD][2];
extern char (*csize)[MAXHEAD][4];
extern char (*vsize)[MAXHEAD][4];
extern char (*cll)[CLLEN];
extern int (*flags)[MAXHEAD];
# define ZEROW 001
# define HALFUP 002
# define CTOP 004
# define CDOWN 010
extern int stynum[];
extern int qcol;
extern int *doubled, *acase, *topat;
extern int F1, F2;
extern int (*lefline)[MAXHEAD];
extern int fullbot[];
extern char *instead[];
extern int expflg;
extern int ctrflg;
extern int evenflg;
extern int *evenup;
extern int boxflg;
extern int dboxflg;
extern int linsize;
extern int tab;
extern int pr1403;
extern int linsize, delim1, delim2;
extern int allflg;
extern int textflg;
extern int left1flg;
extern int rightl;
struct colstr {char *col, *rcol;};
extern struct colstr *table[];
extern char *cspace, *cstore;
extern char *exstore, *exlim, *exspace;
extern int *sep;
extern int *used, *lused, *rused;
extern int linestop[];
extern char *leftover;
extern char *last, *ifile;
extern int texname;
extern int texct, texmax;
extern char texstr[];
extern int linstart;
extern Biobuf *tabin, tabout;
# define CRIGHT 2
# define CLEFT 0
# define CMID 1
# define S1 31
# define S2 32
# define S3 33
# define TMP 38
#define S9 39
# define SF 35
# define SL 34
# define LSIZE 33
# define SIND 37
# define SVS 36
# define LEFT 1
# define RIGHT 2
# define THRU 3
# define TOP 1
# define BOT 2
int tbl(int argc,char *argv[]);
void setinp(int, char **);
int swapin(void);
void tableput(void);
void getcomm(void);
void backrest(char *);
void getspec(void);
void readspec(void);
int findcol(void);
void garray(int);
char *getcore(int, int);
void freearr(void);
void gettbl(void);
int nodata(int);
int oneh(int);
int vspand(int, int, int);
int vspen(char *);
void permute(void);
void maktab(void);
void wide(char *, char *, char *);
int filler(char *);
void runout(void);
void runtabs(int, int);
int ifline(char *);
void need(void);
void deftail(void);
void putline(int, int);
void puttext(char *, char *, char *);
void funnies(int, int);
void putfont(char *);
void putsize(char *);
void yetmore(void);
int domore(char *);
void checkuse(void);
int real(char *);
char *chspace(void);
int *alocv(int);
void release(void);
void choochar(void);
int point(char *);
void error(char *);
char *gets1(char *, int);
void un1getc(int);
int get1char(void);
void savefill(void);
void rstofill(void);
void endoff(void);
void freearr(void);
void saveline(void);
void ifdivert(void);
void restline(void);
void cleanfc(void);
int gettext(char *, int, int, char *, char *);
void untext(void);
int interv(int, int);
int interh(int, int);
int up1(int);
char *maknew(char *);
int ineqn (char *, char *);
char *reg(int, int);
int match (char *, char *);
int prefix(char *, char *);
int letter (int);
int numb(char *);
int digit(int);
int max(int, int);
void tcopy (char *, char *);
int ctype(int, int);
int min(int, int);
int fspan(int, int);
int lspan(int, int);
int ctspan(int, int);
void tohcol(int);
int allh(int);
int thish(int, int);
void makeline(int, int, int);
void fullwide(int, int);
void drawline(int, int, int, int, int, int);
void getstop(void);
int left(int, int, int *);
int lefdata(int, int);
int next(int);
int prev(int);
void drawvert(int, int, int, int);
int midbar(int, int);
int midbcol(int, int);
int barent(char *);