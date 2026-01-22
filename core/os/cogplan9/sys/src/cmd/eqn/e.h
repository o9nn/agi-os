#include <stdio.h>
#include <stdlib.h>
#include <string.h>
enum charclass {
OTHER, OLET, ILET, DIG, LPAR, RPAR, SLASH, PLUS, ILETF, ILETJ, VBAR,
NONE, LAST
};
extern int class[LAST][LAST];
#define dprintf if (dbg) printf
#define max(x,y) (((x) >= (y)) ? (x) : (y))
#define min(x,y) (((x) <= (y)) ? (x) : (y))
extern char errbuf[200];
extern char *cmdname;
#define ERROR sprintf(errbuf,
#define FATAL ), error(1, errbuf)
#define WARNING ), error(0, errbuf)
#define SYNTAX ), yyerror(errbuf)
#define ROM '1'
#define ITAL '2'
#define BLD '3'
#define BDIT '4'
#define DEFGAP -999
extern int dbg;
extern int ct;
extern int lp[];
extern int used[];
extern int ps;
extern int deltaps;
extern int dps_set;
extern int gsize;
extern int ft;
extern int display;
extern int synerr;
extern char *typesetter;
extern int minsize;
extern int ttype;
#define DEVCAT 1
#define DEV202 2
#define DEVAPS 3
#define DEVPOST 4
extern double eht[];
extern double ebase[];
extern int lfont[];
extern int rfont[];
extern int lclass[];
extern int rclass[];
extern int yyval;
extern int yylval;
extern int eqnreg;
extern double eqnht;
extern int lefteq, righteq;
extern int markline;
#define TBLSIZE 100
typedef struct s_tbl {
char *name;
char *cval;
int ival;
struct s_tbl *next;
} tbl;
extern char *spaceval;
#define String 01
#define Macro 02
#define File 04
#define Char 010
#define Free 040
typedef struct infile {
FILE *fin;
char *fname;
int lineno;
} Infile;
typedef struct {
int type;
char *sp;
} Src;
extern Src src[], *srcp;
#define MAXARGS 20
typedef struct {
char *argstk[MAXARGS];
char *argval;
} Arg;
typedef struct {
int ft;
char name[10];
} Font;
extern Font ftstack[];
extern Font *ftp;
extern int szstack[];
extern int nszstack;
extern Infile infile[10];
extern Infile *curfile;
extern tbl *lookup(tbl **tblp, char *name);
extern void install(tbl **tblp, char *name, char *cval, int ival);
extern tbl *keytbl[], *deftbl[], *restbl[], *ftunetbl[];
extern int salloc(void);
extern void sfree(int);
extern void nrwid(int, int, int);
extern char *ABSPS(int);
extern char *DPS(int, int);
extern int EFFPS(int);
extern double EM(double, int);
extern double REL(double, int);
extern char *pad(int);
extern void getstr(char *, int);
extern char *strsave(char *);
extern int input(void);
extern int unput(int);
extern void pbstr(char *);
extern void error(int, char *);
extern void yyerror(char *);
extern void diacrit(int, int);
extern void eqnbox(int, int, int);
extern void setfont(char *);
extern void font(int, int);
extern void globfont(void);
extern void fatbox(int);
extern void fromto(int, int, int);
extern void funny(int);
extern void integral(int, int, int);
extern void setintegral(void);
extern void pushsrc(int, char *);
extern void popsrc(void);
extern void putout(int);
extern void text(int, char *);
extern void subsup(int, int, int);
extern void bshiftb(int, int, int);
extern void shift2(int, int, int);
extern void setsize(char *);
extern void size(int, int);
extern void globsize(void);
extern void sqrt(int);
extern void text(int, char *);
extern void boverb(int, int);
extern void lineup(int);
extern void mark(int);
extern void paren(int, int, int);
extern void move(int, int, int);
extern void pile(int);
extern int startcol(int);
extern void column(int, int);
extern void matrix(int);