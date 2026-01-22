#include <stdio.h>
#include "tdef.h"
#include "fns.h"
#include "ext.h"
char termtab[NS];
char fontdir[NS];
char devname[20];
Numtab numtab[NN] = {
{ PAIR('%', 0) },
{ PAIR('n', 'l') },
{ PAIR('y', 'r') },
{ PAIR('h', 'p') },
{ PAIR('c', 't') },
{ PAIR('d', 'n') },
{ PAIR('m', 'o') },
{ PAIR('d', 'y') },
{ PAIR('d', 'w') },
{ PAIR('l', 'n') },
{ PAIR('d', 'l') },
{ PAIR('s', 't') },
{ PAIR('s', 'b') },
{ PAIR('c', '.') },
{ PAIR('$', '$') },
};
int alphabet = 256;
int pto = 10000;
int pfrom = 1;
int print = 1;
char nextf[NS] = TMACDIR;
char mfiles[NMF][NS];
int nmfi = 0;
int oldbits = -1;
int init = 1;
int fc = IMP;
int eschar = '\\';
int pl;
int po;
FILE *ptid = stdout;
int dfact = 1;
int dfactd = 1;
int res = 1;
int smnt = 0;
int ascii = 0;
int lg;
int pnlist[NPN] = { -1 };
int *pnp = pnlist;
int npn = 1;
int npnflg = 1;
int dpn = -1;
int totout = 1;
int ulfont = ULFONT;
int tabch = TAB;
int ldrch = LEADER;
Contab contab[NM] = {
C(PAIR('d', 's'), caseds),
C(PAIR('a', 's'), caseas),
C(PAIR('s', 'p'), casesp),
C(PAIR('f', 't'), caseft),
C(PAIR('p', 's'), caseps),
C(PAIR('v', 's'), casevs),
C(PAIR('n', 'r'), casenr),
C(PAIR('i', 'f'), caseif),
C(PAIR('i', 'e'), caseie),
C(PAIR('e', 'l'), caseel),
C(PAIR('p', 'o'), casepo),
C(PAIR('t', 'l'), casetl),
C(PAIR('t', 'm'), casetm),
C(PAIR('f', 'm'), casefm),
C(PAIR('b', 'p'), casebp),
C(PAIR('c', 'h'), casech),
C(PAIR('p', 'n'), casepn),
C(PAIR('b', 'r'), tbreak),
C(PAIR('t', 'i'), caseti),
C(PAIR('n', 'e'), casene),
C(PAIR('n', 'f'), casenf),
C(PAIR('c', 'e'), casece),
C(PAIR('f', 'i'), casefi),
C(PAIR('i', 'n'), casein),
C(PAIR('l', 'l'), casell),
C(PAIR('n', 's'), casens),
C(PAIR('m', 'k'), casemk),
C(PAIR('r', 't'), casert),
C(PAIR('a', 'm'), caseam),
C(PAIR('d', 'e'), casede),
C(PAIR('d', 'i'), casedi),
C(PAIR('d', 'a'), caseda),
C(PAIR('w', 'h'), casewh),
C(PAIR('d', 't'), casedt),
C(PAIR('i', 't'), caseit),
C(PAIR('r', 'm'), caserm),
C(PAIR('r', 'r'), caserr),
C(PAIR('r', 'n'), casern),
C(PAIR('a', 'd'), casead),
C(PAIR('r', 's'), casers),
C(PAIR('n', 'a'), casena),
C(PAIR('p', 'l'), casepl),
C(PAIR('t', 'a'), caseta),
C(PAIR('t', 'r'), casetr),
C(PAIR('u', 'l'), caseul),
C(PAIR('c', 'u'), casecu),
C(PAIR('l', 't'), caselt),
C(PAIR('n', 'x'), casenx),
C(PAIR('s', 'o'), caseso),
C(PAIR('i', 'g'), caseig),
C(PAIR('t', 'c'), casetc),
C(PAIR('f', 'c'), casefc),
C(PAIR('e', 'c'), caseec),
C(PAIR('e', 'o'), caseeo),
C(PAIR('l', 'c'), caselc),
C(PAIR('e', 'v'), caseev),
C(PAIR('r', 'd'), caserd),
C(PAIR('a', 'b'), caseab),
C(PAIR('f', 'l'), casefl),
C(PAIR('e', 'x'), caseex),
C(PAIR('s', 's'), casess),
C(PAIR('f', 'p'), casefp),
C(PAIR('c', 's'), casecs),
C(PAIR('b', 'd'), casebd),
C(PAIR('l', 'g'), caselg),
C(PAIR('h', 'c'), casehc),
C(PAIR('h', 'y'), casehy),
C(PAIR('n', 'h'), casenh),
C(PAIR('n', 'm'), casenm),
C(PAIR('n', 'n'), casenn),
C(PAIR('s', 'v'), casesv),
C(PAIR('o', 's'), caseos),
C(PAIR('l', 's'), casels),
C(PAIR('c', 'c'), casecc),
C(PAIR('c', '2'), casec2),
C(PAIR('e', 'm'), caseem),
C(PAIR('a', 'f'), caseaf),
C(PAIR('h', 'a'), caseha),
C(PAIR('h', 'w'), casehw),
C(PAIR('m', 'c'), casemc),
C(PAIR('p', 'm'), casepm),
C(PAIR('p', 'i'), casepi),
C(PAIR('u', 'f'), caseuf),
C(PAIR('p', 'c'), casepc),
C(PAIR('h', 't'), caseht),
C(PAIR('c', 'f'), casecf),
C(PAIR('s', 'y'), casesy),
C(PAIR('l', 'f'), caself),
C(PAIR('p', 't'), casept),
C(PAIR('g', 'd'), casegd),
};
Tbuf _oline;
Env env[NEV] = { {
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
PS,
PS,
PS,
PS,
FT,
FT,
1,
1,
1,
1,
1,
3,
1,
'.',
'\'',
OHC,
IMP,
1,
0,
HYPHALG,
-1,
0,
'.',
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
(Tchar *)0,
0,
0,
0,
0,
} };
Env *envp = env;
Numerr numerr;
Stack *frame, *stk, *ejl;
Stack *nxf;
int pipeflg;
int hflg;
int eqflg;
int xpts;
int ppts;
int pfont;
int mpts;
int mfont;
int cs;
int ccs;
int bd;
int stdi;
int quiet;
int stop;
char ibuf[IBUFSZ];
char xbuf[IBUFSZ];
char *ibufp;
char *xbufp;
char *eibuf;
char *xeibuf;
Tchar pbbuf[NC];
Tchar *pbp = pbbuf;
Tchar *lastpbp = pbbuf;
int nx;
int mflg;
Tchar ch = 0;
int ibf;
int ifi;
int iflg;
int rargc;
char **argp;
Ushort trtab[NTRTAB];
int lgf;
int copyf;
Offset ip;
int nlflg;
int donef;
int nflush;
int nfo;
int padc;
int raw;
int flss;
int nonumb;
int trap;
int tflg;
int ejf;
int dilev;
Offset offset;
int em;
int ds;
Offset woff;
int app;
int ndone;
int lead;
int ralss;
Offset nextb;
Tchar nrbits;
int nform;
int oldmn;
int newmn;
int macerr;
Offset apptr;
int diflg;
int evi;
int vflag;
int noscale;
int po1;
int nlist[NTRAP];
int mlist[NTRAP];
int evlist[EVLSZ];
int ev;
int tty;
int sfont = FT;
int sv;
int esc;
int widthp;
int xfont;
int setwdf;
int over;
int nhyp;
Tchar **hyp;
Tchar *olinep;
int dotT;
char *unlkp;
Wcache widcache[NWIDCACHE];
Diver d[NDI];
Diver *dip;
int c_hyphen;
int c_emdash;
int c_rule;
int c_minus;
int c_fi;
int c_fl;
int c_ff;
int c_ffi;
int c_ffl;
int c_acute;
int c_grave;
int c_under;
int c_rooten;
int c_boxrule;
int c_lefthand;
int c_dagger;
int c_isalnum;
Spnames spnames[] =
{
&c_hyphen, "hy",
&c_emdash, "em",
&c_rule, "ru",
&c_minus, "\\-",
&c_fi, "fi",
&c_fl, "fl",
&c_ff, "ff",
&c_ffi, "Fi",
&c_ffl, "Fl",
&c_acute, "aa",
&c_grave, "ga",
&c_under, "ul",
&c_rooten, "rn",
&c_boxrule, "br",
&c_lefthand, "lh",
&c_dagger, "dg",
&c_isalnum, "__",
0, 0
};
Tchar (*hmot)(void);
Tchar (*makem)(int i);
Tchar (*setabs)(void);
Tchar (*setch)(int c);
Tchar (*sethl)(int k);
Tchar (*setht)(void);
Tchar (*setslant)(void);
Tchar (*vmot)(void);
Tchar (*xlss)(void);
int (*findft)(int i);
int (*width)(Tchar j);
void (*mchbits)(void);
void (*ptlead)(void);
void (*ptout)(Tchar i);
void (*ptpause)(void);
void (*setfont)(int a);
void (*setps)(void);
void (*setwd)(void);