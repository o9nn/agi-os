#include <lib9.h>
#include <bio.h>
#include <ctype.h>
#define Bungetrune Bungetc
#define TBITSET ((32+NTERMS)/32)
#define BIT(a,i) ((a)[(i)>>5] & (1<<((i)&037)))
#define SETBIT(a,i) ((a)[(i)>>5] |= (1<<((i)&037)))
#define NWORDS(n) (((n)+32)/32)
#ifndef PARSER
#define PARSER "yaccpar"
#define PARSERS "yaccpars"
#endif
#define TEMPNAME "y.tmp.XXXXXX"
#define ACTNAME "y.acts.XXXXXX"
#define OFILE "tab.c"
#define FILEU "output"
#define FILED "tab.h"
#define FILEDEBUG "debug"
enum
{
ACTSIZE = 30000,
MEMSIZE = 30000,
NSTATES = 2000,
NTERMS = 255,
NPROD = 800,
NNONTERM = 300,
TEMPSIZE = 2000,
CNAMSZ = 5000,
LSETSIZE = 2400,
WSETSIZE = 350,
NAMESIZE = 50,
NTYPES = 63,
ISIZE = 400,
PRIVATE = 0xE000,
NTBASE = 010000,
ERRCODE = 8190,
ACCEPTCODE = 8191,
NOASC = 0,
LASC = 1,
RASC = 2,
BASC = 3,
DONE = 0,
MUSTDO = 1,
MUSTLOOKAHEAD = 2,
ACTFLAG = 04,
REDFLAG = 010,
YYFLAG1 = -1000,
IDENTIFIER = PRIVATE,
MARK,
TERM,
LEFT,
RIGHT,
BINARY,
PREC,
LCURLY,
IDENTCOLON,
NUMBER,
START,
TYPEDEF,
TYPENAME,
UNION,
ENDFILE = 0,
EMPTY = 1,
WHOKNOWS = 0,
OK = 1,
NOMORE = -1000,
};
#define ASSOC(i) ((i)&03)
#define PLEVEL(i) (((i)>>4)&077)
#define TYPE(i) (((i)>>10)&077)
#define SETASC(i,j) i |= j
#define SETPLEV(i,j) i |= (j<<4)
#define SETTYPE(i,j) i |= (j<<10)
#define TLOOP(i) for(i=1; i<=ntokens; i++)
#define NTLOOP(i) for(i=0; i<=nnonter; i++)
#define PLOOP(s,i) for(i=s; i<nprod; i++)
#define SLOOP(i) for(i=0; i<nstate; i++)
#define WSBUMP(x) x++
#define WSLOOP(s,j) for(j=s; j<cwp; j++)
#define ITMLOOP(i,p,q) for(q=pstate[i+1], p=pstate[i]; p<q; p++)
#define SETLOOP(i) for(i=0; i<tbitset; i++)
#define ZAPFILE(x) if(x) remove(x)
Biobuf* faction;
Biobuf* fdefine;
Biobuf* fdebug;
Biobuf* ftable;
Biobuf* ftemp;
Biobuf* finput;
Biobuf* foutput;
char* infile;
int numbval;
char tokname[NAMESIZE+4];
typedef
struct
{
int lset[TBITSET];
} Lkset;
typedef
struct
{
int* pitem;
Lkset* look;
} Item;
typedef
struct
{
char* name;
int value;
} Symb;
typedef
struct
{
int* pitem;
int flag;
Lkset ws;
} Wset;
char cnames[CNAMSZ];
int cnamsz = CNAMSZ;
char* cnamp = cnames;
int ndefout = 4;
char* tempname;
char* actname;
char ttempname[] = TEMPNAME;
char tactname[] = ACTNAME;
char* parser = PARSER;
char* yydebug;
char par[256];
int ntypes;
char* typeset[NTYPES];
int ntokens = 0 ;
Symb tokset[NTERMS];
int toklev[NTERMS];
int nnonter = -1;
Symb nontrst[NNONTERM];
int start;
int extval = 0;
char* ytabc = OFILE;
int mem0[MEMSIZE] ;
int* mem = mem0;
int nprod = 1;
int* prdptr[NPROD];
int levprd[NPROD];
int rlines[NPROD];
int nstate = 0;
Item* pstate[NSTATES+2];
int tystate[NSTATES];
int defact[NSTATES];
int tstates[NTERMS];
int ntstates[NNONTERM];
int mstates[NSTATES];
int lastred;
Lkset lkst[LSETSIZE];
int nolook;
int tbitset;
int nlset = 0;
int nolook = 0;
Lkset clset;
Wset wsets[WSETSIZE];
Wset* cwp;
int amem[ACTSIZE];
int* memp = amem;
int indgo[NSTATES];
int temp1[TEMPSIZE];
int lineno = 1;
int fatfl = 1;
int nerrors = 0;
int zzgoent;
int zzgobest;
int zzacent;
int zzexcp;
int zzclose;
int zzrrconf;
int zzsrconf;
int* ggreed = lkst[0].lset;
int* pgo = wsets[0].ws.lset;
int* yypgo = &nontrst[0].value;
int maxspr = 0;
int maxoff = 0;
int* pmem = mem0;
int* maxa;
int nxdb = 0;
int adb = 0;
int** pres[NNONTERM+2];
Lkset* pfirst[NNONTERM+2];
int pempty[NNONTERM+1];
int indebug = 0;
Wset* zzcwp = wsets;
int zzgoent = 0;
int zzgobest = 0;
int zzacent = 0;
int zzexcp = 0;
int zzclose = 0;
int zzsrconf = 0;
int* zzmemsz = mem0;
int zzrrconf = 0;
int pidebug = 0;
int gsdebug = 0;
int cldebug = 0;
int pkdebug = 0;
int g2debug = 0;
struct
{
char* name;
long value;
} resrv[] =
{
"binary", BINARY,
"left", LEFT,
"nonassoc", BINARY,
"prec", PREC,
"right", RIGHT,
"start", START,
"term", TERM,
"token", TERM,
"type", TYPEDEF,
"union", UNION,
0,
};
void main(int, char**);
void others(void);
char* chcopy(char*, char*);
char* writem(int*);
char* symnam(int);
void summary(void);
void error(char*, ...);
void aryfil(int*, int, int);
int setunion(int*, int*);
void prlook(Lkset*);
void cpres(void);
void cpfir(void);
int state(int);
void putitem(int*, Lkset*);
void cempty(void);
void stagen(void);
void closure(int);
Lkset* flset(Lkset*);
void cleantmp(void);
void intr(void);
void setup(int, char**);
void finact(void);
int defin(int, char*);
void defout(int);
char* cstash(char*);
long gettok(void);
int fdtype(int);
int chfind(int, char*);
void cpyunion(void);
void cpycode(void);
int skipcom(void);
void cpyact(int);
void openup(char*, int, int, int, char*);
void output(void);
int apack(int*, int);
void go2out(void);
void go2gen(int);
void precftn(int, int, int);
void wract(int);
void wrstate(int);
void warray(char*, int*, int);
void hideprod(void);
void callopt(void);
void gin(int);
void stin(int);
int nxti(void);
void osummary(void);
void aoutput(void);
void arout(char*, int*, int);
int gtnm(void);
void
main(int argc, char *argv[])
{
setup(argc, argv);
tbitset = NWORDS(ntokens);
cpres();
cempty();
cpfir();
stagen();
output();
go2out();
hideprod();
summary();
callopt();
others();
exits(0);
}
void
others(void)
{
int c, i, j;
finput = Bopen(parser, OREAD);
if(finput == 0)
error("cannot find parser %s", parser);
warray("yyr1", levprd, nprod);
aryfil(temp1, nprod, 0);
PLOOP(1, i)
temp1[i] = prdptr[i+1]-prdptr[i]-2;
warray("yyr2", temp1, nprod);
aryfil(temp1, nstate, -1000);
TLOOP(i)
for(j=tstates[i]; j!=0; j=mstates[j])
temp1[j] = i;
NTLOOP(i)
for(j=ntstates[i]; j!=0; j=mstates[j])
temp1[j] = -i;
warray("yychk", temp1, nstate);
warray("yydef", defact, nstate);
aryfil(temp1, 256, 0);
c = 0;
TLOOP(i) {
j = tokset[i].value;
if(j >= 0 && j < 256) {
if(temp1[j]) {
print("yacc bug -- cant have 2 different Ts with same value\n");
print("	%s and %s\n", tokset[i].name, tokset[temp1[j]].name);
nerrors++;
}
temp1[j] = i;
if(j > c)
c = j;
}
}
warray("yytok1", temp1, c+1);
aryfil(temp1, 256, 0);
c = 0;
TLOOP(i) {
j = tokset[i].value - PRIVATE;
if(j >= 0 && j < 256) {
if(temp1[j]) {
print("yacc bug -- cant have 2 different Ts with same value\n");
print("	%s and %s\n", tokset[i].name, tokset[temp1[j]].name);
nerrors++;
}
temp1[j] = i;
if(j > c)
c = j;
}
}
warray("yytok2", temp1, c+1);
Bprint(ftable, "long	yytok3[] =\n{\n");
c = 0;
TLOOP(i) {
j = tokset[i].value;
if(j >= 0 && j < 256)
continue;
if(j >= PRIVATE && j < 256+PRIVATE)
continue;
Bprint(ftable, "%4d,%4d,", j, i);
c++;
if(c%5 == 0)
Bprint(ftable, "\n");
}
Bprint(ftable, "%4d\n};\n", 0);
while((c=Bgetrune(finput)) != Beof) {
if(c == '$') {
if((c = Bgetrune(finput)) != 'A')
Bputrune(ftable, '$');
else {
faction = Bopen(actname, OREAD);
if(faction == 0)
error("cannot reopen action tempfile");
while((c=Bgetrune(faction)) != Beof)
Bputrune(ftable, c);
Bterm(faction);
ZAPFILE(actname);
c = Bgetrune(finput);
}
}
Bputrune(ftable, c);
}
Bterm(ftable);
}
char*
chcopy(char* p, char* q)
{
int c;
while(c = *q) {
if(c == '"')
*p++ = '\\';
*p++ = c;
q++;
}
*p = 0;
return p;
}
char*
writem(int *pp)
{
int i,*p;
static char sarr[ISIZE];
char* q;
for(p=pp; *p>0; p++)
;
p = prdptr[-*p];
q = chcopy(sarr, nontrst[*p-NTBASE].name);
q = chcopy(q, ": ");
for(;;) {
*q = ' ';
p++;
if(p == pp)
*q = '.';
q++;
*q = '\0';
i = *p;
if(i <= 0)
break;
q = chcopy(q, symnam(i));
if(q > &sarr[ISIZE-30])
error("item too big");
}
i = *pp;
if(i < 0 ) {
q = chcopy(q, "    (");
sprint(q, "%d)", -i);
}
return sarr;
}
char*
symnam(int i)
{
char* cp;
cp = (i >= NTBASE)? nontrst[i-NTBASE].name: tokset[i].name;
if(*cp == ' ')
cp++;
return cp;
}
void
summary(void)
{
if(foutput != 0) {
Bprint(foutput, "\n%d/%d terminals, %d/%d nonterminals\n",
ntokens, NTERMS, nnonter, NNONTERM);
Bprint(foutput, "%d/%d grammar rules, %d/%d states\n",
nprod, NPROD, nstate, NSTATES);
Bprint(foutput, "%d shift/reduce, %d reduce/reduce conflicts reported\n",
zzsrconf, zzrrconf);
Bprint(foutput, "%d/%d working sets used\n",
(int)(zzcwp-wsets), WSETSIZE);
Bprint(foutput, "memory: states,etc. %d/%d, parser %d/%d\n",
(int)(zzmemsz-mem0), MEMSIZE, (int)(memp-amem), ACTSIZE);
Bprint(foutput, "%d/%d distinct lookahead sets\n", nlset, LSETSIZE);
Bprint(foutput, "%d extra closures\n", zzclose - 2*nstate);
Bprint(foutput, "%d shift entries, %d exceptions\n", zzacent, zzexcp);
Bprint(foutput, "%d goto entries\n", zzgoent);
Bprint(foutput, "%d entries saved by goto default\n", zzgobest);
}
if(zzsrconf != 0 || zzrrconf != 0) {
print("\nconflicts: ");
if(zzsrconf)
print("%d shift/reduce", zzsrconf);
if(zzsrconf && zzrrconf)
print(", ");
if(zzrrconf)
print("%d reduce/reduce", zzrrconf);
print("\n");
}
if(ftemp != 0) {
Bterm(ftemp);
ftemp = 0;
}
if(fdefine != 0) {
Bterm(fdefine);
fdefine = 0;
}
}
void
error(char *s, ...)
{
nerrors++;
fprint(2, "\n fatal error:");
fprint(2, s, (&s)[1]);
fprint(2, ", %s:%d\n", infile, lineno);
if(!fatfl)
return;
summary();
cleantmp();
exits("error");
}
void
aryfil(int *v, int n, int c)
{
int i;
for(i=0; i<n; i++)
v[i] = c;
}
int
setunion(int *a, int *b)
{
int i, x, sub;
sub = 0;
SETLOOP(i) {
x = *a;
*a |= *b;
if(*a != x)
sub = 1;
a++;
b++;
}
return sub;
}
void
prlook(Lkset* p)
{
int j, *pp;
pp = p->lset;
if(pp == 0)
Bprint(foutput, "\tNULL");
else {
Bprint(foutput, " { ");
TLOOP(j)
if(BIT(pp,j))
Bprint(foutput, "%s ", symnam(j));
Bprint(foutput, "}");
}
}
void
cpres(void)
{
int c, j, i, **pmem;
static int *pyield[NPROD];
pmem = pyield;
NTLOOP(i) {
c = i+NTBASE;
pres[i] = pmem;
fatfl = 0;
PLOOP(0, j)
if(*prdptr[j] == c)
*pmem++ = prdptr[j]+1;
if(pres[i] == pmem)
error("nonterminal %s not defined!", nontrst[i].name);
}
pres[i] = pmem;
fatfl = 1;
if(nerrors) {
summary();
cleantmp();
exits("error");
}
if(pmem != &pyield[nprod])
error("internal Yacc error: pyield %d", pmem-&pyield[nprod]);
}
void
cpfir(void)
{
int *p, **s, i, **t, ch, changes;
zzcwp = &wsets[nnonter];
NTLOOP(i) {
aryfil(wsets[i].ws.lset, tbitset, 0);
t = pres[i+1];
for(s=pres[i]; s<t; ++s)
for(p = *s; (ch = *p) > 0; ++p) {
if(ch < NTBASE) {
SETBIT(wsets[i].ws.lset, ch);
break;
}
if(!pempty[ch-NTBASE])
break;
}
}
changes = 1;
while(changes) {
changes = 0;
NTLOOP(i) {
t = pres[i+1];
for(s = pres[i]; s < t; ++s)
for(p = *s; (ch = (*p-NTBASE)) >= 0; ++p) {
changes |= setunion(wsets[i].ws.lset, wsets[ch].ws.lset);
if(!pempty[ch])
break;
}
}
}
NTLOOP(i)
pfirst[i] = flset(&wsets[i].ws);
if(!indebug)
return;
if(foutput != 0)
NTLOOP(i) {
Bprint(foutput, "\n%s: ", nontrst[i].name);
prlook(pfirst[i]);
Bprint(foutput, " %d\n", pempty[i]);
}
}
int
state(int c)
{
Item *p1, *p2, *k, *l, *q1, *q2;
int size1, size2, i;
p1 = pstate[nstate];
p2 = pstate[nstate+1];
if(p1 == p2)
return 0;
for(k = p2-1; k > p1; k--)
for(l = k-1; l >= p1; --l)
if(l->pitem > k->pitem) {
int *s;
Lkset *ss;
s = k->pitem;
k->pitem = l->pitem;
l->pitem = s;
ss = k->look;
k->look = l->look;
l->look = ss;
}
size1 = p2 - p1;
for(i = (c>=NTBASE)? ntstates[c-NTBASE]: tstates[c]; i != 0; i = mstates[i]) {
q1 = pstate[i];
q2 = pstate[i+1];
size2 = q2 - q1;
if(size1 != size2)
continue;
k = p1;
for(l = q1; l < q2; l++) {
if(l->pitem != k->pitem)
break;
k++;
}
if(l != q2)
continue;
pstate[nstate+1] = pstate[nstate];
if(nolook)
return i;
for(l = q1, k = p1; l < q2; ++l, ++k ) {
int s;
SETLOOP(s)
clset.lset[s] = l->look->lset[s];
if(setunion(clset.lset, k->look->lset)) {
tystate[i] = MUSTDO;
l->look = flset( &clset );
}
}
return i;
}
if(nolook)
error("yacc state/nolook error");
pstate[nstate+2] = p2;
if(nstate+1 >= NSTATES)
error("too many states");
if(c >= NTBASE) {
mstates[nstate] = ntstates[c-NTBASE];
ntstates[c-NTBASE] = nstate;
} else {
mstates[nstate] = tstates[c];
tstates[c] = nstate;
}
tystate[nstate] = MUSTDO;
return nstate++;
}
void
putitem(int *ptr, Lkset *lptr)
{
Item *j;
if(pidebug && foutput != 0)
Bprint(foutput, "putitem(%s), state %d\n", writem(ptr), nstate);
j = pstate[nstate+1];
j->pitem = ptr;
if(!nolook)
j->look = flset(lptr);
pstate[nstate+1] = ++j;
if((int*)j > zzmemsz) {
zzmemsz = (int*)j;
if(zzmemsz >= &mem0[MEMSIZE])
error("out of state space");
}
}
void
cempty(void)
{
int i, *p;
aryfil(pempty, nnonter+1, WHOKNOWS);
more:
PLOOP(0, i) {
if(pempty[*prdptr[i] - NTBASE])
continue;
for(p = prdptr[i]+1; *p >= 0; ++p)
if(*p >= NTBASE && pempty[*p-NTBASE] == WHOKNOWS)
break;
if(*p < 0) {
pempty[*prdptr[i]-NTBASE] = OK;
goto more;
}
}
NTLOOP(i) {
if(i == 0)
continue;
if(pempty[i] != OK) {
fatfl = 0;
error("nonterminal %s never derives any token string", nontrst[i].name);
}
}
if(nerrors) {
summary();
cleantmp();
exits("error");
}
aryfil( pempty, nnonter+1, WHOKNOWS);
again:
PLOOP(1, i) {
if(pempty[*prdptr[i]-NTBASE] == WHOKNOWS) {
for(p = prdptr[i]+1; *p >= NTBASE && pempty[*p-NTBASE] == EMPTY ; ++p)
;
if(*p < 0) {
pempty[*prdptr[i]-NTBASE] = EMPTY;
goto again;
}
}
}
}
void
stagen(void)
{
int c, i, j, more;
Wset *p, *q;
nstate = 0;
pstate[0] = pstate[1] = (Item*)mem;
aryfil(clset.lset, tbitset, 0);
putitem(prdptr[0]+1, &clset);
tystate[0] = MUSTDO;
nstate = 1;
pstate[2] = pstate[1];
aryfil(amem, ACTSIZE, 0);
for(more=1; more;) {
more = 0;
SLOOP(i) {
if(tystate[i] != MUSTDO)
continue;
tystate[i] = DONE;
aryfil(temp1, nnonter+1, 0);
closure(i);
WSLOOP(wsets, p) {
if(p->flag)
continue;
p->flag = 1;
c = *(p->pitem);
if(c <= 1) {
if(pstate[i+1]-pstate[i] <= p-wsets)
tystate[i] = MUSTLOOKAHEAD;
continue;
}
WSLOOP(p, q)
if(c == *(q->pitem)) {
putitem(q->pitem+1, &q->ws);
q->flag = 1;
}
if(c < NTBASE)
state(c);
else
temp1[c-NTBASE] = state(c);
}
if(gsdebug && foutput != 0) {
Bprint(foutput, "%d: ", i);
NTLOOP(j)
if(temp1[j])
Bprint(foutput, "%s %d, ",
nontrst[j].name, temp1[j]);
Bprint(foutput, "\n");
}
indgo[i] = apack(&temp1[1], nnonter-1) - 1;
more = 1;
}
}
}
void
closure(int i)
{
Wset *u, *v;
Item *p, *q;
int c, ch, work, k, *pi, **s, **t;
zzclose++;
cwp = wsets;
ITMLOOP(i, p, q) {
cwp->pitem = p->pitem;
cwp->flag = 1;
SETLOOP(k)
cwp->ws.lset[k] = p->look->lset[k];
WSBUMP(cwp);
}
work = 1;
while(work) {
work = 0;
WSLOOP(wsets, u) {
if(u->flag == 0)
continue;
c = *(u->pitem);
if(c < NTBASE) {
u->flag = 0;
continue;
}
aryfil(clset.lset, tbitset, 0);
WSLOOP(u, v)
if(v->flag == 1 && *(pi=v->pitem) == c) {
v->flag = 0;
if(nolook)
continue;
while((ch = *++pi) > 0) {
if(ch < NTBASE) {
SETBIT(clset.lset, ch);
break;
}
setunion(clset.lset, pfirst[ch-NTBASE]->lset);
if(!pempty[ch-NTBASE])
break;
}
if(ch <= 0)
setunion(clset.lset, v->ws.lset);
}
c -= NTBASE;
t = pres[c+1];
for(s = pres[c]; s < t; ++s) {
WSLOOP(wsets, v)
if(v->pitem == *s) {
if(nolook)
goto nexts;
if(setunion(v->ws.lset, clset.lset))
v->flag = work = 1;
goto nexts;
}
if(cwp-wsets+1 >= WSETSIZE)
error( "working set overflow");
cwp->pitem = *s;
cwp->flag = 1;
if(!nolook) {
work = 1;
SETLOOP(k) cwp->ws.lset[k] = clset.lset[k];
}
WSBUMP(cwp);
nexts:;
}
}
}
if(cwp > zzcwp)
zzcwp = cwp;
if(cldebug && foutput != 0) {
Bprint(foutput, "\nState %d, nolook = %d\n", i, nolook);
WSLOOP(wsets, u) {
if(u->flag)
Bprint(foutput, "flag set!\n");
u->flag = 0;
Bprint(foutput, "\t%s", writem(u->pitem));
prlook(&u->ws);
Bprint(foutput, "\n");
}
}
}
Lkset*
flset(Lkset *p)
{
Lkset *q;
int *u, *v, *w, j;
for(q = &lkst[nlset]; q-- > lkst;) {
u = p->lset;
v = q->lset;
w = &v[tbitset];
while(v < w)
if(*u++ != *v++)
goto more;
return q;
more:;
}
q = &lkst[nlset++];
if(nlset >= LSETSIZE)
error("too many lookahead sets");
SETLOOP(j)
q->lset[j] = p->lset[j];
return q;
}
void
cleantmp(void)
{
ZAPFILE(actname);
ZAPFILE(tempname);
}
void
intr(void)
{
cleantmp();
exits("interrupted");
}
void
setup(int argc, char *argv[])
{
long c, t;
int i, j, lev, ty, ytab, *p;
int vflag, dflag, stem;
char actnm[8], *stemc, *cp;
ytab = 0;
vflag = 0;
dflag = 0;
stem = 0;
stemc = "y";
foutput = 0;
fdefine = 0;
fdebug = 0;
ARGBEGIN{
case 'v':
case 'V':
vflag++;
break;
case 'D':
yydebug = ARGF();
break;
case 'd':
dflag++;
break;
case 'o':
ytab++;
ytabc = ARGF();
break;
case 's':
stem++;
stemc = ARGF();
break;
case 'S':
parser = PARSERS;
break;
default:
error("illegal option: %c", ARGC());
}ARGEND
cp = getenv("ROOT");
if(cp == 0)
cp = ROOT;
snprint(par, sizeof(par), "%s/utils/lib/%s", cp, parser);
parser = par;
openup(stemc, dflag, vflag, ytab, ytabc);
ftemp = Bopen(tempname = mktemp(ttempname), OWRITE);
faction = Bopen(actname = mktemp(tactname), OWRITE);
if(ftemp == 0 || faction == 0)
error("cannot open temp file");
if(argc < 1)
error("no input file");
infile = argv[0];
finput = Bopen(infile, OREAD);
if(finput == 0)
error("cannot open '%s'", argv[0]);
cnamp = cnames;
defin(0, "$end");
extval = PRIVATE;
defin(0, "error");
defin(1, "$accept");
defin(0, "$unk");
mem = mem0;
i = 0;
for(t = gettok(); t != MARK && t != ENDFILE;)
switch(t) {
case ';':
t = gettok();
break;
case START:
if(gettok() != IDENTIFIER)
error("bad %%start construction");
start = chfind(1, tokname);
t = gettok();
continue;
case TYPEDEF:
if(gettok() != TYPENAME)
error("bad syntax in %%type");
ty = numbval;
for(;;) {
t = gettok();
switch(t) {
case IDENTIFIER:
if((t=chfind(1, tokname)) < NTBASE) {
j = TYPE(toklev[t]);
if(j != 0 && j != ty)
error("type redeclaration of token %s",
tokset[t].name);
else
SETTYPE(toklev[t], ty);
} else {
j = nontrst[t-NTBASE].value;
if(j != 0 && j != ty)
error("type redeclaration of nonterminal %s",
nontrst[t-NTBASE].name );
else
nontrst[t-NTBASE].value = ty;
}
case ',':
continue;
case ';':
t = gettok();
default:
break;
}
break;
}
continue;
case UNION:
cpyunion();
t = gettok();
continue;
case LEFT:
case BINARY:
case RIGHT:
i++;
case TERM:
lev = t-TERM;
ty = 0;
t = gettok();
if(t == TYPENAME) {
ty = numbval;
t = gettok();
}
for(;;) {
switch(t) {
case ',':
t = gettok();
continue;
case ';':
break;
case IDENTIFIER:
j = chfind(0, tokname);
if(j >= NTBASE)
error("%s defined earlier as nonterminal", tokname);
if(lev) {
if(ASSOC(toklev[j]))
error("redeclaration of precedence of %s", tokname);
SETASC(toklev[j], lev);
SETPLEV(toklev[j], i);
}
if(ty) {
if(TYPE(toklev[j]))
error("redeclaration of type of %s", tokname);
SETTYPE(toklev[j],ty);
}
t = gettok();
if(t == NUMBER) {
tokset[j].value = numbval;
if(j < ndefout && j > 3)
error("please define type number of %s earlier",
tokset[j].name);
t = gettok();
}
continue;
}
break;
}
continue;
case LCURLY:
defout(0);
cpycode();
t = gettok();
continue;
default:
error("syntax error");
}
if(t == ENDFILE)
error("unexpected EOF before %%");
Bprint(ftable, "extern	int	yyerrflag;\n");
Bprint(ftable, "#ifndef	YYMAXDEPTH\n");
Bprint(ftable, "#define	YYMAXDEPTH	150\n");
Bprint(ftable, "#endif\n" );
if(!ntypes) {
Bprint(ftable, "#ifndef	YYSTYPE\n");
Bprint(ftable, "#define	YYSTYPE	int\n");
Bprint(ftable, "#endif\n");
}
Bprint(ftable, "YYSTYPE	yylval;\n");
Bprint(ftable, "YYSTYPE	yyval;\n");
prdptr[0] = mem;
*mem++ = NTBASE;
*mem++ = start;
*mem++ = 1;
*mem++ = 0;
prdptr[1] = mem;
while((t=gettok()) == LCURLY)
cpycode();
if(t != IDENTCOLON)
error("bad syntax on first rule");
if(!start)
prdptr[0][1] = chfind(1, tokname);
while(t != MARK && t != ENDFILE) {
rlines[nprod] = lineno;
if(t == '|')
*mem++ = *prdptr[nprod-1];
else
if(t == IDENTCOLON) {
*mem = chfind(1, tokname);
if(*mem < NTBASE)
error("token illegal on LHS of grammar rule");
mem++;
} else
error("illegal rule: missing semicolon or | ?");
t = gettok();
more_rule:
while(t == IDENTIFIER) {
*mem = chfind(1, tokname);
if(*mem < NTBASE)
levprd[nprod] = toklev[*mem];
mem++;
t = gettok();
}
if(t == PREC) {
if(gettok() != IDENTIFIER)
error("illegal %%prec syntax");
j = chfind(2, tokname);
if(j >= NTBASE)
error("nonterminal %s illegal after %%prec",
nontrst[j-NTBASE].name);
levprd[nprod] = toklev[j];
t = gettok();
}
if(t == '=') {
levprd[nprod] |= ACTFLAG;
Bprint(faction, "\ncase %d:", nprod);
cpyact(mem-prdptr[nprod]-1);
Bprint(faction, " break;");
if((t=gettok()) == IDENTIFIER) {
sprint(actnm, "$$%d", nprod);
j = chfind(1, actnm);
for(p = mem; p >= prdptr[nprod]; --p)
p[2] = *p;
mem += 2;
p = prdptr[nprod];
*p++ = j;
*p++ = -nprod;
levprd[nprod+1] = levprd[nprod] & ~ACTFLAG;
levprd[nprod] = ACTFLAG;
if(++nprod >= NPROD)
error("more than %d rules", NPROD);
prdptr[nprod] = p;
*mem++ = j;
goto more_rule;
}
}
while(t == ';')
t = gettok();
*mem++ = -nprod;
if(ntypes && !(levprd[nprod]&ACTFLAG) && nontrst[*prdptr[nprod]-NTBASE].value) {
int tempty;
tempty = prdptr[nprod][1];
if(tempty < 0)
error("must return a value, since LHS has a type");
else
if(tempty >= NTBASE)
tempty = nontrst[tempty-NTBASE].value;
else
tempty = TYPE(toklev[tempty]);
if(tempty != nontrst[*prdptr[nprod]-NTBASE].value)
error("default action causes potential type clash");
}
nprod++;
if(nprod >= NPROD)
error("more than %d rules", NPROD);
prdptr[nprod] = mem;
levprd[nprod] = 0;
}
defout(1);
finact();
if(t == MARK) {
Bprint(ftable, "\n#line\t%d\t\"%s\"\n", lineno, infile);
while((c=Bgetrune(finput)) != Beof)
Bputrune(ftable, c);
}
Bterm(finput);
}
void
finact(void)
{
Bterm(faction);
Bprint(ftable, "#define YYEOFCODE %d\n", 1);
Bprint(ftable, "#define YYERRCODE %d\n", 2);
}
int
defin(int nt, char *s)
{
int val;
Rune rune;
val = 0;
if(nt) {
nnonter++;
if(nnonter >= NNONTERM)
error("too many nonterminals, limit %d",NNONTERM);
nontrst[nnonter].name = cstash(s);
return NTBASE + nnonter;
}
ntokens++;
if(ntokens >= NTERMS)
error("too many terminals, limit %d", NTERMS);
tokset[ntokens].name = cstash(s);
if(s[0] == ' ') {
val = chartorune(&rune, &s[1]);
if(s[val+1] == 0) {
val = rune;
goto out;
}
}
if(s[0] == ' ' && s[1] == '\\') {
if(s[3] == 0) {
switch(s[2]) {
case 'n': val = '\n'; break;
case 'r': val = '\r'; break;
case 'b': val = '\b'; break;
case 't': val = '\t'; break;
case 'f': val = '\f'; break;
case '\'': val = '\''; break;
case '"':	val = '"'; break;
case '\\': val = '\\'; break;
default: error("invalid escape");
}
goto out;
}
if(s[2] >= '0' && s[2] <= '7') {
if(s[3] < '0' ||
s[3] > '7' ||
s[4] < '0' ||
s[4] > '7' ||
s[5] != 0)
error("illegal \\nnn construction");
val = 64*s[2] + 8*s[3] + s[4] - 73*'0';
if(val == 0)
error("'\\000' is illegal");
goto out;
}
error("unknown escape");
}
val = extval++;
out:
tokset[ntokens].value = val;
toklev[ntokens] = 0;
return ntokens;
}
void
defout(int last)
{
int i, c;
char sar[NAMESIZE+10];
for(i=ndefout; i<=ntokens; i++) {
c = tokset[i].name[0];
if(c != ' ' && c != '$') {
Bprint(ftable, "#define	%s	%d\n",
tokset[i].name, tokset[i].value);
if(fdefine)
Bprint(fdefine, "#define\t%s\t%d\n",
tokset[i].name, tokset[i].value);
}
}
ndefout = ntokens+1;
if(last && fdebug) {
Bprint(fdebug, "char*	yytoknames[] =\n{\n");
TLOOP(i) {
if(tokset[i].name) {
chcopy(sar, tokset[i].name);
Bprint(fdebug, "\t\"%s\",\n", sar);
continue;
}
Bprint(fdebug, "\t0,\n");
}
Bprint(fdebug, "};\n");
}
}
char*
cstash(char *s)
{
char *temp;
temp = cnamp;
do {
if(cnamp >= &cnames[cnamsz])
error("too many characters in id's and literals");
else
*cnamp++ = *s;
} while(*s++);
return temp;
}
long
gettok(void)
{
long c;
Rune rune;
int i, base, match, reserve;
static int peekline;
begin:
reserve = 0;
lineno += peekline;
peekline = 0;
c = Bgetrune(finput);
while(c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r') {
if(c == '\n')
lineno++;
c = Bgetrune(finput);
}
if(c == '/') {
lineno += skipcom();
goto begin;
}
switch(c) {
case Beof:
return ENDFILE;
case '{':
Bungetrune(finput);
return '=';
case '<':
i = 0;
while((c=Bgetrune(finput)) != '>' && c >= 0 && c != '\n' && c != '\r') {
rune = c;
c = runetochar(&tokname[i], &rune);
if(i < NAMESIZE)
i += c;
}
if(c != '>')
error("unterminated < ... > clause");
tokname[i] = 0;
for(i=1; i<=ntypes; i++)
if(!strcmp(typeset[i], tokname)) {
numbval = i;
return TYPENAME;
}
ntypes++;
numbval = ntypes;
typeset[numbval] = cstash(tokname);
return TYPENAME;
case '"':
case '\'':
match = c;
tokname[0] = ' ';
i = 1;
for(;;) {
c = Bgetrune(finput);
if(c == '\n' || c == '\r' || c <= 0)
error("illegal or missing ' or \"" );
if(c == '\\') {
tokname[i] = '\\';
if(i < NAMESIZE)
i++;
c = Bgetrune(finput);
} else
if(c == match)
break;
rune = c;
c = runetochar(&tokname[i], &rune);
if(i < NAMESIZE)
i += c;
}
break;
case '%':
case '\\':
switch(c = Bgetrune(finput)) {
case '0': return TERM;
case '<': return LEFT;
case '2': return BINARY;
case '>': return RIGHT;
case '%':
case '\\': return MARK;
case '=': return PREC;
case '{': return LCURLY;
default: reserve = 1;
}
default:
if(isdigit(c)) {
numbval = c-'0';
base = (c=='0')? 8: 10;
for(c = Bgetrune(finput); isdigit(c); c = Bgetrune(finput))
numbval = numbval*base + (c-'0');
Bungetrune(finput);
return NUMBER;
}
if(islower(c) || isupper(c) || c=='_' || c=='.' || c=='$') {
i = 0;
while(islower(c) || isupper(c) || isdigit(c) ||
c == '-' || c=='_' || c=='.' || c=='$') {
if(reserve && isupper(c))
c += 'a'-'A';
rune = c;
c = runetochar(&tokname[i], &rune);
if(i < NAMESIZE)
i += c;
c = Bgetrune(finput);
}
} else
return c;
Bungetrune(finput);
}
tokname[i] = 0;
if(reserve) {
for(c=0; resrv[c].name; c++)
if(strcmp(tokname, resrv[c].name) == 0)
return resrv[c].value;
error("invalid escape, or illegal reserved word: %s", tokname);
}
c = Bgetrune(finput);
while(c == ' ' || c == '\t'|| c == '\n' || c == '\r' || c == '\f' || c == '/') {
if(c == '\n')
peekline++;
if(c == '/')
peekline += skipcom();
c = Bgetrune(finput);
}
if(c == ':')
return IDENTCOLON;
Bungetrune(finput);
return IDENTIFIER;
}
int
fdtype(int t)
{
int v;
if(t >= NTBASE)
v = nontrst[t-NTBASE].value;
else
v = TYPE(toklev[t]);
if(v <= 0)
error("must specify type for %s", (t>=NTBASE)?
nontrst[t-NTBASE].name: tokset[t].name);
return v;
}
int
chfind(int t, char *s)
{
int i;
if(s[0] == ' ')
t = 0;
TLOOP(i)
if(!strcmp(s, tokset[i].name))
return i;
NTLOOP(i)
if(!strcmp(s, nontrst[i].name))
return NTBASE+i;
if(t > 1)
error("%s should have been defined earlier", s);
return defin(t, s);
}
void
cpyunion(void)
{
long c;
int level;
Bprint(ftable, "\n#line\t%d\t\"%s\"\n", lineno, infile);
Bprint(ftable, "typedef union ");
if(fdefine != 0)
Bprint(fdefine, "\ntypedef union ");
level = 0;
for(;;) {
if((c=Bgetrune(finput)) == Beof)
error("EOF encountered while processing %%union");
Bputrune(ftable, c);
if(fdefine != 0)
Bputrune(fdefine, c);
switch(c) {
case '\n':
lineno++;
break;
case '{':
level++;
break;
case '}':
level--;
if(level == 0) {
Bprint(ftable, " YYSTYPE;\n");
if(fdefine != 0)
Bprint(fdefine, "\tYYSTYPE;\nextern\tYYSTYPE\tyylval;\n");
return;
}
}
}
}
void
cpycode(void)
{
long c;
c = Bgetrune(finput);
if(c == '\n') {
c = Bgetrune(finput);
lineno++;
}
Bprint(ftable, "\n#line\t%d\t\"%s\"\n", lineno, infile);
while(c != Beof) {
if(c == '\\') {
if((c=Bgetrune(finput)) == '}')
return;
Bputc(ftable, '\\');
}
if(c == '%') {
if((c=Bgetrune(finput)) == '}')
return;
Bputc(ftable, '%');
}
Bputrune(ftable, c);
if(c == '\n')
lineno++;
c = Bgetrune(finput);
}
error("eof before %%}");
}
int
skipcom(void)
{
long c;
int i;
i = 0;
if(Bgetrune(finput) != '*')
error("illegal comment");
c = Bgetrune(finput);
while(c != Beof) {
while(c == '*')
if((c=Bgetrune(finput)) == '/')
return i;
if(c == '\n')
i++;
c = Bgetrune(finput);
}
error("EOF inside comment");
return 0;
}
void
cpyact(int offset)
{
long c;
int brac, match, j, s, fnd, tok;
Bprint(faction, "\n#line\t%d\t\"%s\"\n", lineno, infile);
brac = 0;
loop:
c = Bgetrune(finput);
swt:
switch(c) {
case ';':
if(brac == 0) {
Bputrune(faction, c);
return;
}
goto lcopy;
case '{':
brac++;
goto lcopy;
case '$':
s = 1;
tok = -1;
c = Bgetrune(finput);
if(c == '<') {
Bungetrune(finput);
if(gettok() != TYPENAME)
error("bad syntax on $<ident> clause");
tok = numbval;
c = Bgetrune(finput);
}
if(c == '$') {
Bprint(faction, "yyval");
if(ntypes) {
if(tok < 0)
tok = fdtype(*prdptr[nprod]);
Bprint(faction, ".%s", typeset[tok]);
}
goto loop;
}
if(c == '-') {
s = -s;
c = Bgetrune(finput);
}
if(isdigit(c)) {
j = 0;
while(isdigit(c)) {
j = j*10 + (c-'0');
c = Bgetrune(finput);
}
Bungetrune(finput);
j = j*s - offset;
if(j > 0)
error("Illegal use of $%d", j+offset);
dollar:
Bprint(faction, "yypt[-%d].yyv", -j);
if(ntypes) {
if(j+offset <= 0 && tok < 0)
error("must specify type of $%d", j+offset);
if(tok < 0)
tok = fdtype(prdptr[nprod][j+offset]);
Bprint(faction, ".%s", typeset[tok]);
}
goto loop;
}
if(isupper(c) || islower(c) || c == '_' || c == '.') {
int tok;
Bungetrune(finput);
if(gettok() != IDENTIFIER)
error("$ must be followed by an identifier");
tok = chfind(2, tokname);
if((c = Bgetrune(finput)) != '#') {
Bungetrune(finput);
fnd = -1;
} else
if(gettok() != NUMBER) {
error("# must be followed by number");
fnd = -1;
} else
fnd = numbval;
for(j=1; j<=offset; ++j)
if(tok == prdptr[nprod][j]) {
if(--fnd <= 0) {
j -= offset;
goto dollar;
}
}
error("$name or $name#number not found");
}
Bputc(faction, '$');
if(s < 0 )
Bputc(faction, '-');
goto swt;
case '}':
brac--;
if(brac)
goto lcopy;
Bputrune(faction, c);
return;
case '/':
Bputrune(faction, c);
c = Bgetrune(finput);
if(c != '*')
goto swt;
Bputrune(faction, c);
c = Bgetrune(finput);
while(c >= 0) {
while(c == '*') {
Bputrune(faction, c);
if((c=Bgetrune(finput)) == '/')
goto lcopy;
}
Bputrune(faction, c);
if(c == '\n')
lineno++;
c = Bgetrune(finput);
}
error("EOF inside comment");
case '\'':
match = '\'';
goto string;
case '"':
match = '"';
string:
Bputrune(faction, c);
while(c = Bgetrune(finput)) {
if(c == '\\') {
Bputrune(faction, c);
c = Bgetrune(finput);
if(c == '\n')
lineno++;
} else
if(c == match)
goto lcopy;
if(c == '\n')
error("newline in string or char. const.");
Bputrune(faction, c);
}
error("EOF in string or character constant");
case Beof:
error("action does not terminate");
case '\n':
lineno++;
goto lcopy;
}
lcopy:
Bputrune(faction, c);
goto loop;
}
void
openup(char *stem, int dflag, int vflag, int ytab, char *ytabc)
{
char buf[256];
if(vflag) {
sprint(buf, "%s.%s", stem, FILEU);
foutput = Bopen(buf, OWRITE);
if(foutput == 0)
error("cannot open %s", buf);
}
if(yydebug) {
sprint(buf, "%s.%s", stem, FILEDEBUG);
if((fdebug = Bopen(buf, OWRITE)) == 0)
error("can't open %s", buf);
}
if(dflag) {
sprint(buf, "%s.%s", stem, FILED);
fdefine = Bopen(buf, OWRITE);
if(fdefine == 0)
error("can't create %s", buf);
}
if(ytab == 0)
sprint(buf, "%s.%s", stem, OFILE);
else
strcpy(buf, ytabc);
ftable = Bopen(buf, OWRITE);
if(ftable == 0)
error("cannot open table file %s", buf);
}
void
output(void)
{
int i, k, c;
Wset *u, *v;
Bprint(ftable, "short	yyexca[] =\n{");
if(fdebug)
Bprint(fdebug, "char*	yystates[] =\n{\n");
SLOOP(i) {
nolook = tystate[i]!=MUSTLOOKAHEAD;
closure(i);
nolook = 1;
aryfil(temp1, ntokens+nnonter+1, 0);
WSLOOP(wsets, u) {
c = *(u->pitem);
if(c > 1 && c < NTBASE && temp1[c] == 0) {
WSLOOP(u, v)
if(c == *(v->pitem))
putitem(v->pitem+1, (Lkset*)0);
temp1[c] = state(c);
} else
if(c > NTBASE && temp1[(c -= NTBASE) + ntokens] == 0)
temp1[c+ntokens] = amem[indgo[i]+c];
}
if(i == 1)
temp1[1] = ACCEPTCODE;
lastred = 0;
WSLOOP(wsets, u) {
c = *u->pitem;
if(c <= 0) {
lastred = -c;
TLOOP(k)
if(BIT(u->ws.lset, k)) {
if(temp1[k] == 0)
temp1[k] = c;
else
if(temp1[k] < 0) {
if(foutput)
Bprint(foutput,
"\n%d: reduce/reduce conflict"
" (red'ns %d and %d ) on %s",
i, -temp1[k], lastred,
symnam(k));
if(-temp1[k] > lastred)
temp1[k] = -lastred;
zzrrconf++;
} else
precftn( lastred, k, i );
}
}
}
wract(i);
}
if(fdebug)
Bprint(fdebug, "};\n");
Bprint(ftable, "};\n");
Bprint(ftable, "#define	YYNPROD	%d\n", nprod);
Bprint(ftable, "#define	YYPRIVATE %d\n", PRIVATE);
if(yydebug)
Bprint(ftable, "#define	yydebug	%s\n", yydebug);
}
int
apack(int *p, int n)
{
int *pp, *qq, *rr, off, *q, *r;
q = p+n;
for(pp = p, off = 0; *pp == 0 && pp <= q; ++pp, --off)
;
if(pp > q)
return 0;
p = pp;
r = &amem[ACTSIZE-1];
for(rr = amem; rr <= r; rr++, off++) {
for(qq = rr, pp = p; pp <= q; pp++, qq++)
if(*pp != 0)
if(*pp != *qq && *qq != 0)
goto nextk;
if(pkdebug && foutput != 0)
Bprint(foutput, "off = %d, k = %d\n", off, (int)(rr-amem));
for(qq = rr, pp = p; pp <= q; pp++, qq++)
if(*pp) {
if(qq > r)
error("action table overflow");
if(qq > memp)
memp = qq;
*qq = *pp;
}
if(pkdebug && foutput != 0)
for(pp = amem; pp <= memp; pp += 10) {
Bprint(foutput, "\t");
for(qq = pp; qq <= pp+9; qq++)
Bprint(foutput, "%d ", *qq);
Bprint(foutput, "\n");
}
return(off);
nextk:;
}
error("no space in action table");
return 0;
}
void
go2out(void)
{
int i, j, k, best, count, cbest, times;
Bprint(ftemp, "$\n");
for(i = 1; i <= nnonter; i++) {
go2gen(i);
best = -1;
times = 0;
for(j = 0; j <= nstate; j++) {
if(tystate[j] == 0)
continue;
if(tystate[j] == best)
continue;
count = 0;
cbest = tystate[j];
for(k = j; k <= nstate; k++)
if(tystate[k] == cbest)
count++;
if(count > times) {
best = cbest;
times = count;
}
}
zzgobest += times-1;
for(j = 0; j <= nstate; j++)
if(tystate[j] != 0 && tystate[j] != best) {
Bprint(ftemp, "%d,%d,", j, tystate[j]);
zzgoent++;
}
if(best == -1)
best = 0;
zzgoent++;
Bprint(ftemp, "%d\n", best);
}
}
void
go2gen(int c)
{
int i, work, cc;
Item *p, *q;
aryfil(temp1, nnonter+1, 0);
temp1[c] = 1;
work = 1;
while(work) {
work = 0;
PLOOP(0, i)
if((cc=prdptr[i][1]-NTBASE) >= 0)
if(temp1[cc] != 0) {
cc = *prdptr[i]-NTBASE;
if(temp1[cc] == 0) {
work = 1;
temp1[cc] = 1;
}
}
}
if(g2debug && foutput != 0) {
Bprint(foutput, "%s: gotos on ", nontrst[c].name);
NTLOOP(i)
if(temp1[i])
Bprint(foutput, "%s ", nontrst[i].name);
Bprint(foutput, "\n");
}
aryfil(tystate, nstate, 0);
SLOOP(i)
ITMLOOP(i, p, q)
if((cc = *p->pitem) >= NTBASE)
if(temp1[cc-NTBASE]) {
tystate[i] = amem[indgo[i]+c];
break;
}
}
void
precftn(int r, int t, int s)
{
int lp, lt, action;
lp = levprd[r];
lt = toklev[t];
if(PLEVEL(lt) == 0 || PLEVEL(lp) == 0) {
if(foutput != 0)
Bprint(foutput,
"\n%d: shift/reduce conflict (shift %d(%d), red'n %d(%d)) on %s",
s, temp1[t], PLEVEL(lt), r, PLEVEL(lp), symnam(t));
zzsrconf++;
return;
}
if(PLEVEL(lt) == PLEVEL(lp))
action = ASSOC(lt);
else
if(PLEVEL(lt) > PLEVEL(lp))
action = RASC;
else
action = LASC;
switch(action) {
case BASC:
temp1[t] = ERRCODE;
break;
case LASC:
temp1[t] = -r;
break;
}
}
void
wract(int i)
{
int p, p0, p1, ntimes, tred, count, j, flag;
lastred = 0;
ntimes = 0;
TLOOP(j) {
if(temp1[j] >= 0)
continue;
if(temp1[j]+lastred == 0)
continue;
count = 0;
tred = -temp1[j];
levprd[tred] |= REDFLAG;
TLOOP(p)
if(temp1[p]+tred == 0)
count++;
if(count > ntimes) {
lastred = tred;
ntimes = count;
}
}
if(temp1[2] > 0)
lastred = 0;
TLOOP(p)
if(temp1[p]+lastred == 0)
temp1[p] = 0;
wrstate(i);
defact[i] = lastred;
flag = 0;
TLOOP(p0)
if((p1=temp1[p0]) != 0) {
if(p1 < 0) {
p1 = -p1;
goto exc;
}
if(p1 == ACCEPTCODE) {
p1 = -1;
goto exc;
}
if(p1 == ERRCODE) {
p1 = 0;
exc:
if(flag++ == 0)
Bprint(ftable, "-1, %d,\n", i);
Bprint(ftable, "\t%d, %d,\n", p0, p1);
zzexcp++;
continue;
}
Bprint(ftemp, "%d,%d,", p0, p1);
zzacent++;
}
if(flag) {
defact[i] = -2;
Bprint(ftable, "\t-2, %d,\n", lastred);
}
Bprint(ftemp, "\n");
}
void
wrstate(int i)
{
int j0, j1;
Item *pp, *qq;
Wset *u;
if(fdebug) {
if(lastred) {
Bprint(fdebug, "	0, \n", i);
} else {
Bprint(fdebug, "	\"");
ITMLOOP(i, pp, qq)
Bprint(fdebug, "%s\\n", writem(pp->pitem));
if(tystate[i] == MUSTLOOKAHEAD)
WSLOOP(wsets + (pstate[i+1] - pstate[i]), u)
if(*u->pitem < 0)
Bprint(fdebug, "%s\\n", writem(u->pitem));
Bprint(fdebug, "\", \n", i);
}
}
if(foutput == 0)
return;
Bprint(foutput, "\nstate %d\n", i);
ITMLOOP(i, pp, qq)
Bprint(foutput, "\t%s\n", writem(pp->pitem));
if(tystate[i] == MUSTLOOKAHEAD)
WSLOOP(wsets+(pstate[i+1]-pstate[i]), u)
if(*u->pitem < 0)
Bprint(foutput, "\t%s\n", writem(u->pitem));
TLOOP(j0)
if((j1=temp1[j0]) != 0) {
Bprint(foutput, "\n\t%s  ", symnam(j0));
if(j1 > 0) {
if(j1 == ACCEPTCODE)
Bprint(foutput, "accept");
else
if(j1 == ERRCODE)
Bprint(foutput, "error");
else
Bprint(foutput, "shift %d", j1);
} else
Bprint(foutput, "reduce %d (src line %d)", -j1, rlines[-j1]);
}
if(lastred)
Bprint(foutput, "\n\t.  reduce %d (src line %d)\n\n",
lastred, rlines[lastred]);
else
Bprint(foutput, "\n\t.  error\n\n");
j1 = ntokens;
for(j0 = 1; j0 <= nnonter; j0++) {
j1++;
if(temp1[j1])
Bprint(foutput, "\t%s  goto %d\n", symnam(j0+NTBASE), temp1[j1]);
}
}
void
warray(char *s, int *v, int n)
{
int i;
Bprint(ftable, "short	%s[] =\n{", s);
for(i=0;;) {
if(i%10 == 0)
Bprint(ftable, "\n");
Bprint(ftable, "%4d", v[i]);
i++;
if(i >= n) {
Bprint(ftable, "\n};\n");
break;
}
Bprint(ftable, ",");
}
}
void
hideprod(void)
{
int i, j;
j = 0;
levprd[0] = 0;
PLOOP(1, i) {
if(!(levprd[i] & REDFLAG)) {
j++;
if(foutput != 0)
Bprint(foutput, "Rule not reduced:   %s\n", writem(prdptr[i]));
}
levprd[i] = *prdptr[i] - NTBASE;
}
if(j)
print("%d rules never reduced\n", j);
}
void
callopt(void)
{
int i, *p, j, k, *q;
finput = Bopen(tempname, OREAD);
if(finput == 0)
error("optimizer cannot open tempfile");
pgo[0] = 0;
temp1[0] = 0;
nstate = 0;
nnonter = 0;
for(;;) {
switch(gtnm()) {
case '\n':
nstate++;
pmem--;
temp1[nstate] = pmem - mem0;
case ',':
continue;
case '$':
break;
default:
error("bad tempfile");
}
break;
}
pmem--;
temp1[nstate] = yypgo[0] = pmem - mem0;
for(;;) {
switch(gtnm()) {
case '\n':
nnonter++;
yypgo[nnonter] = pmem-mem0;
case ',':
continue;
case -1:
break;
default:
error("bad tempfile");
}
break;
}
pmem--;
yypgo[nnonter--] = pmem - mem0;
for(i = 0; i < nstate; i++) {
k = 32000;
j = 0;
q = mem0 + temp1[i+1];
for(p = mem0 + temp1[i]; p < q ; p += 2) {
if(*p > j)
j = *p;
if(*p < k)
k = *p;
}
if(k <= j) {
if(k > maxoff)
maxoff = k;
}
tystate[i] = (temp1[i+1]-temp1[i]) + 2*j;
if(j > maxspr)
maxspr = j;
}
for(i = 1; i <= nnonter; i++) {
ggreed[i] = 1;
j = 0;
q = mem0 + yypgo[i+1] - 1;
for(p = mem0+yypgo[i]; p < q ; p += 2) {
ggreed[i] += 2;
if(*p > j)
j = *p;
}
ggreed[i] = ggreed[i] + 2*j;
if(j > maxoff)
maxoff = j;
}
for(i = 0; i < ACTSIZE; i++)
amem[i] = 0;
maxa = amem;
for(i = 0; i < nstate; i++) {
if(tystate[i] == 0 && adb > 1)
Bprint(ftable, "State %d: null\n", i);
indgo[i] = YYFLAG1;
}
while((i = nxti()) != NOMORE)
if(i >= 0)
stin(i);
else
gin(-i);
if(adb > 2 )
for(p = amem; p <= maxa; p += 10) {
Bprint(ftable, "%4d  ", (int)(p-amem));
for(i = 0; i < 10; ++i)
Bprint(ftable, "%4d  ", p[i]);
Bprint(ftable, "\n");
}
aoutput();
osummary();
Bterm(finput);
ZAPFILE(tempname);
}
void
gin(int i)
{
int *p, *r, *s, *q1, *q2;
ggreed[i] = 0;
q2 = mem0+ yypgo[i+1] - 1;
q1 = mem0 + yypgo[i];
for(p = amem; p < &amem[ACTSIZE]; p++) {
if(*p)
continue;
for(r = q1; r < q2; r += 2) {
s = p + *r + 1;
if(*s)
goto nextgp;
if(s > maxa)
if((maxa = s) > &amem[ACTSIZE])
error("a array overflow");
}
*p = *q2;
if(p > maxa)
if((maxa = p) > &amem[ACTSIZE])
error("a array overflow");
for(r = q1; r < q2; r += 2) {
s = p + *r + 1;
*s = r[1];
}
pgo[i] = p-amem;
if(adb > 1)
Bprint(ftable, "Nonterminal %d, entry at %d\n", i, pgo[i]);
return;
nextgp:;
}
error("cannot place goto %d\n", i);
}
void
stin(int i)
{
int *r, *s, n, flag, j, *q1, *q2;
tystate[i] = 0;
q2 = mem0+temp1[i+1];
q1 = mem0+temp1[i];
for(n = -maxoff; n < ACTSIZE; n++) {
flag = 0;
for(r = q1; r < q2; r += 2) {
if((s = *r + n + amem) < amem)
goto nextn;
if(*s == 0)
flag++;
else
if(*s != r[1])
goto nextn;
}
for(j=0; j<nstate; j++) {
if(indgo[j] == n) {
if(flag)
goto nextn;
if(temp1[j+1]+temp1[i] == temp1[j]+temp1[i+1]) {
indgo[i] = n;
if(adb > 1)
Bprint(ftable,
"State %d: entry at %d equals state %d\n",
i, n, j);
return;
}
goto nextn;
}
}
for(r = q1; r < q2; r += 2) {
if((s = *r+n+amem) >= &amem[ACTSIZE])
error("out of space in optimizer a array");
if(s > maxa)
maxa = s;
if(*s != 0 && *s != r[1])
error("clobber of a array, pos'n %d, by %d", s-amem, r[1]);
*s = r[1];
}
indgo[i] = n;
if(adb > 1)
Bprint(ftable, "State %d: entry at %d\n", i, indgo[i]);
return;
nextn:;
}
error("Error; failure to place state %d\n", i);
}
int
nxti(void)
{
int i, max, maxi;
max = 0;
maxi = 0;
for(i = 1; i <= nnonter; i++)
if(ggreed[i] >= max) {
max = ggreed[i];
maxi = -i;
}
for(i = 0; i < nstate; ++i)
if(tystate[i] >= max) {
max = tystate[i];
maxi = i;
}
if(nxdb)
Bprint(ftable, "nxti = %d, max = %d\n", maxi, max);
if(max == 0)
return NOMORE;
return maxi;
}
void
osummary(void)
{
int i, *p;
if(foutput == 0)
return;
i = 0;
for(p = maxa; p >= amem; p--)
if(*p == 0)
i++;
Bprint(foutput, "Optimizer space used: input %d/%d, output %d/%d\n",
(int)(pmem-mem0+1), MEMSIZE, (int)(maxa-amem+1), ACTSIZE);
Bprint(foutput, "%d table entries, %d zero\n", (int)(maxa-amem+1), i);
Bprint(foutput, "maximum spread: %d, maximum offset: %d\n", maxspr, maxoff);
}
void
aoutput(void)
{
Bprint(ftable, "#define\tYYLAST\t%d\n", (int)(maxa-amem+1));
arout("yyact", amem, (maxa-amem)+1);
arout("yypact", indgo, nstate);
arout("yypgo", pgo, nnonter+1);
}
void
arout(char *s, int *v, int n)
{
int i;
Bprint(ftable, "short	%s[] =\n{", s);
for(i = 0; i < n;) {
if(i%10 == 0)
Bprint(ftable, "\n");
Bprint(ftable, "%4d", v[i]);
i++;
if(i == n)
Bprint(ftable, "\n};\n");
else
Bprint(ftable, ",");
}
}
int
gtnm(void)
{
int sign, val, c;
sign = 0;
val = 0;
while((c=Bgetrune(finput)) != Beof) {
if(isdigit(c)) {
val = val*10 + c-'0';
continue;
}
if(c == '-') {
sign = 1;
continue;
}
break;
}
if(sign)
val = -val;
*pmem++ = val;
if(pmem >= &mem0[MEMSIZE])
error("out of space");
return c;
}