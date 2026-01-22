#include "tdef.h"
#include "fns.h"
#include "ext.h"
#include "dwbinit.h"
#include <setjmp.h>
#include <time.h>
char	*Version	= "March 11, 1994";
#ifndef DWBVERSION
#define DWBVERSION      "???"
#endif
char	*DWBfontdir = FONTDIR;
char	*DWBntermdir = NTERMDIR;
char	*DWBalthyphens = ALTHYPHENS;
char	*DWBhomedir = "";
dwbinit dwbpaths[] = {
&DWBfontdir, NULL, 0,
&DWBntermdir, NULL, 0,
&DWBalthyphens, NULL, 0,
&DWBhomedir, NULL, 0,
NULL, nextf, NS,
NULL, NULL, 0
};
int	TROFF	= 1;
jmp_buf sjbuf;
Offset	ipl[NSO];
static	FILE	*ifile	= stdin;
static	FILE	*ifl[NSO];
char	cfname[NSO+1][NS] = {  "stdin" };
int	cfline[NSO];
char	*progname;
int	trace = 0;
int	trace1 = 0;
main(int argc, char *argv[])
{
char *p;
int j;
Tchar i;
char buf[100];
buf[0] = '\0';
progname = argv[0];
if ((p = strrchr(progname, '/')) == NULL)
p = progname;
else
p++;
DWBinit(progname, dwbpaths);
if (strcmp(p, "nroff") == 0)
TROFF = 0;
#ifdef UNICODE
alphabet = 128;
#endif
mnspace();
nnspace();
mrehash();
nrehash();
numtabp[NL].val = -1;
while (--argc > 0 && (++argv)[0][0] == '-')
switch (argv[0][1]) {
case 'N':
TROFF = 0;
break;
case 'd':
fprintf(stderr, "troff/nroff version %s\n", Version);
break;
case 'F':
if (argv[0][2] != '\0') {
strcpy(termtab, &argv[0][2]);
strcpy(fontdir, &argv[0][2]);
} else {
argv++; argc--;
strcpy(termtab, argv[0]);
strcpy(fontdir, argv[0]);
}
break;
case 0:
goto start;
case 'i':
stdi++;
break;
case 'n':
npn = atoi(&argv[0][2]);
break;
case 'u':
bdtab[3] = atoi(&argv[0][2]);
if (bdtab[3] < 0 || bdtab[3] > 50)
bdtab[3] = 0;
break;
case 's':
if (!(stop = atoi(&argv[0][2])))
stop++;
break;
case 'r':
sprintf(buf + strlen(buf), ".nr %c %s\n",
argv[0][2], &argv[0][3]);
break;
case 'm':
if (mflg++ >= NMF) {
ERROR "Too many macro packages: %s", argv[0] WARN;
break;
}
strcpy(mfiles[nmfi], nextf);
strcat(mfiles[nmfi++], &argv[0][2]);
break;
case 'o':
getpn(&argv[0][2]);
break;
case 'T':
strcpy(devname, &argv[0][2]);
dotT++;
break;
case 'a':
ascii = 1;
break;
case 'h':
hflg++;
break;
case 'e':
eqflg++;
break;
case 'q':
quiet++;
save_tty();
break;
case 'V':
fprintf(stdout, "%croff: DWB %s\n",
TROFF ? 't' : 'n', DWBVERSION);
exit(0);
case 't':
if (argv[0][2] != '\0')
trace = trace1 = argv[0][2];
break;
default:
ERROR "unknown option %s", argv[0] WARN;
done(02);
}
start:
if (buf[0]) {
char *p = buf;
while(*p++)
;
while(p > buf) {
while(strncmp(p, ".nr", 3) != 0)
p--;
cpushback(p);
*p-- = '\0';
}
}
argp = argv;
rargc = argc;
nmfi = 0;
init2();
setjmp(sjbuf);
loop:
copyf = lgf = nb = nflush = nlflg = 0;
if (ip && rbf0(ip) == 0 && ejf && frame->pframe <= ejl && dip == d) {
nflush++;
trap = 0;
eject((Stack *)0);
goto loop;
}
i = getch();
if (pendt)
goto Lt;
if ((j = cbits(i)) == XPAR) {
copyf++;
tflg++;
while (cbits(i) != '\n')
pchar(i = getch());
tflg = 0;
copyf--;
goto loop;
}
if (j == cc || j == c2) {
if (j == c2)
nb++;
copyf++;
while ((j = cbits(i = getch())) == ' ' || j == '\t')
;
ch = i;
copyf--;
control(getrq(), 1);
flushi();
goto loop;
}
Lt:
ch = i;
text();
if (nlflg)
numtabp[HP].val = 0;
goto loop;
}
void init2(void)
{
int i;
char buf[100];
for (i = NTRTAB; --i; )
trtab[i] = i;
trtab[UNPAD] = ' ';
iflg = 0;
obufp = obuf;
if (TROFF)
t_ptinit();
else
n_ptinit();
mchbits();
cvtime();
numtabp[PID].val = getpid();
numtabp[HP].val = init = 0;
numtabp[NL].val = -1;
nfo = 0;
copyf = raw = 0;
sprintf(buf, ".ds .T %s\n", devname);
cpushback(buf);
sprintf(buf, ".ds .P %s\n", DWBhomedir);
cpushback(buf);
numtabp[CD].val = -1;
nx = mflg;
frame = stk = (Stack *)setbrk(STACKSIZE);
dip = &d[0];
nxf = frame + 1;
for (i = 1; i < NEV; i++)
envcopy(&env[i], &env[0]);
for (i = 0; i < NEV; i++) {
if ((env[i]._word._bufp = (Tchar *)calloc(WDSIZE, sizeof(Tchar))) == NULL) {
ERROR "not enough room for word buffers" WARN;
done2(1);
}
env[i]._word._size = WDSIZE;
if ((env[i]._line._bufp = (Tchar *)calloc(LNSIZE, sizeof(Tchar))) == NULL) {
ERROR "not enough room for line buffers" WARN;
done2(1);
}
env[i]._line._size = LNSIZE;
}
if ((oline = (Tchar *)calloc(OLNSIZE, sizeof(Tchar))) == NULL) {
ERROR "not enough room for line buffers" WARN;
done2(1);
}
olinep = oline;
olnsize = OLNSIZE;
blockinit();
}
void cvtime(void)
{
long tt;
struct tm *ltime;
time(&tt);
ltime = localtime(&tt);
numtabp[YR].val = ltime->tm_year % 100;
numtabp[YR].fmt = 2;
numtabp[MO].val = ltime->tm_mon + 1;
numtabp[DY].val = ltime->tm_mday;
numtabp[DW].val = ltime->tm_wday + 1;
}
char	errbuf[200];
void errprint(void)
{
int savecd = numtabp[CD].val;
if (!nlflg)
numtabp[CD].val++;
fprintf(stderr, "%s: ", progname);
fputs(errbuf, stderr);
if (cfname[ifi][0])
fprintf(stderr, "; %s:%d", cfname[ifi], numtabp[CD].val);
fputs("\n", stderr);
if (cfname[ifi][0])
stackdump();
numtabp[CD].val = savecd;
}
int control(int a, int b)
{
int j, k;
extern Contab *contabp;
numerr.type = RQERR;
numerr.req = a;
if (a == 0 || (j = findmn(a)) == -1)
return(0);
if (contabp[j].f == 0) {
if (trace & TRMAC)
fprintf(stderr, "invoke macro %s\n", unpair(a));
if (dip != d)
for (k = dilev; k; k--)
if (d[k].curd == a) {
ERROR "diversion %s invokes itself during diversion",
unpair(a) WARN;
edone(0100);
}
nxf->nargs = 0;
if (b)
collect();
flushi();
return pushi(contabp[j].mx, a);
}
if (b) {
if (trace & TRREQ)
fprintf(stderr, "invoke request %s\n", unpair(a));
(*contabp[j].f)();
}
return(0);
}
void casept(void)
{
int i;
noscale++;
if (skip())
i = trace1;
else {
i = max(inumb(&trace), 0);
if (nonumb)
i = trace1;
}
trace1 = trace;
trace = i;
noscale = 0;
}
int getrq(void)
{
int i, j;
if ((i = getach()) == 0 || (j = getach()) == 0)
goto rtn;
i = PAIR(i, j);
rtn:
return(i);
}
char gchtab[NCHARS] = {
000,004,000,000,010,000,000,000,
001,002,001,000,001,000,000,000,
000,000,000,000,000,000,000,000,
000,001,000,001,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,001,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
000,000,000,000,000,000,000,000,
};
int realcbits(Tchar c)
{
if (ismot(c))
return MOTCH;
else
return c & 0xFFFF;
}
Tchar getch(void)
{
int k;
Tchar i, j;
g0:
if (ch) {
i = ch;
if (cbits(i) == '\n')
nlflg++;
ch = 0;
return(i);
}
if (nlflg)
return('\n');
i = getch0();
if (ismot(i))
return(i);
k = cbits(i);
if (k >= sizeof(gchtab)/sizeof(gchtab[0]) || gchtab[k] == 0)
return(i);
if (k != ESC) {
if (k == '\n') {
nlflg++;
if (ip == 0)
numtabp[CD].val++;
return(k);
}
if (k == FLSS) {
copyf++;
raw++;
i = getch0();
if (!fi)
flss = i;
copyf--;
raw--;
goto g0;
}
if (k == RPT) {
setrpt();
goto g0;
}
if (!copyf) {
if (k == 'f' && lg && !lgf) {
i = getlg(i);
return(i);
}
if (k == fc || k == tabch || k == ldrch) {
if ((i = setfield(k)) == 0)
goto g0;
else
return(i);
}
if (k == '\b') {
i = makem(-width(' ' | chbits));
return(i);
}
}
return(i);
}
k = cbits(j = getch0());
if (ismot(j))
return(j);
switch (k) {
case 'n':
setn();
goto g0;
case '$':
seta();
goto g0;
case '*':
setstr();
goto g0;
case '{':
i = LEFT;
goto gx;
case '}':
i = RIGHT;
goto gx;
case '"':
while (cbits(i = getch0()) != '\n')
;
if (ip == 0)
numtabp[CD].val++;
nlflg++;
return(i);
case '(':
case 'C':
if ((i = setch(k)) == 0)
goto g0;
goto gx;
case ESC:
i = eschar;
goto gx;
case 'e':
i = PRESC;
goto gx;
case '\n':
numtabp[CD].val++;
goto g0;
case ' ':
i = UNPAD;
goto gx;
case '\'':
i = ACUTE;
goto gx;
case '`':
i = GRAVE;
goto gx;
case '_':
i = UNDERLINE;
goto gx;
case '-':
i = MINUS;
goto gx;
case '&':
i = FILLER;
goto gx;
case 'c':
i = CONT;
goto gx;
case '!':
i = XPAR;
goto gx;
case 't':
i = '\t';
return(i);
case 'a':
i = LEADER;
return i;
case '%':
i = OHC;
return(i);
case 'g':
setaf();
goto g0;
case '.':
i = '.';
gx:
setsfbits(i, sfbits(j));
return(i);
}
if (copyf) {
*pbp++ = j;
return(eschar);
}
switch (k) {
case 'f':
setfont(0);
goto g0;
case 's':
setps();
goto g0;
case 'v':
numerr.type = numerr.escarg = 0; numerr.esc = k;
if (i = vmot()) {
return(i);
}
goto g0;
case 'h':
numerr.type = numerr.escarg = 0; numerr.esc = k;
if (i = hmot())
return(i);
goto g0;
case '|':
if (NROFF)
goto g0;
return(makem((int)(EM)/6));
case '^':
if (NROFF)
goto g0;
return(makem((int)(EM)/12));
case 'w':
setwd();
goto g0;
case 'p':
spread++;
goto g0;
case 'N':
numerr.type = numerr.escarg = 0; numerr.esc = k;
if ((i = setabs()) == 0)
goto g0;
return i;
case 'H':
numerr.type = numerr.escarg = 0; numerr.esc = k;
return(setht());
case 'S':
numerr.type = numerr.escarg = 0; numerr.esc = k;
return(setslant());
case 'z':
return(setz());
case 'l':
numerr.type = numerr.escarg = 0; numerr.esc = k;
setline();
goto g0;
case 'L':
numerr.type = numerr.escarg = 0; numerr.esc = k;
setvline();
goto g0;
case 'D':
numerr.type = numerr.escarg = 0; numerr.esc = k;
setdraw();
goto g0;
case 'X':
setxon();
goto g0;
case 'b':
setbra();
goto g0;
case 'o':
setov();
goto g0;
case 'k':
if ((k = findr(getsn())) != -1) {
numtabp[k].val = numtabp[HP].val;
}
goto g0;
case '0':
return(makem(width('0' | chbits)));
case 'x':
numerr.type = numerr.escarg = 0; numerr.esc = k;
if (i = xlss())
return(i);
goto g0;
case 'u':
case 'r':
case 'd':
return(sethl(k));
default:
return(j);
}
}
void setxon(void)
{
Tchar xbuf[NC];
Tchar *i;
Tchar c;
int delim, k;
if (ismot(c = getch()))
return;
delim = cbits(c);
i = xbuf;
*i++ = XON | chbits;
while ((k = cbits(c = getch())) != delim && k != '\n' && i < xbuf+NC-1) {
if (k == ' ')
setcbits(c, WORDSP);
*i++ = c | ZBIT;
}
*i++ = XOFF | chbits;
*i = 0;
pushback(xbuf);
}
char	ifilt[32] = { 0, 001, 002, 003, 0, 005, 006, 007, 010, 011, 012 };
Tchar getch0(void)
{
Tchar i;
again:
if (pbp > lastpbp)
i = *--pbp;
else if (ip) {
i = rbf0(ip);
if (i == 0)
i = rbf();
else {
++ip;
if (pastend(ip)) {
--ip;
rbf();
}
}
} else {
if (donef || ndone)
done(0);
if (nx || 1) {
if (nfo < 0)
ERROR "in getch0, nfo = %d", nfo WARN;
if (nfo == 0) {
g0:
if (nextfile()) {
if (ip)
goto again;
}
}
nx = 0;
#ifdef UNICODE
if (MB_CUR_MAX > 1)
i = get1ch(ifile);
else
#endif
i = getc(ifile);
if (i == EOF)
goto g0;
if (ip)
goto again;
}
if (i >= 040)
goto g4;
i = ifilt[i];
}
if (cbits(i) == IMP && !raw)
goto again;
if (i == 0 && !init && !raw) {
goto again;
}
g4:
if (ismot(i))
return i;
if (copyf == 0 && sfbits(i) == 0)
i |= chbits;
if (cbits(i) == eschar && !raw)
setcbits(i, ESC);
return(i);
}
#ifdef UNICODE
Tchar get1ch(FILE *fp)
{
wchar_t wc;
char buf[100], *p;
int i, n, c;
n = c = 0;
for (i = 0, p = buf; i < MB_CUR_MAX; i++) {
if ((c = getc(fp)) == EOF)
return c;
*p++ = c;
if ((n = mbtowc(&wc, buf, p-buf)) >= 0)
break;
}
if (n == 1)
return wc;
if (n == 0)
return p[-1];
if (c == EOF)
return EOF;
*p = 0;
return chadd(buf, MBchar, Install);
}
#endif
void pushback(Tchar *b)
{
Tchar *ob = b;
while (*b++)
;
b--;
while (b > ob && pbp < &pbbuf[NC-3])
*pbp++ = *--b;
if (pbp >= &pbbuf[NC-3]) {
ERROR "pushback overflow" WARN;
done(2);
}
}
void cpushback(char *b)
{
char *ob = b;
while (*b++)
;
b--;
while (b > ob && pbp < &pbbuf[NC-3])
*pbp++ = *--b;
if (pbp >= &pbbuf[NC-3]) {
ERROR "cpushback overflow" WARN;
done(2);
}
}
int nextfile(void)
{
char *p;
n0:
if (ifile != stdin)
fclose(ifile);
if (ifi > 0 && !nx) {
if (popf())
goto n0;
return(1);
}
if (nx || nmfi < mflg) {
p = mfiles[nmfi++];
if (*p != 0)
goto n1;
}
if (rargc-- <= 0) {
if ((nfo -= mflg) && !stdi) {
done(0);
}
nfo++;
numtabp[CD].val = stdi = mflg = 0;
ifile = stdin;
strcpy(cfname[ifi], "stdin");
return(0);
}
p = (argp++)[0];
if (rargc >= 0)
cfname[ifi][0] = 0;
n1:
numtabp[CD].val = 0;
if (p[0] == '-' && p[1] == 0) {
ifile = stdin;
strcpy(cfname[ifi], "stdin");
} else if ((ifile = fopen(p, "r")) == NULL) {
ERROR "cannot open file %s", p WARN;
nfo -= mflg;
done(02);
} else
strcpy(cfname[ifi],p);
nfo++;
return(0);
}
popf(void)
{
--ifi;
if (ifi < 0) {
ERROR "popf went negative" WARN;
return 1;
}
numtabp[CD].val = cfline[ifi];
ip = ipl[ifi];
ifile = ifl[ifi];
return(0);
}
void flushi(void)
{
if (nflush)
return;
ch = 0;
copyf++;
while (!nlflg) {
if (donef && frame == stk)
break;
getch();
}
copyf--;
}
getach(void)
{
Tchar i;
int j;
lgf++;
j = cbits(i = getch());
if (ismot(i)
|| j > SHORTMASK
|| (j <= 040 && j != 002
&& j != 003
&& j != 005
&& j != 006
&& j != 007)) {
ch = i;
j = 0;
}
lgf--;
return j;
}
void casenx(void)
{
lgf++;
skip();
getname();
nx++;
if (nmfi > 0)
nmfi--;
strcpy(mfiles[nmfi], nextf);
nextfile();
nlflg++;
ip = 0;
pendt = 0;
frame = stk;
nxf = frame + 1;
}
getname(void)
{
int j, k;
lgf++;
for (k = 0; k < NS - 1; k++) {
j = getach();
if (!j)
break;
nextf[k] = j;
}
nextf[k] = 0;
lgf--;
return(nextf[0]);
}
void caseso(void)
{
FILE *fp;
lgf++;
nextf[0] = 0;
fp = NULL;
if (skip() || !getname() || (fp = fopen(nextf, "r")) == NULL || ifi >= NSO) {
ERROR "can't open file %s", nextf WARN;
done(02);
}
strcpy(cfname[ifi+1], nextf);
cfline[ifi] = numtabp[CD].val;
numtabp[CD].val = 0;
flushi();
ifl[ifi] = ifile;
ifile = fp;
ipl[ifi] = ip;
ip = 0;
nx++;
nflush++;
ifi++;
}
void caself(void)
{
int n;
if (skip())
return;
n = atoi0();
if (!nonumb)
cfline[ifi] = numtabp[CD].val = n - 1;
if (!skip())
if (getname()) {
strcpy(cfname[ifi], nextf);
if (!nonumb)
numtabp[CD].val--;
}
}
void cpout(FILE *fin, char *token)
{
int n;
char buf[1024];
if (token) {
char *newl = buf;
while ((fgets(buf, sizeof buf, fin)) != NULL) {
if (newl) {
numtabp[CD].val++;
if (strcmp(token, buf) == 0)
return;
}
newl = strchr(buf, '\n');
fputs(buf, ptid);
}
} else {
while ((n = fread(buf, sizeof *buf, sizeof buf, fin)) > 0)
fwrite(buf, n, 1, ptid);
fclose(fin);
}
}
void casecf(void)
{
FILE *fd;
char *eof, *p;
extern int hpos, esc, po;
lgf++;
nextf[0] = 0;
if (!skip() && getname()) {
if (strncmp("<<", nextf, 2) != 0) {
if ((fd = fopen(nextf, "r")) == NULL) {
ERROR "can't open file %s", nextf WARN;
done(02);
}
eof = (char *) NULL;
} else {
if (pbp > lastpbp || ip) {
ERROR "casecf: not reading from file" WARN;
done(02);
}
eof = &nextf[2];
if (!*eof)  {
ERROR "casecf: missing end of input token" WARN;
done(02);
}
p = eof;
while(*++p)
;
*p++ = '\n';
*p = 0;
fd = ifile;
}
} else {
ERROR "casecf: no argument" WARN;
lgf--;
return;
}
lgf--;
tbreak();
hpos = po;
esc = 0;
ptesc();
esc = un;
ptesc();
ptlead();
ptps();
ptfont();
flusho();
cpout(fd, eof);
ptps();
ptfont();
}
void getline(char *s, int n)
{
int i;
lgf++;
copyf++;
skip();
for (i = 0; i < n-1; i++)
if ((s[i] = cbits(getch())) == '\n' || s[i] == RIGHT)
break;
s[i] = 0;
copyf--;
lgf--;
}
void casesy(void)
{
char sybuf[NTM];
getline(sybuf, NTM);
system(sybuf);
}
void getpn(char *a)
{
int n, neg;
if (*a == 0)
return;
neg = 0;
for ( ; *a; a++)
switch (*a) {
case '+':
case ',':
continue;
case '-':
neg = 1;
continue;
default:
n = 0;
if (isdigit(*a)) {
do
n = 10 * n + *a++ - '0';
while (isdigit(*a));
a--;
} else
n = 9999;
*pnp++ = neg ? -n : n;
neg = 0;
if (pnp >= &pnlist[NPN-2]) {
ERROR "too many page numbers" WARN;
done3(-3);
}
}
if (neg)
*pnp++ = -9999;
*pnp = -INT_MAX;
print = 0;
pnp = pnlist;
if (*pnp != -INT_MAX)
chkpn();
}
void setrpt(void)
{
Tchar i, j;
copyf++;
raw++;
i = getch0();
copyf--;
raw--;
if ((long) i < 0 || cbits(j = getch0()) == RPT)
return;
while (i > 0 && pbp < &pbbuf[NC-3]) {
i--;
*pbp++ = j;
}
}