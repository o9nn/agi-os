#include "e.h"
#include "y.tab.h"
#include <ctype.h>
#include <utf.h>
#define	CSSIZE	1000
char	cs[CSSIZE+20];
char	*csp;
char	*psp;
int	lf, rf;
int	lastft;
int	nextft;
int	pclass;
int	nclass;
int class[LAST][LAST] ={
{ 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 0 },
{ 1, 0, 1, 1, 1, 1, 1, 2, 2, 2, 0 },
{ 1, 1, 0, 1, 1, 1, 1, 3, 2, 1, 0 },
{ 1, 1, 1, 0, 1, 1, 1, 2, 2, 2, 0 },
{ 1, 1, 1, 1, 1, 2, 1, 2, 3, 3, 0 },
{ 2, 2, 2, 1, 1, 1, 1, 2, 3, 3, 0 },
{ 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 0 },
{ 2, 2, 2, 2, 2, 2, 3, 2, 3, 2, 0 },
{ 3, 3, 1, 2, 2, 3, 2, 3, 0, 1, 1 },
{ 1, 1, 1, 1, 1, 1, 1, 2, 2, 0, 0 },
{ 4, 4, 4, 4, 4, 4, 4, 4, 5, 4, 1 },
};
extern void shim(int, int);
extern void roman(int);
extern void sadd(char *);
extern void cadd(int);
extern int trans(int, char *);
int textc(void)
{
wchar_t r;
int w;
w = mbtowc(&r, psp, UTFmax);
if(w == 0){
psp++;
return 0;
}
if(w < 0){
psp += 1;
return Runeerror;
}
psp += w;
return r;
}
void text(int t, char *p1)
{
int c;
char *p;
tbl *tp;
yyval = salloc();
ebase[yyval] = 0;
eht[yyval] = EM(1.0, ps);
lfont[yyval] = rfont[yyval] = ROM;
lclass[yyval] = rclass[yyval] = OTHER;
if (t == QTEXT) {
for (p = p1; *p; p++)
if (*p == '\\' && *(p+1) == 'f')
break;
if (*p)
p = p1;
else {
sprintf(cs, "\\f%s%s\\fP", ftp->name, p1);
p = cs;
}
} else if (t == SPACE)
p = "\\ ";
else if (t == THIN)
p = "\\|";
else if (t == TAB)
p = "\\t";
else if ((tp = lookup(restbl, p1)) != NULL) {
p = tp->cval;
} else {
lf = rf = 0;
lastft = 0;
nclass = NONE;
csp = cs;
for (psp = p1; (c = textc()) != '\0'; ) {
nextft = ft;
pclass = nclass;
rf = trans(c, p1);
if (lf == 0) {
lf = rf;
lclass[yyval] = nclass;
}
if (csp-cs > CSSIZE)
ERROR "converted token %.25s... too long", p1 FATAL ;
}
sadd("\\fP");
*csp = '\0';
p = cs;
lfont[yyval] = lf;
rfont[yyval] = rf;
rclass[yyval] = nclass;
}
dprintf(".\t%dtext: S%d <- %s; b=%g,h=%g,lf=%c,rf=%c,ps=%d\n",
t, yyval, p, ebase[yyval], eht[yyval], lfont[yyval], rfont[yyval], ps);
printf(".ds %d \"%s\n", yyval, p);
}
int isdigitrune(int c)
{
return ('0'<=c && c<='9');
}
int
trans(int c, char *)
{
int f;
if (isalpharune(c) && ft == ITAL && c != 'f' && c != 'j') {
shim(pclass, nclass = ILET);
cadd(c);
return ITAL;
}
if (isalpharune(c) && ft != ITAL) {
shim(pclass, nclass = OLET);
cadd(c);
return ROM;
}
if (isdigitrune(c)) {
shim(pclass, nclass = DIG);
roman(c);
return ROM;
}
f = ROM;
nclass = OTHER;
switch (c) {
case ':': case ';': case '!': case '%': case '?':
shim(pclass, nclass);
roman(c);
return f;
case '(': case '[':
shim(pclass, nclass = LPAR);
roman(c);
return f;
case ')': case ']':
shim(pclass, nclass = RPAR);
roman(c);
return f;
case ',':
shim(pclass, nclass = OTHER);
roman(c);
return f;
case '.':
if (rf == ROM)
roman(c);
else
cadd(c);
return f;
case '|':
shim(pclass, nclass = VBAR);
sadd("\\v'.17m'\\z|\\v'-.17m'\\|");
return f;
case '=':
shim(pclass, nclass = PLUS);
sadd("\\(eq");
return f;
case '+':
shim(pclass, nclass = PLUS);
sadd("\\(pl");
return f;
case '>':
case '<':
shim(pclass, nclass = PLUS);
if (*psp == '=') {
sadd(c == '<' ? "\\(<=" : "\\(>=");
psp++;
} else if (c == '<' && *psp == '-') {
sadd("\\(<-");
psp++;
} else if (*psp == c) {
cadd(c);
cadd(c);
psp++;
} else {
cadd(c);
}
return f;
case '-':
shim(pclass, nclass = PLUS);
if (*psp == '>') {
sadd("\\(->");
psp++;
} else {
sadd("\\(mi");
}
return f;
case '/':
shim(pclass, nclass = SLASH);
cadd('/');
return f;
case '~':
case ' ':
sadd("\\|\\|");
return f;
case '^':
sadd("\\|");
return f;
case '\\':
shim(pclass, nclass);
cadd('\\');
cadd(c = *psp++);
if (c == '(' && *psp && *(psp+1)) {
cadd(*psp++);
cadd(*psp++);
} else
fprintf(stderr, "eqn warning: unquoted troff command \\%c, file %s:%d\n",
c, curfile->fname, curfile->lineno);
return f;
case '\'':
shim(pclass, nclass);
sadd("\\(fm");
return f;
case 'f':
if (ft == ITAL) {
shim(pclass, nclass = ILETF);
cadd('f');
f = ITAL;
} else
cadd('f');
return f;
case 'j':
if (ft == ITAL) {
shim(pclass, nclass = ILETJ);
cadd('j');
f = ITAL;
} else
cadd('j');
return f;
default:
shim(pclass, nclass);
cadd(c);
return ft==ITAL ? ITAL : ROM;
}
}
char *pad(int n)
{
static char buf[20];
buf[0] = 0;
if (n < 0) {
sprintf(buf, "\\h'-%du*\\w'\\^'u'", -n);
return buf;
}
for ( ; n > 1; n -= 2)
strcat(buf, "\\|");
if (n > 0)
strcat(buf, "\\^");
return buf;
}
void shim(int lc, int rc)
{
sadd(pad(class[lc][rc]));
}
void roman(int c)
{
nextft = ROM;
cadd(c);
}
void sadd(char *s)
{
while (*s)
cadd(*s++);
}
void cadd(int c)
{
char *p;
int w;
if (lastft != nextft) {
if (lastft != 0) {
*csp++ = '\\';
*csp++ = 'f';
*csp++ = 'P';
}
*csp++ = '\\';
*csp++ = 'f';
if (ftp == ftstack) {
if (ftp->ft == ITAL)
*csp++ = nextft;
else
for (p = ftp->name; *csp = *p++; )
csp++;
} else {
for (p = ftp->name; *csp = *p++; )
csp++;
}
lastft = nextft;
}
w = wctomb(csp, c);
if(w > 0)
csp += w;
}