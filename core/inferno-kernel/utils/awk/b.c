#define	DEBUG
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "awk.h"
#include "ytab.h"
#define	HAT	(NCHARS-2)
#define MAXLIN 22
#define type(v)		(v)->nobj
#define info(v)		(v)->ntype
#define left(v)		(v)->narg[0]
#define right(v)	(v)->narg[1]
#define parent(v)	(v)->nnext
#define LEAF	case CCL: case NCCL: case CHAR: case DOT: case FINAL: case ALL:
#define UNARY	case STAR: case PLUS: case QUEST:
int	*setvec;
int	*tmpset;
int	maxsetvec = 0;
int	rtok;
int	rlxval;
static uschar	*rlxstr;
static uschar	*prestr;
static uschar	*lastre;
static	int setcnt;
static	int poscnt;
char	*patbeg;
int	patlen;
#define	NFA	20
fa	*fatab[NFA];
int	nfatab	= 0;
fa *makedfa(char *s, int anchor)
{
int i, use, nuse;
fa *pfa;
static int now = 1;
if (setvec == 0) {
maxsetvec = MAXLIN;
setvec = (int *) malloc(maxsetvec * sizeof(int));
tmpset = (int *) malloc(maxsetvec * sizeof(int));
if (setvec == 0 || tmpset == 0)
overflo("out of space initializing makedfa");
}
if (compile_time)
return mkdfa(s, anchor);
for (i = 0; i < nfatab; i++)
if (fatab[i]->anchor == anchor
&& strcmp(fatab[i]->restr, s) == 0) {
fatab[i]->use = now++;
return fatab[i];
}
pfa = mkdfa(s, anchor);
if (nfatab < NFA) {
fatab[nfatab] = pfa;
fatab[nfatab]->use = now++;
nfatab++;
return pfa;
}
use = fatab[0]->use;
nuse = 0;
for (i = 1; i < nfatab; i++)
if (fatab[i]->use < use) {
use = fatab[i]->use;
nuse = i;
}
freefa(fatab[nuse]);
fatab[nuse] = pfa;
pfa->use = now++;
return pfa;
}
fa *mkdfa(char *s, int anchor)
{
Node *p, *p1;
fa *f;
p = reparse(s);
p1 = op2(CAT, op2(STAR, op2(ALL, NIL, NIL), NIL), p);
p1 = op2(CAT, p1, op2(FINAL, NIL, NIL));
poscnt = 0;
penter(p1);
if ((f = (fa *) calloc(1, sizeof(fa) + poscnt*sizeof(rrow))) == NULL)
overflo("out of space for fa");
f->accept = poscnt-1;
cfoll(f, p1);
freetr(p1);
if ((f->posns[0] = (int *) calloc(1, *(f->re[0].lfollow)*sizeof(int))) == NULL)
overflo("out of space in makedfa");
if ((f->posns[1] = (int *) calloc(1, sizeof(int))) == NULL)
overflo("out of space in makedfa");
*f->posns[1] = 0;
f->initstat = makeinit(f, anchor);
f->anchor = anchor;
f->restr = (uschar *) tostring(s);
return f;
}
int makeinit(fa *f, int anchor)
{
int i, k;
f->curstat = 2;
f->out[2] = 0;
f->reset = 0;
k = *(f->re[0].lfollow);
xfree(f->posns[2]);
if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
overflo("out of space in makeinit");
for (i=0; i <= k; i++) {
(f->posns[2])[i] = (f->re[0].lfollow)[i];
}
if ((f->posns[2])[1] == f->accept)
f->out[2] = 1;
for (i=0; i < NCHARS; i++)
f->gototab[2][i] = 0;
f->curstat = cgoto(f, 2, HAT);
if (anchor) {
*f->posns[2] = k-1;
for (i=0; i < k; i++) {
(f->posns[0])[i] = (f->posns[2])[i];
}
f->out[0] = f->out[2];
if (f->curstat != 2)
--(*f->posns[f->curstat]);
}
return f->curstat;
}
void penter(Node *p)
{
switch (type(p)) {
LEAF
info(p) = poscnt;
poscnt++;
break;
UNARY
penter(left(p));
parent(left(p)) = p;
break;
case CAT:
case OR:
penter(left(p));
penter(right(p));
parent(left(p)) = p;
parent(right(p)) = p;
break;
default:
FATAL("can't happen: unknown type %d in penter", type(p));
break;
}
}
void freetr(Node *p)
{
switch (type(p)) {
LEAF
xfree(p);
break;
UNARY
freetr(left(p));
xfree(p);
break;
case CAT:
case OR:
freetr(left(p));
freetr(right(p));
xfree(p);
break;
default:
FATAL("can't happen: unknown type %d in freetr", type(p));
break;
}
}
int hexstr(char **pp)
{
uschar *p;
int n = 0;
int i;
for (i = 0, p = (uschar *) *pp; i < 2 && isxdigit(*p); i++, p++) {
if (isdigit(*p))
n = 16 * n + *p - '0';
else if (*p >= 'a' && *p <= 'f')
n = 16 * n + *p - 'a' + 10;
else if (*p >= 'A' && *p <= 'F')
n = 16 * n + *p - 'A' + 10;
}
*pp = (char *) p;
return n;
}
#define isoctdigit(c) ((c) >= '0' && (c) <= '7')
int quoted(char **pp)
{
char *p = *pp;
int c;
if ((c = *p++) == 't')
c = '\t';
else if (c == 'n')
c = '\n';
else if (c == 'f')
c = '\f';
else if (c == 'r')
c = '\r';
else if (c == 'b')
c = '\b';
else if (c == '\\')
c = '\\';
else if (c == 'x') {
c = hexstr(&p);
} else if (isoctdigit(c)) {
int n = c - '0';
if (isoctdigit(*p)) {
n = 8 * n + *p++ - '0';
if (isoctdigit(*p))
n = 8 * n + *p++ - '0';
}
c = n;
}
*pp = p;
return c;
}
char *cclenter(char *argp)
{
int i, c, c2;
uschar *p = (uschar *) argp;
uschar *op, *bp;
static uschar *buf = 0;
static int bufsz = 100;
op = p;
if (buf == 0 && (buf = (uschar *) malloc(bufsz)) == NULL)
FATAL("out of space for character class [%.10s...] 1", p);
bp = buf;
for (i = 0; (c = *p++) != 0; ) {
if (c == '\\') {
c = quoted((char **) &p);
} else if (c == '-' && i > 0 && bp[-1] != 0) {
if (*p != 0) {
c = bp[-1];
c2 = *p++;
if (c2 == '\\')
c2 = quoted((char **) &p);
if (c > c2) {
bp--;
i--;
continue;
}
while (c < c2) {
if (!adjbuf((char **) &buf, &bufsz, bp-buf+2, 100, (char **) &bp, 0))
FATAL("out of space for character class [%.10s...] 2", p);
*bp++ = ++c;
i++;
}
continue;
}
}
if (!adjbuf((char **) &buf, &bufsz, bp-buf+2, 100, (char **) &bp, 0))
FATAL("out of space for character class [%.10s...] 3", p);
*bp++ = c;
i++;
}
*bp = 0;
dprintf( ("cclenter: in = |%s|, out = |%s|\n", op, buf) );
xfree(op);
return (char *) tostring((char *) buf);
}
void overflo(char *s)
{
FATAL("regular expression too big: %.30s...", s);
}
void cfoll(fa *f, Node *v)
{
int i;
int *p;
switch (type(v)) {
LEAF
f->re[info(v)].ltype = type(v);
f->re[info(v)].lval.np = right(v);
while (f->accept >= maxsetvec) {
maxsetvec *= 4;
setvec = (int *) realloc(setvec, maxsetvec * sizeof(int));
tmpset = (int *) realloc(tmpset, maxsetvec * sizeof(int));
if (setvec == 0 || tmpset == 0)
overflo("out of space in cfoll()");
}
for (i = 0; i <= f->accept; i++)
setvec[i] = 0;
setcnt = 0;
follow(v);
if ((p = (int *) calloc(1, (setcnt+1)*sizeof(int))) == NULL)
overflo("out of space building follow set");
f->re[info(v)].lfollow = p;
*p = setcnt;
for (i = f->accept; i >= 0; i--)
if (setvec[i] == 1)
*++p = i;
break;
UNARY
cfoll(f,left(v));
break;
case CAT:
case OR:
cfoll(f,left(v));
cfoll(f,right(v));
break;
default:
FATAL("can't happen: unknown type %d in cfoll", type(v));
}
}
int first(Node *p)
{
int b, lp;
switch (type(p)) {
LEAF
lp = info(p);
while (setcnt >= maxsetvec || lp >= maxsetvec) {
maxsetvec *= 4;
setvec = (int *) realloc(setvec, maxsetvec * sizeof(int));
tmpset = (int *) realloc(tmpset, maxsetvec * sizeof(int));
if (setvec == 0 || tmpset == 0)
overflo("out of space in first()");
}
if (setvec[lp] != 1) {
setvec[lp] = 1;
setcnt++;
}
if (type(p) == CCL && (*(char *) right(p)) == '\0')
return(0);
else return(1);
case PLUS:
if (first(left(p)) == 0) return(0);
return(1);
case STAR:
case QUEST:
first(left(p));
return(0);
case CAT:
if (first(left(p)) == 0 && first(right(p)) == 0) return(0);
return(1);
case OR:
b = first(right(p));
if (first(left(p)) == 0 || b == 0) return(0);
return(1);
}
FATAL("can't happen: unknown type %d in first", type(p));
return(-1);
}
void follow(Node *v)
{
Node *p;
if (type(v) == FINAL)
return;
p = parent(v);
switch (type(p)) {
case STAR:
case PLUS:
first(v);
follow(p);
return;
case OR:
case QUEST:
follow(p);
return;
case CAT:
if (v == left(p)) {
if (first(right(p)) == 0) {
follow(p);
return;
}
} else
follow(p);
return;
}
}
int member(int c, char *sarg)
{
uschar *s = (uschar *) sarg;
while (*s)
if (c == *s++)
return(1);
return(0);
}
int match(fa *f, char *p0)
{
int s, ns;
uschar *p = (uschar *) p0;
s = f->reset ? makeinit(f,0) : f->initstat;
if (f->out[s])
return(1);
do {
if ((ns = f->gototab[s][*p]) != 0)
s = ns;
else
s = cgoto(f, s, *p);
if (f->out[s])
return(1);
} while (*p++ != 0);
return(0);
}
int pmatch(fa *f, char *p0)
{
int s, ns;
uschar *p = (uschar *) p0;
uschar *q;
int i, k;
s = f->reset ? makeinit(f,1) : f->initstat;
patbeg = (char *) p;
patlen = -1;
do {
q = p;
do {
if (f->out[s])
patlen = q-p;
if ((ns = f->gototab[s][*q]) != 0)
s = ns;
else
s = cgoto(f, s, *q);
if (s == 1) {
if (patlen >= 0) {
patbeg = (char *) p;
return(1);
}
else
goto nextin;
}
} while (*q++ != 0);
if (f->out[s])
patlen = q-p-1;
if (patlen >= 0) {
patbeg = (char *) p;
return(1);
}
nextin:
s = 2;
if (f->reset) {
for (i = 2; i <= f->curstat; i++)
xfree(f->posns[i]);
k = *f->posns[0];
if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
overflo("out of space in pmatch");
for (i = 0; i <= k; i++)
(f->posns[2])[i] = (f->posns[0])[i];
f->initstat = f->curstat = 2;
f->out[2] = f->out[0];
for (i = 0; i < NCHARS; i++)
f->gototab[2][i] = 0;
}
} while (*p++ != 0);
return (0);
}
int nematch(fa *f, char *p0)
{
int s, ns;
uschar *p = (uschar *) p0;
uschar *q;
int i, k;
s = f->reset ? makeinit(f,1) : f->initstat;
patlen = -1;
while (*p) {
q = p;
do {
if (f->out[s])
patlen = q-p;
if ((ns = f->gototab[s][*q]) != 0)
s = ns;
else
s = cgoto(f, s, *q);
if (s == 1) {
if (patlen > 0) {
patbeg = (char *) p;
return(1);
} else
goto nnextin;
}
} while (*q++ != 0);
if (f->out[s])
patlen = q-p-1;
if (patlen > 0 ) {
patbeg = (char *) p;
return(1);
}
nnextin:
s = 2;
if (f->reset) {
for (i = 2; i <= f->curstat; i++)
xfree(f->posns[i]);
k = *f->posns[0];
if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
overflo("out of state space");
for (i = 0; i <= k; i++)
(f->posns[2])[i] = (f->posns[0])[i];
f->initstat = f->curstat = 2;
f->out[2] = f->out[0];
for (i = 0; i < NCHARS; i++)
f->gototab[2][i] = 0;
}
p++;
}
return (0);
}
Node *reparse(char *p)
{
Node *np;
dprintf( ("reparse <%s>\n", p) );
lastre = prestr = (uschar *) p;
rtok = relex();
if (rtok == '\0')
FATAL("empty regular expression");
np = regexp();
if (rtok != '\0')
FATAL("syntax error in regular expression %s at %s", lastre, prestr);
return(np);
}
Node *regexp(void)
{
return (alt(concat(primary())));
}
Node *primary(void)
{
Node *np;
switch (rtok) {
case CHAR:
np = op2(CHAR, NIL, itonp(rlxval));
rtok = relex();
return (unary(np));
case ALL:
rtok = relex();
return (unary(op2(ALL, NIL, NIL)));
case DOT:
rtok = relex();
return (unary(op2(DOT, NIL, NIL)));
case CCL:
np = op2(CCL, NIL, (Node*) cclenter((char *) rlxstr));
rtok = relex();
return (unary(np));
case NCCL:
np = op2(NCCL, NIL, (Node *) cclenter((char *) rlxstr));
rtok = relex();
return (unary(np));
case '^':
rtok = relex();
return (unary(op2(CHAR, NIL, itonp(HAT))));
case '$':
rtok = relex();
return (unary(op2(CHAR, NIL, NIL)));
case '(':
rtok = relex();
if (rtok == ')') {
rtok = relex();
return unary(op2(CCL, NIL, (Node *) tostring("")));
}
np = regexp();
if (rtok == ')') {
rtok = relex();
return (unary(np));
}
else
FATAL("syntax error in regular expression %s at %s", lastre, prestr);
default:
FATAL("illegal primary in regular expression %s at %s", lastre, prestr);
}
return 0;
}
Node *concat(Node *np)
{
switch (rtok) {
case CHAR: case DOT: case ALL: case CCL: case NCCL: case '$': case '(':
return (concat(op2(CAT, np, primary())));
}
return (np);
}
Node *alt(Node *np)
{
if (rtok == OR) {
rtok = relex();
return (alt(op2(OR, np, concat(primary()))));
}
return (np);
}
Node *unary(Node *np)
{
switch (rtok) {
case STAR:
rtok = relex();
return (unary(op2(STAR, np, NIL)));
case PLUS:
rtok = relex();
return (unary(op2(PLUS, np, NIL)));
case QUEST:
rtok = relex();
return (unary(op2(QUEST, np, NIL)));
default:
return (np);
}
}
int relex(void)
{
int c, n;
int cflag;
static uschar *buf = 0;
static int bufsz = 100;
uschar *bp;
switch (c = *prestr++) {
case '|': return OR;
case '*': return STAR;
case '+': return PLUS;
case '?': return QUEST;
case '.': return DOT;
case '\0': prestr--; return '\0';
case '^':
case '$':
case '(':
case ')':
return c;
case '\\':
rlxval = quoted((char **) &prestr);
return CHAR;
default:
rlxval = c;
return CHAR;
case '[':
if (buf == 0 && (buf = (uschar *) malloc(bufsz)) == NULL)
FATAL("out of space in reg expr %.10s..", lastre);
bp = buf;
if (*prestr == '^') {
cflag = 1;
prestr++;
}
else
cflag = 0;
n = 2 * strlen(prestr)+1;
if (!adjbuf((char **) &buf, &bufsz, n, n, (char **) &bp, 0))
FATAL("out of space for reg expr %.10s...", lastre);
for (; ; ) {
if ((c = *prestr++) == '\\') {
*bp++ = '\\';
if ((c = *prestr++) == '\0')
FATAL("nonterminated character class %.20s...", lastre);
*bp++ = c;
} else if (c == '\0') {
FATAL("nonterminated character class %.20s", lastre);
} else if (bp == buf) {
*bp++ = c;
} else if (c == ']') {
*bp++ = 0;
rlxstr = (uschar *) tostring((char *) buf);
if (cflag == 0)
return CCL;
else
return NCCL;
} else
*bp++ = c;
}
}
}
int cgoto(fa *f, int s, int c)
{
int i, j, k;
int *p, *q;
if (c < 0 || c > 255)
FATAL("can't happen: neg char %d in cgoto", c);
while (f->accept >= maxsetvec) {
maxsetvec *= 4;
setvec = (int *) realloc(setvec, maxsetvec * sizeof(int));
tmpset = (int *) realloc(tmpset, maxsetvec * sizeof(int));
if (setvec == 0 || tmpset == 0)
overflo("out of space in cgoto()");
}
for (i = 0; i <= f->accept; i++)
setvec[i] = 0;
setcnt = 0;
p = f->posns[s];
for (i = 1; i <= *p; i++) {
if ((k = f->re[p[i]].ltype) != FINAL) {
if ((k == CHAR && c == ptoi(f->re[p[i]].lval.np))
|| (k == DOT && c != 0 && c != HAT)
|| (k == ALL && c != 0)
|| (k == CCL && member(c, (char *) f->re[p[i]].lval.up))
|| (k == NCCL && !member(c, (char *) f->re[p[i]].lval.up) && c != 0 && c != HAT)) {
q = f->re[p[i]].lfollow;
for (j = 1; j <= *q; j++) {
if (q[j] >= maxsetvec) {
maxsetvec *= 4;
setvec = (int *) realloc(setvec, maxsetvec * sizeof(int));
tmpset = (int *) realloc(setvec, maxsetvec * sizeof(int));
if (setvec == 0 || tmpset == 0)
overflo("cgoto overflow");
}
if (setvec[q[j]] == 0) {
setcnt++;
setvec[q[j]] = 1;
}
}
}
}
}
tmpset[0] = setcnt;
j = 1;
for (i = f->accept; i >= 0; i--)
if (setvec[i]) {
tmpset[j++] = i;
}
for (i = 1; i <= f->curstat; i++) {
p = f->posns[i];
if ((k = tmpset[0]) != p[0])
goto different;
for (j = 1; j <= k; j++)
if (tmpset[j] != p[j])
goto different;
f->gototab[s][c] = i;
return i;
different:;
}
if (f->curstat >= NSTATES-1) {
f->curstat = 2;
f->reset = 1;
for (i = 2; i < NSTATES; i++)
xfree(f->posns[i]);
} else
++(f->curstat);
for (i = 0; i < NCHARS; i++)
f->gototab[f->curstat][i] = 0;
xfree(f->posns[f->curstat]);
if ((p = (int *) calloc(1, (setcnt+1)*sizeof(int))) == NULL)
overflo("out of space in cgoto");
f->posns[f->curstat] = p;
f->gototab[s][c] = f->curstat;
for (i = 0; i <= setcnt; i++)
p[i] = tmpset[i];
if (setvec[f->accept])
f->out[f->curstat] = 1;
else
f->out[f->curstat] = 0;
return f->curstat;
}
void freefa(fa *f)
{
int i;
if (f == NULL)
return;
for (i = 0; i <= f->curstat; i++)
xfree(f->posns[i]);
for (i = 0; i <= f->accept; i++) {
xfree(f->re[i].lfollow);
if (f->re[i].ltype == CCL || f->re[i].ltype == NCCL)
xfree((f->re[i].lval.np));
}
xfree(f->restr);
xfree(f);
}