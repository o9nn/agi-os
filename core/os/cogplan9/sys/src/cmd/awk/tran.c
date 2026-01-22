#define	DEBUG
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "awk.h"
#include "y.tab.h"
#define	FULLTAB	2
#define	GROWTAB 4
Array	*symtab;
char	**FS;
char	**RS;
char	**OFS;
char	**ORS;
char	**OFMT;
char	**CONVFMT;
Awkfloat *NF;
Awkfloat *NR;
Awkfloat *FNR;
char	**FILENAME;
Awkfloat *ARGC;
char	**SUBSEP;
Awkfloat *RSTART;
Awkfloat *RLENGTH;
Cell	*nrloc;
Cell	*nfloc;
Cell	*fnrloc;
Array	*ARGVtab;
Array	*ENVtab;
Cell	*rstartloc;
Cell	*rlengthloc;
Cell	*symtabloc;
Cell	*nullloc;
Node	*nullnode;
Cell	*literal0;
extern Cell **fldtab;
void syminit(void)
{
literal0 = setsymtab("0", "0", 0.0, NUM|STR|CON|DONTFREE, symtab);
nullloc = setsymtab("$zero&null", "", 0.0, NUM|STR|CON|DONTFREE, symtab);
nullnode = celltonode(nullloc, CCON);
FS = &setsymtab("FS", " ", 0.0, STR|DONTFREE, symtab)->sval;
RS = &setsymtab("RS", "\n", 0.0, STR|DONTFREE, symtab)->sval;
OFS = &setsymtab("OFS", " ", 0.0, STR|DONTFREE, symtab)->sval;
ORS = &setsymtab("ORS", "\n", 0.0, STR|DONTFREE, symtab)->sval;
OFMT = &setsymtab("OFMT", "%.6g", 0.0, STR|DONTFREE, symtab)->sval;
CONVFMT = &setsymtab("CONVFMT", "%.6g", 0.0, STR|DONTFREE, symtab)->sval;
FILENAME = &setsymtab("FILENAME", "", 0.0, STR|DONTFREE, symtab)->sval;
nfloc = setsymtab("NF", "", 0.0, NUM, symtab);
NF = &nfloc->fval;
nrloc = setsymtab("NR", "", 0.0, NUM, symtab);
NR = &nrloc->fval;
fnrloc = setsymtab("FNR", "", 0.0, NUM, symtab);
FNR = &fnrloc->fval;
SUBSEP = &setsymtab("SUBSEP", "\034", 0.0, STR|DONTFREE, symtab)->sval;
rstartloc = setsymtab("RSTART", "", 0.0, NUM, symtab);
RSTART = &rstartloc->fval;
rlengthloc = setsymtab("RLENGTH", "", 0.0, NUM, symtab);
RLENGTH = &rlengthloc->fval;
symtabloc = setsymtab("SYMTAB", "", 0.0, ARR, symtab);
symtabloc->sval = (char *) symtab;
}
void arginit(int ac, char **av)
{
Cell *cp;
int i;
char temp[50];
ARGC = &setsymtab("ARGC", "", (Awkfloat) ac, NUM, symtab)->fval;
cp = setsymtab("ARGV", "", 0.0, ARR, symtab);
ARGVtab = makesymtab(NSYMTAB);
cp->sval = (char *) ARGVtab;
for (i = 0; i < ac; i++) {
sprintf(temp, "%d", i);
if (is_number(*av))
setsymtab(temp, *av, atof(*av), STR|NUM, ARGVtab);
else
setsymtab(temp, *av, 0.0, STR, ARGVtab);
av++;
}
}
void envinit(char **envp)
{
Cell *cp;
char *p;
cp = setsymtab("ENVIRON", "", 0.0, ARR, symtab);
ENVtab = makesymtab(NSYMTAB);
cp->sval = (char *) ENVtab;
for ( ; *envp; envp++) {
if ((p = strchr(*envp, '=')) == NULL)
continue;
*p++ = 0;
if (is_number(p))
setsymtab(*envp, p, atof(p), STR|NUM, ENVtab);
else
setsymtab(*envp, p, 0.0, STR, ENVtab);
p[-1] = '=';
}
}
Array *makesymtab(int n)
{
Array *ap;
Cell **tp;
ap = (Array *) malloc(sizeof(Array));
tp = (Cell **) calloc(n, sizeof(Cell *));
if (ap == NULL || tp == NULL)
FATAL("out of space in makesymtab");
ap->nelem = 0;
ap->size = n;
ap->tab = tp;
return(ap);
}
void freesymtab(Cell *ap)
{
Cell *cp, *temp;
Array *tp;
int i;
if (!isarr(ap))
return;
tp = (Array *) ap->sval;
if (tp == NULL)
return;
for (i = 0; i < tp->size; i++) {
for (cp = tp->tab[i]; cp != NULL; cp = temp) {
xfree(cp->nval);
if (freeable(cp))
xfree(cp->sval);
temp = cp->cnext;
free(cp);
}
tp->tab[i] = 0;
}
free(tp->tab);
free(tp);
}
void freeelem(Cell *ap, char *s)
{
Array *tp;
Cell *p, *prev = NULL;
int h;
tp = (Array *) ap->sval;
h = hash(s, tp->size);
for (p = tp->tab[h]; p != NULL; prev = p, p = p->cnext)
if (strcmp(s, p->nval) == 0) {
if (prev == NULL)
tp->tab[h] = p->cnext;
else
prev->cnext = p->cnext;
if (freeable(p))
xfree(p->sval);
free(p->nval);
free(p);
tp->nelem--;
return;
}
}
Cell *setsymtab(char *n, char *s, Awkfloat f, unsigned t, Array *tp)
{
int h;
Cell *p;
if (n != NULL && (p = lookup(n, tp)) != NULL) {
dprintf( ("setsymtab found %p: n=%s s=\"%s\" f=%g t=%o\n",
p, p->nval, p->sval, p->fval, p->tval) );
return(p);
}
p = (Cell *) malloc(sizeof(Cell));
if (p == NULL)
FATAL("out of space for symbol table at %s", n);
p->nval = tostring(n);
p->sval = s ? tostring(s) : tostring("");
p->fval = f;
p->tval = t;
p->csub = CUNK;
p->ctype = OCELL;
tp->nelem++;
if (tp->nelem > FULLTAB * tp->size)
rehash(tp);
h = hash(n, tp->size);
p->cnext = tp->tab[h];
tp->tab[h] = p;
dprintf( ("setsymtab set %p: n=%s s=\"%s\" f=%g t=%o\n",
p, p->nval, p->sval, p->fval, p->tval) );
return(p);
}
int hash(char *s, int n)
{
unsigned hashval;
for (hashval = 0; *s != '\0'; s++)
hashval = (*s + 31 * hashval);
return hashval % n;
}
void rehash(Array *tp)
{
int i, nh, nsz;
Cell *cp, *op, **np;
nsz = GROWTAB * tp->size;
np = (Cell **) calloc(nsz, sizeof(Cell *));
if (np == NULL)
return;
for (i = 0; i < tp->size; i++) {
for (cp = tp->tab[i]; cp; cp = op) {
op = cp->cnext;
nh = hash(cp->nval, nsz);
cp->cnext = np[nh];
np[nh] = cp;
}
}
free(tp->tab);
tp->tab = np;
tp->size = nsz;
}
Cell *lookup(char *s, Array *tp)
{
Cell *p;
int h;
h = hash(s, tp->size);
for (p = tp->tab[h]; p != NULL; p = p->cnext)
if (strcmp(s, p->nval) == 0)
return(p);
return(NULL);
}
Awkfloat setfval(Cell *vp, Awkfloat f)
{
int fldno;
if ((vp->tval & (NUM | STR)) == 0)
funnyvar(vp, "assign to");
if (isfld(vp)) {
donerec = 0;
fldno = atoi(vp->nval);
if (fldno > *NF)
newfld(fldno);
dprintf( ("setting field %d to %g\n", fldno, f) );
} else if (isrec(vp)) {
donefld = 0;
donerec = 1;
}
if (freeable(vp))
xfree(vp->sval);
vp->tval &= ~STR;
vp->tval |= NUM;
dprintf( ("setfval %p: %s = %g, t=%o\n", vp, vp->nval, f, vp->tval) );
return vp->fval = f;
}
void funnyvar(Cell *vp, char *rw)
{
if (isarr(vp))
FATAL("can't %s %s; it's an array name.", rw, vp->nval);
if (vp->tval & FCN)
FATAL("can't %s %s; it's a function.", rw, vp->nval);
WARNING("funny variable %p: n=%s s=\"%s\" f=%g t=%o",
vp, vp->nval, vp->sval, vp->fval, vp->tval);
}
char *setsval(Cell *vp, char *s)
{
char *t;
int fldno;
dprintf( ("starting setsval %p: %s = \"%s\", t=%o\n", vp, vp->nval, s, vp->tval) );
if ((vp->tval & (NUM | STR)) == 0)
funnyvar(vp, "assign to");
if (isfld(vp)) {
donerec = 0;
fldno = atoi(vp->nval);
if (fldno > *NF)
newfld(fldno);
dprintf( ("setting field %d to %s (%p)\n", fldno, s, s) );
} else if (isrec(vp)) {
donefld = 0;
donerec = 1;
}
t = tostring(s);
vp->tval &= ~NUM;
vp->tval |= STR;
if (freeable(vp))
xfree(vp->sval);
vp->tval &= ~DONTFREE;
dprintf( ("setsval %p: %s = \"%s (%p)\", t=%o\n", vp, vp->nval, t,t, vp->tval) );
return(vp->sval = t);
}
Awkfloat getfval(Cell *vp)
{
if ((vp->tval & (NUM | STR)) == 0)
funnyvar(vp, "read value of");
if (isfld(vp) && donefld == 0)
fldbld();
else if (isrec(vp) && donerec == 0)
recbld();
if (!isnum(vp)) {
vp->fval = atof(vp->sval);
if (is_number(vp->sval) && !(vp->tval&CON))
vp->tval |= NUM;
}
dprintf( ("getfval %p: %s = %g, t=%o\n", vp, vp->nval, vp->fval, vp->tval) );
return(vp->fval);
}
char *getsval(Cell *vp)
{
char s[100];
double dtemp;
if ((vp->tval & (NUM | STR)) == 0)
funnyvar(vp, "read value of");
if (isfld(vp) && donefld == 0)
fldbld();
else if (isrec(vp) && donerec == 0)
recbld();
if (isstr(vp) == 0) {
if (freeable(vp))
xfree(vp->sval);
if (modf(vp->fval, &dtemp) == 0)
sprintf(s, "%.30g", vp->fval);
else
sprintf(s, *CONVFMT, vp->fval);
vp->sval = tostring(s);
vp->tval &= ~DONTFREE;
vp->tval |= STR;
}
dprintf( ("getsval %p: %s = \"%s (%p)\", t=%o\n", vp, vp->nval, vp->sval, vp->sval, vp->tval) );
return(vp->sval);
}
char *tostring(char *s)
{
char *p;
p = (char *) malloc(strlen(s)+1);
if (p == NULL)
FATAL("out of space in tostring on %s", s);
strcpy(p, s);
return(p);
}
char *qstring(char *s, int delim)
{
char *os = s;
int c, n;
char *buf, *bp;
if ((buf = (char *) malloc(strlen(s)+3)) == NULL)
FATAL( "out of space in qstring(%s)", s);
for (bp = buf; (c = *s) != delim; s++) {
if (c == '\n')
SYNTAX( "newline in string %.20s...", os );
else if (c != '\\')
*bp++ = c;
else {
c = *++s;
if (c == 0) {
*bp++ = '\\';
break;
}
switch (c) {
case '\\':	*bp++ = '\\'; break;
case 'n':	*bp++ = '\n'; break;
case 't':	*bp++ = '\t'; break;
case 'b':	*bp++ = '\b'; break;
case 'f':	*bp++ = '\f'; break;
case 'r':	*bp++ = '\r'; break;
default:
if (!isdigit(c)) {
*bp++ = c;
break;
}
n = c - '0';
if (isdigit(s[1])) {
n = 8 * n + *++s - '0';
if (isdigit(s[1]))
n = 8 * n + *++s - '0';
}
*bp++ = n;
break;
}
}
}
*bp++ = 0;
return buf;
}