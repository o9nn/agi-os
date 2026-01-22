#define DEBUG
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>
#include "awk.h"
#include "ytab.h"
FILE	*infile	= NULL;
char	*file	= "";
char	*record;
int	recsize	= RECSIZE;
char	*fields;
int	fieldssize = RECSIZE;
Cell	**fldtab;
char	inputFS[100] = " ";
#define	MAXFLD	200
int	nfields	= MAXFLD;
int	donefld;
int	donerec;
int	lastfld	= 0;
int	argno	= 1;
extern	Awkfloat *ARGC;
static Cell dollar0 = { OCELL, CFLD, NULL, "", 0.0, REC|STR|DONTFREE };
static Cell dollar1 = { OCELL, CFLD, NULL, "", 0.0, FLD|STR|DONTFREE };
void recinit(unsigned int n)
{
record = (char *) malloc(n);
fields = (char *) malloc(n);
fldtab = (Cell **) malloc((nfields+1) * sizeof(Cell *));
if (record == NULL || fields == NULL || fldtab == NULL)
FATAL("out of space for $0 and fields");
fldtab[0] = (Cell *) malloc(sizeof (Cell));
*fldtab[0] = dollar0;
fldtab[0]->sval = record;
fldtab[0]->nval = tostring("0");
makefields(1, nfields);
}
void makefields(int n1, int n2)
{
char temp[50];
int i;
for (i = n1; i <= n2; i++) {
fldtab[i] = (Cell *) malloc(sizeof (struct Cell));
if (fldtab[i] == NULL)
FATAL("out of space in makefields %d", i);
*fldtab[i] = dollar1;
sprintf(temp, "%d", i);
fldtab[i]->nval = tostring(temp);
}
}
void initgetrec(void)
{
int i;
char *p;
for (i = 1; i < *ARGC; i++) {
if (!isclvar(p = getargv(i))) {
setsval(lookup("FILENAME", symtab), getargv(i));
return;
}
setclvar(p);
argno++;
}
infile = stdin;
}
int getrec(char **pbuf, int *pbufsize, int isrecord)
{
int c;
static int firsttime = 1;
char *buf = *pbuf;
int bufsize = *pbufsize;
if (firsttime) {
firsttime = 0;
initgetrec();
}
dprintf( ("RS=<%s>, FS=<%s>, ARGC=%g, FILENAME=%s\n",
*RS, *FS, *ARGC, *FILENAME) );
if (isrecord) {
donefld = 0;
donerec = 1;
}
buf[0] = 0;
while (argno < *ARGC || infile == stdin) {
dprintf( ("argno=%d, file=|%s|\n", argno, file) );
if (infile == NULL) {
file = getargv(argno);
if (*file == '\0') {
argno++;
continue;
}
if (isclvar(file)) {
setclvar(file);
argno++;
continue;
}
*FILENAME = file;
dprintf( ("opening file %s\n", file) );
if (*file == '-' && *(file+1) == '\0')
infile = stdin;
else if ((infile = fopen(file, "r")) == NULL)
FATAL("can't open file %s", file);
setfval(fnrloc, 0.0);
}
c = readrec(&buf, &bufsize, infile);
if (c != 0 || buf[0] != '\0') {
if (isrecord) {
if (freeable(fldtab[0]))
xfree(fldtab[0]->sval);
fldtab[0]->sval = buf;
fldtab[0]->tval = REC | STR | DONTFREE;
if (is_number(fldtab[0]->sval)) {
fldtab[0]->fval = atof(fldtab[0]->sval);
fldtab[0]->tval |= NUM;
}
}
setfval(nrloc, nrloc->fval+1);
setfval(fnrloc, fnrloc->fval+1);
*pbuf = buf;
*pbufsize = bufsize;
return 1;
}
if (infile != stdin)
fclose(infile);
infile = NULL;
argno++;
}
*pbuf = buf;
*pbufsize = bufsize;
return 0;
}
void nextfile(void)
{
if (infile != stdin)
fclose(infile);
infile = NULL;
argno++;
}
int readrec(char **pbuf, int *pbufsize, FILE *inf)
{
int sep, c;
char *rr, *buf = *pbuf;
int bufsize = *pbufsize;
if (strlen(*FS) >= sizeof(inputFS))
FATAL("field separator %.10s... is too long", *FS);
strcpy(inputFS, *FS);
if ((sep = **RS) == 0) {
sep = '\n';
while ((c=getc(inf)) == '\n' && c != EOF)
;
if (c != EOF)
ungetc(c, inf);
}
for (rr = buf; ; ) {
for (; (c=getc(inf)) != sep && c != EOF; ) {
if (rr-buf+1 > bufsize)
if (!adjbuf(&buf, &bufsize, 1+rr-buf, recsize, &rr, "readrec 1"))
FATAL("input record `%.30s...' too long", buf);
*rr++ = c;
}
if (**RS == sep || c == EOF)
break;
if ((c = getc(inf)) == '\n' || c == EOF)
break;
if (!adjbuf(&buf, &bufsize, 2+rr-buf, recsize, &rr, "readrec 2"))
FATAL("input record `%.30s...' too long", buf);
*rr++ = '\n';
*rr++ = c;
}
if (!adjbuf(&buf, &bufsize, 1+rr-buf, recsize, &rr, "readrec 3"))
FATAL("input record `%.30s...' too long", buf);
*rr = 0;
dprintf( ("readrec saw <%s>, returns %d\n", buf, c == EOF && rr == buf ? 0 : 1) );
*pbuf = buf;
*pbufsize = bufsize;
return c == EOF && rr == buf ? 0 : 1;
}
char *getargv(int n)
{
Cell *x;
char *s, temp[50];
extern Array *ARGVtab;
sprintf(temp, "%d", n);
x = setsymtab(temp, "", 0.0, STR, ARGVtab);
s = getsval(x);
dprintf( ("getargv(%d) returns |%s|\n", n, s) );
return s;
}
void setclvar(char *s)
{
char *p;
Cell *q;
for (p=s; *p != '='; p++)
;
*p++ = 0;
p = qstring(p, '\0');
q = setsymtab(s, p, 0.0, STR, symtab);
setsval(q, p);
if (is_number(q->sval)) {
q->fval = atof(q->sval);
q->tval |= NUM;
}
dprintf( ("command line set %s to |%s|\n", s, p) );
}
void fldbld(void)
{
char *r, *fr, sep;
Cell *p;
int i, j, n;
if (donefld)
return;
if (!isstr(fldtab[0]))
getsval(fldtab[0]);
r = fldtab[0]->sval;
n = strlen(r);
if (n > fieldssize) {
xfree(fields);
if ((fields = (char *) malloc(n+1)) == NULL)
FATAL("out of space for fields in fldbld %d", n);
fieldssize = n;
}
fr = fields;
i = 0;
if (strlen(inputFS) > 1) {
i = refldbld(r, inputFS);
} else if ((sep = *inputFS) == ' ') {
for (i = 0; ; ) {
while (*r == ' ' || *r == '\t' || *r == '\n')
r++;
if (*r == 0)
break;
i++;
if (i > nfields)
growfldtab(i);
if (freeable(fldtab[i]))
xfree(fldtab[i]->sval);
fldtab[i]->sval = fr;
fldtab[i]->tval = FLD | STR | DONTFREE;
do
*fr++ = *r++;
while (*r != ' ' && *r != '\t' && *r != '\n' && *r != '\0');
*fr++ = 0;
}
*fr = 0;
} else if ((sep = *inputFS) == 0) {
for (i = 0; *r != 0; r++) {
char buf[2];
i++;
if (i > nfields)
growfldtab(i);
if (freeable(fldtab[i]))
xfree(fldtab[i]->sval);
buf[0] = *r;
buf[1] = 0;
fldtab[i]->sval = tostring(buf);
fldtab[i]->tval = FLD | STR;
}
*fr = 0;
} else if (*r != 0) {
for (;;) {
i++;
if (i > nfields)
growfldtab(i);
if (freeable(fldtab[i]))
xfree(fldtab[i]->sval);
fldtab[i]->sval = fr;
fldtab[i]->tval = FLD | STR | DONTFREE;
while (*r != sep && *r != '\n' && *r != '\0')
*fr++ = *r++;
*fr++ = 0;
if (*r++ == 0)
break;
}
*fr = 0;
}
if (i > nfields)
FATAL("record `%.30s...' has too many fields; can't happen", r);
cleanfld(i+1, lastfld);
lastfld = i;
donefld = 1;
for (j = 1; j <= lastfld; j++) {
p = fldtab[j];
if(is_number(p->sval)) {
p->fval = atof(p->sval);
p->tval |= NUM;
}
}
setfval(nfloc, (Awkfloat) lastfld);
if (dbg) {
for (j = 0; j <= lastfld; j++) {
p = fldtab[j];
printf("field %d (%s): |%s|\n", j, p->nval, p->sval);
}
}
}
void cleanfld(int n1, int n2)
{
Cell *p;
int i;
for (i = n1; i <= n2; i++) {
p = fldtab[i];
if (freeable(p))
xfree(p->sval);
p->sval = "";
p->tval = FLD | STR | DONTFREE;
}
}
void newfld(int n)
{
if (n > nfields)
growfldtab(n);
cleanfld(lastfld+1, n);
lastfld = n;
setfval(nfloc, (Awkfloat) n);
}
Cell *fieldadr(int n)
{
if (n < 0)
FATAL("trying to access field %d", n);
if (n > nfields)
growfldtab(n);
return(fldtab[n]);
}
void growfldtab(int n)
{
int nf = 2 * nfields;
if (n > nf)
nf = n;
fldtab = (Cell **) realloc(fldtab, (nf+1) * (sizeof (struct Cell *)));
if (fldtab == NULL)
FATAL("out of space creating %d fields", nf);
makefields(nfields+1, nf);
nfields = nf;
}
int refldbld(char *rec, char *fs)
{
char *fr;
int i, tempstat, n;
fa *pfa;
n = strlen(rec);
if (n > fieldssize) {
xfree(fields);
if ((fields = (char *) malloc(n+1)) == NULL)
FATAL("out of space for fields in refldbld %d", n);
fieldssize = n;
}
fr = fields;
*fr = '\0';
if (*rec == '\0')
return 0;
pfa = makedfa(fs, 1);
dprintf( ("into refldbld, rec = <%s>, pat = <%s>\n", rec, fs) );
tempstat = pfa->initstat;
for (i = 1; ; i++) {
if (i > nfields)
growfldtab(i);
if (freeable(fldtab[i]))
xfree(fldtab[i]->sval);
fldtab[i]->tval = FLD | STR | DONTFREE;
fldtab[i]->sval = fr;
dprintf( ("refldbld: i=%d\n", i) );
if (nematch(pfa, rec)) {
pfa->initstat = 2;
dprintf( ("match %s (%d chars)\n", patbeg, patlen) );
strncpy(fr, rec, patbeg-rec);
fr += patbeg - rec + 1;
*(fr-1) = '\0';
rec = patbeg + patlen;
} else {
dprintf( ("no match %s\n", rec) );
strcpy(fr, rec);
pfa->initstat = tempstat;
break;
}
}
return i;
}
void recbld(void)
{
int i;
char *r, *p;
if (donerec == 1)
return;
r = record;
for (i = 1; i <= *NF; i++) {
p = getsval(fldtab[i]);
if (!adjbuf(&record, &recsize, 1+strlen(p)+r-record, recsize, &r, "recbld 1"))
FATAL("created $0 `%.30s...' too long", record);
while ((*r = *p++) != 0)
r++;
if (i < *NF) {
if (!adjbuf(&record, &recsize, 2+strlen(*OFS)+r-record, recsize, &r, "recbld 2"))
FATAL("created $0 `%.30s...' too long", record);
for (p = *OFS; (*r = *p++) != 0; )
r++;
}
}
if (!adjbuf(&record, &recsize, 2+r-record, recsize, &r, "recbld 3"))
FATAL("built giant record `%.30s...'", record);
*r = '\0';
dprintf( ("in recbld inputFS=%s, fldtab[0]=%p\n", inputFS, fldtab[0]) );
if (freeable(fldtab[0]))
xfree(fldtab[0]->sval);
fldtab[0]->tval = REC | STR | DONTFREE;
fldtab[0]->sval = record;
dprintf( ("in recbld inputFS=%s, fldtab[0]=%p\n", inputFS, fldtab[0]) );
dprintf( ("recbld = |%s|\n", record) );
donerec = 1;
}
int	errorflag	= 0;
void yyerror(char *s)
{
SYNTAX(s);
}
void SYNTAX(char *fmt, ...)
{
extern char *cmdname, *curfname;
static int been_here = 0;
va_list varg;
if (been_here++ > 2)
return;
fprintf(stderr, "%s: ", cmdname);
va_start(varg, fmt);
vfprintf(stderr, fmt, varg);
va_end(varg);
fprintf(stderr, " at source line %d", lineno);
if (curfname != NULL)
fprintf(stderr, " in function %s", curfname);
if (compile_time == 1 && cursource() != NULL)
fprintf(stderr, " source file %s", cursource());
fprintf(stderr, "\n");
errorflag = 2;
eprint();
}
void fpecatch(int n)
{
FATAL("floating point exception %d", n);
}
extern int bracecnt, brackcnt, parencnt;
void bracecheck(void)
{
int c;
static int beenhere = 0;
if (beenhere++)
return;
while ((c = input()) != EOF && c != '\0')
bclass(c);
bcheck2(bracecnt, '{', '}');
bcheck2(brackcnt, '[', ']');
bcheck2(parencnt, '(', ')');
}
void bcheck2(int n, int c1, int c2)
{
if (n == 1)
fprintf(stderr, "\tmissing %c\n", c2);
else if (n > 1)
fprintf(stderr, "\t%d missing %c's\n", n, c2);
else if (n == -1)
fprintf(stderr, "\textra %c\n", c2);
else if (n < -1)
fprintf(stderr, "\t%d extra %c's\n", -n, c2);
}
void FATAL(char *fmt, ...)
{
extern char *cmdname;
va_list varg;
fflush(stdout);
fprintf(stderr, "%s: ", cmdname);
va_start(varg, fmt);
vfprintf(stderr, fmt, varg);
va_end(varg);
error();
if (dbg > 1)
abort();
exit(2);
}
void WARNING(char *fmt, ...)
{
extern char *cmdname;
va_list varg;
fflush(stdout);
fprintf(stderr, "%s: ", cmdname);
va_start(varg, fmt);
vfprintf(stderr, fmt, varg);
va_end(varg);
error();
}
void error()
{
extern Node *curnode;
fprintf(stderr, "\n");
if (compile_time != 2 && NR && *NR > 0) {
fprintf(stderr, " input record number %d", (int) (*FNR));
if (strcmp(*FILENAME, "-") != 0)
fprintf(stderr, ", file %s", *FILENAME);
fprintf(stderr, "\n");
}
if (compile_time != 2 && curnode)
fprintf(stderr, " source line number %d", curnode->lineno);
else if (compile_time != 2 && lineno)
fprintf(stderr, " source line number %d", lineno);
if (compile_time == 1 && cursource() != NULL)
fprintf(stderr, " source file %s", cursource());
fprintf(stderr, "\n");
eprint();
}
void eprint(void)
{
char *p, *q;
int c;
static int been_here = 0;
extern char ebuf[], *ep;
if (compile_time == 2 || compile_time == 0 || been_here++ > 0)
return;
p = ep - 1;
if (p > ebuf && *p == '\n')
p--;
for ( ; p > ebuf && *p != '\n' && *p != '\0'; p--)
;
while (*p == '\n')
p++;
fprintf(stderr, " context is\n\t");
for (q=ep-1; q>=p && *q!=' ' && *q!='\t' && *q!='\n'; q--)
;
for ( ; p < q; p++)
if (*p)
putc(*p, stderr);
fprintf(stderr, " >>> ");
for ( ; p < ep; p++)
if (*p)
putc(*p, stderr);
fprintf(stderr, " <<< ");
if (*ep)
while ((c = input()) != '\n' && c != '\0' && c != EOF) {
putc(c, stderr);
bclass(c);
}
putc('\n', stderr);
ep = ebuf;
}
void bclass(int c)
{
switch (c) {
case '{': bracecnt++; break;
case '}': bracecnt--; break;
case '[': brackcnt++; break;
case ']': brackcnt--; break;
case '(': parencnt++; break;
case ')': parencnt--; break;
}
}
double errcheck(double x, char *s)
{
if (errno == EDOM) {
errno = 0;
WARNING("%s argument out of domain", s);
x = 1;
} else if (errno == ERANGE) {
errno = 0;
WARNING("%s result out of range", s);
x = 1;
}
return x;
}
int isclvar(char *s)
{
char *os = s;
if (!isalpha((uschar) *s) && *s != '_')
return 0;
for ( ; *s; s++)
if (!(isalnum((uschar) *s) || *s == '_'))
break;
return *s == '=' && s > os && *(s+1) != '=';
}
#include <math.h>
int is_number(char *s)
{
double r;
char *ep;
errno = 0;
r = strtod(s, &ep);
if (ep == s || r == HUGE_VAL || errno == ERANGE)
return 0;
while (*ep == ' ' || *ep == '\t' || *ep == '\n')
ep++;
if (*ep == '\0')
return 1;
else
return 0;
}