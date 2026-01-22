#include "pax.h"
#ifndef lint
static char    *Ident = "$Id: regexp.c,v 1.2 89/02/12 10:05:39 mark Exp $";
#endif
#define	END	0
#define	BOL	1
#define	EOL	2
#define	ANY	3
#define	ANYOF	4
#define	ANYBUT	5
#define	BRANCH	6
#define	BACK	7
#define	EXACTLY	8
#define	NOTHING	9
#define	STAR	10
#define	OPEN	20
#define	CLOSE	30
#define	OP(p)	(*(p))
#define	NEXT(p)	(((*((p)+1)&0377)<<8) + (*((p)+2)&0377))
#define	OPERAND(p)	((p) + 3)
#define	FAIL(m)	{ regerror(m); return(NULL); }
#define	ISMULT(c)	((c) == '*')
#define	META	"^$.[()|*\\"
#ifndef CHARBITS
#define	UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define	UCHARAT(p)	((int)*(p)&CHARBITS)
#endif
#define	HASWIDTH	01
#define	SIMPLE		02
#define	SPSTART		04
#define	WORST		0
static char    *regparse;
static int      regnpar;
static char     regdummy;
static char    *regcode;
static long     regsize;
#ifndef STATIC
#define	STATIC	static
#endif
STATIC char    *reg();
STATIC char    *regbranch();
STATIC char    *regpiece();
STATIC char    *regatom();
STATIC char    *regnode();
STATIC char    *regnext();
STATIC void     regc();
STATIC void     reginsert();
STATIC void     regtail();
STATIC void     regoptail();
#ifdef STRCSPN
STATIC int      strcspn();
#endif
regexp *regcomp(exp)
char           *exp;
{
register regexp *r;
register char  *scan;
register char  *longest;
register int    len;
int             flags;
extern char    *malloc();
if (exp == (char *)NULL)
FAIL("NULL argument");
regparse = exp;
regnpar = 1;
regsize = 0L;
regcode = &regdummy;
regc(MAGIC);
if (reg(0, &flags) == (char *)NULL)
return ((regexp *)NULL);
if (regsize >= 32767L)
FAIL("regexp too big");
r = (regexp *) malloc(sizeof(regexp) + (unsigned) regsize);
if (r == (regexp *) NULL)
FAIL("out of space");
regparse = exp;
regnpar = 1;
regcode = r->program;
regc(MAGIC);
if (reg(0, &flags) == NULL)
return ((regexp *) NULL);
r->regstart = '\0';
r->reganch = 0;
r->regmust = NULL;
r->regmlen = 0;
scan = r->program + 1;
if (OP(regnext(scan)) == END) {
scan = OPERAND(scan);
if (OP(scan) == EXACTLY)
r->regstart = *OPERAND(scan);
else if (OP(scan) == BOL)
r->reganch++;
if (flags & SPSTART) {
longest = NULL;
len = 0;
for (; scan != NULL; scan = regnext(scan))
if (OP(scan) == EXACTLY && strlen(OPERAND(scan)) >= len) {
longest = OPERAND(scan);
len = strlen(OPERAND(scan));
}
r->regmust = longest;
r->regmlen = len;
}
}
return (r);
}
static char *reg(paren, flagp)
int             paren;
int            *flagp;
{
register char  *ret;
register char  *br;
register char  *ender;
register int    parno;
int             flags;
*flagp = HASWIDTH;
if (paren) {
if (regnpar >= NSUBEXP)
FAIL("too many ()");
parno = regnpar;
regnpar++;
ret = regnode(OPEN + parno);
} else
ret = (char *)NULL;
br = regbranch(&flags);
if (br == (char *)NULL)
return ((char *)NULL);
if (ret != (char *)NULL)
regtail(ret, br);
else
ret = br;
if (!(flags & HASWIDTH))
*flagp &= ~HASWIDTH;
*flagp |= flags & SPSTART;
while (*regparse == '|') {
regparse++;
br = regbranch(&flags);
if (br == (char *)NULL)
return ((char *)NULL);
regtail(ret, br);
if (!(flags & HASWIDTH))
*flagp &= ~HASWIDTH;
*flagp |= flags & SPSTART;
}
ender = regnode((paren) ? CLOSE + parno : END);
regtail(ret, ender);
for (br = ret; br != (char *)NULL; br = regnext(br))
regoptail(br, ender);
if (paren && *regparse++ != ')') {
FAIL("unmatched ()");
} else if (!paren && *regparse != '\0') {
if (*regparse == ')') {
FAIL("unmatched ()");
} else
FAIL("junk on end");
}
return (ret);
}
static char  *regbranch(flagp)
int            *flagp;
{
register char  *ret;
register char  *chain;
register char  *latest;
int             flags;
*flagp = WORST;
ret = regnode(BRANCH);
chain = (char *)NULL;
while (*regparse != '\0' && *regparse != '|' && *regparse != ')') {
latest = regpiece(&flags);
if (latest == (char *)NULL)
return ((char *)NULL);
*flagp |= flags & HASWIDTH;
if (chain == (char *)NULL)
*flagp |= flags & SPSTART;
else
regtail(chain, latest);
chain = latest;
}
if (chain == (char *)NULL)
regnode(NOTHING);
return (ret);
}
static char *regpiece(flagp)
int            *flagp;
{
register char  *ret;
register char   op;
register char  *nxt;
int             flags;
ret = regatom(&flags);
if (ret == (char *)NULL)
return ((char *)NULL);
op = *regparse;
if (!ISMULT(op)) {
*flagp = flags;
return (ret);
}
if (!(flags & HASWIDTH))
FAIL("* operand could be empty");
*flagp = (WORST | SPSTART);
if (op == '*' && (flags & SIMPLE))
reginsert(STAR, ret);
else if (op == '*') {
reginsert(BRANCH, ret);
regoptail(ret, regnode(BACK));
regoptail(ret, ret);
regtail(ret, regnode(BRANCH));
regtail(ret, regnode(NOTHING));
}
regparse++;
if (ISMULT(*regparse))
FAIL("nested *");
return (ret);
}
static char *regatom(flagp)
int            *flagp;
{
register char  *ret;
int             flags;
*flagp = WORST;
switch (*regparse++) {
case '^':
ret = regnode(BOL);
break;
case '$':
ret = regnode(EOL);
break;
case '.':
ret = regnode(ANY);
*flagp |= HASWIDTH | SIMPLE;
break;
case '[':{
register int    class;
register int    classend;
if (*regparse == '^') {
ret = regnode(ANYBUT);
regparse++;
} else
ret = regnode(ANYOF);
if (*regparse == ']' || *regparse == '-')
regc(*regparse++);
while (*regparse != '\0' && *regparse != ']') {
if (*regparse == '-') {
regparse++;
if (*regparse == ']' || *regparse == '\0')
regc('-');
else {
class = UCHARAT(regparse - 2) + 1;
classend = UCHARAT(regparse);
if (class > classend + 1)
FAIL("invalid [] range");
for (; class <= classend; class++)
regc(class);
regparse++;
}
} else
regc(*regparse++);
}
regc('\0');
if (*regparse != ']')
FAIL("unmatched []");
regparse++;
*flagp |= HASWIDTH | SIMPLE;
}
break;
case '(':
ret = reg(1, &flags);
if (ret == (char *)NULL)
return ((char *)NULL);
*flagp |= flags & (HASWIDTH | SPSTART);
break;
case '\0':
case '|':
case ')':
FAIL("internal urp");
break;
case '*':
FAIL("* follows nothing");
break;
case '\\':
if (*regparse == '\0')
FAIL("trailing \\");
ret = regnode(EXACTLY);
regc(*regparse++);
regc('\0');
*flagp |= HASWIDTH | SIMPLE;
break;
default:{
register int    len;
register char   ender;
regparse--;
len = strcspn(regparse, META);
if (len <= 0)
FAIL("internal disaster");
ender = *(regparse + len);
if (len > 1 && ISMULT(ender))
len--;
*flagp |= HASWIDTH;
if (len == 1)
*flagp |= SIMPLE;
ret = regnode(EXACTLY);
while (len > 0) {
regc(*regparse++);
len--;
}
regc('\0');
}
break;
}
return (ret);
}
static char *regnode(op)
char            op;
{
register char  *ret;
register char  *ptr;
ret = regcode;
if (ret == &regdummy) {
regsize += 3;
return (ret);
}
ptr = ret;
*ptr++ = op;
*ptr++ = '\0';
*ptr++ = '\0';
regcode = ptr;
return (ret);
}
static void regc(b)
char            b;
{
if (regcode != &regdummy)
*regcode++ = b;
else
regsize++;
}
static void reginsert(op, opnd)
char            op;
char           *opnd;
{
register char  *src;
register char  *dst;
register char  *place;
if (regcode == &regdummy) {
regsize += 3;
return;
}
src = regcode;
regcode += 3;
dst = regcode;
while (src > opnd)
*--dst = *--src;
place = opnd;
*place++ = op;
*place++ = '\0';
*place++ = '\0';
}
static void regtail(p, val)
char           *p;
char           *val;
{
register char  *scan;
register char  *temp;
register int    offset;
if (p == &regdummy)
return;
scan = p;
for (;;) {
temp = regnext(scan);
if (temp == (char *)NULL)
break;
scan = temp;
}
if (OP(scan) == BACK)
offset = scan - val;
else
offset = val - scan;
*(scan + 1) = (offset >> 8) & 0377;
*(scan + 2) = offset & 0377;
}
static void regoptail(p, val)
char           *p;
char           *val;
{
if (p == (char *)NULL || p == &regdummy || OP(p) != BRANCH)
return;
regtail(OPERAND(p), val);
}
static char    *reginput;
static char    *regbol;
static char   **regstartp;
static char   **regendp;
STATIC int      regtry();
STATIC int      regmatch();
STATIC int      regrepeat();
#ifdef DEBUG
int             regnarrate = 0;
void            regdump();
STATIC char    *regprop();
#endif
int regexec(prog, string)
register regexp *prog;
register char  *string;
{
register char  *s;
if (prog == (regexp *)NULL || string == (char *)NULL) {
regerror("NULL parameter");
return (0);
}
if (UCHARAT(prog->program) != MAGIC) {
regerror("corrupted program");
return (0);
}
if (prog->regmust != (char *)NULL) {
s = string;
while ((s = strchr(s, prog->regmust[0])) != (char *)NULL) {
if (strncmp(s, prog->regmust, prog->regmlen) == 0)
break;
s++;
}
if (s == (char *)NULL)
return (0);
}
regbol = string;
if (prog->reganch)
return (regtry(prog, string));
s = string;
if (prog->regstart != '\0')
while ((s = strchr(s, prog->regstart)) != (char *)NULL) {
if (regtry(prog, s))
return (1);
s++;
}
else
do {
if (regtry(prog, s))
return (1);
} while (*s++ != '\0');
return (0);
}
#ifdef __STDC__
static int regtry(regexp *prog, char *string)
#else
static int regtry(prog, string)
regexp         *prog;
char           *string;
#endif
{
register int    i;
register char **sp;
register char **ep;
reginput = string;
regstartp = prog->startp;
regendp = prog->endp;
sp = prog->startp;
ep = prog->endp;
for (i = NSUBEXP; i > 0; i--) {
*sp++ = (char *)NULL;
*ep++ = (char *)NULL;
}
if (regmatch(prog->program + 1)) {
prog->startp[0] = string;
prog->endp[0] = reginput;
return (1);
} else
return (0);
}
#ifdef __STDC__
static int regmatch(char *prog)
#else
static int regmatch(prog)
char           *prog;
#endif
{
register char  *scan;
char           *nxt;
scan = prog;
#ifdef DEBUG
if (scan != (char *)NULL && regnarrate)
fprintf(stderr, "%s(\n", regprop(scan));
#endif
while (scan != (char *)NULL) {
#ifdef DEBUG
if (regnarrate)
fprintf(stderr, "%s...\n", regprop(scan));
#endif
nxt = regnext(scan);
switch (OP(scan)) {
case BOL:
if (reginput != regbol)
return (0);
break;
case EOL:
if (*reginput != '\0')
return (0);
break;
case ANY:
if (*reginput == '\0')
return (0);
reginput++;
break;
case EXACTLY:{
register int    len;
register char  *opnd;
opnd = OPERAND(scan);
if (*opnd != *reginput)
return (0);
len = strlen(opnd);
if (len > 1 && strncmp(opnd, reginput, len) != 0)
return (0);
reginput += len;
}
break;
case ANYOF:
if (*reginput == '\0' ||
strchr(OPERAND(scan), *reginput) == (char *)NULL)
return (0);
reginput++;
break;
case ANYBUT:
if (*reginput == '\0' ||
strchr(OPERAND(scan), *reginput) != (char *)NULL)
return (0);
reginput++;
break;
case NOTHING:
break;
case BACK:
break;
case OPEN + 1:
case OPEN + 2:
case OPEN + 3:
case OPEN + 4:
case OPEN + 5:
case OPEN + 6:
case OPEN + 7:
case OPEN + 8:
case OPEN + 9:{
register int    no;
register char  *save;
no = OP(scan) - OPEN;
save = reginput;
if (regmatch(nxt)) {
if (regstartp[no] == (char *)NULL)
regstartp[no] = save;
return (1);
} else
return (0);
}
break;
case CLOSE + 1:
case CLOSE + 2:
case CLOSE + 3:
case CLOSE + 4:
case CLOSE + 5:
case CLOSE + 6:
case CLOSE + 7:
case CLOSE + 8:
case CLOSE + 9:{
register int    no;
register char  *save;
no = OP(scan) - CLOSE;
save = reginput;
if (regmatch(nxt)) {
if (regendp[no] == (char *)NULL)
regendp[no] = save;
return (1);
} else
return (0);
}
break;
case BRANCH:{
register char  *save;
if (OP(nxt) != BRANCH)
nxt = OPERAND(scan);
else {
do {
save = reginput;
if (regmatch(OPERAND(scan)))
return (1);
reginput = save;
scan = regnext(scan);
} while (scan != (char *)NULL && OP(scan) == BRANCH);
return (0);
}
}
break;
case STAR:{
register char   nextch;
register int    no;
register char  *save;
register int    minimum;
nextch = '\0';
if (OP(nxt) == EXACTLY)
nextch = *OPERAND(nxt);
minimum = (OP(scan) == STAR) ? 0 : 1;
save = reginput;
no = regrepeat(OPERAND(scan));
while (no >= minimum) {
if (nextch == '\0' || *reginput == nextch)
if (regmatch(nxt))
return (1);
no--;
reginput = save + no;
}
return (0);
}
break;
case END:
return (1);
break;
default:
regerror("memory corruption");
return (0);
break;
}
scan = nxt;
}
regerror("corrupted pointers");
return (0);
}
#ifdef __STDC__
static int regrepeat(char *p)
#else
static int regrepeat(p)
char           *p;
#endif
{
register int    count = 0;
register char  *scan;
register char  *opnd;
scan = reginput;
opnd = OPERAND(p);
switch (OP(p)) {
case ANY:
count = strlen(scan);
scan += count;
break;
case EXACTLY:
while (*opnd == *scan) {
count++;
scan++;
}
break;
case ANYOF:
while (*scan != '\0' && strchr(opnd, *scan) != (char *)NULL) {
count++;
scan++;
}
break;
case ANYBUT:
while (*scan != '\0' && strchr(opnd, *scan) == (char *)NULL) {
count++;
scan++;
}
break;
default:
regerror("internal foulup");
count = 0;
break;
}
reginput = scan;
return (count);
}
#ifdef __STDC__
static char *regnext(register char *p)
#else
static char *regnext(p)
register char  *p;
#endif
{
register int    offset;
if (p == &regdummy)
return ((char *)NULL);
offset = NEXT(p);
if (offset == 0)
return ((char *)NULL);
if (OP(p) == BACK)
return (p - offset);
else
return (p + offset);
}
#ifdef DEBUG
STATIC char    *regprop();
#ifdef __STDC__
void regdump(regexp *r)
#else
void regdump(r)
regexp         *r;
#endif
{
register char  *s;
register char   op = EXACTLY;
register char  *nxt;
extern char    *strchr();
s = r->program + 1;
while (op != END) {
op = OP(s);
printf("%2d%s", s - r->program, regprop(s));
nxt = regnext(s);
if (nxt == (char *)NULL)
printf("(0)");
else
printf("(%d)", (s - r->program) + (nxt - s));
s += 3;
if (op == ANYOF || op == ANYBUT || op == EXACTLY) {
while (*s != '\0') {
putchar(*s);
s++;
}
s++;
}
putchar('\n');
}
if (r->regstart != '\0')
printf("start `%c' ", r->regstart);
if (r->reganch)
printf("anchored ");
if (r->regmust != (char *)NULL)
printf("must have \"%s\"", r->regmust);
printf("\n");
}
#ifdef __STDC__
static char *regprop(char *op)
#else
static char *regprop(op)
char           *op;
#endif
{
register char  *p;
static char     buf[50];
strcpy(buf, ":");
switch (OP(op)) {
case BOL:
p = "BOL";
break;
case EOL:
p = "EOL";
break;
case ANY:
p = "ANY";
break;
case ANYOF:
p = "ANYOF";
break;
case ANYBUT:
p = "ANYBUT";
break;
case BRANCH:
p = "BRANCH";
break;
case EXACTLY:
p = "EXACTLY";
break;
case NOTHING:
p = "NOTHING";
break;
case BACK:
p = "BACK";
break;
case END:
p = "END";
break;
case OPEN + 1:
case OPEN + 2:
case OPEN + 3:
case OPEN + 4:
case OPEN + 5:
case OPEN + 6:
case OPEN + 7:
case OPEN + 8:
case OPEN + 9:
sprintf(buf + strlen(buf), "OPEN%d", OP(op) - OPEN);
p = (char *)NULL;
break;
case CLOSE + 1:
case CLOSE + 2:
case CLOSE + 3:
case CLOSE + 4:
case CLOSE + 5:
case CLOSE + 6:
case CLOSE + 7:
case CLOSE + 8:
case CLOSE + 9:
sprintf(buf + strlen(buf), "CLOSE%d", OP(op) - CLOSE);
p = (char *)NULL;
break;
case STAR:
p = "STAR";
break;
default:
regerror("corrupted opcode");
break;
}
if (p != (char *)NULL)
strcat(buf, p);
return (buf);
}
#endif
#ifdef STRCSPN
#ifdef __STDC__
static int strcspn(char *s1, char *s2)
#else
static int strcspn(s1, s2)
char           *s1;
char           *s2;
#endif
{
register char  *scan1;
register char  *scan2;
register int    count;
count = 0;
for (scan1 = s1; *scan1 != '\0'; scan1++) {
for (scan2 = s2; *scan2 != '\0';)
if (*scan1 == *scan2++)
return (count);
count++;
}
return (count);
}
#endif
#ifdef __STDC__
void regsub(regexp *prog, char *source, char *dest)
#else
void regsub(prog, source, dest)
regexp         *prog;
char           *source;
char           *dest;
#endif
{
register char  *src;
register char  *dst;
register char   c;
register int    no;
register int    len;
extern char    *strncpy();
if (prog == (regexp *)NULL ||
source == (char *)NULL || dest == (char *)NULL) {
regerror("NULL parm to regsub");
return;
}
if (UCHARAT(prog->program) != MAGIC) {
regerror("damaged regexp fed to regsub");
return;
}
src = source;
dst = dest;
while ((c = *src++) != '\0') {
if (c == '&')
no = 0;
else if (c == '\\' && '0' <= *src && *src <= '9')
no = *src++ - '0';
else
no = -1;
if (no < 0) {
if (c == '\\' && (*src == '\\' || *src == '&'))
c = *src++;
*dst++ = c;
} else if (prog->startp[no] != (char *)NULL &&
prog->endp[no] != (char *)NULL) {
len = prog->endp[no] - prog->startp[no];
strncpy(dst, prog->startp[no], len);
dst += len;
if (len != 0 && *(dst - 1) == '\0') {
regerror("damaged match string");
return;
}
}
}
*dst++ = '\0';
}
#ifdef __STDC__
void regerror(char *s)
#else
void regerror(s)
char           *s;
#endif
{
fprintf(stderr, "regexp(3): %s", s);
exit(1);
}