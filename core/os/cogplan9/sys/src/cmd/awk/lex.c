#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "awk.h"
#include "y.tab.h"
extern YYSTYPE yylval;
extern int infunc;
int lineno = 1;
int bracecnt = 0;
int brackcnt = 0;
int parencnt = 0;
typedef struct Keyword {
char *word;
int sub;
int type;
} Keyword;
Keyword keywords[] ={
{ "BEGIN", XBEGIN, XBEGIN },
{ "END", XEND, XEND },
{ "NF", VARNF, VARNF },
{ "atan2", FATAN, BLTIN },
{ "break", BREAK, BREAK },
{ "close", CLOSE, CLOSE },
{ "continue", CONTINUE, CONTINUE },
{ "cos", FCOS, BLTIN },
{ "delete", DELETE, DELETE },
{ "do", DO, DO },
{ "else", ELSE, ELSE },
{ "exit", EXIT, EXIT },
{ "exp", FEXP, BLTIN },
{ "fflush", FFLUSH, BLTIN },
{ "for", FOR, FOR },
{ "func", FUNC, FUNC },
{ "function", FUNC, FUNC },
{ "getline", GETLINE, GETLINE },
{ "gsub", GSUB, GSUB },
{ "if", IF, IF },
{ "in", IN, IN },
{ "index", INDEX, INDEX },
{ "int", FINT, BLTIN },
{ "length", FLENGTH, BLTIN },
{ "log", FLOG, BLTIN },
{ "match", MATCHFCN, MATCHFCN },
{ "next", NEXT, NEXT },
{ "nextfile", NEXTFILE, NEXTFILE },
{ "print", PRINT, PRINT },
{ "printf", PRINTF, PRINTF },
{ "rand", FRAND, BLTIN },
{ "return", RETURN, RETURN },
{ "sin", FSIN, BLTIN },
{ "split", SPLIT, SPLIT },
{ "sprintf", SPRINTF, SPRINTF },
{ "sqrt", FSQRT, BLTIN },
{ "srand", FSRAND, BLTIN },
{ "sub", SUB, SUB },
{ "substr", SUBSTR, SUBSTR },
{ "system", FSYSTEM, BLTIN },
{ "tolower", FTOLOWER, BLTIN },
{ "toupper", FTOUPPER, BLTIN },
{ "utf", FUTF, BLTIN },
{ "while", WHILE, WHILE },
};
#define DEBUG
#ifdef DEBUG
#define RET(x) { if(dbg)printf("lex %s\n", tokname(x)); return(x); }
#else
#define RET(x) return(x)
#endif
int peek(void)
{
int c = input();
unput(c);
return c;
}
int gettok(char **pbuf, int *psz)
{
int c;
char *buf = *pbuf;
int sz = *psz;
char *bp = buf;
c = input();
if (c == 0)
return 0;
buf[0] = c;
buf[1] = 0;
if (!isalnum(c) && c != '.' && c != '_')
return c;
*bp++ = c;
if (isalpha(c) || c == '_') {
for ( ; (c = input()) != 0; ) {
if (bp-buf >= sz)
if (!adjbuf(&buf, &sz, bp-buf+2, 100, &bp, 0))
FATAL( "out of space for name %.10s...", buf );
if (isalnum(c) || c == '_')
*bp++ = c;
else {
*bp = 0;
unput(c);
break;
}
}
} else {
char *rem;
for ( ; (c = input()) != 0; ) {
if (bp-buf >= sz)
if (!adjbuf(&buf, &sz, bp-buf+2, 100, &bp, 0))
FATAL( "out of space for number %.10s...", buf );
if (isdigit(c) || c == 'e' || c == 'E'
|| c == '.' || c == '+' || c == '-')
*bp++ = c;
else {
unput(c);
break;
}
}
*bp = 0;
strtod(buf, &rem);
unputstr(rem);
rem[0] = 0;
}
*pbuf = buf;
*psz = sz;
return buf[0];
}
int word(char *);
int string(void);
int regexpr(void);
int sc = 0;
int reg = 0;
int yylex(void)
{
int c;
static char *buf = 0;
static int bufsize = 500;
if (buf == 0 && (buf = (char *) malloc(bufsize)) == NULL)
FATAL( "out of space in yylex" );
if (sc) {
sc = 0;
RET('}');
}
if (reg) {
reg = 0;
return regexpr();
}
for (;;) {
c = gettok(&buf, &bufsize);
if (c == 0)
return 0;
if (isalpha(c) || c == '_')
return word(buf);
if (isdigit(c) || c == '.') {
yylval.cp = setsymtab(buf, tostring(buf), atof(buf), CON|NUM, symtab);
RET(NUMBER);
}
yylval.i = c;
switch (c) {
case '\n':
RET(NL);
case '\r':
case ' ':
case '\t':
break;
case '#':
while ((c = input()) != '\n' && c != 0)
;
unput(c);
break;
case ';':
RET(';');
case '\\':
if (peek() == '\n') {
input();
} else if (peek() == '\r') {
input(); input();
lineno++;
} else {
RET(c);
}
break;
case '&':
if (peek() == '&') {
input(); RET(AND);
} else
RET('&');
case '|':
if (peek() == '|') {
input(); RET(BOR);
} else
RET('|');
case '!':
if (peek() == '=') {
input(); yylval.i = NE; RET(NE);
} else if (peek() == '~') {
input(); yylval.i = NOTMATCH; RET(MATCHOP);
} else
RET(NOT);
case '~':
yylval.i = MATCH;
RET(MATCHOP);
case '<':
if (peek() == '=') {
input(); yylval.i = LE; RET(LE);
} else {
yylval.i = LT; RET(LT);
}
case '=':
if (peek() == '=') {
input(); yylval.i = EQ; RET(EQ);
} else {
yylval.i = ASSIGN; RET(ASGNOP);
}
case '>':
if (peek() == '=') {
input(); yylval.i = GE; RET(GE);
} else if (peek() == '>') {
input(); yylval.i = APPEND; RET(APPEND);
} else {
yylval.i = GT; RET(GT);
}
case '+':
if (peek() == '+') {
input(); yylval.i = INCR; RET(INCR);
} else if (peek() == '=') {
input(); yylval.i = ADDEQ; RET(ASGNOP);
} else
RET('+');
case '-':
if (peek() == '-') {
input(); yylval.i = DECR; RET(DECR);
} else if (peek() == '=') {
input(); yylval.i = SUBEQ; RET(ASGNOP);
} else
RET('-');
case '*':
if (peek() == '=') {
input(); yylval.i = MULTEQ; RET(ASGNOP);
} else if (peek() == '*') {
input();
if (peek() == '=') {
input(); yylval.i = POWEQ; RET(ASGNOP);
} else {
RET(POWER);
}
} else
RET('*');
case '/':
RET('/');
case '%':
if (peek() == '=') {
input(); yylval.i = MODEQ; RET(ASGNOP);
} else
RET('%');
case '^':
if (peek() == '=') {
input(); yylval.i = POWEQ; RET(ASGNOP);
} else
RET(POWER);
case '$':
c = gettok(&buf, &bufsize);
if (c == '(' || c == '[' || (infunc && isarg(buf) >= 0)) {
unputstr(buf);
RET(INDIRECT);
} else if (isalpha(c)) {
if (strcmp(buf, "NF") == 0) {
unputstr("(NF)");
RET(INDIRECT);
}
yylval.cp = setsymtab(buf, "", 0.0, STR|NUM, symtab);
RET(IVAR);
} else {
unputstr(buf);
RET(INDIRECT);
}
case '}':
if (--bracecnt < 0)
SYNTAX( "extra }" );
sc = 1;
RET(';');
case ']':
if (--brackcnt < 0)
SYNTAX( "extra ]" );
RET(']');
case ')':
if (--parencnt < 0)
SYNTAX( "extra )" );
RET(')');
case '{':
bracecnt++;
RET('{');
case '[':
brackcnt++;
RET('[');
case '(':
parencnt++;
RET('(');
case '"':
return string();
default:
RET(c);
}
}
}
int string(void)
{
int c, n;
char *s, *bp;
static char *buf = 0;
static int bufsz = 500;
if (buf == 0 && (buf = (char *) malloc(bufsz)) == NULL)
FATAL("out of space for strings");
for (bp = buf; (c = input()) != '"'; ) {
if (!adjbuf(&buf, &bufsz, bp-buf+2, 500, &bp, 0))
FATAL("out of space for string %.10s...", buf);
switch (c) {
case '\n':
case '\r':
case 0:
SYNTAX( "non-terminated string %.10s...", buf );
lineno++;
break;
case '\\':
c = input();
switch (c) {
case '"': *bp++ = '"'; break;
case 'n': *bp++ = '\n'; break;
case 't': *bp++ = '\t'; break;
case 'f': *bp++ = '\f'; break;
case 'r': *bp++ = '\r'; break;
case 'b': *bp++ = '\b'; break;
case 'v': *bp++ = '\v'; break;
case 'a': *bp++ = '\007'; break;
case '\\': *bp++ = '\\'; break;
case '0': case '1': case '2':
case '3': case '4': case '5': case '6': case '7':
n = c - '0';
if ((c = peek()) >= '0' && c < '8') {
n = 8 * n + input() - '0';
if ((c = peek()) >= '0' && c < '8')
n = 8 * n + input() - '0';
}
*bp++ = n;
break;
case 'x':
{ char xbuf[100], *px;
for (px = xbuf; (c = input()) != 0 && px-xbuf < 100-2; ) {
if (isdigit(c)
|| (c >= 'a' && c <= 'f')
|| (c >= 'A' && c <= 'F'))
*px++ = c;
else
break;
}
*px = 0;
unput(c);
sscanf(xbuf, "%x", &n);
*bp++ = n;
break;
}
default:
*bp++ = c;
break;
}
break;
default:
*bp++ = c;
break;
}
}
*bp = 0;
s = tostring(buf);
*bp++ = ' '; *bp++ = 0;
yylval.cp = setsymtab(buf, s, 0.0, CON|STR|DONTFREE, symtab);
RET(STRING);
}
int binsearch(char *w, Keyword *kp, int n)
{
int cond, low, mid, high;
low = 0;
high = n - 1;
while (low <= high) {
mid = (low + high) / 2;
if ((cond = strcmp(w, kp[mid].word)) < 0)
high = mid - 1;
else if (cond > 0)
low = mid + 1;
else
return mid;
}
return -1;
}
int word(char *w)
{
Keyword *kp;
int c, n;
n = binsearch(w, keywords, sizeof(keywords)/sizeof(keywords[0]));
kp = keywords + n;
if (n != -1) {
yylval.i = kp->sub;
switch (kp->type) {
case FSYSTEM:
if (safe)
SYNTAX( "system is unsafe" );
RET(kp->type);
case FUNC:
if (infunc)
SYNTAX( "illegal nested function" );
RET(kp->type);
case RETURN:
if (!infunc)
SYNTAX( "return not in function" );
RET(kp->type);
case VARNF:
yylval.cp = setsymtab("NF", "", 0.0, NUM, symtab);
RET(VARNF);
default:
RET(kp->type);
}
}
c = peek();
if (c != '(' && infunc && (n=isarg(w)) >= 0) {
yylval.i = n;
RET(ARG);
} else {
yylval.cp = setsymtab(w, "", 0.0, STR|NUM|DONTFREE, symtab);
if (c == '(') {
RET(CALL);
} else {
RET(VAR);
}
}
}
void startreg(void)
{
reg = 1;
}
int regexpr(void)
{
int c;
static char *buf = 0;
static int bufsz = 500;
char *bp;
if (buf == 0 && (buf = (char *) malloc(bufsz)) == NULL)
FATAL("out of space for rex expr");
bp = buf;
for ( ; (c = input()) != '/' && c != 0; ) {
if (!adjbuf(&buf, &bufsz, bp-buf+3, 500, &bp, 0))
FATAL("out of space for reg expr %.10s...", buf);
if (c == '\n') {
SYNTAX( "newline in regular expression %.10s...", buf );
unput('\n');
break;
} else if (c == '\\') {
*bp++ = '\\';
*bp++ = input();
} else {
*bp++ = c;
}
}
*bp = 0;
yylval.s = tostring(buf);
unput('/');
RET(REGEXPR);
}
char ebuf[300];
char *ep = ebuf;
char yysbuf[100];
char *yysptr = yysbuf;
FILE *yyin = 0;
int input(void)
{
int c;
extern char *lexprog;
if (yysptr > yysbuf)
c = *--yysptr;
else if (lexprog != NULL) {
if ((c = *lexprog) != 0)
lexprog++;
} else
c = pgetc();
if (c == '\n')
lineno++;
else if (c == EOF)
c = 0;
if (ep >= ebuf + sizeof ebuf)
ep = ebuf;
return *ep++ = c;
}
void unput(int c)
{
if (c == '\n')
lineno--;
if (yysptr >= yysbuf + sizeof(yysbuf))
FATAL("pushed back too much: %.20s...", yysbuf);
*yysptr++ = c;
if (--ep < ebuf)
ep = ebuf + sizeof(ebuf) - 1;
}
void unputstr(char *s)
{
int i;
for (i = strlen(s)-1; i >= 0; i--)
unput(s[i]);
}