#include "e.h"
#include "y.tab.h"
#include <ctype.h>
#define	SSIZE	1000
char	token[SSIZE];
int	sp;
void	space(void);
void	dodef(tbl *);
void	define(int);
void	ifdef(void);
void	include(void);
void	delim(void);
yylex(void)
{
register int c;
tbl *tp;
begin:
while ((c = input()) == ' ' || c == '\n' || c == '\t')
;
yylval = c;
switch (c) {
case EOF:
ERROR "unexpected end of input inside equation" WARNING;
return(EOF);
case '~':
return(SPACE);
case '^':
return(THIN);
case '{':
return('{');
case '}':
return('}');
case '"':
for (sp = 0; (c=input())!='"' && c != '\n'; ) {
if (c == '\\')
if ((c = input()) != '"')
token[sp++] = '\\';
token[sp++] = c;
if (sp >= SSIZE)
ERROR "quoted string %.20s... too long", token FATAL;
}
token[sp] = '\0';
yylval = (int) &token[0];
if (c == '\n')
ERROR "missing \" in %.20s", token WARNING;
return(QTEXT);
}
if (!display && c == righteq)
return(EOF);
unput(c);
getstr(token, SSIZE);
dprintf(".\tlex token = |%s|\n", token);
if ((tp = lookup(deftbl, token)) != NULL) {
c = input();
unput(c);
if (c == '(')
dodef(tp);
else {
unput(' ');
pbstr(tp->cval);
dprintf(".\tfound %s|=%s|\n", token, tp->cval);
}
goto begin;
}
if ((tp = lookup(keytbl, token)) == NULL)
return CONTIG;
switch (tp->ival) {
case DEFINE: case TDEFINE: case NDEFINE:
define(tp->ival);
break;
case IFDEF:
ifdef();
break;
case DELIM:
delim();
break;
case GSIZE:
globsize();
break;
case GFONT:
globfont();
break;
case INCLUDE:
include();
break;
case SPACE:
space();
break;
case DOTEQ:
break;
case DOTEN:
if (curfile == infile)
return EOF;
break;
default:
return tp->ival;
}
goto begin;
}
void getstr(char *s, int n)
{
register int c;
register char *p;
p = s;
while ((c = input()) == ' ' || c == '\n')
;
if (c == EOF) {
*s = 0;
return;
}
while (c != ' ' && c != '\t' && c != '\n' && c != '{' && c != '}'
&& c != '"' && c != '~' && c != '^') {
if (!display && c == righteq)
break;
if (c == '(' && p > s) {
*p = '\0';
if (lookup(deftbl, s) != NULL)
break;
}
if (c == '\\')
if ((c = input()) != '"')
*p++ = '\\';
*p++ = c;
if (--n <= 0)
ERROR "token %.20s... too long", s FATAL;
c = input();
}
unput(c);
*p = '\0';
yylval = (int) s;
}
cstr(char *s, int quote, int maxs)
{
int del, c, i;
s[0] = 0;
while ((del=input()) == ' ' || del == '\t')
;
if (quote)
for (i=0; (c=input()) != del && c != EOF;) {
s[i++] = c;
if (i >= maxs)
return(1);
}
else {
if (del == '\n')
return(1);
s[0] = del;
for (i=1; (c=input())!=' ' && c!= '\t' && c!='\n' && c!=EOF;) {
s[i++] = c;
if (i >= maxs)
return(1);
}
}
s[i] = '\0';
if (c == EOF)
ERROR "Unexpected end of input at %.20s", s FATAL;
return(0);
}
void define(int type)
{
char *p1, *p2;
extern int ftune(char *, char *);
getstr(token, SSIZE);
if (type != DEFINE) {
cstr(token, 1, SSIZE);
return;
}
p1 = strsave(token);
if (cstr(token, 1, SSIZE))
ERROR "Unterminated definition at %.20s", token FATAL;
if (lookup(ftunetbl, p1) != NULL) {
dprintf(".\ttune %s %s\n", p1, token);
ftune(p1, token);
} else {
p2 = strsave(token);
install(deftbl, p1, p2, 0);
dprintf(".\tname %s defined as %s\n", p1, p2);
}
}
void ifdef(void)
{
char name[100], *p;
getstr(name, sizeof(name));
cstr(token, 1, SSIZE);
if (lookup(deftbl, name) != NULL) {
p = strsave(token);
pushsrc(Free, p);
pushsrc(String, p);
}
}
char	*spaceval	= NULL;
void space(void)
{
getstr(token, SSIZE);
spaceval = strsave(token);
dprintf(".\tsetting spaceval to %s\n", token);
}
char *strsave(char *s)
{
register char *q;
q = malloc(strlen(s)+1);
if (q == NULL)
ERROR "out of space in strsave on %s", s FATAL;
strcpy(q, s);
return(q);
}
void include(void)
{
char name[100];
FILE *fin;
int c;
extern int errno;
while ((c = input()) == ' ')
;
unput(c);
cstr(name, c == '"', sizeof(name));
if ((fin = fopen(name, "r")) == NULL)
ERROR "can't open file %s", name FATAL;
errno = 0;
curfile++;
curfile->fin = fin;
curfile->fname = strsave(name);
curfile->lineno = 0;
printf(".lf 1 %s\n", curfile->fname);
pushsrc(File, curfile->fname);
}
void delim(void)
{
yyval = eqnreg = 0;
if (cstr(token, 0, SSIZE))
ERROR "Bizarre delimiters" FATAL;
lefteq = token[0];
righteq = token[1];
if (!isprint(lefteq) || !isprint(righteq))
ERROR "Bizarre delimiters" FATAL;
if (lefteq == 'o' && righteq == 'f')
lefteq = righteq = '\0';
}