#include	"mk.h"
char	*termchars = "\"'= \t";
char	*shflags = 0;
int	IWS = ' ';
static char *
squote(char *cp, int c)
{
Rune r;
int n;
while(*cp){
n = chartorune(&r, cp);
if(r == c)
return cp;
if(r == '\\')
n += chartorune(&r, cp+n);
cp += n;
}
SYNERR(-1);
fprint(2, "missing closing '\n");
return 0;
}
char *
charin(char *cp, char *pat)
{
Rune r;
int n, vargen;
vargen = 0;
while(*cp){
n = chartorune(&r, cp);
switch(r){
case '\\':
cp += n;
n = chartorune(&r, cp);
break;
case '\'':
case '"':
cp = squote(cp+1, r);
if(!cp)
return 0;
break;
case '$':
if(*(cp+1) == '{')
vargen = 1;
break;
case '}':
if(vargen)
vargen = 0;
else if(utfrune(pat, r))
return cp;
break;
default:
if(vargen == 0 && utfrune(pat, r))
return cp;
break;
}
cp += n;
}
if(vargen){
SYNERR(-1);
fprint(2, "missing closing } in pattern generator\n");
}
return 0;
}
char*
expandquote(char *s, Rune esc, Bufblock *b)
{
Rune r;
if (esc == '\\') {
s += chartorune(&r, s);
rinsert(b, r);
return s;
}
while(*s){
s += chartorune(&r, s);
if(r == esc)
return s;
if (r == '\\') {
rinsert(b, r);
s += chartorune(&r, s);
}
rinsert(b, r);
}
return 0;
}
int
escapetoken(Biobuf *bp, Bufblock *buf, int preserve, int esc)
{
int c, line;
if(esc == '\\') {
c = Bgetrune(bp);
if(c == '\r')
c = Bgetrune(bp);
if (c == '\n')
mkinline++;
rinsert(buf, c);
return 1;
}
line = mkinline;
while((c = nextrune(bp, 0)) >= 0){
if(c == esc){
if(preserve)
rinsert(buf, c);
return 1;
}
if(c == '\\') {
rinsert(buf, c);
c = Bgetrune(bp);
if(c == '\r')
c = Bgetrune(bp);
if (c < 0)
break;
if (c == '\n')
mkinline++;
}
rinsert(buf, c);
}
SYNERR(line); fprint(2, "missing closing %c\n", esc);
return 0;
}
static char *
copysingle(char *s, Rune q, Bufblock *buf)
{
Rune r;
while(*s){
s += chartorune(&r, s);
rinsert(buf, r);
if(r == q)
break;
}
return s;
}
char *
copyq(char *s, Rune q, Bufblock *buf)
{
if(q == '\'' || q == '"')
return copysingle(s, q, buf);
if(q != '`')
return s;
while(*s){
s += chartorune(&q, s);
rinsert(buf, q);
if(q == '`')
break;
if(q == '\'' || q == '"')
s = copysingle(s, q, buf);
}
return s;
}