#include	"mk.h"
char	*termchars = "'= \t";
char	*shflags = "-I";
int	IWS = '\1';
static char *
squote(char *cp)
{
Rune r;
int n;
while(*cp){
n = chartorune(&r, cp);
if(r == '\'') {
n += chartorune(&r, cp+n);
if(r != '\'')
return(cp);
}
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
case '\'':
cp = squote(cp+1);
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
expandquote(char *s, Rune r, Bufblock *b)
{
if (r != '\'') {
rinsert(b, r);
return s;
}
while(*s){
s += chartorune(&r, s);
if(r == '\'') {
if(*s == '\'')
s++;
else
return s;
}
rinsert(b, r);
}
return 0;
}
int
escapetoken(Biobuf *bp, Bufblock *buf, int preserve, int esc)
{
int c, line;
if(esc != '\'')
return 1;
line = mkinline;
while((c = nextrune(bp, 0)) > 0){
if(c == '\''){
if(preserve)
rinsert(buf, c);
c = Bgetrune(bp);
if (c < 0)
break;
if(c != '\''){
Bungetrune(bp);
return 1;
}
}
rinsert(buf, c);
}
SYNERR(line); fprint(2, "missing closing %c\n", esc);
return 0;
}
static char *
copysingle(char *s, Bufblock *buf)
{
Rune r;
while(*s){
s += chartorune(&r, s);
rinsert(buf, r);
if(r == '\'')
break;
}
return s;
}
char *
copyq(char *s, Rune q, Bufblock *buf)
{
if(q == '\'')
return copysingle(s, buf);
if(q != '`')
return s;
while(*s){
s += chartorune(&q, s);
rinsert(buf, q);
if(q == '}')
break;
if(q == '\'')
s = copysingle(s, buf);
}
return s;
}