#include <u.h>
#include <libc.h>
#include <draw.h>
#include <bio.h>
#include <ctype.h>
#include "cons.h"
int	wraparound = 1;
int	originrelative = 0;
int	tabcol[200];
struct funckey vt100fk[NKEYS] = {
{ "up key",		"\033OA", },
{ "down key",		"\033OB", },
{ "left key",		"\033OD", },
{ "right key",		"\033OC", },
};
struct funckey ansifk[NKEYS] = {
{ "up key",		"\033[A", },
{ "down key",		"\033[B", },
{ "left key",		"\033[D", },
{ "right key",		"\033[C", },
{ "F1",			"\033OP", },
{ "F2",			"\033OQ", },
{ "F3",			"\033OR", },
{ "F4",			"\033OS", },
{ "F5",			"\033OT", },
{ "F6",			"\033OU", },
{ "F7",			"\033OV", },
{ "F8",			"\033OW", },
{ "F9",			"\033OX", },
{ "F10",		"\033OY", },
{ "F11",		"\033OZ", },
{ "F12",		"\033O1", },
};
struct funckey vt220fk[NKEYS] = {
{ "up key",		"\033[A", },
{ "down key",		"\033[B", },
{ "left key",		"\033[D", },
{ "right key",		"\033[C", },
};
struct funckey xtermfk[NKEYS] = {
{ "page up",	"\033[5~", },
{ "page down",	"\033[6~", },
{ "up key",		"\033[A", },
{ "down key",		"\033[B", },
{ "left key",		"\033[D", },
{ "right key",		"\033[C", },
{ "F1",			"\033[11~", },
{ "F2",			"\033[12~", },
{ "F3",			"\033[13~", },
{ "F4",			"\033[14~", },
{ "F5",			"\033[15~", },
{ "F6",			"\033[17~", },
{ "F7",			"\033[18~", },
{ "F8",			"\033[19~", },
{ "F9",			"\033[20~", },
{ "F10",		"\033[21~", },
{ "F11",		"\033[22~", },
{ "F12",		"\033[23~", },
};
char gmap[256] = {
['_']	' ',
['\\']	'*',
['a']	'X',
['b']	'\t',
['c']	'\x0C',
['d']	'\r',
['e']	'\n',
['f']	'o',
['g']	'+',
['h']	'\n',
['i']	'\v',
['j']	'+',
['k']	'+',
['l']	'+',
['m']	'+',
['n']	'+',
['o']	'-',
['p']	'-',
['q']	'-',
['r']	'-',
['s']	'-',
['t']	'+',
['u']	'+',
['v']	'+',
['w']	'+',
['x']	'|',
['y']	'<',
['z']	'>',
['{']	'p',
['|']	'!',
['}']	'L',
['~']	'.',
};
static void setattr(int argc, int *argv);
void
fixops(int *operand)
{
if(operand[0] < 1)
operand[0] = 1;
}
void
emulate(void)
{
char buf[BUFS+1];
int i;
int n;
int c;
int operand[10];
int noperand;
int savex, savey, saveattr, saveisgraphics;
int isgraphics;
int g0set, g1set;
int dch;
isgraphics = 0;
g0set = 'B';
g1set = 'B';
savex = savey = 0;
yscrmin = 0;
yscrmax = ymax;
saveattr = 0;
saveisgraphics = 0;
for(c=0; (c+=8)<nelem(tabcol);)
tabcol[c] = 1;
for (;;) {
if (y > ymax) {
x = 0;
newline();
}
buf[0] = get_next_char();
buf[1] = '\0';
switch(buf[0]) {
case '\000':
case '\001':
case '\002':
case '\003':
case '\004':
case '\005':
case '\006':
goto Default;
case '\007':
ringbell();
break;
case '\010':
if (x > 0)
--x;
break;
case '\011':
for(c=x+1; c<nelem(tabcol) && !tabcol[c]; c++)
;
if(c < nelem(tabcol))
x = c;
else
x = xmax;
break;
case '\012':
case '\013':
case '\014':
newline();
if (ttystate[cs->raw].nlcr)
x = 0;
break;
case '\015':
x = 0;
if (ttystate[cs->raw].crnl)
newline();
break;
case '\016':
isgraphics = (isdigit(g1set));
break;
case '\017':
isgraphics = (isdigit(g0set));
break;
case '\020':
case '\021':
case '\022':
case '\023':
case '\024':
case '\025':
case '\026':
case '\027':
case '\030':
case '\031':
case '\032':
goto Default;
;
case '\034':
case '\035':
case '\036':
case '\037':
break;
case '\177':
break;
case '\033':
switch(dch = get_next_char()){
case '1':
break;
case '2':
break;
case '7':
savex = x;
savey = y;
saveattr = attr;
saveisgraphics = isgraphics;
break;
case '8':
x = savex;
y = savey;
attr = saveattr;
isgraphics = saveisgraphics;
break;
case 'c':
print("resetterminal\n");
cursoron = 1;
ttystate[cs->raw].nlcr = 0;
break;
case 'D':
if(++y > yscrmax) {
y = yscrmax;
scroll(yscrmin+1, yscrmax+1, yscrmin, yscrmax);
}
break;
case 'E':
x = 0;
if(++y > yscrmax) {
y = yscrmax;
scroll(yscrmin+1, yscrmax+1, yscrmin, yscrmax);
}
break;
case 'H':
if(x < nelem(tabcol))
tabcol[x] = 1;
break;
case 'M':
if(--y < yscrmin) {
y = yscrmin;
scroll(yscrmin, yscrmax, yscrmin+1, yscrmin);
}
break;
case 'Z':
Ident:
sendnchars2(7, "\033[?1;2c");
break;
case '<':
break;
case '>':
break;
case '=':
break;
case '#':
switch(get_next_char()){
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
break;
}
break;
case '(':
g0set = get_next_char();
break;
case ')':
g1set = get_next_char();
break;
case '[':
memset(operand, 0, sizeof(operand));
operand[0] = number(buf, &i);
noperand = 1;
while(buf[0] == ';' || buf[0] == '?'){
if(noperand < nelem(operand)){
noperand++;
operand[noperand-1] = number(buf, nil);
} else
number(buf, nil);
}
switch(dch = buf[0]){
case 'c':
goto Ident;
case 'g':
switch(operand[0]){
case 0:
if(x < nelem(tabcol))
tabcol[x] = 0;
break;
case 3:
memset(tabcol, 0, sizeof tabcol);
break;
}
break;
case 'l':
if(noperand == 1){
switch(operand[0]){
case 20:
ttystate[cs->raw].nlcr = 1;
break;
case 30:
break;
}
}else while(--noperand > 0){
switch(operand[noperand]){
case 1:
break;
case 2:
break;
case 3:
setdim(-1, 80);
break;
case 4:
break;
case 5:
break;
case 6:
originrelative = 0;
x = y = 0;
break;
case 7:
wraparound = 0;
break;
case 8:
break;
case 9:
break;
case 25:
cursoron = 0;
break;
}
}
break;
case 's':
break;
case 'h':
if(noperand == 1){
switch(operand[0]){
default:
break;
case 20:
ttystate[cs->raw].nlcr = 0;
break;
case 30:
break;
}
}else while(--noperand > 0){
switch(operand[noperand]){
default:
break;
case 1:
break;
case 2:
break;
case 3:
setdim(-1, 132);
break;
case 4:
break;
case 5:
break;
case 6:
originrelative = 1;
x = 0;
y = yscrmin;
break;
case 7:
wraparound = 1;
break;
case 8:
break;
case 9:
break;
case 25:
cursoron = 1;
break;
}
}
break;
case 'm':
setattr(noperand, operand);
break;
case 'n':
switch(operand[0]){
case 5:
sendnchars2(4, "\033[0n");
break;
case 6:
sendnchars2(sprint(buf, "\033[%d;%dR",
originrelative ? y+1 - yscrmin : y+1, x+1), buf);
break;
}
break;
case 'q':
break;
case 'r':
yscrmin = 0;
yscrmax = ymax;
switch(noperand){
case 2:
yscrmax = operand[1]-1;
if(yscrmax > ymax)
yscrmax = ymax;
case 1:
yscrmin = operand[0]-1;
if(yscrmin < 0)
yscrmin = 0;
}
x = 0;
y = yscrmin;
break;
case 'x':
sendnchars2(20, "\033[3;1;1;120;120;1;0x");
break;
case 'y':
break;
case 'e':
case 'A':
fixops(operand);
y -= operand[0];
if(y < yscrmin)
y = yscrmin;
olines -= operand[0];
if(olines < 0)
olines = 0;
break;
case 'B':
fixops(operand);
y += operand[0];
if(y > yscrmax)
y=yscrmax;
break;
case 'a':
case 'C':
fixops(operand);
x += operand[0];
if(x > xmax)
x = xmax;
break;
case 'D':
fixops(operand);
x -= operand[0];
if(x < 0)
x = 0;
break;
case '\'':
case 'G':
fixops(operand);
x = operand[0] - 1;
if(x > xmax)
x = xmax;
break;
case 'H':
case 'f':
fixops(operand+1);
x = operand[1] - 1;
if(x > xmax)
x = xmax;
case 'd':
fixops(operand);
y = operand[0] - 1;
if(originrelative){
y += yscrmin;
if(y > yscrmax)
y = yscrmax;
}else{
if(y > ymax)
y = ymax;
}
break;
case 'J':
switch (operand[0]) {
case 2:
clear(Rpt(pt(0, 0), pt(xmax+1, ymax+1)));
break;
case 1:
clear(Rpt(pt(0, 0), pt(xmax+1, y)));
clear(Rpt(pt(0, y), pt(x+1, y+1)));
break;
default:
clear(Rpt(pt(x, y), pt(xmax+1, y+1)));
clear(Rpt(pt(0, y+1), pt(xmax+1, ymax+1)));
break;
}
break;
case 'K':
switch (operand[0]) {
case 2:
clear(Rpt(pt(0, y), pt(xmax+1, y+1)));
break;
case 1:
clear(Rpt(pt(0, y), pt(x+1, y+1)));
break;
default:
clear(Rpt(pt(x, y), pt(xmax+1, y+1)));
break;
}
break;
case 'P':
fixops(operand);
i = x + operand[0];
draw(screen, Rpt(pt(x, y), pt(xmax+1, y+1)), screen, nil, pt(i, y));
clear(Rpt(pt(xmax-operand[0], y), pt(xmax+1, y+1)));
break;
case '@':
fixops(operand);
i = x + operand[0];
draw(screen, Rpt(pt(i, y), pt(xmax+1, y+1)), screen, nil, pt(x, y));
clear(Rpt(pt(x, y), pt(i, y+1)));
break;
case 'X':
fixops(operand);
i = x + operand[0];
clear(Rpt(pt(x, y), pt(i, y+1)));
break;
case 'L':
fixops(operand);
for(i = 0; i < operand[0]; ++i)
scroll(y, yscrmax, y+1, y);
break;
case 'M':
fixops(operand);
for(i = 0; i < operand[0]; ++i)
scroll(y+1, yscrmax+1, y, yscrmax);
break;
case 'T':
fixops(operand);
for(i = 0; i < operand[0]; ++i)
scroll(yscrmin, yscrmax, yscrmin+1, yscrmin);
break;
case 'S':
fixops(operand);
for(i = 0; i < operand[0]; ++i)
scroll(yscrmin+1, yscrmax+1, yscrmin, yscrmin);
break;
case '=':
number(buf, nil);
switch(buf[0]) {
case 'h':
case 'l':
break;
}
break;
default:
print("unknown escape2 '%c' (0x%x)\n", dch, dch);
break;
}
break;
case '\033':
peekc = '\033';
break;
case ']':
{
int ch, fd;
number(buf, nil);
i = 0;
while((ch = get_next_char()) != '\a')
if(i < sizeof buf)
buf[i++] = ch;
fd = open("/dev/label", OWRITE);
write(fd, buf, i);
close(fd);
}
break;
default:
print("unknown command '%c' (0x%x)\n", dch, dch);
break;
}
break;
default:
Default:
if(isgraphics && gmap[(uchar) buf[0]])
buf[0] = gmap[(uchar) buf[0]];
if (x > xmax){
if(wraparound){
x = 0;
newline();
}else{
continue;
}
}
n = 1;
c = 0;
while (!cs->raw && host_avail() && x+n<=xmax && n<BUFS
&& (c = get_next_char())>=' ' && c<'\177') {
buf[n++] = c;
c = 0;
}
buf[n] = 0;
drawstring(pt(x, y), buf, attr);
x += n;
peekc = c;
break;
}
}
}
static void
setattr(int argc, int *argv)
{
int i;
for(i=0; i<argc; i++) {
switch(argv[i]) {
case 0:
attr = defattr;
fgcolor = fgdefault;
bgcolor = bgdefault;
break;
case 1:
attr |= THighIntensity;
break;
case 4:
attr |= TUnderline;
break;
case 5:
attr |= TBlink;
break;
case 7:
attr |= TReverse;
break;
case 8:
attr |= TInvisible;
break;
case 22:
attr &= ~THighIntensity;
break;
case 24:
attr &= ~TUnderline;
break;
case 25:
attr &= ~TBlink;
break;
case 27:
attr &= ~TReverse;
break;
case 28:
attr &= ~TInvisible;
break;
case 30:
case 31:
case 32:
case 33:
case 34:
case 35:
case 36:
case 37:
fgcolor = (nocolor? fgdefault: colors[argv[i]-30]);
break;
case 39:
fgcolor = fgdefault;
break;
case 40:
case 41:
case 42:
case 43:
case 44:
case 45:
case 46:
case 47:
bgcolor = (nocolor? bgdefault: colors[argv[i]-40]);
break;
case 49:
bgcolor = bgdefault;
break;
}
}
}