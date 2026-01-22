#include <u.h>
#include <libc.h>
#include <ndraw.h>
#include <bio.h>
#include "cons.h"
char *term = "2621";
struct funckey fk[32];
void
emulate(void)
{
char buf[BUFS+1];
int n;
int c;
int standout = 0;
int insmode = 0;
for (;;) {
if (x > xmax || y > ymax) {
x = 0;
newline();
}
buf[0] = get_next_char();
buf[1] = '\0';
switch(buf[0]) {
case '\000':
break;
case '\007':
ringbell();
break;
case '\t':
x = (x|7)+1;
break;
case '\033':
switch(get_next_char()) {
case 'j':
get_next_char();
break;
case '&':
switch(get_next_char()) {
case 'a':
for (;;) {
n = number(buf, nil);
switch(buf[0]) {
case 'r':
case 'y':
y = n;
continue;
case 'c':
x = n;
continue;
case 'R':
case 'Y':
y = n;
break;
case 'C':
x = n;
break;
}
break;
}
break;
case 'd':
if ((n=get_next_char())>='A' && n <= 'O')
standout++;
else if (n == '@')
standout = 0;
break;
default:
get_next_char();
break;
}
break;
case 'i':
if (x>0)
x = (x-1) & ~07;
break;
case 'H':
case 'h':
x = 0;
y = 0;
break;
case 'L':
scroll(y, ymax, y+1, y);
break;
case 'M':
scroll(y+1, ymax+1, y, ymax);
break;
case 'J':
xtipple(Rpt(pt(0, y+1),
pt(xmax+1, ymax+1)));
case 'K':
xtipple(Rpt(pt(x, y),
pt(xmax+1, y+1)));
break;
case 'P':
bitblt(&screen, pt(x, y),
&screen, Rpt(pt(x+1, y),
pt(xmax+1, y+1)),
S);
xtipple(Rpt(pt(xmax, y),
pt(xmax+1, y+1)));
break;
case 'Q':
insmode++;
break;
case 'R':
insmode = 0;
break;
case 'S':
scroll(1, ymax+1, 0, ymax);
break;
case 'T':
scroll(0, ymax, 1, 0);
break;
case 'A':
case 't':
if (y>0)
y--;
if (olines > 0)
olines--;
break;
case 'B':
case 'w':
y++;
break;
case 'C':
case 'v':
x++;
break;
case 'D':
case 'u':
x--;
}
break;
case '\b':
if(x > 0)
--x;
break;
case '\n':
newline();
standout = 0;
if( ttystate[cs->raw].nlcr )
x = 0;
break;
case '\r':
x = 0;
standout = 0;
if( ttystate[cs->raw].crnl )
newline();
break;
default:
n = 1;
c = 0;
while (!cs->raw && host_avail() && x+n<=xmax && n<BUFS
&& (c = get_next_char())>=' ' && c<'\177') {
buf[n++] = c;
c = 0;
}
buf[n] = 0;
if (insmode) {
bitblt(&screen, pt(x+n, y), &screen,
Rpt(pt(x, y), pt(xmax-n+1, y+1)), S);
}
xtipple(Rpt(pt(x,y), pt(x+n, y+1)));
string(&screen, pt(x, y), font, buf, DxorS);
if (standout)
rectf(&screen,
Rpt(pt(x,y),pt(x+n,y+1)),
DxorS);
x += n;
peekc = c;
break;
}
}
}