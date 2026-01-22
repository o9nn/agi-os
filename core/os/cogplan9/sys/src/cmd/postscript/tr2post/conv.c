#include <u.h>
#include <libc.h>
#include <bio.h>
#include "../common/common.h"
#include "tr2post.h"
void
conv(Biobufhdr *Bp) {
long n;
int r;
char special[10];
int save;
inputlineno = 1;
if (debug)
fprint(2, "conv(Biobufhdr *Bp=%#p)\n", Bp);
while ((r = Bgetrune(Bp)) >= 0) {
switch (r) {
case 's':
Bgetfield(Bp, 'd', &fontsize, 0);
break;
case 'f':
Bgetfield(Bp, 'd', &fontpos, 0);
save = inputlineno;
settrfont();
inputlineno = save;
break;
case 'c':
r = Bgetrune(Bp);
runeout(r);
break;
case 'C':
Bgetfield(Bp, 's', special, 10);
specialout(special);
break;
case 'N':
Bgetfield(Bp, 'd', &n, 0);
break;
case 'H':
Bgetfield(Bp, 'd', &n, 0);
hgoto(n);
break;
case 'V':
Bgetfield(Bp, 'd', &n, 0);
vgoto(n);
break;
case 'h':
Bgetfield(Bp, 'd', &n, 0);
hmot(n);
break;
case 'v':
Bgetfield(Bp, 'd', &n, 0);
vmot(n);
break;
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
n = (r - '0') * 10;
r = Bgetrune(Bp);
if (r < 0)
error(FATAL, "EOF or error reading input\n");
else if (r < '0' || r > '9')
error(FATAL, "integer expected\n");
n += r - '0';
r = Bgetrune(Bp);
hmot(n);
runeout(r);
break;
case 'p':
Bgetfield(Bp, 'd', &n, 0);
endpage();
startpage();
break;
case 'n':
Brdline(Bp, '\n');
inputlineno++;
break;
case 'w':
break;
case 'D':
draw(Bp);
break;
case 'x':
devcntl(Bp);
break;
case '#':
Brdline(Bp, '\n');
case '\n':
inputlineno++;
break;
default:
error(WARNING, "unknown troff function <%c>\n", r);
break;
}
}
endpage();
if (debug) {
fprint(2, "r=%#ux\n", r);
fprint(2, "leaving conv\n");
}
}