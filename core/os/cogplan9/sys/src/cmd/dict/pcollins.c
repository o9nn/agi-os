#include <u.h>
#include <libc.h>
#include <bio.h>
#include "dict.h"
enum {
Buflen=1000,
};
enum {
B = MULTIE+1,
H,
I,
Ps,
Pe,
R,
X,
};
static Assoc tagtab[] = {
{"AA", L'Å'},
{"AC", LACU},
{"B", B},
{"CE", LCED},
{"CI", LFRN},
{"Di", L'ı'},
{"EL", L'-'},
{"GR", LGRV},
{"H", H},
{"I", I},
{"OE", L'Œ'},
{"R", R},
{"TI", LTIL},
{"UM", LUML},
{"X", X},
{"[", Ps},
{"]", Pe},
{"ac", LACU},
{"ce", LCED},
{"ci", LFRN},
{"gr", LGRV},
{"oe", L'œ'},
{"supe", L'e'},
{"supo", L'o'},
{"ti", LTIL},
{"um", LUML},
{"{", Ps},
{"~", L'~'},
{"~~", MTT},
};
static Rune normtab[128] = {
NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
NONE, NONE, L' ', NONE, NONE, NONE, NONE, NONE,
NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
L' ', L'!', L'"',	L'#',	L'$',	L'%',	L'&',	L'\'',
L'(', L')', L'*', L'+', L',', L'-', L'.', L'/',
L'0', L'1', L'2', L'3', L'4', L'5', L'6', L'7',
L'8', L'9', L':', L';', TAGE, L'=', TAGS, L'?',
L'@', L'A', L'B', L'C', L'D', L'E', L'F', L'G',
L'H', L'I', L'J', L'K', L'L', L'M', L'N', L'O',
L'P', L'Q', L'R', L'S', L'T', L'U', L'V', L'W',
L'X', L'Y', L'Z', L'[', L'\\', L']', L'^', L'_',
L'`', L'a', L'b', L'c', L'd', L'e', L'f', L'g',
L'h', L'i', L'j', L'k', L'l', L'm', L'n', L'o',
L'p', L'q', L'r', L's', L't', L'u', L'v', L'w',
L'x', L'y', L'z', L'{', L'|', L'}', L'~', NONE,
};
static char *gettag(char *, char *);
static Entry curentry;
static char tag[Buflen];
#define cursize (curentry.end-curentry.start)
void
pcollprintentry(Entry e, int cmd)
{
char *p, *pe;
long r, rprev, t, rlig;
int saveoi;
Rune *transtab;
p = e.start;
pe = e.end;
transtab = normtab;
rprev = NONE;
changett(0, 0, 0);
curentry = e;
saveoi = 0;
if(cmd == 'h')
outinhibit = 1;
while(p < pe) {
if(cmd == 'r') {
outchar(*p++);
continue;
}
r = transtab[(*p++)&0x7F];
if(r < NONE) {
if(rprev != NONE)
outrune(rprev);
rprev = r;
} else if(r == TAGS) {
p = gettag(p, pe);
t = lookassoc(tagtab, asize(tagtab), tag);
if(t == -1) {
if(debug && !outinhibit)
err("tag %ld %d %s",
e.doff, cursize, tag);
continue;
}
if(t < NONE) {
if(rprev != NONE)
outrune(rprev);
rprev = t;
} else if(t >= LIGS && t < LIGE) {
rlig = liglookup(t, rprev);
if(rlig != NONE)
rprev = rlig;
else {
if(rprev != NONE) outrune(rprev);
rprev = NONE;
}
} else if(t >= MULTI && t < MULTIE) {
if(rprev != NONE) {
outrune(rprev);
rprev = NONE;
}
outrunes(multitab[t-MULTI]);
} else {
if(rprev != NONE) {
outrune(rprev);
rprev = NONE;
}
switch(t){
case H:
if(cmd == 'h')
outinhibit = 0;
else
outnl(0);
break;
case X:
if(cmd == 'h')
outinhibit = 1;
else
outchars(".  ");
break;
case Ps:
saveoi = outinhibit;
outinhibit = 1;
break;
case Pe:
outinhibit = saveoi;
break;
}
}
}
}
if(cmd == 'h')
outinhibit = 0;
outnl(0);
}
long
pcollnextoff(long fromoff)
{
long a;
char *p;
a = Bseek(bdict, fromoff, 0);
if(a < 0)
return -1;
for(;;) {
p = Brdline(bdict, '\n');
if(!p)
break;
if(p[0] == '>' && p[1] == 'H' && p[2] == '<')
return (Boffset(bdict)-Blinelen(bdict));
}
return -1;
}
void
pcollprintkey(void)
{
Bprint(bout, "No pronunciation key yet\n");
}
static char *
gettag(char *f, char *fe)
{
char *t;
int c, i;
t = tag;
i = Buflen;
while(--i > 0) {
c = *f++;
if(c == '<' || f == fe)
break;
*t++ = c;
}
*t = 0;
return f;
}