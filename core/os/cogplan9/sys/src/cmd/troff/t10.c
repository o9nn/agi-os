#include "tdef.h"
#include "fns.h"
#include "ext.h"
int	vpos	 = 0;
int	hpos	 = 0;
extern Font fonts[MAXFONTS+1];
int	Inch;
int	Hor;
int	Vert;
int	Unitwidth;
int	nfonts;
void t_ptinit(void)
{
int i;
char buf[100], *p;
hmot = t_hmot;
makem = t_makem;
setabs = t_setabs;
setch = t_setch;
sethl = t_sethl;
setht = t_setht;
setslant = t_setslant;
vmot = t_vmot;
xlss = t_xlss;
findft = t_findft;
width = t_width;
mchbits = t_mchbits;
ptlead = t_ptlead;
ptout = t_ptout;
ptpause = t_ptpause;
setfont = t_setfont;
setps = t_setps;
setwd = t_setwd;
if ((p = getenv("TYPESETTER")) != 0){
strncpy(devname, p, sizeof devname);
devname[sizeof devname-1] = 0;
}
if (termtab[0] == 0)
strcpy(termtab, DWBfontdir);
if (fontdir[0] == 0)
strcpy(fontdir, DWBfontdir);
if (devname[0] == 0)
strcpy(devname, TDEVNAME);
hyf = 1;
lg = 1;
snprintf(buf, sizeof buf, "/dev%s/DESC", devname);
strcat(termtab, buf);
if (getdesc(termtab) < 0) {
ERROR "can't open DESC file %s", termtab WARN;
done3(1);
}
if (!ascii) {
OUT "x T %s\n", devname PUT;
OUT "x res %d %d %d\n", Inch, Hor, Vert PUT;
OUT "x init\n" PUT;
}
for (i = 1; i <= nfonts; i++)
setfp(i, fontlab[i], (char *) 0, 0);
sps = EM/3;
ics = EM;
for (i = 0; i < (NTAB - 1) && DTAB * (i + 1) < TABMASK; i++)
tabtab[i] = DTAB * (i + 1);
tabtab[NTAB-1] = 0;
pl = 11 * INCH;
po = PO;
spacesz = SS;
lss = lss1 = VS;
ll = ll1 = lt = lt1 = LL;
t_specnames();
}
void t_specnames(void)
{
int	i;
for (i = 0; spnames[i].n; i++)
*spnames[i].n = chadd(spnames[i].v, Troffchar, Install);
}
void t_ptout(Tchar i)
{
int dv;
Tchar *k;
int temp, a, b;
int diff;
if (cbits(i) != '\n') {
if (olinep >= oline + olnsize) {
diff = olinep - oline;
olnsize += OLNSIZE;
if ((oline = (Tchar *)realloc((char *)oline, olnsize * sizeof(Tchar))) != NULL) {
if (diff && olinep)
olinep = oline + diff;
} else {
ERROR "Output line overflow." WARN;
done(2);
}
}
*olinep++ = i;
return;
}
if (olinep == oline) {
lead += lss;
return;
}
hpos = po;
esc = 0;
ptesc();
dv = 0;
for (k = oline; k < olinep; k++) {
if (ismot(*k) && isvmot(*k)) {
temp = absmot(*k);
if (isnmot(*k))
temp = -temp;
dv += temp;
}
}
if (dv) {
vflag++;
*olinep++ = makem(-dv);
vflag = 0;
}
b = dip->blss + lss;
lead += dip->blss + lss;
dip->blss = 0;
for (k = oline; k < olinep; )
k += ptout0(k);
olinep = oline;
lead += dip->alss;
a = dip->alss;
dip->alss = 0;
OUT "n%d %d\n", b, a PUT;
}
int ptout0(Tchar *pi)
{
int j, k, w, z, dx, dy, dx2, dy2, n;
int outsize;
Tchar i;
w = 0;
outsize = 1;
i = *pi;
k = cbits(i);
if (ismot(i)) {
j = absmot(i);
if (isnmot(i))
j = -j;
if (isvmot(i))
lead += j;
else
esc += j;
return(outsize);
}
if (k == CHARHT) {
xpts = fbits(i);
if (xpts != mpts)
ptps();
OUT "x H %d\n", sbits(i) PUT;
return(outsize);
}
if (k == SLANT) {
OUT "x S %d\n", sfbits(i)-180 PUT;
return(outsize);
}
if (k == WORDSP) {
oput('w');
return(outsize);
}
if (sfbits(i) == oldbits) {
xfont = pfont;
xpts = ppts;
} else
xbits(i, 2);
if (k == XON) {
extern int xon;
ptflush();
if (esc)
ptesc();
if (xfont != mfont)
ptfont();
if (xpts != mpts)
ptps();
if (lead)
ptlead();
OUT "x X " PUT;
xon++;
for (j = 1; cbits(pi[j]) != XOFF; j++)
outascii(pi[j]);
oput('\n');
xon--;
return j+1;
}
if (k < 040 && k != DRAWFCN)
return(outsize);
j = z = 0;
if (k != DRAWFCN) {
if (widcache[k].fontpts == (xfont<<8) + xpts  && !setwdf) {
w = widcache[k].width;
bd = 0;
cs = 0;
} else
w = getcw(k);
if (cs) {
if (bd)
w += (bd - 1) * HOR;
j = (cs - w) / 2;
w = cs - j;
if (bd)
w -= (bd - 1) * HOR;
}
if (iszbit(i)) {
if (cs)
w = -j;
else
w = 0;
z = 1;
}
}
esc += j;
if (xfont != mfont)
ptfont();
if (xpts != mpts)
ptps();
if (lead)
ptlead();
if (k == DRAWFCN) {
if (esc)
ptesc();
w = 0;
dx = absmot(pi[3]);
if (isnmot(pi[3]))
dx = -dx;
dy = absmot(pi[4]);
if (isnmot(pi[4]))
dy = -dy;
switch (cbits(pi[1])) {
case DRAWCIRCLE:
OUT "D%c %d\n", DRAWCIRCLE, dx PUT;
hpos += dx;
break;
case DRAWELLIPSE:
OUT "D%c %d %d\n", DRAWELLIPSE, dx, dy PUT;
hpos += dx;
break;
case DRAWBUILD:
k = cbits(pi[2]);
OUT "D%c %d ", DRAWBUILD, dx PUT;
if (k < ALPHABET)
OUT "%c\n", k PUT;
else
ptchname(k);
hpos += dx;
break;
case DRAWLINE:
k = cbits(pi[2]);
OUT "D%c %d %d ", DRAWLINE, dx, dy PUT;
if (k < ALPHABET)
OUT "%c\n", k PUT;
else
ptchname(k);
hpos += dx;
vpos += dy;
break;
case DRAWARC:
dx2 = absmot(pi[5]);
if (isnmot(pi[5]))
dx2 = -dx2;
dy2 = absmot(pi[6]);
if (isnmot(pi[6]))
dy2 = -dy2;
OUT "D%c %d %d %d %d\n", DRAWARC,
dx, dy, dx2, dy2 PUT;
hpos += dx + dx2;
vpos += dy + dy2;
break;
case 's':
pi[1] = '~';
case DRAWSPLINE:
default:
OUT "D%c %d %d", cbits(pi[1]), dx, dy PUT;
hpos += dx;
vpos += dy;
if (cbits(pi[3]) == DRAWFCN || cbits(pi[4]) == DRAWFCN) {
OUT "\n" PUT;
break;
}
for (n = 5; cbits(pi[n]) != DRAWFCN; n += 2) {
dx = absmot(pi[n]);
if (isnmot(pi[n]))
dx = -dx;
dy = absmot(pi[n+1]);
if (isnmot(pi[n+1]))
dy = -dy;
OUT " %d %d", dx, dy PUT;
hpos += dx;
vpos += dy;
}
OUT "\n" PUT;
break;
}
for (n = 3; cbits(pi[n]) != DRAWFCN; n++)
;
outsize = n + 1;
} else if (k < ALPHABET) {
if (esc > 0 && esc < 100) {
oput(esc / 10 + '0');
oput(esc % 10 + '0');
oput(k);
hpos += esc;
esc = 0;
} else {
if (esc)
ptesc();
oput('c');
oput(k);
oput('\n');
}
} else {
if (esc)
ptesc();
ptchname(k);
}
if (bd) {
bd -= HOR;
if (esc += bd)
ptesc();
if (k < ALPHABET)
OUT "c%c\n", k PUT;
else
ptchname(k);
if (z)
esc -= bd;
}
esc += w;
return(outsize);
}
void ptchname(int k)
{
char *chn = chname(k);
switch (chn[0]) {
case MBchar:
OUT "c%s\n", chn+1 PUT;
break;
case Number:
OUT "N%s\n", chn+1 PUT;
break;
case Troffchar:
OUT "C%s\n", chn+1 PUT;
break;
default:
ERROR "illegal char type %s", chn WARN;
break;
}
}
void ptflush(void)
{
if (TROFF) {
hpos += esc;
OUT "\nH%d\n", hpos PUT;
esc = 0;
ptps();
ptfont();
ptlead();
}
}
void ptps(void)
{
int i, j, k;
i = xpts;
for (j = 0; i > (k = pstab[j]); j++)
if (!k) {
k = pstab[--j];
break;
}
if (!ascii)
OUT "s%d\n", k PUT;
mpts = i;
}
void ptfont(void)
{
mfont = xfont;
if (ascii)
return;
if (xfont > nfonts) {
ptfpcmd(0, fonts[xfont].longname, 0);
OUT "f0\n" PUT;
} else
OUT "f%d\n", xfont PUT;
}
void ptfpcmd(int f, char *s, char *longname)
{
if (f > nfonts)
f = 0;
if (longname) {
OUT "x font %d %s %s\n", f, s, longname PUT;
} else {
OUT "x font %d %s\n", f, s PUT;
}
}
void t_ptlead(void)
{
vpos += lead;
if (!ascii)
OUT "V%d\n", vpos PUT;
lead = 0;
}
void ptesc(void)
{
hpos += esc;
if (!ascii)
if (esc > 0) {
oput('h');
if (esc>=10 && esc<100) {
oput(esc/10 + '0');
oput(esc%10 + '0');
} else
OUT "%d", esc PUT;
} else
OUT "H%d\n", hpos PUT;
esc = 0;
}
void ptpage(int n)
{
int i;
if (NROFF)
return;
ptlead();
vpos = 0;
if (ascii)
return;
OUT "p%d\n", n PUT;
for (i = 0; i <= nfonts; i++)
if (fontlab[i]) {
if (fonts[i].truename)
OUT "x font %d %s %s\n", i, fonts[i].longname, fonts[i].truename PUT;
else
OUT "x font %d %s\n", i, fonts[i].longname PUT;
}
ptps();
ptfont();
}
void pttrailer(void)
{
if (TROFF)
OUT "x trailer\n" PUT;
}
void ptstop(void)
{
if (TROFF)
OUT "x stop\n" PUT;
}
void t_ptpause(void)
{
if (ascii)
return;
ptlead();
vpos = 0;
pttrailer();
ptlead();
OUT "x pause\n" PUT;
flusho();
mpts = mfont = 0;
ptesc();
esc = po;
hpos = vpos = 0;
}