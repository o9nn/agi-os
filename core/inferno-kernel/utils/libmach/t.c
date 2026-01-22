#include <lib9.h>
#include <bio.h>
#include "uregt.h"
#include "mach.h"
#define REGOFF(x) (ulong) (&((struct Ureg *) 0)->x)
#define SP REGOFF(r13)
#define PC REGOFF(pc)
#define REGSIZE sizeof(struct Ureg)
Reglist thumbreglist[] =
{
{"LINK", REGOFF(link), RINT|RRDONLY, 'X'},
{"TYPE", REGOFF(type), RINT|RRDONLY, 'X'},
{"PSR", REGOFF(psr), RINT|RRDONLY, 'X'},
{"PC", PC, RINT, 'X'},
{"SP", SP, RINT, 'X'},
{"R15", PC, RINT, 'X'},
{"R14", REGOFF(r14), RINT, 'X'},
{"R13", REGOFF(r13), RINT, 'X'},
{"R12", REGOFF(r12), RINT, 'X'},
{"R11", REGOFF(r11), RINT, 'X'},
{"R10", REGOFF(r10), RINT, 'X'},
{"R9", REGOFF(r9), RINT, 'X'},
{"R8", REGOFF(r8), RINT, 'X'},
{"R7", REGOFF(r7), RINT, 'X'},
{"R6", REGOFF(r6), RINT, 'X'},
{"R5", REGOFF(r5), RINT, 'X'},
{"R4", REGOFF(r4), RINT, 'X'},
{"R3", REGOFF(r3), RINT, 'X'},
{"R2", REGOFF(r2), RINT, 'X'},
{"R1", REGOFF(r1), RINT, 'X'},
{"R0", REGOFF(r0), RINT, 'X'},
{ 0 }
};
Mach mthumb =
{
"thumb",
MARM,
thumbreglist,
REGSIZE,
0,
"PC",
"SP",
"R15",
"setR12",
0,
0x1000,
0x80000000,
0,
0x7FFFFFFF,
2,
4,
4,
4,
8,
};
typedef struct pcentry pcentry;
struct pcentry{
long start;
long stop;
};
static pcentry *pctab;
static int npctab;
void
thumbpctab(Biobuf *b, Fhdr *fp)
{
int n, o, ta;
uchar c[8];
pcentry *tab;
Bseek(b, fp->lnpcoff+fp->lnpcsz, 0);
o = (int)Boffset(b);
Bseek(b, 0, 2);
n = (int)Boffset(b)-o;
pctab = (pcentry*)malloc(n);
if(pctab == 0)
return;
ta = fp->txtaddr;
tab = pctab;
Bseek(b, fp->lnpcoff+fp->lnpcsz, 0);
while(Bread(b, c, sizeof(c)) == sizeof(c)){
tab->start = ta + (c[0]<<24)|(c[1]<<16)|(c[2]<<8)|c[3];
tab->stop = ta + (c[4]<<24)|(c[5]<<16)|(c[6]<<8)|c[7];
tab++;
}
npctab = n/sizeof(c);
}
int
thumbpclookup(uvlong pc)
{
uvlong l, u, m;
pcentry *tab = pctab;
l = 0;
u = npctab-1;
while(l < u){
m = (l+u)/2;
if(pc < tab[m].start)
u = m-1;
else if(pc > tab[m].stop)
l = m+1;
else
l = u = m;
}
if(l == u && u < npctab && tab[u].start <= pc && pc <= tab[u].stop)
return 1;
return 0;
}