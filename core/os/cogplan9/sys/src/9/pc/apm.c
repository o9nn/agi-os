#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
extern int apmfarcall(ushort, ulong, Ureg*);
static int
getreg(ulong *reg, ISAConf *isa, char *name)
{
int i;
int nl;
nl = strlen(name);
for(i=0; i<isa->nopt; i++){
if(cistrncmp(isa->opt[i], name, nl)==0 && isa->opt[i][nl] == '='){
*reg = strtoul(isa->opt[i]+nl+1, nil, 16);
return 0;
}
}
return -1;
}
static void
setgdt(int sel, ulong base, ulong limit, int flag)
{
if(sel < 0 || sel >= NGDT)
panic("setgdt");
base = (ulong)KADDR(base);
m->gdt[sel].d0 = (base<<16) | (limit&0xFFFF);
m->gdt[sel].d1 = (base&0xFF000000) | (limit&0x000F0000) |
((base>>16)&0xFF) | SEGP | SEGPL(0) | flag;
}
static	ulong ax, cx, dx, di, ebx, esi;
static Ureg apmu;
static long
apmread(Chan*, void *a, long n, vlong off)
{
if(off < 0)
error("badarg");
if(n+off > sizeof apmu)
n = sizeof apmu - off;
if(n <= 0)
return 0;
memmove(a, (char*)&apmu+off, n);
return n;
}
static long
apmwrite(Chan*, void *a, long n, vlong off)
{
int s;
if(off || n != sizeof apmu)
error("write a Ureg");
memmove(&apmu, a, sizeof apmu);
s = splhi();
apmfarcall(APMCSEL, ebx, &apmu);
splx(s);
return n;
}
void
apmlink(void)
{
ISAConf isa;
char *s;
if(isaconfig("apm", 0, &isa) == 0)
return;
if(getreg(&ax, &isa, s="ax") < 0
|| getreg(&ebx, &isa, s="ebx") < 0
|| getreg(&cx, &isa, s="cx") < 0
|| getreg(&dx, &isa, s="dx") < 0
|| getreg(&esi, &isa, s="esi") < 0
|| getreg(&di, &isa, s="di") < 0){
print("apm: missing register %s\n", s);
return;
}
esi = 0xFFFFFFFF;
setgdt(APMCSEG, ax<<4, ((esi&0xFFFF)-1)&0xFFFF, SEGEXEC|SEGR|SEGD);
setgdt(APMCSEG16, cx<<4, ((esi>>16)-1)&0xFFFF, SEGEXEC|SEGR);
setgdt(APMDSEG, dx<<4, (di-1)&0xFFFF, SEGDATA|SEGW|SEGD);
addarchfile("apm", 0660, apmread, apmwrite);
print("apm0: configured cbase %.8lux off %.8lux\n", ax<<4, ebx);
return;
}