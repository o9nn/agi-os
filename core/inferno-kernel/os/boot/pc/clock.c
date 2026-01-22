#include	"u.h"
#include	"lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
enum
{
T0cntr=	0x40,
T1cntr=	0x41,
T2cntr=	0x42,
Tmode=	0x43,
Latch0=	0x00,
Load0=	0x30,
Square=	0x36,
Freq=	1193182,
};
static uvlong cpuhz = 66000000;
static int cpumhz = 66;
static int loopconst = 100;
int cpuidax, cpuiddx;
int havetsc;
extern void _cycles(uvlong*);
extern void wrmsr(int, vlong);
static void
clockintr(Ureg*, void*)
{
m->ticks++;
checkalarms();
}
#define STEPPING(x)	((x)&0xf)
#define X86MODEL(x)	(((x)>>4)&0xf)
#define X86FAMILY(x)	(((x)>>8)&0xf)
enum
{
CpuidFPU	= 0x001,
CpuidMCE	= 0x080,
CpuidCX8	= 0x100,
};
typedef struct
{
int family;
int model;
int aalcycles;
char *name;
} X86type;
X86type x86intel[] =
{
{ 4,	0,	22,	"486DX", },
{ 4,	1,	22,	"486DX50", },
{ 4,	2,	22,	"486SX", },
{ 4,	3,	22,	"486DX2", },
{ 4,	4,	22,	"486SL", },
{ 4,	5,	22,	"486SX2", },
{ 4,	7,	22,	"DX2WB", },
{ 4,	8,	22,	"DX4", },
{ 4,	9,	22,	"DX4WB", },
{ 5,	0,	23,	"P5", },
{ 5,	1,	23,	"P5", },
{ 5,	2,	23,	"P54C", },
{ 5,	3,	23,	"P24T", },
{ 5,	4,	23,	"P55C MMX", },
{ 5,	7,	23,	"P54C VRT", },
{ 6,	1,	16,	"PentiumPro", },
{ 6,	3,	16,	"PentiumII", },
{ 6,	5,	16,	"PentiumII/Xeon", },
{ 6,	6,	16,	"Celeron", },
{ 6,	7,	16,	"PentiumIII/Xeon", },
{ 6,	8,	16,	"PentiumIII/Xeon", },
{ 6,	0xB,	16,	"PentiumIII/Xeon", },
{ 0xF,	1,	16,	"P4", },
{ 0xF,	2,	16,	"PentiumIV/Xeon", },
{ 3,	-1,	32,	"386", },
{ 4,	-1,	22,	"486", },
{ 5,	-1,	23,	"P5", },
{ 6,	-1,	16,	"P6", },
{ 0xF,	-1,	16,	"P4", },
{ -1,	-1,	16,	"unknown", },
};
static X86type x86amd[] =
{
{ 5,	0,	23,	"AMD-K5", },
{ 5,	1,	23,	"AMD-K5", },
{ 5,	2,	23,	"AMD-K5", },
{ 5,	3,	23,	"AMD-K5", },
{ 5,	4,	23,	"AMD Geode GX1", },
{ 5,	5,	23,	"AMD Geode GX2", },
{ 5,	6,	11,	"AMD-K6", },
{ 5,	7,	11,	"AMD-K6", },
{ 5,	8,	11,	"AMD-K6-2", },
{ 5,	9,	11,	"AMD-K6-III", },
{ 5,	0xa,	23,	"AMD Geode LX", },
{ 6,	1,	11,	"AMD-Athlon", },
{ 6,	2,	11,	"AMD-Athlon", },
{ 4,	-1,	22,	"Am486", },
{ 5,	-1,	23,	"AMD-K5/K6", },
{ 6,	-1,	11,	"AMD-Athlon", },
{ 0xF,	-1,	11,	"AMD64", },
{ -1,	-1,	11,	"unknown", },
};
static X86type	*cputype;
void
delay(int millisecs)
{
millisecs *= loopconst;
if(millisecs <= 0)
millisecs = 1;
aamloop(millisecs);
}
void
microdelay(int microsecs)
{
microsecs *= loopconst;
microsecs /= 1000;
if(microsecs <= 0)
microsecs = 1;
aamloop(microsecs);
}
extern void cpuid(char*, int*, int*);
X86type*
cpuidentify(void)
{
int family, model;
X86type *t;
char cpuidid[16];
int cpuidax, cpuiddx;
cpuid(cpuidid, &cpuidax, &cpuiddx);
if(strncmp(cpuidid, "AuthenticAMD", 12) == 0 ||
strncmp(cpuidid, "Geode by NSC", 12) == 0)
t = x86amd;
else
t = x86intel;
family = X86FAMILY(cpuidax);
model = X86MODEL(cpuidax);
if (0)
print("cpuidentify: cpuidax 0x%ux cpuiddx 0x%ux\n",
cpuidax, cpuiddx);
while(t->name){
if((t->family == family && t->model == model)
|| (t->family == family && t->model == -1)
|| (t->family == -1))
break;
t++;
}
if(t->name == nil)
panic("cpuidentify");
if(cpuiddx & 0x10){
havetsc = 1;
if(cpuiddx & 0x20)
wrmsr(0x10, 0);
}
return t;
}
void
clockinit(void)
{
uvlong a, b, cpufreq;
int loops, incr, x, y;
X86type *t;
setvec(VectorCLOCK, clockintr, 0);
t = cpuidentify();
outb(Tmode, Load0|Square);
outb(T0cntr, (Freq/HZ));
outb(T0cntr, (Freq/HZ)>>8);
x = (Freq/HZ);
for(loops = 0; loops < 100000 && x >= (Freq/HZ); loops++){
outb(Tmode, Latch0);
x = inb(T0cntr);
x |= inb(T0cntr)<<8;
}
incr = 16000000/(t->aalcycles*HZ*2);
x = 2000;
for(loops = incr; loops < 64*1024; loops += incr) {
outb(Tmode, Latch0);
if(havetsc)
_cycles(&a);
x = inb(T0cntr);
x |= inb(T0cntr)<<8;
aamloop(loops);
outb(Tmode, Latch0);
if(havetsc)
_cycles(&b);
y = inb(T0cntr);
y |= inb(T0cntr)<<8;
x -= y;
if(x < 0)
x += Freq/HZ;
if(x > Freq/(3*HZ))
break;
}
cpufreq = (vlong)loops*((t->aalcycles*2*Freq)/x);
loopconst = (cpufreq/1000)/t->aalcycles;
if(havetsc){
b = (b-a)<<1;
b *= Freq;
b /= x;
cpumhz = (b+500000)/1000000L;
cpuhz = b;
}
else{
cpumhz = (cpufreq + cpufreq/200)/1000000;
cpuhz = cpufreq;
}
if(debug){
int timeo;
print("%dMHz %s loop %d\n", cpumhz, t->name, loopconst);
print("tick...");
for(timeo = 0; timeo < 10; timeo++)
delay(1000);
print("tock...\n");
}
}