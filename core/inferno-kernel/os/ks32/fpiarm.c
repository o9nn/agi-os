#include "u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include "fpi.h"
#define	REG(x) (*(long*)(((char*)ur)+roff[(x)]))
#define	FPENV	(*ufp)
#define	FR(x) (*(Internal*)ufp->regs[(x)&7])
#define	getubyte(a) (*(uchar*)(a))
#define	getuword(a) (*(ushort*)(a))
#define	getulong(a) (*(ulong*)(a))
typedef struct FP2 FP2;
typedef struct FP1 FP1;
struct FP2 {
char*	name;
void	(*f)(Internal, Internal, Internal*);
};
struct FP1 {
char*	name;
void	(*f)(Internal*, Internal*);
};
enum {
N = 1<<31,
Z = 1<<30,
C = 1<<29,
V = 1<<28,
REGPC = 15,
};
int	fpemudebug = 0;
#undef OFR
#define	OFR(X)	((ulong)&((Ureg*)0)->X)
static	int	roff[] = {
OFR(r0), OFR(r1), OFR(r2), OFR(r3),
OFR(r4), OFR(r5), OFR(r6), OFR(r7),
OFR(r8), OFR(r9), OFR(r10), OFR(r11),
#ifdef R13OK
OFR(r12), OFR(r13), OFR(r14), OFR(pc),
#else
OFR(r12), OFR(type), OFR(r14), OFR(pc),
#endif
};
static Internal fpconst[8] = {
{0, 0x1, 0x00000000, 0x00000000},
{0, 0x3FF, 0x00000000, 0x08000000},
{0, 0x400, 0x00000000, 0x08000000},
{0, 0x400, 0x00000000, 0x0C000000},
{0, 0x401, 0x00000000, 0x08000000},
{0, 0x401, 0x00000000, 0x0A000000},
{0, 0x3FE, 0x00000000, 0x08000000},
{0, 0x402, 0x00000000, 0x0A000000},
};
static void
fadd(Internal m, Internal n, Internal *d)
{
(m.s == n.s? fpiadd: fpisub)(&m, &n, d);
}
static void
fsub(Internal m, Internal n, Internal *d)
{
m.s ^= 1;
(m.s == n.s? fpiadd: fpisub)(&m, &n, d);
}
static void
fsubr(Internal m, Internal n, Internal *d)
{
n.s ^= 1;
(n.s == m.s? fpiadd: fpisub)(&n, &m, d);
}
static void
fmul(Internal m, Internal n, Internal *d)
{
fpimul(&m, &n, d);
}
static void
fdiv(Internal m, Internal n, Internal *d)
{
fpidiv(&m, &n, d);
}
static void
fdivr(Internal m, Internal n, Internal *d)
{
fpidiv(&n, &m, d);
}
static void
fmov(Internal *m, Internal *d)
{
*d = *m;
}
static void
fmovn(Internal *m, Internal *d)
{
*d = *m;
d->s ^= 1;
}
static void
fabsf(Internal *m, Internal *d)
{
*d = *m;
d->s = 0;
}
static void
frnd(Internal *m, Internal *d)
{
short e;
(m->s? fsub: fadd)(fpconst[6], *m, d);
if(IsWeird(d))
return;
fpiround(d);
e = (d->e - ExpBias) + 1;
if(e <= 0)
SetZero(d);
else if(e > FractBits){
if(e < 2*FractBits)
d->l &= ~((1<<(2*FractBits - e))-1);
}else{
d->l = 0;
if(e < FractBits)
d->h &= ~((1<<(FractBits-e))-1);
}
}
static	FP1	optab1[16] = {
[0]	{"MOVF",	fmov},
[1]	{"NEGF",	fmovn},
[2]	{"ABSF",	fabsf},
[3]	{"RNDF",	frnd},
[4]	{"SQTF",	0},
};
static	FP2	optab2[16] = {
[0]	{"ADDF",	fadd},
[1]	{"MULF",	fmul},
[2]	{"SUBF",	fsub},
[3]	{"RSUBF",	fsubr},
[4]	{"DIVF",	fdiv},
[5]	{"RDIVF",	fdivr},
[8]	{"REMF",	0},
[9]	{"FMF",	fmul},
[10]	{"FDV",	fdiv},
[11]	{"FRD",	fdivr},
};
static ulong
fcmp(Internal *n, Internal *m)
{
int i;
if(IsWeird(m) || IsWeird(n)){
return V|C;
}
i = fpicmp(n, m);
if(i > 0)
return C;
else if(i == 0)
return C|Z;
else
return N;
}
static void
fld(void (*f)(Internal*, void*), int d, ulong ea, int n, FPenv *ufp)
{
void *mem;
mem = (void*)ea;
(*f)(&FR(d), mem);
if(fpemudebug)
print("MOV%c #%lux, F%d\n", n==8? 'D': 'F', ea, d);
}
static void
fst(void (*f)(void*, Internal*), ulong ea, int s, int n, FPenv *ufp)
{
Internal tmp;
void *mem;
mem = (void*)ea;
tmp = FR(s);
if(fpemudebug)
print("MOV%c	F%d,#%lux\n", n==8? 'D': 'F', s, ea);
(*f)(mem, &tmp);
}
static int
condok(int cc, int c)
{
switch(c){
case 0:
return cc&Z;
case 1:
return (cc&Z) == 0;
case 2:
return cc&C;
case 3:
return (cc&C) == 0;
case 4:
return cc&N;
case 5:
return (cc&N) == 0;
case 6:
return cc&V;
case 7:
return (cc&V) == 0;
case 8:
return cc&C && (cc&Z) == 0;
case 9:
return (cc&C) == 0 || cc&Z;
case 10:
return (~cc&(N|V))==0 || (cc&(N|V)) == 0;
case 11:
return (cc&(N|V))==N || (cc&(N|V))==V;
case 12:
return (cc&Z) == 0 && ((~cc&(N|V))==0 || (cc&(N|V))==0);
case 13:
return (cc&Z) || (cc&(N|V))==N || (cc&(N|V))==V;
case 14:
return 1;
case 15:
return 0;
}
return 0;
}
static void
unimp(ulong pc, ulong op)
{
char buf[60];
snprint(buf, sizeof(buf), "sys: fp: pc=%lux unimp fp 0x%.8lux", pc, op);
if(fpemudebug)
print("FPE: %s\n", buf);
error(buf);
}
static void
fpemu(ulong pc, ulong op, Ureg *ur, FPenv *ufp)
{
int rn, rd, tag, o;
long off;
ulong ea;
Internal tmp, *fm, *fn;
if(((op>>25)&7) == 6){
if(op & (1<<22))
unimp(pc, op);
rn = (op>>16)&0xF;
off = (op&0xFF)<<2;
if((op & (1<<23)) == 0)
off = -off;
ea = REG(rn);
if(rn == REGPC)
ea += 8;
if(op & (1<<24))
ea += off;
rd = (op>>12)&7;
if(op & (1<<20)){
if(op & (1<<15))
fld(fpid2i, rd, ea, 8, ufp);
else
fld(fpis2i, rd, ea, 4, ufp);
}else{
if(op & (1<<15))
fst(fpii2d, ea, rd, 8, ufp);
else
fst(fpii2s, ea, rd, 4, ufp);
}
if((op & (1<<24)) == 0)
ea += off;
if(op & (1<<21))
REG(rn) = ea;
return;
}
if(op & (1<<4)){
rd = (op>>12) & 0xF;
if(rd == 15 && op & (1<<20)){
rn = (op>>16)&7;
fn = &FR(rn);
if(op & (1<<3)){
fm = &fpconst[op&7];
tag = 'C';
}else{
fm = &FR(op&7);
tag = 'F';
}
switch((op>>21)&7){
default:
unimp(pc, op);
case 4:
case 6:
ur->psr &= ~(N|C|Z|V);
ur->psr |= fcmp(fn, fm);
break;
case 5:
case 7:
tmp = *fm;
tmp.s ^= 1;
ur->psr &= ~(N|C|Z|V);
ur->psr |= fcmp(fn, &tmp);
break;
}
if(fpemudebug)
print("CMPF	%c%d,F%ld =%x\n", tag, rn, op&7, ur->psr>>28);
return;
}
switch((op>>20)&0xF){
default:
unimp(pc, op);
case 0:
rn = (op>>16) & 7;
fpiw2i(&FR(rn), &REG(rd));
if(fpemudebug)
print("MOVW[FD]	R%d, F%d\n", rd, rn);
break;
case 1:
if(op & (1<<3))
unimp(pc, op);
rn = op & 7;
tmp = FR(rn);
fpii2w(&REG(rd), &tmp);
if(fpemudebug)
print("MOV[FD]W	F%d, R%d =%ld\n", rn, rd, REG(rd));
break;
case 2:
FPENV.status = REG(rd);
if(fpemudebug)
print("MOVW	R%d, FPSR\n", rd);
break;
case 3:
REG(rd) = FPENV.status;
if(fpemudebug)
print("MOVW	FPSR, R%d\n", rd);
break;
case 4:
FPENV.control = REG(rd);
if(fpemudebug)
print("MOVW	R%d, FPCR\n", rd);
break;
case 5:
REG(rd) = FPENV.control;
if(fpemudebug)
print("MOVW	FPCR, R%d\n", rd);
break;
}
return;
}
if(op & (1<<3)){
fm = &fpconst[op&7];
tag = 'C';
}else{
fm = &FR(op&7);
tag = 'F';
}
rd = (op>>12)&7;
o = (op>>20)&0xF;
if(op & (1<<15)){
FP1 *fp;
fp = &optab1[o];
if(fp->f == nil)
unimp(pc, op);
if(fpemudebug)
print("%s	%c%ld,F%d\n", fp->name, tag, op&7, rd);
(*fp->f)(fm, &FR(rd));
} else {
FP2 *fp;
fp = &optab2[o];
if(fp->f == nil)
unimp(pc, op);
rn = (op>>16)&7;
if(fpemudebug)
print("%s	%c%ld,F%d,F%d\n", fp->name, tag, op&7, rn, rd);
(*fp->f)(*fm, FR(rn), &FR(rd));
}
}
int
fpiarm(Ureg *ur)
{
ulong op, o;
FPenv *ufp;
int n;
#ifndef R13OK
ur->type = (ulong)(ur + 1);
#endif
if (up == nil)
panic("fpiarm not in a process");
ufp = &up->env->fpu;
if(ufp->fpistate != FPACTIVE) {
ufp->fpistate = FPACTIVE;
ufp->control = 0;
ufp->status = (0x01<<28)|(1<<12);
for(n = 0; n < 8; n++)
FR(n) = fpconst[0];
}
for(n=0;;n++){
op = getulong(ur->pc);
o = (op>>24) & 0xF;
if(((op>>8) & 0xF) != 1 || o != 0xE && (o&~1) != 0xC)
break;
if(condok(ur->psr, op>>28))
fpemu(ur->pc, op, ur, ufp);
ur->pc += 4;
if(anyhigher())
sched();
}
return n;
}