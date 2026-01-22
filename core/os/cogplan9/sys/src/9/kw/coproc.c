#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "arm.h"
#define MAP2PCSPACE(va, pc) ((uintptr)(va) & ~KSEGM | (pc) & KSEGM)
enum {
Retinst	= 0xe1a0f00e,
Fpproc	= 10,
};
void
cpwr(int cp, int op1, int crn, int crm, int op2, ulong val)
{
int s;
volatile ulong instr[2];
void *pcaddr;
void (*fp)(ulong);
s = splhi();
op1 &= 7;
op2 &= 7;
crn &= 017;
crm &= 017;
cp &= 017;
instr[0] = 0xee000010 |
op1 << 21 | crn << 16 | cp << 8 | op2 << 5 | crm;
instr[1] = Retinst;
coherence();
pcaddr = (void *)MAP2PCSPACE(instr, getcallerpc(&cp));
cachedwbse(pcaddr, sizeof instr);
cacheiinv();
fp = (void (*)(ulong))pcaddr;
(*fp)(val);
coherence();
splx(s);
}
void
cpwrsc(int op1, int crn, int crm, int op2, ulong val)
{
cpwr(CpSC, op1, crn, crm, op2, val);
}
ulong
cprd(int cp, int op1, int crn, int crm, int op2)
{
int s;
ulong res;
volatile ulong instr[2];
void *pcaddr;
ulong (*fp)(void);
s = splhi();
op1 &= 7;
op2 &= 7;
crn &= 017;
crm &= 017;
instr[0] = 0xee100010 |
op1 << 21 | crn << 16 | cp << 8 | op2 << 5 | crm;
instr[1] = Retinst;
coherence();
pcaddr = (void *)MAP2PCSPACE(instr, getcallerpc(&cp));
cachedwbse(pcaddr, sizeof instr);
cacheiinv();
fp = (ulong (*)(void))pcaddr;
res = (*fp)();
splx(s);
return res;
}
ulong
cprdsc(int op1, int crn, int crm, int op2)
{
return cprd(CpSC, op1, crn, crm, op2);
}
ulong
fprd(int fpreg)
{
int s;
ulong res;
volatile ulong instr[2];
void *pcaddr;
ulong (*fp)(void);
s = splhi();
fpreg &= 017;
instr[0] = 0xeef00010 | fpreg << 16 | 0 << 12 | Fpproc << 8;
instr[1] = Retinst;
coherence();
pcaddr = (void *)MAP2PCSPACE(instr, getcallerpc(&fpreg));
cachedwbse(pcaddr, sizeof instr);
cacheiinv();
fp = (ulong (*)(void))pcaddr;
res = (*fp)();
splx(s);
return res;
}
void
fpwr(int fpreg, ulong val)
{
int s;
volatile ulong instr[2];
void *pcaddr;
void (*fp)(ulong);
s = splhi();
fpreg &= 017;
instr[0] = 0xeee00010 | fpreg << 16 | 0 << 12 | Fpproc << 8;
instr[1] = Retinst;
coherence();
pcaddr = (void *)MAP2PCSPACE(instr, getcallerpc(&fpreg));
cachedwbse(pcaddr, sizeof instr);
cacheiinv();
fp = (void (*)(ulong))pcaddr;
(*fp)(val);
coherence();
splx(s);
}