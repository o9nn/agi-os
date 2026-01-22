#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "arm.h"
enum {
Retinst = 0xe1a0f00e,
Opmask = MASK(3),
Regmask = MASK(4),
};
typedef ulong (*Pufv)(void);
typedef void (*Pvfu)(ulong);
static void
setupcpop(ulong instr[2], ulong opcode, int cp, int op1, int crn, int crm,
int op2)
{
ulong instrsz[2];
op1 &= Opmask;
op2 &= Opmask;
crn &= Regmask;
crm &= Regmask;
cp &= Regmask;
instr[0] = opcode | op1 << 21 | crn << 16 | cp << 8 | op2 << 5 | crm;
instr[1] = Retinst;
cachedwbse(instr, sizeof instrsz);
cacheiinv();
}
ulong
cprd(int cp, int op1, int crn, int crm, int op2)
{
int s, r;
volatile ulong instr[2];
Pufv fp;
s = splhi();
setupcpop(instr, 0xee100010, cp, op1, crn, crm, op2);
fp = (Pufv)instr;
r = fp();
splx(s);
return r;
}
void
cpwr(int cp, int op1, int crn, int crm, int op2, ulong val)
{
int s;
volatile ulong instr[2];
Pvfu fp;
s = splhi();
setupcpop(instr, 0xee000010, cp, op1, crn, crm, op2);
fp = (Pvfu)instr;
fp(val);
coherence();
splx(s);
}
ulong
cprdsc(int op1, int crn, int crm, int op2)
{
return cprd(CpSC, op1, crn, crm, op2);
}
void
cpwrsc(int op1, int crn, int crm, int op2, ulong val)
{
cpwr(CpSC, op1, crn, crm, op2, val);
}
static void
setupfpctlop(ulong instr[2], int opcode, int fpctlreg)
{
ulong instrsz[2];
fpctlreg &= Nfpctlregs - 1;
instr[0] = opcode | fpctlreg << 16 | 0 << 12 | CpFP << 8;
instr[1] = Retinst;
cachedwbse(instr, sizeof instrsz);
cacheiinv();
}
ulong
fprd(int fpreg)
{
int s, r;
volatile ulong instr[2];
Pufv fp;
if (!m->fpon) {
dumpstack();
panic("fprd: cpu%d fpu off", m->machno);
}
s = splhi();
setupfpctlop(instr, 0xeef00010, fpreg);
fp = (Pufv)instr;
r = fp();
splx(s);
return r;
}
void
fpwr(int fpreg, ulong val)
{
int s;
volatile ulong instr[2];
Pvfu fp;
s = splhi();
setupfpctlop(instr, 0xeee00010, fpreg);
fp = (Pvfu)instr;
fp(val);
coherence();
splx(s);
}
static void
setupfpop(ulong instr[2], int opcode, int fpreg)
{
ulong instrsz[2];
instr[0] = opcode | 0 << 16 | (fpreg & (16 - 1)) << 12;
if (fpreg >= 16)
instr[0] |= 1 << 22;
instr[1] = Retinst;
cachedwbse(instr, sizeof instrsz);
cacheiinv();
}
ulong
fpsavereg(int fpreg, uvlong *fpp)
{
int s, r;
volatile ulong instr[2];
ulong (*fp)(uvlong *);
if (!m->fpon)
panic("fpsavereg: cpu%d fpu off", m->machno);
s = splhi();
setupfpop(instr, 0xed000000 | CpDFP << 8, fpreg);
fp = (ulong (*)(uvlong *))instr;
r = fp(fpp);
splx(s);
coherence();
return r;
}
void
fprestreg(int fpreg, uvlong val)
{
int s;
volatile ulong instr[2];
void (*fp)(uvlong *);
if (!m->fpon)
panic("fprestreg: cpu%d fpu off", m->machno);
s = splhi();
setupfpop(instr, 0xed100000 | CpDFP << 8, fpreg);
fp = (void (*)(uvlong *))instr;
fp(&val);
coherence();
splx(s);
}