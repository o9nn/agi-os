#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "ureg.h"
#include "../port/fpi.h"
#include <tos.h>
#ifdef FPEMUDEBUG
#define DBG(bits) (fpemudebug & (bits))
#define intpr _intpr
#define internsane _internsane
#define dbgstuck _dbgstuck
#else
#define DBG(bits) (0)
#define internsane(i, ur) do { USED(ur); } while(0)
#define intpr(i, reg, fmt, ufp) do {} while(0)
#define dbgstuck(pc, ur, ufp) do {} while(0)
#endif
#define OFR(memb) (uintptr)&((Ureg*)0)->memb
#define REG(ur, r) *acpureg(ur, r)
#define FREG(ufp, fr) (ufp)->reg[(fr) & REGMASK]
#define OP(ul) ((ul) >> 26)
#define REGMASK MASK(5)
#define FMT(ul) (((ul) >> 21) & REGMASK)
#define REGT(ul) (((ul) >> 16) & REGMASK)
#define REGS(ul) (((ul) >> 11) & REGMASK)
#define REGD(ul) (((ul) >> 6) & REGMASK)
#define FUNC(ul) ((ul) & MASK(6))
enum {
Dbgbasic = 1<<0,
Dbgmoves = 1<<1,
Dbgregs = 1<<2,
Dbgdelay = 1<<3,
Failed = -1,
Advpc,
Leavepc,
Leavepcret,
Nomatch,
NOP = 0x27,
MIPSNOP = 0,
COP1 = 0x11,
LWC1 = 0x31,
LDC1 = 0x35,
SWC1 = 0x39,
SDC1 = 0x3d,
N = 1<<31,
Z = 1<<30,
C = 1<<29,
V = 1<<28,
MFC1 = 0,
DMFC1,
CFC1,
MTC1 = 4,
DMTC1,
CTC1,
BRANCH = 8,
Ffloat = 16,
Fdouble,
Flong = 20,
Fvlong,
Fpimp = 0,
Fpcsr = 31,
};
typedef struct FP1 FP1;
typedef struct FP2 FP2;
typedef struct FPcvt FPcvt;
typedef struct Instr Instr;
struct Instr {
int iw;
uintptr pc;
int o;
int fmt;
int rm;
int rn;
int rd;
Internal *fm;
Internal *fn;
char *dfmt;
FPsave *ufp;
Ureg *ur;
};
struct FP2 {
char* name;
void (*f)(Internal*, Internal*, Internal*);
};
struct FP1 {
char* name;
void (*f)(Internal*, Internal*);
};
struct FPcvt {
char* name;
void (*f)(int, int, int, Ureg *, FPsave *);
};
static int roff[32] = {
0, OFR(r1), OFR(r2), OFR(r3),
OFR(r4), OFR(r5), OFR(r6), OFR(r7),
OFR(r8), OFR(r9), OFR(r10), OFR(r11),
OFR(r12), OFR(r13), OFR(r14), OFR(r15),
OFR(r16), OFR(r17), OFR(r18), OFR(r19),
OFR(r20), OFR(r21), OFR(r22), OFR(r23),
OFR(r24), OFR(r25), OFR(r26), OFR(r27),
OFR(r28), OFR(sp), OFR(r30), OFR(r31),
};
enum {
FZERO = 24,
FHALF = 26,
};
static Internal fpconst[Nfpregs] = {
[FZERO] {0, 0x1, 0x00000000, 0x00000000},
[FHALF] {0, 0x3FE, 0x00000000, 0x08000000},
[28] {0, 0x3FF, 0x00000000, 0x08000000},
[30] {0, 0x400, 0x00000000, 0x08000000},
};
static char *fmtnames[] = {
[MFC1] "MF",
[DMFC1] "DMF",
[CFC1] "CF",
[MTC1] "MT",
[DMTC1] "DMT",
[CTC1] "CT",
[BRANCH]"BR",
[Ffloat]"F",
[Fdouble]"D",
[Flong] "W",
[Fvlong]"L",
};
static char *prednames[] = {
[0] "F",
[1] "UN",
[2] "EQ",
[3] "UEQ",
[4] "OLT",
[5] "ULT",
[6] "OLE",
[7] "ULE",
[8] "SF",
[9] "NGLE",
[10] "SEQ",
[11] "NGL",
[12] "LT",
[13] "NGE",
[14] "LE",
[15] "NGT",
};
int fpemudebug = 0;
static ulong dummyr0;
static QLock watchlock;
ulong branch(Ureg*, ulong);
int isbranch(ulong *);
static int fpimips(ulong, ulong, Ureg *, FPsave *);
char *
fpemuprint(char *p, char *ep)
{
#ifdef FPEMUDEBUG
return seprint(p, ep, "fpemudebug %d\n", fpemudebug);
#else
USED(ep);
return p;
#endif
}
static ulong *
acpureg(Ureg *ur, int r)
{
r &= REGMASK;
if (r == 0 || roff[r] == 0) {
dummyr0 = 0;
return &dummyr0;
}
return (ulong *)((char*)ur + roff[r]);
}
ulong *
reg(Ureg *ur, int r)
{
return &REG(ur, r);
}
static void
_internsane(Internal *i, Ureg *ur)
{
static char buf[ERRMAX];
USED(i);
if (!(DBG(Dbgbasic)))
return;
if ((unsigned)i->s > 1) {
snprint(buf, sizeof buf,
"fpuemu: bogus Internal sign at pc=%#p", ur->pc);
error(buf);
}
if ((unsigned)i->e > DoubleExpMax) {
snprint(buf, sizeof buf,
"fpuemu: bogus Internal exponent at pc=%#p", ur->pc);
error(buf);
}
}
static void
fadd(Internal *m, Internal *n, Internal *d)
{
(m->s == n->s? fpiadd: fpisub)(m, n, d);
}
static void
fsub(Internal *m, Internal *n, Internal *d)
{
m->s ^= 1;
(m->s == n->s? fpiadd: fpisub)(m, n, d);
}
static void
frnd(Internal *m, Internal *d)
{
short e;
Internal tmp;
tmp = fpconst[FHALF];
(m->s? fsub: fadd)(&tmp, m, d);
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
static void
_intpr(Internal *i, int reg, int fmt, FPsave *ufp)
{
USED(i);
if (!(DBG(Dbgregs)))
return;
if (fmt == Fdouble && reg < 31)
iprint("\tD%02d: l %08lux h %08lux =\ts %d e %04d h %08lux l %08lux\n",
reg, FREG(ufp, reg), FREG(ufp, reg+1),
i->s, i->e, i->h, i->l);
else
iprint("\tF%02d: %08lux =\ts %d e %04d h %08lux l %08lux\n",
reg, FREG(ufp, reg),
i->s, i->e, i->h, i->l);
delay(75);
}
static void
dreg2dbl(Double *dp, int reg, FPsave *ufp)
{
reg &= ~1;
dp->l = FREG(ufp, reg);
dp->h = FREG(ufp, reg+1);
}
static void
dbl2dreg(int reg, Double *dp, FPsave *ufp)
{
reg &= ~1;
FREG(ufp, reg) = dp->l;
FREG(ufp, reg+1) = dp->h;
}
static void
vreg2dbl(Double *dp, int reg, FPsave *ufp)
{
reg &= ~1;
dp->l = FREG(ufp, reg+1);
dp->h = FREG(ufp, reg);
}
static void
dbl2vreg(int reg, Double *dp, FPsave *ufp)
{
reg &= ~1;
FREG(ufp, reg+1) = dp->l;
FREG(ufp, reg) = dp->h;
}
static void
fcvtd(int fmt, int rm, int rd, Ureg *ur, FPsave *ufp)
{
Double d;
Internal intrn;
switch (fmt) {
case Ffloat:
fpis2i(&intrn, &FREG(ufp, rm));
internsane(&intrn, ur);
fpii2d(&d, &intrn);
break;
case Fdouble:
dreg2dbl(&d, rm, ufp);
break;
case Flong:
fpiw2i(&intrn, &FREG(ufp, rm));
internsane(&intrn, ur);
fpii2d(&d, &intrn);
break;
case Fvlong:
vreg2dbl(&d, rm, ufp);
fpiv2i(&intrn, &d);
internsane(&intrn, ur);
fpii2d(&d, &intrn);
break;
}
dbl2dreg(rd, &d, ufp);
if (fmt != Fdouble && DBG(Dbgregs))
intpr(&intrn, rm, Fdouble, ufp);
}
static void
fcvts(int fmt, int rm, int rd, Ureg *ur, FPsave *ufp)
{
Double d;
Internal intrn;
switch (fmt) {
case Ffloat:
FREG(ufp, rd) = FREG(ufp, rm);
break;
case Fdouble:
dreg2dbl(&d, rm, ufp);
fpid2i(&intrn, &d);
break;
case Flong:
fpiw2i(&intrn, &FREG(ufp, rm));
break;
case Fvlong:
vreg2dbl(&d, rm, ufp);
fpiv2i(&intrn, &d);
break;
}
if (fmt != Ffloat) {
if(DBG(Dbgregs))
intpr(&intrn, rm, Ffloat, ufp);
internsane(&intrn, ur);
fpii2s(&FREG(ufp, rd), &intrn);
}
}
static void
fcvtw(int fmt, int rm, int rd, Ureg *ur, FPsave *ufp)
{
Double d;
Internal intrn;
switch (fmt) {
case Ffloat:
fpis2i(&intrn, &FREG(ufp, rm));
break;
case Fdouble:
dreg2dbl(&d, rm, ufp);
fpid2i(&intrn, &d);
break;
case Flong:
FREG(ufp, rd) = FREG(ufp, rm);
break;
case Fvlong:
vreg2dbl(&d, rm, ufp);
fpiv2i(&intrn, &d);
break;
}
if (fmt != Flong) {
if(DBG(Dbgregs))
intpr(&intrn, rm, Flong, ufp);
internsane(&intrn, ur);
fpii2w((long *)&FREG(ufp, rd), &intrn);
}
}
static void
fcvtv(int fmt, int rm, int rd, Ureg *ur, FPsave *ufp)
{
Double d;
Internal intrn;
switch (fmt) {
case Ffloat:
fpis2i(&intrn, &FREG(ufp, rm));
break;
case Fdouble:
dreg2dbl(&d, rm, ufp);
fpid2i(&intrn, &d);
break;
case Flong:
fpiw2i(&intrn, &FREG(ufp, rm));
break;
case Fvlong:
vreg2dbl(&d, rm, ufp);
dbl2vreg(rd, &d, ufp);
break;
}
if (fmt != Fvlong) {
if(DBG(Dbgregs))
intpr(&intrn, rm, Fvlong, ufp);
internsane(&intrn, ur);
fpii2v((vlong *)&FREG(ufp, rd), &intrn);
}
}
static FP2 optab2[] = {
[0] {"ADDF", fadd},
[1] {"SUBF", fsub},
[2] {"MULF", fpimul},
[3] {"DIVF", fpidiv},
};
static FP1 optab1[32] = {
[4] {"SQTF", 0},
[5] {"ABSF", 0},
[6] {"MOVF", 0},
[7] {"NEGF", 0},
[8] {"ROUND.L", 0},
[9] {"TRUNC.L", 0},
[10] {"CEIL.L", 0},
[11] {"FLOOR.L", 0},
[12] {"ROUND.W", frnd},
[13] {"TRUNC.W", 0},
[14] {"CEIL.W", 0},
[15] {"FLOOR.W", 0},
};
static FPcvt optabcvt[] = {
[32] {"CVT.S", fcvts},
[33] {"CVT.D", fcvtd},
[36] {"CVT.W", fcvtw},
[37] {"CVT.L", fcvtv},
};
static void
fld(int d, ulong ea, int n, FPsave *ufp)
{
if(DBG(Dbgmoves))
iprint("MOV%c #%lux, F%d\n", n==8? 'D': 'F', ea, d);
if (n == 4)
memmove(&FREG(ufp, d), (void *)ea, 4);
else if (n == 8){
d &= ~1;
memmove(&FREG(ufp, d), (void *)(ea+4), 4);
memmove(&FREG(ufp, d+1), (void *)ea, 4);
} else
panic("fld: n (%d) not 4 nor 8", n);
}
static void
fst(ulong ea, int s, int n, FPsave *ufp)
{
if(DBG(Dbgmoves))
iprint("MOV%c	F%d,#%lux\n", n==8? 'D': 'F', s, ea);
if (n == 4)
memmove((void *)ea, &FREG(ufp, s), 4);
else if (n == 8){
s &= ~1;
memmove((void *)(ea+4), &FREG(ufp, s), 4);
memmove((void *)ea, &FREG(ufp, s+1), 4);
} else
panic("fst: n (%d) not 4 nor 8", n);
}
void
unimp(ulong pc, ulong op, char *msg)
{
char buf[120];
snprint(buf, sizeof(buf), "sys: fp: pc=%#lux unimp fp %#.8lux: %s",
pc, op, msg);
if(DBG(Dbgbasic))
iprint("FPE: %s\n", buf);
error(buf);
}
static int
isfpop(ulong iw)
{
switch (OP(iw)) {
case COP1:
case LWC1:
case LDC1:
case SWC1:
case SDC1:
return 1;
default:
return 0;
}
}
static int
ldst(ulong op, Ureg *ur, FPsave *ufp)
{
int rn, rd, o, size, wr;
short off;
ulong ea;
o = OP(op);
rn = FMT(op);
off = op;
ea = REG(ur, rn) + off;
rd = REGT(op);
size = 4;
if (o == LDC1 || o == SDC1)
size = 8;
wr = (o == SWC1 || o == SDC1);
validaddr(ea, size, wr);
switch (o) {
case LWC1:
case LDC1:
fld(rd, ea, size, ufp);
break;
case SWC1:
case SDC1:
fst(ea, rd, size, ufp);
break;
default:
unimp(ur->pc, op, "unknown non-COP1 load or store");
return Failed;
}
return Advpc;
}
static int
cop1mov(Instr *ip)
{
int fs, rt;
uvlong vl;
FPsave *ufp;
Ureg *ur;
fs = ip->rm;
rt = ip->rn;
ur = ip->ur;
ufp = ip->ufp;
switch (ip->fmt) {
case MTC1:
fld(fs, (uintptr)&REG(ur, rt), 4, ufp);
return Advpc;
case DMTC1:
iprint("fpemu: 64-bit DMTC1 may have words backward\n");
rt &= ~1;
vl = (uvlong)REG(ur, rt+1) << 32 | REG(ur, rt);
fld(fs & ~1, (uintptr)&vl, 8, ufp);
return Advpc;
case MFC1:
fst((uintptr)&REG(ur, rt), fs, 4, ufp);
return Advpc;
case DMFC1:
iprint("fpemu: 64-bit DMFC1 may have words backward\n");
fst((uintptr)&vl, fs & ~1, 8, ufp);
rt &= ~1;
REG(ur, rt) = (ulong)vl;
REG(ur, rt+1) = vl>>32;
return Advpc;
case CFC1:
switch (fs) {
case Fpimp:
REG(ur, rt) = 0x500;
break;
case Fpcsr:
REG(ur, rt) = ufp->fpcontrol;
break;
}
if(DBG(Dbgbasic))
iprint("MOVW	FCR%d, R%d\n", fs, rt);
return Advpc;
case CTC1:
switch (fs) {
case Fpcsr:
ufp->fpcontrol = REG(ur, rt);
break;
}
if(DBG(Dbgbasic))
iprint("MOVW	R%d, FCR%d\n", rt, fs);
return Advpc;
}
return Nomatch;
}
static char *
decodefmt(int fmt)
{
if (fmtnames[fmt])
return fmtnames[fmt];
else
return "GOK";
}
static char *
predname(int pred)
{
if (prednames[pred])
return prednames[pred];
else
return "GOK";
}
static int
fcmpf(Internal m, Internal n, int, int cond)
{
int i;
if(IsWeird(&m) || IsWeird(&n)){
return 0;
}
fpiround(&n);
fpiround(&m);
i = fpicmp(&m, &n);
switch (cond) {
case 0:
case 1:
return 0;
case 2:
case 3:
return i == 0;
case 4:
case 5:
return i < 0;
case 6:
case 7:
return i <= 0;
case 8:
case 9:
return 0;
case 10:
return i == 0;
case 11:
return i != 0;
case 12:
case 13:
return i < 0;
case 14:
case 15:
return i <= 0;
}
return 0;
}
static uintptr
followbr(Ureg *ur)
{
uintptr npc;
npc = branch(ur, up->fpsave.fpstatus);
if(npc == 0)
panic("fpemu: branch expected but not seen at %#p", ur->pc);
ur->pc = npc;
return npc;
}
static void
dsemu(Instr *ip, ulong dsinsn, Ureg *ur, FPsave *ufp)
{
uintptr npc;
npc = ur->pc;
if(DBG(Dbgdelay))
iprint(">>> emulating br delay slot\n");
fpimips(ip->pc + 4, dsinsn, ur, ufp);
if(DBG(Dbgdelay))
iprint("<<< done emulating br delay slot\n");
ur->pc = npc;
}
static void
dsexec(Instr *ip, Ureg *ur, FPsave *ufp)
{
ulong dsaddr, wpaddr;
Tos *tos;
dsaddr = ip->pc + 4;
tos = (Tos*)(USTKTOP-sizeof(Tos));
tos->kscr[0] = *(ulong *)dsaddr;
tos->kscr[1] = 0xc0;
tos->kscr[2] = 0xc0;
tos->kscr[3] = 0xc0;
dcflush(tos->kscr, sizeof tos->kscr);
icflush(tos->kscr, sizeof tos->kscr);
wpaddr = (ulong)&tos->kscr[2] & ~7;
ufp->fpdelayexec = 1;
ufp->fpdelaypc = ip->pc;
ufp->fpdelaysts = ufp->fpstatus;
ur->pc = (ulong)tos->kscr;
qlock(&watchlock);
setwatchlo0(wpaddr | 1<<2);
setwatchhi0(TLBPID(tlbvirt())<<16);
if (DBG(Dbgdelay))
iprint("fpemu: set %s watch point at %#lux, after br ds %#lux...",
up->text, wpaddr, *(ulong *)dsaddr);
}
void
fpwatch(Ureg *ur)
{
FPsave *ufp;
ufp = &up->fpsave;
if(ufp->fpdelayexec == 0)
panic("fpwatch: unexpected watch trap");
ufp->fpdelayexec = 0;
setwatchlo0(0);
setwatchhi0(0);
qunlock(&watchlock);
ur->pc = ufp->fpdelaypc;
ur->cause &= BD;
ufp->fpstatus = ufp->fpdelaysts;
followbr(ur);
if (DBG(Dbgdelay))
iprint("delay slot executed; resuming at %#lux\n", ur->pc);
}
static ulong
validiw(uintptr pc)
{
validaddr(pc, 4, 0);
return *(ulong*)pc;
}
static int
bremu(Instr *ip)
{
int off, taken;
ulong dsinsn;
FPsave *ufp;
Ureg *ur;
if (ip->iw & (1<<17))
error("fpuemu: `likely' fp branch (obs)");
ufp = ip->ufp;
if (ufp->fpstatus & FPCOND)
taken = ip->iw & (1<<16);
else
taken = !(ip->iw & (1<<16));
dsinsn = validiw(ip->pc + 4);
if(DBG(Dbgdelay)){
off = (short)(ip->iw & MASK(16));
iprint("BFP%c\t%d(PC): %staken\n", (ip->iw & (1<<16)? 'T': 'F'),
off, taken? "": "not ");
iprint("\tdelay slot: %08lux\n", dsinsn);
delay(75);
}
ur = ip->ur;
assert(ur->pc == ip->pc);
if(!taken)
return Advpc;
if(dsinsn == NOP || dsinsn == MIPSNOP){
;
}else if(isbranch((ulong *)(ip->pc + 4)))
error("fpuemu: branch in fp branch delay slot");
else if (isfpop(dsinsn))
dsemu(ip, dsinsn, ur, ufp);
else{
dsexec(ip, ur, ufp);
return Leavepcret;
}
followbr(ur);
return Leavepc;
}
static void
reg2intern(Internal *i, int reg, int fmt, Ureg *ur)
{
Double d;
FPsave *ufp;
ufp = &up->fpsave;
switch (fmt) {
case Ffloat:
fpis2i(i, &FREG(ufp, reg));
internsane(i, ur);
break;
case Fdouble:
dreg2dbl(&d, reg, ufp);
fpid2i(i, &d);
internsane(i, ur);
break;
default:
SetQNaN(i);
break;
}
}
static void
intern2reg(int reg, Internal *i, int fmt, Ureg *ur)
{
Double d;
FPsave *ufp;
Internal tmp;
ufp = &up->fpsave;
tmp = *i;
internsane(&tmp, ur);
switch (fmt) {
case Ffloat:
fpii2s(&FREG(ufp, reg), &tmp);
break;
case Fdouble:
fpii2d(&d, &tmp);
dbl2dreg(reg, &d, ufp);
break;
default:
panic("intern2reg: bad fmt %d", fmt);
}
}
static int
cmpemu(Instr *ip)
{
int cc, cond;
cc = ip->rd >> 2;
cond = ip->o & MASK(4);
reg2intern(ip->fn, ip->rn, ip->fmt, ip->ur);
if (fcmpf(*ip->fm, *ip->fn, cc, cond))
ip->ufp->fpstatus |= FPCOND;
else
ip->ufp->fpstatus &= ~FPCOND;
if(DBG(Dbgbasic))
iprint("CMP%s.%s	F%d,F%d =%d\n", predname(cond), ip->dfmt,
ip->rm, ip->rn, (ip->ufp->fpstatus & FPCOND? 1: 0));
if(DBG(Dbgregs)) {
intpr(ip->fm, ip->rm, ip->fmt, ip->ufp);
intpr(ip->fn, ip->rn, ip->fmt, ip->ufp);
delay(75);
}
return Advpc;
}
static int
binemu(Instr *ip)
{
FP2 *fp;
Internal fd, prfd;
Internal *fn;
fp = &optab2[ip->o];
if(fp->f == nil)
unimp(ip->pc, ip->iw, "missing binary op");
fn = ip->fn;
reg2intern(fn, ip->rn, ip->fmt, ip->ur);
if(DBG(Dbgregs))
intpr(fn, ip->rn, ip->fmt, ip->ufp);
if(DBG(Dbgbasic)){
iprint("%s.%s\tF%d,F%d,F%d\n", fp->name, ip->dfmt,
ip->rm, ip->rn, ip->rd);
delay(75);
}
(*fp->f)(fn, ip->fm, &fd);
if(DBG(Dbgregs))
prfd = fd;
intern2reg(ip->rd, &fd, ip->fmt, ip->ur);
if(DBG(Dbgregs))
intpr(&prfd, ip->rd, ip->fmt, ip->ufp);
return Advpc;
}
static int
unaryemu(Instr *ip)
{
int o;
FP1 *fp;
FPsave *ufp;
o = ip->o;
fp = &optab1[o];
if(DBG(Dbgbasic)){
iprint("%s.%s\tF%d,F%d\n", fp->name, ip->dfmt, ip->rm, ip->rd);
delay(75);
}
if(o == 6){
int rm, rd;
ufp = ip->ufp;
rd = ip->rd;
rm = ip->rm;
if(ip->fmt == Fdouble){
rd &= ~1;
rm &= ~1;
FREG(ufp, rd+1) = FREG(ufp, rm+1);
}
FREG(ufp, rd) = FREG(ufp, rm);
}else{
Internal fdint, prfd;
Internal *fd;
switch(o){
case 5:
fd = ip->fm;
fd->s = 0;
break;
case 7:
fd = ip->fm;
fd->s ^= 1;
break;
default:
if(fp->f == nil)
unimp(ip->pc, ip->iw, "missing unary op");
fd = &fdint;
(*fp->f)(ip->fm, fd);
break;
}
if(DBG(Dbgregs))
prfd = *fd;
intern2reg(ip->rd, fd, ip->fmt, ip->ur);
if(DBG(Dbgregs))
intpr(&prfd, ip->rd, ip->fmt, ip->ufp);
}
return Advpc;
}
static int
cvtemu(Instr *ip)
{
FPcvt *fp;
fp = &optabcvt[ip->o];
if(fp->f == nil)
unimp(ip->pc, ip->iw, "missing conversion op");
if(DBG(Dbgbasic)){
iprint("%s.%s\tF%d,F%d\n", fp->name, ip->dfmt, ip->rm, ip->rd);
delay(75);
}
(*fp->f)(ip->fmt, ip->rm, ip->rd, ip->ur, ip->ufp);
return Advpc;
}
static void
cop1decode(Instr *ip, ulong iw, ulong pc, Ureg *ur, FPsave *ufp,
Internal *imp, Internal *inp)
{
ip->iw = iw;
ip->pc = pc;
ip->ur = ur;
ip->ufp = ufp;
ip->fmt = FMT(iw);
ip->rm = REGS(iw);
ip->rn = REGT(iw);
ip->rd = REGD(iw);
ip->o = FUNC(iw);
ip->fm = imp;
ip->fn = inp;
if (DBG(Dbgbasic))
ip->dfmt = decodefmt(ip->fmt);
}
void
fpstuck(uintptr pc, FPsave *fp)
{
USED(pc);
if(!(DBG(Dbgbasic)))
return;
if (fp->fppc == pc) {
fp->fpcnt++;
if (fp->fpcnt > 4)
panic("fpuemu: cpu%d stuck at pid %ld %s pc %#p "
"instr %#8.8lux", m->machno, up->pid, up->text,
pc, *(ulong *)pc);
} else {
fp->fppc = pc;
fp->fpcnt = 0;
}
}
static void
_dbgstuck(ulong pc, Ureg *ur, FPsave *ufp)
{
fpstuck(pc, ufp);
if (DBG(Dbgdelay) && ur->cause & BD)
iprint("fpuemu: FP in a branch delay slot\n");
}
static int
fpimips(ulong pc, ulong op, Ureg *ur, FPsave *ufp)
{
int r, o;
Instr insn;
Instr *ip;
Internal im, in;
dummyr0 = 0;
switch (OP(op)) {
case LWC1:
case LDC1:
case SWC1:
case SDC1:
dbgstuck(pc, ur, ufp);
return ldst(op, ur, ufp);
default:
unimp(pc, op, "non-FP instruction");
return Failed;
case COP1:
dbgstuck(pc, ur, ufp);
break;
}
ip = &insn;
cop1decode(ip, op, pc, ur, ufp, &im, &in);
if (ip->fmt == BRANCH) {
r = bremu(ip);
if(DBG(Dbgdelay)){
iprint("resuming after br, at %#lux", ur->pc);
if (r == Leavepcret)
iprint("...");
else
iprint("\n");
}
return r;
}
o = ip->o;
if (o == 0 && ip->rd == 0) {
r = cop1mov(ip);
if (r != Nomatch)
return r;
}
if(o >= 32 && o < 40)
return cvtemu(ip);
reg2intern(ip->fm, ip->rm, ip->fmt, ip->ur);
if(DBG(Dbgregs))
intpr(&im, ip->rm, ip->fmt, ip->ufp);
if(o >= 4 && o < 32)
return unaryemu(ip);
if(o < 4)
return binemu(ip);
if(o >= 48 && (ip->rd & MASK(2)) == 0)
return cmpemu(ip);
if(DBG(Dbgbasic))
iprint("fp at %#lux: %#8.8lux BOGON\n", pc, op);
unimp(pc, op, "unknown opcode");
return Failed;
}
static FPsave *
fpinit(Ureg *ur)
{
int i, n;
Double d;
FPsave *ufp;
Internal tmp;
ufp = &up->fpsave;
switch(up->fpstate){
case FPactive:
case FPinactive:
error("fpu (in)active but fp is emulated");
case FPinit:
up->fpstate = FPemu;
ufp->fpcontrol = 0;
ufp->fpstatus = 0;
ufp->fpcnt = 0;
ufp->fppc = 0;
for(n = 0; n < Nfpregs-1; n += 2) {
if (fpconst[n].h == 0)
i = FZERO;
else
i = n;
tmp = fpconst[i];
internsane(&tmp, ur);
fpii2d(&d, &tmp);
dbl2dreg(n, &d, ufp);
}
break;
}
return ufp;
}
int
fpuemu(Ureg *ureg)
{
int s;
uintptr pc;
ulong iw, r;
if(waserror()){
postnote(up, 1, up->errstr, NDebug);
return -1;
}
if(up->fpstate & FPillegal)
error("floating point in note handler");
if(up->fpsave.fpdelayexec)
panic("fpuemu: entered with outstanding watch trap");
pc = ureg->pc;
validaddr(pc, 4, 0);
if(ureg->cause & BD) {
pc += 4;
validaddr(pc, 4, 0);
}
iw = *(ulong*)pc;
do {
if (iw == 0x44410000){
ureg->r1 = 0x500;
r = Advpc;
if (DBG(Dbgbasic))
iprint("faked MOVW FCR0,R1\n");
}else{
s = spllo();
if(waserror()){
splx(s);
nexterror();
}
r = fpimips(pc, iw, ureg, fpinit(ureg));
splx(s);
poperror();
if (r == Failed || r == Leavepcret)
break;
}
if (r == Advpc)
if(ureg->cause & BD)
followbr(ureg);
else
ureg->pc += 4;
ureg->cause &= ~BD;
pc = ureg->pc;
iw = validiw(pc);
while (iw == NOP || iw == MIPSNOP) {
pc += 4;
ureg->pc = pc;
iw = validiw(pc);
}
} while (isfpop(iw));
if (r == Failed){
iprint("fpuemu: fp emulation failed for %#lux"
" at pc %#p in %lud %s\n",
iw, ureg->pc, up->pid, up->text);
unimp(ureg->pc, iw, "no fp instruction");
}
ureg->cause &= ~BD;
poperror();
return 0;
}
int
isbranch(ulong *pc)
{
ulong iw;
iw = *(ulong*)pc;
switch(iw>>26){
case 0:
switch(iw&0x3F){
case 0x09:
case 0x08:
return 1;
default:
return 0;
}
case 1:
switch((iw>>16) & 0x1F){
case 0x10:
case 0x00:
case 0x11:
case 0x01:
return 1;
default:
return 0;
}
case 3:
case 2:
case 4:
case 5:
case 6:
case 7:
return 1;
}
if((iw>>26) == COP1)
switch((iw>>16) & 0x3C1){
case 0x101:
case 0x181:
case 0x100:
case 0x180:
return 1;
}
return 0;
}
ulong
branch(Ureg *ur, ulong fcr31)
{
ulong iw, npc, rs, rt, rd, offset, targ, next;
iw = ur->pc;
iw = *(ulong*)iw;
rs = (iw>>21) & 0x1F;
if(rs)
rs = REG(ur, rs);
rt = (iw>>16) & 0x1F;
if(rt)
rt = REG(ur, rt);
offset = iw & ((1<<16)-1);
if(offset & (1<<15))
offset |= ~((1<<16)-1);
offset <<= 2;
targ = ur->pc + 4 + offset;
next = ur->pc + 8;
switch(iw>>26){
case 0:
switch(iw&0x3F){
case 0x09:
rd = (iw>>11) & 0x1F;
if(rd)
REG(ur, rd) = next;
case 0x08:
return rs;
default:
return 0;
}
case 1:
switch((iw>>16) & 0x1F){
case 0x10:
ur->r31 = next;
case 0x00:
if((long)rs < 0)
return targ;
return next;
case 0x11:
ur->r31 = next;
case 0x01:
if((long)rs >= 0)
return targ;
return next;
default:
return 0;
}
case 3:
ur->r31 = next;
case 2:
npc = iw & ((1<<26)-1);
npc <<= 2;
return npc | (ur->pc&0xF0000000);
case 4:
if(rs == rt)
return targ;
return next;
case 5:
if(rs != rt)
return targ;
return next;
case 6:
if((long)rs <= 0)
return targ;
return next;
case 7:
if((long)rs > 0)
return targ;
return next;
}
if((iw>>26) == COP1)
switch((iw>>16) & 0x3C1){
case 0x101:
case 0x181:
if(fcr31 & FPCOND)
return targ;
return next;
case 0x100:
case 0x180:
if(!(fcr31 & FPCOND))
return targ;
return next;
}
return 0;
}