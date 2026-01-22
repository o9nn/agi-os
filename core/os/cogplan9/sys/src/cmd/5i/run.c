#include <u.h>
#include <libc.h>
#include <bio.h>
#include <mach.h>
#include "arm.h"
static int dummy;
static char* shtype[4] =
{
"<<",
">>",
"->",
"@>",
};
static char* cond[16] =
{
".EQ", ".NE", ".HS", ".LO",
".MI", ".PL", ".VS", ".VC",
".HI", ".LS", ".GE", ".LT",
".GT", ".LE", "", ".NO",
};
void Idp0(ulong);
void Idp1(ulong);
void Idp2(ulong);
void Idp3(ulong);
void Imul(ulong);
void Imula(ulong);
void Imull(ulong);
void Iswap(ulong);
void Imem1(ulong);
void Imem2(ulong);
void Ilsm(ulong inst);
void Ib(ulong);
void Ibl(ulong);
void Ssyscall(ulong);
Inst itab[] =
{
{ Idp0, "AND", Iarith },
{ Idp0, "EOR", Iarith },
{ Idp0, "SUB", Iarith },
{ Idp0, "RSB", Iarith },
{ Idp0, "ADD", Iarith },
{ Idp0, "ADC", Iarith },
{ Idp0, "SBC", Iarith },
{ Idp0, "RSC", Iarith },
{ Idp0, "TST", Iarith },
{ Idp0, "TEQ", Iarith },
{ Idp0, "CMP", Iarith },
{ Idp0, "CMN", Iarith },
{ Idp0, "ORR", Iarith },
{ Idp0, "MOV", Iarith },
{ Idp0, "BIC", Iarith },
{ Idp0, "MVN", Iarith },
{ Idp1, "AND", Iarith },
{ Idp1, "EOR", Iarith },
{ Idp1, "SUB", Iarith },
{ Idp1, "RSB", Iarith },
{ Idp1, "ADD", Iarith },
{ Idp1, "ADC", Iarith },
{ Idp1, "SBC", Iarith },
{ Idp1, "RSC", Iarith },
{ Idp1, "TST", Iarith },
{ Idp1, "TEQ", Iarith },
{ Idp1, "CMP", Iarith },
{ Idp1, "CMN", Iarith },
{ Idp1, "ORR", Iarith },
{ Idp1, "MOV", Iarith },
{ Idp1, "BIC", Iarith },
{ Idp1, "MVN", Iarith },
{ Idp2, "AND", Iarith },
{ Idp2, "EOR", Iarith },
{ Idp2, "SUB", Iarith },
{ Idp2, "RSB", Iarith },
{ Idp2, "ADD", Iarith },
{ Idp2, "ADC", Iarith },
{ Idp2, "SBC", Iarith },
{ Idp2, "RSC", Iarith },
{ Idp2, "TST", Iarith },
{ Idp2, "TEQ", Iarith },
{ Idp2, "CMP", Iarith },
{ Idp2, "CMN", Iarith },
{ Idp2, "ORR", Iarith },
{ Idp2, "MOV", Iarith },
{ Idp2, "BIC", Iarith },
{ Idp2, "MVN", Iarith },
{ Idp3, "AND", Iarith },
{ Idp3, "EOR", Iarith },
{ Idp3, "SUB", Iarith },
{ Idp3, "RSB", Iarith },
{ Idp3, "ADD", Iarith },
{ Idp3, "ADC", Iarith },
{ Idp3, "SBC", Iarith },
{ Idp3, "RSC", Iarith },
{ Idp3, "TST", Iarith },
{ Idp3, "TEQ", Iarith },
{ Idp3, "CMP", Iarith },
{ Idp3, "CMN", Iarith },
{ Idp3, "ORR", Iarith },
{ Idp3, "MOV", Iarith },
{ Idp3, "BIC", Iarith },
{ Idp3, "MVN", Iarith },
{ Imul, "MUL", Iarith },
{ Imula, "MULA", Iarith },
{ Iswap, "SWPW", Imem },
{ Iswap, "SWPBU", Imem },
{ Imem2, "MOV", Imem },
{ Imem2, "MOV", Imem },
{ Imem2, "MOV", Imem },
{ Imem2, "MOV", Imem },
{ Imem1, "MOVW", Imem },
{ Imem1, "MOVB", Imem },
{ Imem1, "MOVW", Imem },
{ Imem1, "MOVB", Imem },
{ Imem1, "MOVW", Imem },
{ Imem1, "MOVB", Imem },
{ Imem1, "MOVW", Imem },
{ Imem1, "MOVB", Imem },
{ Ilsm, "LDM", Imem },
{ Ilsm, "STM", Imem },
{ Ib, "B", Ibranch },
{ Ibl, "BL", Ibranch },
{ Ssyscall, "SWI", Isyscall },
{ undef, "undef" },
{ undef, "undef" },
{ undef, "undef" },
{ Imull, "MULLU", Iarith },
{ Imull, "MULALU", Iarith },
{ Imull, "MULL", Iarith },
{ Imull, "MULAL", Iarith },
{ undef, "undef" },
{ 0 }
};
int
runcmp(void)
{
switch(reg.cond) {
case 0x0: return (reg.cc1 == reg.cc2);
case 0x1: return (reg.cc1 != reg.cc2);
case 0x2: return ((ulong)reg.cc1 >= (ulong)reg.cc2);
case 0x3: return ((ulong)reg.cc1 < (ulong)reg.cc2);
case 0x4: return (reg.cc1 - reg.cc2 < 0);
case 0x5: return (reg.cc1 - reg.cc2 >= 0);
case 0x8: return ((ulong)reg.cc1 > (ulong)reg.cc2);
case 0x9: return ((ulong)reg.cc1 <= (ulong)reg.cc2);
case 0xa: return (reg.cc1 >= reg.cc2);
case 0xb: return (reg.cc1 < reg.cc2);
case 0xc: return (reg.cc1 > reg.cc2);
case 0xd: return (reg.cc1 <= reg.cc2);
case 0xe: return 1;
case 0xf: return 0;
default:
Bprint(bioout, "unimplemented condition prefix %x (%ld %ld)\n",
reg.cond, reg.cc1, reg.cc2);
undef(reg.ir);
return 0;
}
}
int
runteq(void)
{
long res = reg.cc1 ^ reg.cc2;
switch(reg.cond) {
case 0x0: return res == 0;
case 0x1: return res != 0;
case 0x4: return (res & SIGNBIT) != 0;
case 0x5: return (res & SIGNBIT) == 0;
case 0xe: return 1;
case 0xf: return 0;
default:
Bprint(bioout, "unimplemented condition prefix %x (%ld %ld)\n",
reg.cond, reg.cc1, reg.cc2);
undef(reg.ir);
return 0;
}
}
int
runtst(void)
{
long res = reg.cc1 & reg.cc2;
switch(reg.cond) {
case 0x0: return res == 0;
case 0x1: return res != 0;
case 0x4: return (res & SIGNBIT) != 0;
case 0x5: return (res & SIGNBIT) == 0;
case 0xe: return 1;
case 0xf: return 0;
default:
Bprint(bioout, "unimplemented condition prefix %x (%ld %ld)\n",
reg.cond, reg.cc1, reg.cc2);
undef(reg.ir);
return 0;
}
}
void
run(void)
{
int execute;
do {
if(trace)
Bflush(bioout);
reg.ar = reg.r[REGPC];
reg.ir = ifetch(reg.ar);
reg.class = armclass(reg.ir);
reg.ip = &itab[reg.class];
reg.cond = (reg.ir>>28) & 0xf;
switch(reg.compare_op) {
case CCcmp:
execute = runcmp();
break;
case CCteq:
execute = runteq();
break;
case CCtst:
execute = runtst();
break;
default:
Bprint(bioout, "unimplemented compare operation %x\n",
reg.compare_op);
return;
}
if(execute) {
reg.ip->count++;
(*reg.ip->func)(reg.ir);
} else {
if(trace)
itrace("%s%s	IGNORED",
reg.ip->name, cond[reg.cond]);
}
reg.r[REGPC] += 4;
if(bplist)
brkchk(reg.r[REGPC], Instruction);
} while(--count);
}
void
undef(ulong inst)
{
Bprint(bioout, "undefined instruction trap pc #%lux inst %.8lux class %d\n",
reg.r[REGPC], inst, reg.class);
longjmp(errjmp, 0);
}
long
shift(long v, int st, int sc, int isreg)
{
if(sc == 0) {
switch(st) {
case 0:
reg.cout = reg.cbit;
break;
case 1:
reg.cout = (v >> 31) & 1;
break;
case 2:
reg.cout = reg.cbit;
break;
case 3:
if(isreg) {
reg.cout = reg.cbit;
}
else {
reg.cout = v & 1;
v = ((ulong)v >> 1) | (reg.cbit << 31);
}
}
}
else {
switch(st) {
case 0:
reg.cout = (v >> (32 - sc)) & 1;
v = v << sc;
break;
case 1:
reg.cout = (v >> (sc - 1)) & 1;
v = (ulong)v >> sc;
break;
case 2:
if(sc >= 32) {
reg.cout = (v >> 31) & 1;
if(reg.cout)
v = 0xFFFFFFFF;
else
v = 0;
}
else {
reg.cout = (v >> (sc - 1)) & 1;
v = (long)v >> sc;
}
break;
case 3:
reg.cout = (v >> (sc - 1)) & 1;
v = (v << (32-sc)) | ((ulong)v >> sc);
break;
}
}
return v;
}
void
dpex(long inst, long o1, long o2, int rd)
{
int cbit;
cbit = 0;
switch((inst>>21) & 0xf) {
case 0:
reg.r[rd] = o1 & o2;
cbit = 1;
break;
case 1:
reg.r[rd] = o1 ^ o2;
cbit = 1;
break;
case 2:
reg.r[rd] = o1 - o2;
case 10:
if(inst & Sbit) {
reg.cc1 = o1;
reg.cc2 = o2;
reg.compare_op = CCcmp;
}
return;
case 3:
reg.r[rd] = o2 - o1;
if(inst & Sbit) {
reg.cc1 = o2;
reg.cc2 = o1;
reg.compare_op = CCcmp;
}
return;
case 4:
if(calltree && rd == REGPC && o2 == 0) {
Symbol s;
findsym(o1 + o2, CTEXT, &s);
Bprint(bioout, "%8lux return to %lux %s r0=%lux\n",
reg.r[REGPC], o1 + o2, s.name, reg.r[REGRET]);
}
reg.r[rd] = o1 + o2;
if(inst & Sbit) {
if(((uvlong)(ulong)o1 + (uvlong)(ulong)o2) & (1LL << 32))
reg.cbit = 1;
else
reg.cbit = 0;
reg.cc1 = o2;
reg.cc2 = -o1;
reg.compare_op = CCcmp;
}
return;
case 5:
case 6:
case 7:
undef(inst);
case 8:
if(inst & Sbit) {
reg.cc1 = o1;
reg.cc2 = o2;
reg.compare_op = CCtst;
}
return;
case 9:
if(inst & Sbit) {
reg.cc1 = o1;
reg.cc2 = o2;
reg.compare_op = CCteq;
}
return;
case 11:
if(inst & Sbit) {
reg.cc1 = o1;
reg.cc2 = -o2;
reg.compare_op = CCcmp;
}
return;
case 12:
reg.r[rd] = o1 | o2;
cbit = 1;
break;
case 13:
reg.r[rd] = o2;
cbit = 1;
break;
case 14:
reg.r[rd] = o1 & ~o2;
cbit = 1;
break;
case 15:
reg.r[rd] = ~o2;
cbit = 1;
break;
}
if(inst & Sbit) {
if(cbit)
reg.cbit = reg.cout;
reg.cc1 = reg.r[rd];
reg.cc2 = 0;
reg.compare_op = CCcmp;
}
}
void
Idp0(ulong inst)
{
int rn, rd, rm;
long o1, o2;
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
rm = inst & 0xf;
o1 = reg.r[rn];
if(rn == REGPC)
o1 += 8;
o2 = reg.r[rm];
if(rm == REGPC)
o2 += 8;
dpex(inst, o1, o2, rd);
if(trace)
itrace("%s%s\tR%d,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond],
rm, rn, rd,
reg.r[rd]);
if(rd == REGPC)
reg.r[rd] -= 4;
}
void
Idp1(ulong inst)
{
int rn, rd, rm, st, sc;
long o1, o2;
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
rm = inst & 0xf;
st = (inst>>5) & 0x3;
sc = (inst>>7) & 0x1f;
o1 = reg.r[rn];
if(rn == REGPC)
o1 += 8;
o2 = reg.r[rm];
if(rm == REGPC)
o2 += 8;
o2 = shift(o2, st, sc, 0);
dpex(inst, o1, o2, rd);
if(trace)
itrace("%s%s\tR%d%s%d,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond], rm, shtype[st], sc, rn, rd,
reg.r[rd]);
if(rd == REGPC)
reg.r[rd] -= 4;
}
void
Idp2(ulong inst)
{
int rn, rd, rm, rs, st;
long o1, o2, o3;
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
rm = inst & 0xf;
st = (inst>>5) & 0x3;
rs = (inst>>8) & 0xf;
o1 = reg.r[rn];
if(rn == REGPC)
o1 += 8;
o2 = reg.r[rm];
if(rm == REGPC)
o2 += 8;
o3 = reg.r[rs];
if(rs == REGPC)
o3 += 8;
o2 = shift(o2, st, o3, 1);
dpex(inst, o1, o2, rd);
if(trace)
itrace("%s%s\tR%d%sR%d=%d,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond], rm, shtype[st], rs, o3, rn, rd,
reg.r[rd]);
if(rd == REGPC)
reg.r[rd] -= 4;
}
void
Idp3(ulong inst)
{
int rn, rd, sc;
long o1, o2;
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
o1 = reg.r[rn];
if(rn == REGPC)
o1 += 8;
o2 = inst & 0xff;
sc = (inst>>7) & 0x1e;
o2 = (o2 >> sc) | (o2 << (32 - sc));
dpex(inst, o1, o2, rd);
if(trace)
itrace("%s%s\t#%x,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond], o2, rn, rd,
reg.r[rd]);
if(rd == REGPC)
reg.r[rd] -= 4;
}
void
Imul(ulong inst)
{
int rs, rd, rm;
rd = (inst>>16) & 0xf;
rs = (inst>>8) & 0xf;
rm = inst & 0xf;
if(rd == REGPC || rs == REGPC || rm == REGPC || rd == rm)
undef(inst);
reg.r[rd] = reg.r[rm]*reg.r[rs];
if(trace)
itrace("%s%s\tR%d,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond], rs, rm, rd,
reg.r[rd]);
}
void
Imull(ulong inst)
{
vlong v;
int rs, rd, rm, rn;
rd = (inst>>16) & 0xf;
rn = (inst>>12) & 0xf;
rs = (inst>>8) & 0xf;
rm = inst & 0xf;
if(rd == REGPC || rn == REGPC || rs == REGPC || rm == REGPC
|| rd == rm || rn == rm || rd == rn)
undef(inst);
if(inst & (1<<22)){
v = (vlong)reg.r[rm] * (vlong)reg.r[rs];
if(inst & (1 << 21))
v += reg.r[rn];
}else{
v = (uvlong)(ulong)reg.r[rm] * (uvlong)(ulong)reg.r[rs];
if(inst & (1 << 21))
v += (ulong)reg.r[rn];
}
reg.r[rd] = v >> 32;
reg.r[rn] = v;
if(trace)
itrace("%s%s\tR%d,R%d,(R%d,R%d) =#%llx",
reg.ip->name, cond[reg.cond], rs, rm, rn, rd,
v);
}
void
Imula(ulong inst)
{
int rs, rd, rm, rn;
rd = (inst>>16) & 0xf;
rn = (inst>>12) & 0xf;
rs = (inst>>8) & 0xf;
rm = inst & 0xf;
if(rd == REGPC || rn == REGPC || rs == REGPC || rm == REGPC || rd == rm)
undef(inst);
reg.r[rd] = reg.r[rm]*reg.r[rs] + reg.r[rn];
if(trace)
itrace("%s%s\tR%d,R%d,R%d,R%d =#%x",
reg.ip->name, cond[reg.cond], rs, rm, rn, rd,
reg.r[rd]);
}
void
Iswap(ulong inst)
{
int rn, rd, rm;
ulong address, value, bbit;
bbit = inst & (1<<22);
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
rm = (inst>>0) & 0xf;
address = reg.r[rn];
if(bbit) {
value = getmem_b(address);
putmem_b(address, reg.r[rm]);
} else {
value = getmem_w(address);
putmem_w(address, reg.r[rm]);
}
reg.r[rd] = value;
if(trace) {
char *bw, *dotc;
bw = "";
if(bbit)
bw = "B";
dotc = cond[reg.cond];
itrace("SWP%s%s\t#%x(R%d),R%d #%lux=#%x",
bw, dotc,
rn, rd,
address, value);
}
}
void
Imem1(ulong inst)
{
int rn, rd, off, rm, sc, st;
ulong address, value, pbit, ubit, bbit, wbit, lbit, bit25;
bit25 = inst & (1<<25);
pbit = inst & (1<<24);
ubit = inst & (1<<23);
bbit = inst & (1<<22);
wbit = inst & (1<<21);
lbit = inst & (1<<20);
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
SET(st);
SET(sc);
SET(rm);
if(bit25) {
rm = inst & 0xf;
st = (inst>>5) & 0x3;
sc = (inst>>7) & 0x1f;
off = reg.r[rm];
if(rm == REGPC)
off += 8;
off = shift(off, st, sc, 0);
} else {
off = inst & 0xfff;
}
if(!ubit)
off = -off;
if(rn == REGPC)
off += 8;
address = reg.r[rn];
if(pbit)
address += off;
if(lbit) {
if(bbit)
value = getmem_b(address);
else
value = getmem_w(address);
if(rd == REGPC)
value -= 4;
reg.r[rd] = value;
} else {
value = reg.r[rd];
if(rd == REGPC)
value -= 4;
if(bbit)
putmem_b(address, value);
else
putmem_w(address, value);
}
if(!(pbit && !wbit))
reg.r[rn] += off;
if(trace) {
char *bw, *dotp, *dotc;
bw = "W";
if(bbit)
bw = "BU";
dotp = "";
if(!pbit)
dotp = ".P";
dotc = cond[reg.cond];
if(lbit) {
if(!bit25)
itrace("MOV%s%s%s\t#%x(R%d),R%d #%lux=#%x",
bw, dotp, dotc,
off, rn, rd,
address, value);
else
itrace("MOV%s%s%s\t(R%d%s%d)(R%d),R%d  #%lux=#%x",
bw, dotp, dotc,
rm, shtype[st], sc, rn, rd,
address, value);
} else {
if(!bit25)
itrace("MOV%s%s%s\tR%d,#%x(R%d) #%lux=#%x",
bw, dotp, dotc,
rd, off, rn,
address, value);
else
itrace("MOV%s%s%s\tR%d,(R%d%s%d)(R%d) #%lux=#%x",
bw, dotp, dotc,
rd, rm, shtype[st], sc, rn,
address, value);
}
}
}
void
Imem2(ulong inst)
{
int rn, rd, off, rm;
ulong address, value, pbit, ubit, hbit, sbit, wbit, lbit, bit22;
pbit = inst & (1<<24);
ubit = inst & (1<<23);
bit22 = inst & (1<<22);
wbit = inst & (1<<21);
lbit = inst & (1<<20);
sbit = inst & (1<<6);
hbit = inst & (1<<5);
rn = (inst>>16) & 0xf;
rd = (inst>>12) & 0xf;
SET(rm);
if(bit22) {
off = ((inst>>4) & 0xf0) | (inst & 0xf);
} else {
rm = inst & 0xf;
off = reg.r[rm];
if(rm == REGPC)
off += 8;
}
if(!ubit)
off = -off;
if(rn == REGPC)
off += 8;
address = reg.r[rn];
if(pbit)
address += off;
if(lbit) {
if(hbit) {
value = getmem_h(address);
if(sbit && (value & 0x8000))
value |= 0xffff0000;
} else {
value = getmem_b(address);
if(value & 0x80)
value |= 0xffffff00;
}
if(rd == REGPC)
value -= 4;
reg.r[rd] = value;
} else {
value = reg.r[rd];
if(rd == REGPC)
value -= 4;
if(hbit) {
putmem_h(address, value);
} else {
putmem_b(address, value);
}
}
if(!(pbit && !wbit))
reg.r[rn] += off;
if(trace) {
char *hb, *dotp, *dotc;
hb = "B";
if(hbit)
hb = "H";
dotp = "";
if(!pbit)
dotp = ".P";
dotc = cond[reg.cond];
if(lbit) {
if(bit22)
itrace("MOV%s%s%s\t#%x(R%d),R%d #%lux=#%x",
hb, dotp, dotc,
off, rn, rd,
address, value);
else
itrace("MOV%s%s%s\t(R%d)(R%d),R%d  #%lux=#%x",
hb, dotp, dotc,
rm, rn, rd,
address, value);
} else {
if(bit22)
itrace("MOV%s%s%s\tR%d,#%x(R%d) #%lux=#%x",
hb, dotp, dotc,
rd, off, rn,
address, value);
else
itrace("MOV%s%s%s\tR%d,(R%d)(R%d) #%lux=#%x",
hb, dotp, dotc,
rd, rm, rn,
address, value);
}
}
}
void
Ilsm(ulong inst)
{
char pbit, ubit, sbit, wbit, lbit;
int i, rn, reglist;
ulong address, predelta, postdelta;
pbit = (inst>>24) & 0x1;
ubit = (inst>>23) & 0x1;
sbit = (inst>>22) & 0x1;
wbit = (inst>>21) & 0x1;
lbit = (inst>>20) & 0x1;
rn = (inst>>16) & 0xf;
reglist = inst & 0xffff;
if(reglist & 0x8000)
undef(reg.ir);
if(sbit)
undef(reg.ir);
address = reg.r[rn];
if(pbit) {
predelta = 4;
postdelta = 0;
} else {
predelta = 0;
postdelta = 4;
}
if(ubit) {
for (i = 0; i < 16; ++i) {
if(!(reglist & (1 << i)))
continue;
address += predelta;
if(lbit)
reg.r[i] = getmem_w(address);
else
putmem_w(address, reg.r[i]);
address += postdelta;
}
} else {
for (i = 15; 0 <= i; --i) {
if(!(reglist & (1 << i)))
continue;
address -= predelta;
if(lbit)
reg.r[i] = getmem_w(address);
else
putmem_w(address, reg.r[i]);
address -= postdelta;
}
}
if(wbit) {
reg.r[rn] = address;
}
if(trace) {
itrace("%s.%c%c\tR%d=%lux%s, <%lux>",
(lbit ? "LDM" : "STM"), (ubit ? 'I' : 'D'), (pbit ? 'B' : 'A'),
rn, reg.r[rn], (wbit ? "!" : ""), reglist);
}
}
void
Ib(ulong inst)
{
long v;
v = inst & 0xffffff;
v = reg.r[REGPC] + 8 + ((v << 8) >> 6);
if(trace)
itrace("B%s\t#%lux", cond[reg.cond], v);
reg.r[REGPC] = v - 4;
}
void
Ibl(ulong inst)
{
long v;
Symbol s;
v = inst & 0xffffff;
v = reg.r[REGPC] + 8 + ((v << 8) >> 6);
if(trace)
itrace("BL%s\t#%lux", cond[reg.cond], v);
if(calltree) {
findsym(v, CTEXT, &s);
Bprint(bioout, "%8lux %s(", reg.r[REGPC], s.name);
printparams(&s, reg.r[13]);
Bprint(bioout, "from ");
printsource(reg.r[REGPC]);
Bputc(bioout, '\n');
}
reg.r[REGLINK] = reg.r[REGPC] + 4;
reg.r[REGPC] = v - 4;
}