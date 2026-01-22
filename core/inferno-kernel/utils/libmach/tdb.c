#include <lib9.h>
#include <bio.h>
#include "mach.h"
static int debug = 0;
typedef struct Instr Instr;
struct Instr
{
Map *map;
ulong w;
ulong addr;
uchar op;
uchar rd;
uchar rn;
uchar rs;
long imm;
char* curr;
char* end;
char* err;
};
typedef struct Opcode Opcode;
struct Opcode
{
char* o;
void (*fmt)(Opcode*, Instr*);
uvlong (*foll)(Map*, Rgetter, Instr*, uvlong);
char* a;
};
static void format(char*, Instr*, char*);
static char FRAMENAME[] = ".frame";
static char *thumbexcep(Map*, Rgetter);
static int thumbfoll(Map*, uvlong, Rgetter, uvlong*);
static int thumbinst(Map*, uvlong, char, char*, int);
static int thumbdas(Map*, uvlong, char*, int);
static int thumbinstlen(Map*, uvlong);
Machdata thumbmach =
{
{0x0, 0xE8},
2,
leswab,
leswal,
leswav,
risctrace,
riscframe,
thumbexcep,
0,
0,
0,
thumbfoll,
thumbinst,
thumbdas,
thumbinstlen,
};
static void thumbrrh(Opcode *, Instr *);
static void thumbbcc(Opcode *, Instr *);
static void thumbb(Opcode *, Instr *);
static void thumbbl(Opcode *, Instr *);
static char*
thumbexcep(Map *map, Rgetter rget)
{
long c;
c = (*rget)(map, "TYPE");
switch (c&0x1f) {
case 0x11:
return "Fiq interrupt";
case 0x12:
return "Mirq interrupt";
case 0x13:
return "SVC/SWI Exception";
case 0x17:
return "Prefetch Abort/Data Abort";
case 0x18:
return "Data Abort";
case 0x1b:
return "Undefined instruction/Breakpoint";
case 0x1f:
return "Sys trap";
default:
return "Undefined trap";
}
}
static
char* cond[16] =
{
"EQ", "NE", "CS", "CC",
"MI", "PL", "VS", "VC",
"HI", "LS", "GE", "LT",
"GT", "LE", "\0", "NV"
};
#define B(h, l) bits(ins, h, l)
static int
bits(int i, int h, int l)
{
if(h < l)
print("h < l in bits");
return (i&(((1<<(h-l+1))-1)<<l))>>l;
}
int
thumbclass(long w)
{
int o;
int ins = w;
if(ins&0xffff0000)
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1+1+1+2;
o = B(15, 13);
switch(o){
case 0:
o = B(12, 11);
switch(o){
case 0:
case 1:
case 2:
return B(12, 11);
case 3:
if(B(10, 10) == 0)
return 3+B(9, 9);
else
return 3+2+B(9, 9);
}
case 1:
return 3+2+2+B(12, 11);
case 2:
o = B(12, 10);
if(o == 0)
return 3+2+2+4+B(9, 6);
if(o == 1){
o = B(9, 8);
if(o == 3)
return 3+2+2+4+16+B(9, 8);
return 3+2+2+4+16+B(9, 8);
}
if(o == 2 || o == 3)
return 3+2+2+4+16+4;
return 3+2+2+4+16+4+1+B(11, 9);
case 3:
return 3+2+2+4+16+4+1+8+B(12, 11);
case 4:
if(B(12, 12) == 0)
return 3+2+2+4+16+4+1+8+4+B(11, 11);
return 3+2+2+4+16+4+1+8+6+B(11, 11);
case 5:
if(B(12, 12) == 0)
return 3+2+2+4+16+4+1+8+6+2+B(11, 11);
if(B(11, 8) == 0)
return 3+2+2+4+16+4+1+8+6+2+2+B(7, 7);
return 3+2+2+4+16+4+1+8+6+2+2+2+B(11, 11);
case 6:
if(B(12, 12) == 0)
return 3+2+2+4+16+4+1+8+6+2+2+2+2+B(11, 11);
if(B(11, 8) == 0xf)
return 3+2+2+4+16+4+1+8+6+2+2+2+4;
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1;
case 7:
o = B(12, 11);
switch(o){
case 0:
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1+1;
case 1:
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1+1+1+2;
case 2:
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1+1+1;
case 3:
return 3+2+2+4+16+4+1+8+6+2+2+2+4+1+1+1+1;
}
}
return 0;
}
static int
decode(Map *map, uvlong pc, Instr *i)
{
ushort w;
if(get2(map, pc, &w) < 0) {
werrstr("can't read instruction: %r");
return -1;
}
i->w = w;
i->addr = pc;
i->op = thumbclass(w);
i->map = map;
return 1;
}
static void
bprint(Instr *i, char *fmt, ...)
{
va_list arg;
va_start(arg, fmt);
i->curr = vseprint(i->curr, i->end, fmt, arg);
va_end(arg);
}
static int
plocal(Instr *i)
{
char *reg;
Symbol s;
char *fn;
int class;
int offset;
if(!findsym(i->addr, CTEXT, &s)) {
if(debug)fprint(2,"fn not found @%lux: %r\n", i->addr);
return 0;
}
fn = s.name;
if (!findlocal(&s, FRAMENAME, &s)) {
if(debug)fprint(2,"%s.%s not found @%s: %r\n", fn, FRAMENAME, s.name);
return 0;
}
if(s.value > i->imm) {
class = CAUTO;
offset = s.value-i->imm;
reg = "(SP)";
} else {
class = CPARAM;
offset = i->imm-s.value-4;
reg = "(FP)";
}
if(!getauto(&s, offset, class, &s)) {
if(debug)fprint(2,"%s %s not found @%ux: %r\n", fn,
class == CAUTO ? " auto" : "param", offset);
return 0;
}
bprint(i, "%s%c%d%s", s.name, class == CPARAM ? '+' : '-', s.value, reg);
return 1;
}
static int
gsymoff(char *buf, int n, long v, int space)
{
Symbol s;
int r;
long delta;
r = delta = 0;
if (v) {
r = findsym(v, space, &s);
if (r)
delta = v-s.value;
if (delta < 0)
delta = -delta;
}
if (v == 0 || r == 0 || delta >= 4096)
return snprint(buf, n, "#%lux", v);
if (strcmp(s.name, ".string") == 0)
return snprint(buf, n, "#%lux", v);
if (!delta)
return snprint(buf, n, "%s", s.name);
if (s.type != 't' && s.type != 'T')
return snprint(buf, n, "%s+%llux", s.name, v-s.value);
else
return snprint(buf, n, "#%lux", v);
}
static int
thumbcondpass(Map *map, Rgetter rget, uchar cond)
{
ulong psr;
uchar n;
uchar z;
uchar c;
uchar v;
psr = rget(map, "PSR");
n = (psr >> 31) & 1;
z = (psr >> 30) & 1;
c = (psr >> 29) & 1;
v = (psr >> 28) & 1;
switch(cond) {
case 0: return z;
case 1: return !z;
case 2: return c;
case 3: return !c;
case 4: return n;
case 5: return !n;
case 6: return v;
case 7: return !v;
case 8: return c && !z;
case 9: return !c || z;
case 10: return n == v;
case 11: return n != v;
case 12: return !z && (n == v);
case 13: return z && (n != v);
case 14: return 1;
case 15: return 0;
}
return 0;
}
static uvlong
thumbfbranch(Map *map, Rgetter rget, Instr *i, uvlong pc)
{
char buf[8];
if(i->op == 30){
thumbrrh(nil, i);
sprint(buf, "R%ud", i->rn);
return rget(map, buf)&~1;
}
if(i->op == 57){
thumbbcc(nil, i);
if(thumbcondpass(map, rget, (i->w >> 8) & 0xf))
return i->imm;
return pc+2;
}
if(i->op == 58){
thumbb(nil, i);
return i->imm;
}
if(i->op == 60){
thumbbl(nil, i);
return i->imm;
}
print("bad thumbfbranch call");
return 0;
}
static uvlong
thumbfmov(Map *map, Rgetter rget, Instr *i, uvlong pc)
{
char buf[8];
ulong rd;
thumbrrh(nil, i);
rd = i->rd;
if(rd != 15)
return pc+2;
sprint(buf, "R%ud", i->rn);
return rget(map, buf);
}
static uvlong
thumbfadd(Map *map, Rgetter rget, Instr *i, uvlong pc)
{
char buf[8];
ulong rd, v;
thumbrrh(nil, i);
rd = i->rd;
if(rd != 15)
return pc+2;
sprint(buf, "R%ud", i->rn);
v = rget(map, buf);
sprint(buf, "R15");
return rget(map, buf) + v;
}
static void
thumbshift(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
i->imm = B(10, 6);
format(o->o, i, o->a);
}
static void
thumbrrr(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
i->rs = B(8, 6);
format(o->o, i, o->a);
}
static void
thumbirr(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
i->imm = B(8, 6);
format(o->o, i, o->a);
}
static void
thumbir(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(10, 8);
i->imm = B(7, 0);
format(o->o, i, o->a);
}
static void
thumbrr(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
format(o->o, i, o->a);
}
static void
thumbrrh(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
if(B(6, 6))
i->rn += 8;
if(B(7, 7))
i->rd += 8;
if(o != nil){
if(i->w == 0x46b7 || i->w == 0x46f7 || i->w == 0x4730 || i->w == 0x4770)
format("RET", i, "");
else
format(o->o, i, o->a);
}
}
static void
thumbpcrel(Opcode *o, Instr *i)
{
int ins = i->w;
i->rn = 15;
i->rd = B(10, 8);
i->imm = 4*(B(7, 0)+1);
if(i->addr & 3)
i->imm -= 2;
format(o->o, i, o->a);
}
static void
thumbmovirr(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(2, 0);
i->rn = B(5, 3);
i->imm = B(10, 6);
if(strcmp(o->o, "MOVW") == 0)
i->imm *= 4;
else if(strncmp(o->o, "MOVH", 4) == 0)
i->imm *= 2;
format(o->o, i, o->a);
}
static void
thumbmovsp(Opcode *o, Instr *i)
{
int ins = i->w;
i->rn = 13;
i->rd = B(10, 8);
i->imm = 4*B(7, 0);
format(o->o, i, o->a);
}
static void
thumbaddsppc(Opcode *o, Instr *i)
{
int ins = i->w;
i->rd = B(10, 8);
i->imm = 4*B(7, 0);
if(i->op == 48)
i->imm += 4;
format(o->o, i, o->a);
}
static void
thumbaddsp(Opcode *o, Instr *i)
{
int ins = i->w;
i->imm = 4*B(6, 0);
format(o->o, i, o->a);
}
static void
thumbswi(Opcode *o, Instr *i)
{
int ins = i->w;
i->imm = B(7, 0);
format(o->o, i, o->a);
}
static void
thumbbcc(Opcode *o, Instr *i)
{
int off, ins = i->w;
off = B(7, 0);
if(off & 0x80)
off |= 0xffffff00;
i->imm = i->addr + 2*off + 4;
if(o != nil)
format(o->o, i, o->a);
}
static void
thumbb(Opcode *o, Instr *i)
{
int off, ins = i->w;
off = B(10, 0);
if(off & 0x400)
off |= 0xfffff800;
i->imm = i->addr + 2*off + 4;
if(o != nil)
format(o->o, i, o->a);
}
static void
thumbbl(Opcode *o, Instr *i)
{
int off, h, ins = i->w;
static int reglink;
h = B(11, 11);
off = B(10, 0);
if(h == 0){
if(off & 0x400)
off |= 0xfffff800;
i->imm = i->addr + (off<<12) + 4;
reglink = i->imm;
}
else{
i->imm = reglink + 2*off;
}
if(o != nil)
format(o->o, i, o->a);
}
static void
thumbregs(Opcode *o, Instr *i)
{
int ins = i->w;
if(i->op == 52 || i->op == 53)
i->rd = 13;
else
i->rd = B(10, 8);
i->imm = B(7, 0);
format(o->o, i, o->a);
}
static void
thumbunk(Opcode *o, Instr *i)
{
format(o->o, i, o->a);
}
static Opcode opcodes[] =
{
"LSL", thumbshift, 0, "$#%i,R%n,R%d",
"LSR", thumbshift, 0, "$#%i,R%n,R%d",
"ASR", thumbshift, 0, "$#%i,R%n,R%d",
"ADD", thumbrrr, 0, "R%s,R%n,R%d",
"SUB", thumbrrr, 0, "R%s,R%n,R%d",
"ADD", thumbirr, 0, "$#%i,R%n,R%d",
"SUB", thumbirr, 0, "$#%i,R%n,R%d",
"MOVW", thumbir, 0, "$#%i,R%d",
"CMP", thumbir, 0, "$#%i,R%d",
"ADD", thumbir, 0, "$#%i,R%d,R%d",
"SUB", thumbir, 0, "$#%i,R%d,R%d",
"AND", thumbrr, 0, "R%n,R%d,R%d",
"EOR", thumbrr, 0, "R%n,R%d,R%d",
"LSL", thumbrr, 0, "R%n,R%d,R%d",
"LSR", thumbrr, 0, "R%n,R%d,R%d",
"ASR", thumbrr, 0, "R%n,R%d,R%d",
"ADC", thumbrr, 0, "R%n,R%d,R%d",
"SBC", thumbrr, 0, "R%n,R%d,R%d",
"ROR", thumbrr, 0, "R%n,R%d,R%d",
"TST", thumbrr, 0, "R%n,R%d",
"NEG", thumbrr, 0, "R%n,R%d",
"CMP", thumbrr, 0, "R%n,R%d",
"CMPN", thumbrr, 0, "R%n,R%d",
"OR", thumbrr, 0, "R%n,R%d,R%d",
"MUL", thumbrr, 0, "R%n,R%d,R%d",
"BITC", thumbrr, 0, "R%n,R%d,R%d",
"MOVN", thumbrr, 0, "R%n,R%d",
"ADD", thumbrrh, thumbfadd, "R%n,R%d,R%d",
"CMP", thumbrrh, 0, "R%n,R%d",
"MOVW", thumbrrh, thumbfmov, "R%n,R%d",
"BX", thumbrrh, thumbfbranch, "R%n",
"MOVW", thumbpcrel, 0, "$%I,R%d",
"MOVW", thumbrrr, 0, "R%d, [R%s,R%n]",
"MOVH", thumbrrr, 0, "R%d, [R%s,R%n]",
"MOVB", thumbrrr, 0, "R%d, [R%s,R%n]",
"MOVB", thumbrrr, 0, "[R%s,R%n],R%d",
"MOVW", thumbrrr, 0, "[R%s,R%n],R%d",
"MOVHU", thumbrrr, 0, "[R%s,R%n],R%d",
"MOVBU", thumbrrr, 0, "[R%s,R%n],R%d",
"MOVH", thumbrrr, 0, "[R%s,R%n],R%d",
"MOVW", thumbmovirr, 0, "R%d,%I",
"MOVW", thumbmovirr, 0, "%I,R%d",
"MOVB", thumbmovirr, 0, "R%d,%I",
"MOVBU", thumbmovirr, 0, "$%I,R%d",
"MOVH", thumbmovirr, 0, "R%d,%I",
"MOVHU", thumbmovirr, 0, "%I,R%d",
"MOVW", thumbmovsp, 0, "R%d,%I",
"MOVW", thumbmovsp, 0, "%I,R%d",
"ADD", thumbaddsppc,0, "$#%i,PC,R%d",
"ADD", thumbaddsppc,0, "$#%i,SP,R%d",
"ADD", thumbaddsp, 0, "$#%i,SP,SP",
"SUB", thumbaddsp, 0, "$#%i,SP,SP",
"PUSH", thumbregs, 0, "R%d, %r",
"POP", thumbregs, 0, "R%d, %r",
"STMIA", thumbregs, 0, "R%d, %r",
"LDMIA", thumbregs, 0, "R%d, %r",
"SWI", thumbswi, 0, "$#%i",
"B%c", thumbbcc, thumbfbranch, "%b",
"B", thumbb, thumbfbranch, "%b",
"BL", thumbbl, 0, "",
"BL", thumbbl, thumbfbranch, "%b",
"UNK", thumbunk, 0, "",
};
static void
gaddr(Instr *i)
{
*i->curr++ = '$';
i->curr += gsymoff(i->curr, i->end-i->curr, i->imm, CANY);
}
static void
format(char *mnemonic, Instr *i, char *f)
{
int j, k, m, n;
int g;
char *fmt;
int ins = i->w;
if(mnemonic)
format(0, i, mnemonic);
if(f == 0)
return;
if(mnemonic)
if(i->curr < i->end)
*i->curr++ = '\t';
for ( ; *f && i->curr < i->end; f++) {
if(*f != '%') {
*i->curr++ = *f;
continue;
}
switch (*++f) {
case 'c':
bprint(i, "%s", cond[B(11, 8)]);
break;
case 's':
bprint(i, "%d", i->rs);
break;
case 'n':
bprint(i, "%d", i->rn);
break;
case 'd':
bprint(i, "%d", i->rd);
break;
case 'i':
bprint(i, "%lux", i->imm);
break;
case 'b':
i->curr += symoff(i->curr, i->end-i->curr,
i->imm, CTEXT);
break;
case 'I':
if (i->rn == 13) {
if (plocal(i))
break;
}
g = 0;
fmt = "#%lx(R%d)";
if (i->rn == 15) {
if (get4(i->map, i->addr + i->imm, (ulong*)&i->imm) > 0)
{
g = 1;
fmt = "";
}
}
if (mach->sb)
{
if (i->rn == 12)
{
i->imm += mach->sb;
g = 1;
fmt = "-SB(SB)";
}
}
if (g)
{
gaddr(i);
bprint(i, fmt, i->rn);
}
else
bprint(i, fmt, i->imm, i->rn);
break;
case 'r':
n = i->imm&0xff;
j = 0;
k = 0;
while(n) {
m = j;
while(n&0x1) {
j++;
n >>= 1;
}
if(j != m) {
if(k)
bprint(i, ",");
if(j == m+1)
bprint(i, "R%d", m);
else
bprint(i, "R%d-R%d", m, j-1);
k = 1;
}
j++;
n >>= 1;
}
break;
case '\0':
*i->curr++ = '%';
return;
default:
bprint(i, "%%%c", *f);
break;
}
}
*i->curr = 0;
}
static int
printins(Map *map, uvlong pc, char *buf, int n)
{
Instr i;
i.curr = buf;
i.end = buf+n-1;
if(decode(map, pc, &i) < 0)
return -1;
(*opcodes[i.op].fmt)(&opcodes[i.op], &i);
return 2;
}
static int
thumbinst(Map *map, uvlong pc, char modifier, char *buf, int n)
{
USED(modifier);
return printins(map, pc, buf, n);
}
static int
thumbdas(Map *map, uvlong pc, char *buf, int n)
{
Instr i;
i.curr = buf;
i.end = buf+n;
if(decode(map, pc, &i) < 0)
return -1;
if(i.end-i.curr > 8)
i.curr = _hexify(buf, i.w, 7);
*i.curr = 0;
return 2;
}
static int
thumbinstlen(Map *map, uvlong pc)
{
Instr i;
if(decode(map, pc, &i) < 0)
return -1;
return 2;
}
static int
thumbfoll(Map *map, uvlong pc, Rgetter rget, uvlong *foll)
{
ulong d;
Instr i;
if(decode(map, pc, &i) < 0)
return -1;
if(opcodes[i.op].foll) {
d = (*opcodes[i.op].foll)(map, rget, &i, pc);
if(d == -1)
return -1;
} else
d = pc+2;
foll[0] = d;
return 1;
}