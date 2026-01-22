#include <lib9.h>
typedef struct instr Instr;
struct opcode
{
char *mnemonic;
void (*f)(Instr*, char*);
int flag;
};
static char FRAMENAME[] = ".frame";
struct instr
{
uchar op;
uchar rd;
uchar op2;
uchar a;
uchar cond;
uchar op3;
uchar rs1;
uchar i;
uchar asi;
uchar rs2;
short simm13;
ushort opf;
ulong immdisp22;
ulong simmdisp22;
ulong disp30;
ulong imm32;
int target;
long w0;
long w1;
ulong addr;
char* curr;
char* end;
int size;
char* err;
};
static int dascase;
static int mkinstr(ulong*, Instr*);
static void bra1(Instr*, char*, char*[]);
static void bra(Instr*, char*);
static void fbra(Instr*, char*);
static void cbra(Instr*, char*);
static void unimp(Instr*, char*);
static void fpop(Instr*, char*);
static void shift(Instr*, char*);
static void sethi(Instr*, char*);
static void load(Instr*, char*);
static void loada(Instr*, char*);
static void store(Instr*, char*);
static void storea(Instr*, char*);
static void add(Instr*, char*);
static void cmp(Instr*, char*);
static void wr(Instr*, char*);
static void jmpl(Instr*, char*);
static void rd(Instr*, char*);
static void loadf(Instr*, char*);
static void storef(Instr*, char*);
static void loadc(Instr*, char*);
static void loadcsr(Instr*, char*);
static void trap(Instr*, char*);
static struct opcode sparcop0[8] = {
"UNIMP", unimp, 0,
0, 0, 0,
"B", bra, 0,
0, 0, 0,
"SETHI", sethi, 0,
0, 0, 0,
"FB", fbra, 0,
"CB", cbra, 0,
};
static struct opcode sparcop2[64] = {
"ADD", add, 0,
"AND", add, 0,
"OR", add, 0,
"XOR", add, 0,
"SUB", add, 0,
"ANDN", add, 0,
"ORN", add, 0,
"XORN", add, 0,
"ADDX", add, 0,
0, 0, 0,
"UMUL", add, 0,
"SMUL", add, 0,
"SUBX", add, 0,
0, 0, 0,
"UDIV", add, 0,
"SDIV", add, 0,
"ADDCC", add, 0,
"ANDCC", add, 0,
"ORCC", add, 0,
"XORCC", add, 0,
"SUBCC", cmp, 0,
"ANDNCC", add, 0,
"ORNCC", add, 0,
"XORNCC", add, 0,
"ADDXCC", add, 0,
0, 0, 0,
"UMULCC", add, 0,
"SMULCC", add, 0,
"SUBXCC", add, 0,
0, 0, 0,
"UDIVCC", add, 0,
"SDIVCC", add, 0,
"TADD", add, 0,
"TSUB", add, 0,
"TADDCCTV", add, 0,
"TSUBCCTV", add, 0,
"MULSCC", add, 0,
"SLL", shift, 0,
"SRL", shift, 0,
"SRA", shift, 0,
"rdy", rd, 0,
"rdpsr", rd, 0,
"rdwim", rd, 0,
"rdtbr", rd, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
"wry", wr, 0,
"wrpsr", wr, 0,
"wrwim", wr, 0,
"wrtbr", wr, 0,
"FPOP", fpop, 0,
"FPOP", fpop, 0,
0, 0, 0,
0, 0, 0,
"JMPL", jmpl, 0,
"RETT", add, 0,
"T", trap, 0,
"flush", add, 0,
"SAVE", add, 0,
"RESTORE", add, 0,
};
static struct opcode sparcop3[64]={
"ld", load, 0,
"ldub", load, 0,
"lduh", load, 0,
"ldd", load, 0,
"st", store, 0,
"stb", store, 0,
"sth", store, 0,
"std", store, 0,
0, 0, 0,
"ldsb", load, 0,
"ldsh", load, 0,
0, 0, 0,
0, 0, 0,
"ldstub", store, 0,
0, 0, 0,
"swap", load, 0,
"lda", loada, 0,
"lduba", loada, 0,
"lduha", loada, 0,
"ldda", loada, 0,
"sta", storea, 0,
"stba", storea, 0,
"stha", storea, 0,
"stda", storea, 0,
0, 0, 0,
"ldsba", loada, 0,
"ldsha", loada, 0,
0, 0, 0,
0, 0, 0,
"ldstuba", storea, 0,
0, 0, 0,
"swapa", loada, 0,
"ldf", loadf, 0,
"ldfsr", loadf, 0,
0, 0, 0,
"lddf", loadf, 0,
"stf", storef, 0,
"stfsr", storef, 0,
"stdfq", storef, 0,
"stdf", storef, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
0, 0, 0,
"ldc", loadc, 0,
"ldcsr", loadcsr,0,
0, 0, 0,
"lddc", loadc, 0,
"stc", loadc, 0,
"stcsr", loadcsr,0,
"stdcq", loadcsr,0,
"stdc", loadc, 0,
};
static void
bprint(Instr *i, char *fmt, ...)
{
va_list arg;
va_start(arg, fmt);
i->curr = vseprint(i->curr, i->end, fmt, arg);
va_end(arg);
}
static int
decode(ulong *pc, Instr *i)
{
ulong w;
w = *pc;
i->op = (w >> 30) & 0x03;
i->rd = (w >> 25) & 0x1F;
i->op2 = (w >> 22) & 0x07;
i->a = (w >> 29) & 0x01;
i->cond = (w >> 25) & 0x0F;
i->op3 = (w >> 19) & 0x3F;
i->rs1 = (w >> 14) & 0x1F;
i->i = (w >> 13) & 0x01;
i->asi = (w >> 5) & 0xFF;
i->rs2 = (w >> 0) & 0x1F;
i->simm13 = (w >> 0) & 0x1FFF;
if(i->simm13 & (1<<12))
i->simm13 |= ~((1<<13)-1);
i->opf = (w >> 5) & 0x1FF;
i->immdisp22 = (w >> 0) & 0x3FFFFF;
i->simmdisp22 = i->immdisp22;
if(i->simmdisp22 & (1<<21))
i->simmdisp22 |= ~((1<<22)-1);
i->disp30 = (w >> 0) & 0x3FFFFFFF;
i->w0 = w;
i->target = -1;
i->addr = (ulong)pc;
i->size = 1;
return 1;
}
static int
mkinstr(ulong *pc, Instr *i)
{
Instr xi;
if (decode(pc, i) < 0)
return -1;
if(i->op==0 && i->op2==4 && !dascase){
if(decode(pc+1, &xi) < 0)
return -1;
if(xi.op == 2 && xi.op3 == 0)
if(xi.i == 1 && xi.rs1 == i->rd){
i->imm32 = xi.simm13 + (i->immdisp22<<10);
i->target = xi.rd;
i->w1 = xi.w0;
i->size++;
return 1;
}
}
if(i->op==2 && i->opf==1 && !dascase){
if (decode(pc+1, &xi) < 0)
return -1;
if(i->op==2 && i->opf==1)
if(xi.rd==i->rd+1 && xi.rs2==i->rs2+1){
i->w1 = xi.w0;
i->size++;
}
}
return 1;
}
static int
inst(ulong *pc)
{
long disp;
Instr instr;
static char buf[128];
void (*f)(Instr*, char*);
memset(&instr, 0, sizeof(instr));
instr.curr = buf;
instr.end = buf+sizeof(buf)-1;
if(mkinstr(pc, &instr) < 0)
return 4;
switch(instr.op){
case 0:
f = sparcop0[instr.op2].f;
if(f)
(*f)(&instr, sparcop0[instr.op2].mnemonic);
else
bprint(&instr, "unknown 0x%lux", instr.w0);
break;
case 1:
disp = instr.disp30;
disp = (disp<<2)>>2;
bprint(&instr, "CALL\t0x%lux", pc+disp);
if (!dascase)
bprint(&instr, "(SB)");
break;
case 2:
f = sparcop2[instr.op3].f;
if(f)
(*f)(&instr, sparcop2[instr.op3].mnemonic);
else
bprint(&instr, "unknown 0x%lux", instr.w0);
break;
case 3:
f = sparcop3[instr.op3].f;
if(f)
(*f)(&instr, sparcop3[instr.op3].mnemonic);
else
bprint(&instr, "unknown 0x%lux", instr.w0);
break;
}
if (instr.err) {
if (instr.curr != buf)
bprint(&instr, "\t\t;");
bprint(&instr, instr.err);
}
print("\t%.8lux %s\n", (ulong)pc, buf);
return instr.size;
}
void
das(ulong *pc, int n)
{
ulong *e;
e = pc + n;
while(pc < e)
pc += inst(pc);
}
static void
address(Instr *i)
{
bprint(i, "0x%lux(R%d)", i->simm13, i->rs1);
}
static void
unimp(Instr *i, char *m)
{
bprint(i, "%s", m);
}
static char *bratab[16] = {
"N",
"E",
"LE",
"L",
"LEU",
"CS",
"NEG",
"VS",
"A",
"NE",
"G",
"GE",
"GU",
"CC",
"POS",
"VC",
};
static char *fbratab[16] = {
"N",
"NE",
"LG",
"UL",
"L",
"UG",
"G",
"U",
"A",
"E",
"UE",
"GE",
"UGE",
"LE",
"ULE",
"O",
};
static char *cbratab[16] = {
"N",
"123",
"12",
"13",
"1",
"23",
"2",
"3",
"A",
"0",
"03",
"02",
"023",
"01",
"013",
"012",
};
static void
bra1(Instr *i, char *m, char *tab[])
{
long imm;
imm = i->simmdisp22;
if(i->a)
bprint(i, "%s%s.%c\t", m, tab[i->cond], 'A'+dascase);
else
bprint(i, "%s%s\t", m, tab[i->cond]);
bprint(i, "0x%lux", i->addr+4*imm);
if (!dascase)
bprint(i, "(SB)");
}
static void
bra(Instr *i, char *m)
{
bra1(i, m, bratab);
}
static void
fbra(Instr *i, char *m)
{
bra1(i, m, fbratab);
}
static void
cbra(Instr *i, char *m)
{
bra1(i, m, cbratab);
}
static void
trap(Instr *i, char *m)
{
if(i->i == 0)
bprint(i, "%s%s\tR%d+R%d", m, bratab[i->cond], i->rs2, i->rs1);
else
bprint(i, "%s%s\t$0x%lux+R%d", m, bratab[i->cond], i->simm13, i->rs1);
}
static void
sethi(Instr *i, char *m)
{
ulong imm;
imm = i->immdisp22<<10;
if(dascase){
bprint(i, "%s\t0x%lux, R%d", m, imm, i->rd);
return;
}
if(imm==0 && i->rd==0){
bprint(i, "NOP");
return;
}
if(i->target < 0){
bprint(i, "MOVW\t$0x%lux, R%d", imm, i->rd);
return;
}
bprint(i, "MOVW\t$0x%lux, R%d", i->imm32, i->target);
}
static char ldtab[] = {
'W',
'B',
'H',
'D',
};
static char*
moveinstr(int op3, char *m)
{
char *s;
int c;
static char buf[8];
if(!dascase){
if(op3 == 0xF || op3 == 0x1F)
return "SWAP";
if(op3 == 0xD || op3 == 0x1D)
return "TAS";
c = ldtab[op3&3];
s = "";
if((op3&11)==1 || (op3&11)==2)
s="U";
sprint(buf, "MOV%c%s", c, s);
return buf;
}
return m;
}
static void
load(Instr *i, char *m)
{
m = moveinstr(i->op3, m);
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d), R%d", m, i->rs1, i->rs2, i->rd);
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", R%d", i->rd);
}
}
static void
loada(Instr *i, char *m)
{
m = moveinstr(i->op3, m);
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d, %d), R%d", m, i->rs1, i->rs2, i->asi, i->rd);
else
bprint(i, "unknown ld asi 0x%lux", i->w0);
}
static void
store(Instr *i, char *m)
{
m = moveinstr(i->op3, m);
if(i->i == 0)
bprint(i, "%s\tR%d, (R%d+R%d)",
m, i->rd, i->rs1, i->rs2);
else{
bprint(i, "%s\tR%d, ", m, i->rd);
address(i);
}
}
static void
storea(Instr *i, char *m)
{
m = moveinstr(i->op3, m);
if(i->i == 0)
bprint(i, "%s\tR%d, (R%d+R%d, %d)", m, i->rd, i->rs1, i->rs2, i->asi);
else
bprint(i, "%s\tR%d, %d(R%d, %d), ???", m, i->rd, i->simm13, i->rs1, i->asi);
}
static void
shift(Instr *i, char *m)
{
if(i->i == 0){
if(i->rs1 == i->rd)
if(dascase)
bprint(i, "%s\tR%d, R%d", m, i->rs1, i->rs2);
else
bprint(i, "%s\tR%d, R%d", m, i->rs2, i->rs1);
else
if(dascase)
bprint(i, "%s\tR%d, R%d, R%d", m, i->rs1, i->rs2, i->rd);
else
bprint(i, "%s\tR%d, R%d, R%d", m, i->rs2, i->rs1, i->rd);
}else{
if(i->rs1 == i->rd)
if(dascase)
bprint(i, "%s\t$%d,R%d", m, i->simm13&0x1F, i->rs1);
else
bprint(i, "%s\tR%d, $%d", m, i->rs1, i->simm13&0x1F);
else
if(dascase)
bprint(i, "%s\tR%d, $%d, R%d",m,i->rs1,i->simm13&0x1F,i->rd);
else
bprint(i, "%s\t$%d, R%d, R%d",m,i->simm13&0x1F,i->rs1,i->rd);
}
}
static void
add(Instr *i, char *m)
{
if(i->i == 0){
if(dascase)
bprint(i, "%s\tR%d, R%d", m, i->rs1, i->rs2);
else
if(i->op3==2 && i->rs1==0 && i->rd)
bprint(i, "MOVW\tR%d", i->rs2);
else
bprint(i, "%s\tR%d, R%d", m, i->rs2, i->rs1);
}else{
if(dascase)
bprint(i, "%s\tR%d, $0x%lux", m, i->rs1, i->simm13);
else
if(i->op3==0 && i->rd && i->rs1==0)
bprint(i, "MOVW\t$0x%lux", i->simm13);
else if(i->op3==0 && i->rd && i->rs1==2){
bprint(i, "MOVW\t$");
address(i);
} else
bprint(i, "%s\t$0x%lux, R%d", m, i->simm13, i->rs1);
}
if(i->rs1 != i->rd)
bprint(i, ", R%d", i->rd);
}
static void
cmp(Instr *i, char *m)
{
if(dascase || i->rd){
add(i, m);
return;
}
if(i->i == 0)
bprint(i, "CMP\tR%d, R%d", i->rs1, i->rs2);
else
bprint(i, "CMP\tR%d, $0x%lux", i->rs1, i->simm13);
}
static char *regtab[4] =
{
"Y",
"PSR",
"WIM",
"TBR",
};
static void
wr(Instr *i, char *m)
{
if(dascase){
if(i->i == 0)
bprint(i, "%s\tR%d, R%d", m, i->rs1, i->rs2);
else
bprint(i, "%s\tR%d, $0x%lux", m, i->rs1, i->simm13);
}else{
if(i->i && i->simm13==0)
bprint(i, "MOVW\tR%d", i->rs1);
else if(i->i == 0)
bprint(i, "wr\tR%d, R%d", i->rs2, i->rs1);
else
bprint(i, "wr\t$0x%lux, R%d", i->simm13, i->rs1);
}
bprint(i, ", %s", regtab[i->op3&3]);
}
static void
rd(Instr *i, char *m)
{
if(i->rs1==15 && i->rd==0){
m = "stbar";
if(!dascase)
m = "STBAR";
bprint(i, "%s", m);
}else{
if(!dascase)
m = "MOVW";
bprint(i, "%s\t%s, R%d", m, regtab[i->op3&3], i->rd);
}
}
static void
jmpl(Instr *i, char *m)
{
if(i->i == 0){
if(i->rd == 15)
bprint(i, "CALL\t(R%d+R%d)", i->rs2, i->rs1);
else
bprint(i, "%s\t(R%d+R%d), R%d", m, i->rs2, i->rs1, i->rd);
}else{
if(!dascase && i->simm13==8 && i->rs1==15 && i->rd==0)
bprint(i, "RETURN");
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", R%d", i->rd);
}
}
}
static void
loadf(Instr *i, char *m)
{
if(!dascase){
m = "FMOVD";
if(i->op3 == 0x20)
m = "FMOVF";
else if(i->op3 == 0x21)
m = "MOVW";
}
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d)", m, i->rs1, i->rs2);
else{
bprint(i, "%s\t", m);
address(i);
}
if(i->op3 == 0x21)
bprint(i, ", FSR");
else
bprint(i, ", R%d", i->rd);
}
static
void storef(Instr *i, char *m)
{
if(!dascase){
m = "FMOVD";
if(i->op3 == 0x25 || i->op3 == 0x26)
m = "MOVW";
else if(i->op3 == 0x24)
m = "FMOVF";
}
bprint(i, "%s\t", m);
if(i->op3 == 0x25)
bprint(i, "FSR, ");
else if(i->op3 == 0x26)
bprint(i, "FQ, ");
else
bprint(i, "R%d, ", i->rd);
if(i->i == 0)
bprint(i, "(R%d+R%d)", i->rs1, i->rs2);
else
address(i);
}
static
void loadc(Instr *i, char *m)
{
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d), C%d", m, i->rs1, i->rs2, i->rd);
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", C%d", i->rd);
}
}
static
void loadcsr(Instr *i, char *m)
{
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d), CSR", m, i->rs1, i->rs2);
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", CSR");
}
}
static struct
{
int opf;
char *name;
} fptab1[] = {
0xC4, "FITOS",
0xC8, "FITOD",
0xCC, "FITOX",
0xD1, "FSTOI",
0xD2, "FDTOI",
0xD3, "FXTOI",
0xC9, "FSTOD",
0xCD, "FSTOX",
0xC6, "FDTOS",
0xCE, "FDTOX",
0xC7, "FXTOS",
0xCB, "FXTOD",
0x01, "FMOVS",
0x05, "FNEGS",
0x09, "FABSS",
0x29, "FSQRTS",
0x2A, "FSQRTD",
0x2B, "FSQRTX",
0, 0,
};
static struct{
int opf;
char *name;
} fptab2[] = {
0x41, "FADDS",
0x42, "FADDD",
0x43, "FADDX",
0x45, "FSUBS",
0x46, "FSUBD",
0x47, "FSUBX",
0x49, "FMULS",
0x4A, "FMULD",
0x4B, "FMULX",
0x4D, "FDIVS",
0x4E, "FDIVD",
0x4F, "FDIVX",
0x51, "FCMPS",
0x52, "FCMPD",
0x53, "FCMPX",
0x55, "FCMPES",
0x56, "FCMPED",
0x57, "FCMPEX",
0, 0
};
static void
fpop(Instr *i, char *m)
{
int j;
if(dascase==0 && i->size==2){
bprint(i, "FMOVD\tF%d, F%d", i->rs2, i->rd);
return;
}
for(j=0; fptab1[j].name; j++)
if(fptab1[j].opf == i->opf){
bprint(i, "%s\tF%d, F%d", fptab1[j].name, i->rs2, i->rd);
return;
}
for(j=0; fptab2[j].name; j++)
if(fptab2[j].opf == i->opf){
bprint(i, "%s\tF%d, F%d, F%d", fptab2[j].name, i->rs1, i->rs2, i->rd);
return;
}
bprint(i, "%s%ux\tF%d, F%d, F%d", m, i->opf, i->rs1, i->rs2, i->rd);
}