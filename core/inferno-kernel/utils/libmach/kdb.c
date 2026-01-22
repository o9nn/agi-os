#include <lib9.h>
#include <bio.h>
#include "mach.h"
static	char	*sparcexcep(Map*, Rgetter);
static	int	sparcfoll(Map*, uvlong, Rgetter, uvlong*);
static	int	sparcinst(Map*, uvlong, char, char*, int);
static	int	sparcdas(Map*, uvlong, char*, int);
static	int	sparcinstlen(Map*, uvlong);
Machdata sparcmach =
{
{0x91, 0xd0, 0x20, 0x01},
4,
beswab,
beswal,
beswav,
risctrace,
riscframe,
sparcexcep,
0,
beieeesftos,
beieeedftos,
sparcfoll,
sparcinst,
sparcdas,
sparcinstlen,
};
static char *trapname[] =
{
"reset",
"instruction access exception",
"illegal instruction",
"privileged instruction",
"fp disabled",
"window overflow",
"window underflow",
"unaligned address",
"fp exception",
"data access exception",
"tag overflow",
};
static char*
excname(ulong tbr)
{
static char buf[32];
if(tbr < sizeof trapname/sizeof(char*))
return trapname[tbr];
if(tbr >= 130)
sprint(buf, "trap instruction %ld", tbr-128);
else if(17<=tbr && tbr<=31)
sprint(buf, "interrupt level %ld", tbr-16);
else switch(tbr){
case 36:
return "cp disabled";
case 40:
return "cp exception";
case 128:
return "syscall";
case 129:
return "breakpoint";
default:
sprint(buf, "unknown trap %ld", tbr);
}
return buf;
}
static char*
sparcexcep(Map *map, Rgetter rget)
{
long tbr;
tbr = (*rget)(map, "TBR");
tbr = (tbr&0xFFF)>>4;
return excname(tbr);
}
typedef struct instr Instr;
struct opcode {
char	*mnemonic;
void	(*f)(Instr*, char*);
int	flag;
};
static	char FRAMENAME[] = ".frame";
struct instr {
uchar	op;
uchar	rd;
uchar	op2;
uchar	a;
uchar	cond;
uchar	op3;
uchar	rs1;
uchar	i;
uchar	asi;
uchar	rs2;
short	simm13;
ushort	opf;
ulong	immdisp22;
ulong	simmdisp22;
ulong	disp30;
ulong	imm32;
int	target;
long	w0;
long	w1;
uvlong	addr;
char	*curr;
char	*end;
int 	size;
char	*err;
};
static	Map	*mymap;
static	int	dascase;
static int	mkinstr(uvlong, Instr*);
static void	bra1(Instr*, char*, char*[]);
static void	bra(Instr*, char*);
static void	fbra(Instr*, char*);
static void	cbra(Instr*, char*);
static void	unimp(Instr*, char*);
static void	fpop(Instr*, char*);
static void	shift(Instr*, char*);
static void	sethi(Instr*, char*);
static void	load(Instr*, char*);
static void	loada(Instr*, char*);
static void	store(Instr*, char*);
static void	storea(Instr*, char*);
static void	add(Instr*, char*);
static void	cmp(Instr*, char*);
static void	wr(Instr*, char*);
static void	jmpl(Instr*, char*);
static void	rd(Instr*, char*);
static void	loadf(Instr*, char*);
static void	storef(Instr*, char*);
static void	loadc(Instr*, char*);
static void	loadcsr(Instr*, char*);
static void	trap(Instr*, char*);
static struct opcode sparcop0[8] = {
"UNIMP",	unimp,	0,
"",		0,	0,
"B",		bra,	0,
"",		0,	0,
"SETHI",	sethi,	0,
"",		0,	0,
"FB",		fbra,	0,
"CB",		cbra,	0,
};
static struct opcode sparcop2[64] = {
"ADD",		add,	0,
"AND",		add,	0,
"OR",		add,	0,
"XOR",		add,	0,
"SUB",		add,	0,
"ANDN",		add,	0,
"ORN",		add,	0,
"XORN",		add,	0,
"ADDX",		add,	0,
"",		0,	0,
"UMUL",		add,	0,
"SMUL",		add,	0,
"SUBX",		add,	0,
"",		0,	0,
"UDIV",		add,	0,
"SDIV",		add,	0,
"ADDCC",	add,	0,
"ANDCC",	add,	0,
"ORCC",		add,	0,
"XORCC",	add,	0,
"SUBCC",	cmp,	0,
"ANDNCC",	add,	0,
"ORNCC",	add,	0,
"XORNCC",	add,	0,
"ADDXCC",	add,	0,
"",		0,	0,
"UMULCC",	add,	0,
"SMULCC",	add,	0,
"SUBXCC",	add,	0,
"",		0,	0,
"UDIVCC",	add,	0,
"SDIVCC",	add,	0,
"TADD",		add,	0,
"TSUB",		add,	0,
"TADDCCTV",	add,	0,
"TSUBCCTV",	add,	0,
"MULSCC",	add,	0,
"SLL",		shift,	0,
"SRL",		shift,	0,
"SRA",		shift,	0,
"rdy",		rd,	0,
"rdpsr",	rd,	0,
"rdwim",	rd,	0,
"rdtbr",	rd,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"wry",		wr,	0,
"wrpsr",	wr,	0,
"wrwim",	wr,	0,
"wrtbr",	wr,	0,
"FPOP",		fpop,	0,
"FPOP",		fpop,	0,
"",		0,	0,
"",		0,	0,
"JMPL",		jmpl,	0,
"RETT",		add,	0,
"T",		trap,	0,
"flush",	add,	0,
"SAVE",		add,	0,
"RESTORE",	add,	0,
};
static struct opcode sparcop3[64]={
"ld",		load,	0,
"ldub",		load,	0,
"lduh",		load,	0,
"ldd",		load,	0,
"st",		store,	0,
"stb",		store,	0,
"sth",		store,	0,
"std",		store,	0,
"",		0,	0,
"ldsb",		load,	0,
"ldsh",		load,	0,
"",		0,	0,
"",		0,	0,
"ldstub",	store,	0,
"",		0,	0,
"swap",		load,	0,
"lda",		loada,	0,
"lduba",	loada,	0,
"lduha",	loada,	0,
"ldda",		loada,	0,
"sta",		storea,	0,
"stba",		storea,	0,
"stha",		storea,	0,
"stda",		storea,	0,
"",		0,	0,
"ldsba",	loada,	0,
"ldsha",	loada,	0,
"",		0,	0,
"",		0,	0,
"ldstuba",	storea,	0,
"",		0,	0,
"swapa",	loada,	0,
"ldf",		loadf,	0,
"ldfsr",	loadf,0,
"",		0,	0,
"lddf",		loadf,	0,
"stf",		storef,	0,
"stfsr",	storef,0,
"stdfq",	storef,0,
"stdf",		storef,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"",		0,	0,
"ldc",		loadc,	0,
"ldcsr",	loadcsr,0,
"",		0,	0,
"lddc",		loadc,	0,
"stc",		loadc,	0,
"stcsr",	loadcsr,0,
"stdcq",	loadcsr,0,
"stdc",		loadc,	0,
};
#pragma	varargck	argpos	bprint	2
#pragma	varargck	type	"T"	char*
static int
Tfmt(Fmt *f)
{
char buf[128];
char *s, *t, *oa;
oa = va_arg(f->args, char*);
if(dascase){
for(s=oa,t=buf; *t = *s; s++,t++)
if('A'<=*t && *t<='Z')
*t += 'a'-'A';
return fmtstrcpy(f, buf);
}
return fmtstrcpy(f, oa);
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
decode(uvlong pc, Instr *i)
{
ulong w;
if (get4(mymap, pc, &w) < 0) {
werrstr("can't read instruction: %r");
return -1;
}
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
i->addr = pc;
i->size = 1;
return 1;
}
static int
mkinstr(uvlong pc, Instr *i)
{
Instr xi;
if (decode(pc, i) < 0)
return -1;
if(i->op==0 && i->op2==4 && !dascase){
if (decode(pc+4, &xi) < 0)
return -1;
if(xi.op==2 && xi.op3==0)
if(xi.i == 1 && xi.rs1 == i->rd){
i->imm32 = xi.simm13 + (i->immdisp22<<10);
i->target = xi.rd;
i->w1 = xi.w0;
i->size++;
return 1;
}
}
if(i->op==2 && i->opf==1 && !dascase){
if (decode(pc+4, &xi) < 0)
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
printins(Map *map, uvlong pc, char *buf, int n)
{
Instr instr;
void (*f)(Instr*, char*);
mymap = map;
memset(&instr, 0, sizeof(instr));
instr.curr = buf;
instr.end = buf+n-1;
if (mkinstr(pc, &instr) < 0)
return -1;
switch(instr.op){
case 0:
f = sparcop0[instr.op2].f;
if(f)
(*f)(&instr, sparcop0[instr.op2].mnemonic);
else
bprint(&instr, "unknown %lux", instr.w0);
break;
case 1:
bprint(&instr, "%T", "CALL\t");
instr.curr += symoff(instr.curr, instr.end-instr.curr,
pc+instr.disp30*4, CTEXT);
if (!dascase)
bprint(&instr, "(SB)");
break;
case 2:
f = sparcop2[instr.op3].f;
if(f)
(*f)(&instr, sparcop2[instr.op3].mnemonic);
else
bprint(&instr, "unknown %lux", instr.w0);
break;
case 3:
f = sparcop3[instr.op3].f;
if(f)
(*f)(&instr, sparcop3[instr.op3].mnemonic);
else
bprint(&instr, "unknown %lux", instr.w0);
break;
}
if (instr.err) {
if (instr.curr != buf)
bprint(&instr, "\t\t;");
bprint(&instr, instr.err);
}
return instr.size*4;
}
static int
sparcinst(Map *map, uvlong pc, char modifier, char *buf, int n)
{
static int fmtinstalled = 0;
if (!fmtinstalled) {
fmtinstalled = 1;
fmtinstall('T', Tfmt);
}
if ((asstype == ASUNSPARC && modifier == 'i')
|| (asstype == ASPARC && modifier == 'I'))
dascase = 'a'-'A';
else
dascase = 0;
return printins(map, pc, buf, n);
}
static int
sparcdas(Map *map, uvlong pc, char *buf, int n)
{
Instr instr;
mymap = map;
memset(&instr, 0, sizeof(instr));
instr.curr = buf;
instr.end = buf+n-1;
if (mkinstr(pc, &instr) < 0)
return -1;
if (instr.end-instr.curr > 8)
instr.curr = _hexify(instr.curr, instr.w0, 7);
if (instr.end-instr.curr > 9 && instr.size == 2) {
*instr.curr++ = ' ';
instr.curr = _hexify(instr.curr, instr.w1, 7);
}
*instr.curr = 0;
return instr.size*4;
}
static int
sparcinstlen(Map *map, uvlong pc)
{
Instr i;
mymap = map;
if (mkinstr(pc, &i) < 0)
return -1;
return i.size*4;
}
static int
plocal(Instr *i)
{
int offset;
Symbol s;
if (!findsym(i->addr, CTEXT, &s) || !findlocal(&s, FRAMENAME, &s))
return -1;
if (s.value > i->simm13) {
if(getauto(&s, s.value-i->simm13, CAUTO, &s)) {
bprint(i, "%s+%lld(SP)", s.name, s.value);
return 1;
}
} else {
offset = i->simm13-s.value;
if (getauto(&s, offset-4, CPARAM, &s)) {
bprint(i, "%s+%d(FP)", s.name, offset);
return 1;
}
}
return -1;
}
static void
address(Instr *i)
{
Symbol s, s2;
uvlong off, off1;
if (i->rs1 == 1 && plocal(i) >= 0)
return;
off = mach->sb+i->simm13;
if(i->rs1 == 2	&& findsym(off, CANY, &s)
&& s.value-off < 4096
&& (s.class == CDATA || s.class == CTEXT)) {
if(off==s.value && s.name[0]=='$'){
off1 = 0;
geta(mymap, s.value, &off1);
if(off1 && findsym(off1, CANY, &s2) && s2.value == off1){
bprint(i, "$%s(SB)", s2.name);
return;
}
}
bprint(i, "%s", s.name);
if (s.value != off)
bprint(i, "+%llux", s.value-off);
bprint(i, "(SB)");
return;
}
bprint(i, "%ux(R%d)", i->simm13, i->rs1);
}
static void
unimp(Instr *i, char *m)
{
bprint(i, "%T", m);
}
static char	*bratab[16] = {
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
static char	*fbratab[16] = {
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
static char	*cbratab[16] = {
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
bprint(i, "%T%T.%c\t", m, tab[i->cond], 'A'+dascase);
else
bprint(i, "%T%T\t", m, tab[i->cond]);
i->curr += symoff(i->curr, i->end-i->curr, i->addr+4*imm, CTEXT);
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
bprint(i, "%T%T\tR%d+R%d", m, bratab[i->cond], i->rs2, i->rs1);
else
bprint(i, "%T%T\t$%ux+R%d", m, bratab[i->cond], i->simm13, i->rs1);
}
static void
sethi(Instr *i, char *m)
{
ulong imm;
imm = i->immdisp22<<10;
if(dascase){
bprint(i, "%T\t%lux, R%d", m, imm, i->rd);
return;
}
if(imm==0 && i->rd==0){
bprint(i, "NOP");
return;
}
if(i->target < 0){
bprint(i, "MOVW\t$%lux, R%d", imm, i->rd);
return;
}
bprint(i, "MOVW\t$%lux, R%d", i->imm32, i->target);
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
bprint(i, "unknown ld asi %lux", i->w0);
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
bprint(i, "%s\tR%d, %d(R%d, %d), ?", m, i->rd, i->simm13, i->rs1, i->asi);
}
static void
shift(Instr *i, char *m)
{
if(i->i == 0){
if(i->rs1 == i->rd)
if(dascase)
bprint(i, "%T\tR%d, R%d", m, i->rs1, i->rs2);
else
bprint(i, "%T\tR%d, R%d", m, i->rs2, i->rs1);
else
if(dascase)
bprint(i, "%T\tR%d, R%d, R%d", m, i->rs1, i->rs2, i->rd);
else
bprint(i, "%T\tR%d, R%d, R%d", m, i->rs2, i->rs1, i->rd);
}else{
if(i->rs1 == i->rd)
if(dascase)
bprint(i, "%T\t$%d,R%d", m, i->simm13&0x1F, i->rs1);
else
bprint(i, "%T\tR%d, $%d", m,  i->rs1, i->simm13&0x1F);
else
if(dascase)
bprint(i, "%T\tR%d, $%d, R%d",m,i->rs1,i->simm13&0x1F,i->rd);
else
bprint(i, "%T\t$%d, R%d, R%d",m,i->simm13&0x1F,i->rs1,i->rd);
}
}
static void
add(Instr *i, char *m)
{
if(i->i == 0){
if(dascase)
bprint(i, "%T\tR%d, R%d", m, i->rs1, i->rs2);
else
if(i->op3==2 && i->rs1==0 && i->rd)
bprint(i, "MOVW\tR%d", i->rs2);
else
bprint(i, "%T\tR%d, R%d", m, i->rs2, i->rs1);
}else{
if(dascase)
bprint(i, "%T\tR%d, $%ux", m, i->rs1, i->simm13);
else
if(i->op3==0 && i->rd && i->rs1==0)
bprint(i, "MOVW\t$%ux", i->simm13);
else if(i->op3==0 && i->rd && i->rs1==2){
bprint(i, "MOVW\t$");
address(i);
} else
bprint(i, "%T\t$%ux, R%d", m, i->simm13, i->rs1);
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
bprint(i, "CMP\tR%d, $%ux", i->rs1, i->simm13);
}
static char *regtab[4] = {
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
bprint(i, "%s\tR%d, $%ux", m, i->rs1, i->simm13);
}else{
if(i->i && i->simm13==0)
bprint(i, "MOVW\tR%d", i->rs1);
else if(i->i == 0)
bprint(i, "wr\tR%d, R%d", i->rs2, i->rs1);
else
bprint(i, "wr\t$%ux, R%d", i->simm13, i->rs1);
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
bprint(i, "%T\t(R%d+R%d)", "CALL", i->rs2, i->rs1);
else
bprint(i, "%T\t(R%d+R%d), R%d", m, i->rs2, i->rs1, i->rd);
}else{
if(!dascase && i->simm13==8 && i->rs1==15 && i->rd==0)
bprint(i, "RETURN");
else{
bprint(i, "%T\t", m);
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
static void
storef(Instr *i, char *m)
{
if(!dascase){
m = "FMOVD";
if(i->op3 == 0x25 || i->op3 == 0x26)
m = "MOVW";
else if(i->op3 == 0x20)
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
static void
loadc(Instr *i, char *m)
{
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d), C%d", m, i->rs1, i->rs2, i->rd);
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", C%d", i->rd);
}
}
static void
loadcsr(Instr *i, char *m)
{
if(i->i == 0)
bprint(i, "%s\t(R%d+R%d), CSR", m, i->rs1, i->rs2);
else{
bprint(i, "%s\t", m);
address(i);
bprint(i, ", CSR");
}
}
static struct{
int	opf;
char	*name;
} fptab1[] = {
0xC4,	"FITOS",
0xC8,	"FITOD",
0xCC,	"FITOX",
0xD1,	"FSTOI",
0xD2,	"FDTOI",
0xD3,	"FXTOI",
0xC9,	"FSTOD",
0xCD,	"FSTOX",
0xC6,	"FDTOS",
0xCE,	"FDTOX",
0xC7,	"FXTOS",
0xCB,	"FXTOD",
0x01,	"FMOVS",
0x05,	"FNEGS",
0x09,	"FABSS",
0x29,	"FSQRTS",
0x2A,	"FSQRTD",
0x2B,	"FSQRTX",
0,	0,
};
static struct{
int	opf;
char	*name;
} fptab2[] = {
0x41,	"FADDS",
0x42,	"FADDD",
0x43,	"FADDX",
0x45,	"FSUBS",
0x46,	"FSUBD",
0x47,	"FSUBX",
0x49,	"FMULS",
0x4A,	"FMULD",
0x4B,	"FMULX",
0x4D,	"FDIVS",
0x4E,	"FDIVD",
0x4F,	"FDIVX",
0x51,	"FCMPS",
0x52,	"FCMPD",
0x53,	"FCMPX",
0x55,	"FCMPES",
0x56,	"FCMPED",
0x57,	"FCMPEX",
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
bprint(i, "%T\tF%d, F%d", fptab1[j].name, i->rs2, i->rd);
return;
}
for(j=0; fptab2[j].name; j++)
if(fptab2[j].opf == i->opf){
bprint(i, "%T\tF%d, F%d, F%d", fptab2[j].name, i->rs1, i->rs2, i->rd);
return;
}
bprint(i, "%T%ux\tF%d, F%d, F%d", m, i->opf, i->rs1, i->rs2, i->rd);
}
static int
sparcfoll(Map *map, uvlong pc, Rgetter rget, uvlong *foll)
{
ulong w, r1, r2;
char buf[8];
Instr i;
mymap = map;
if (mkinstr(pc, &i) < 0)
return -1;
w = i.w0;
switch(w & 0xC1C00000){
case 0x00800000:
case 0x01800000:
case 0x01C00000:
foll[0] = pc+8;
foll[1] = pc + (i.simmdisp22<<2);
return 2;
}
if((w&0xC0000000) == 0x40000000){
foll[0] = pc + (i.disp30<<2);
return 1;
}
if((w&0xC1F80000) == 0x81C00000){
sprint(buf, "R%ld", (w>>14)&0xF);
r1 = (*rget)(map, buf);
if(w & 0x2000)
r2 = i.simm13;
else{
sprint(buf, "R%ld", w&0xF);
r2 = (*rget)(map, buf);
}
foll[0] = r1 + r2;
return 1;
}
foll[0] = pc+i.size*4;
return 1;
}