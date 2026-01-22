#if MACH_KDB
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <ddb/db_access.h>
#include <ddb/db_examine.h>
#include <ddb/db_output.h>
#include <ddb/db_sym.h>
#include <kern/task.h>
boolean_t db_disasm_16 = FALSE;
#define BYTE 0
#define WORD 1
#define LONG 2
#define QUAD 3
#define SNGL 4
#define DBLR 5
#define EXTR 6
#define SDEP 7
#define NONE 8
#define E 1
#define Eind 2
#define El 3
#define Ew 4
#define Eb 5
#define R 6
#define Rw 7
#define Ri 8
#define S 9
#define Si 10
#define A 11
#define BX 12
#define CL 13
#define DX 14
#define SI 15
#define DI 16
#define CR 17
#define DR 18
#define TR 19
#define I 20
#define Is 21
#define Ib 22
#define Ibs 23
#define Iw 24
#define Il 25
#define O 26
#define Db 27
#define Dl 28
#define o1 29
#define o3 30
#define OS 31
#define ST 32
#define STI 33
#define X 34
#define XA 35
#define Iba 36
struct inst {
char * i_name;
short i_has_modrm;
short i_size;
int i_mode;
char * i_extra;
};
#define op1(x) (x)
#define op2(x,y) ((x)|((y)<<8))
#define op3(x,y,z) ((x)|((y)<<8)|((z)<<16))
struct finst {
char * f_name;
int f_size;
int f_rrmode;
char * f_rrname;
};
char * db_Grp6[] = {
"sldt",
"str",
"lldt",
"ltr",
"verr",
"verw",
"",
""
};
char * db_Grp7[] = {
"sgdt",
"sidt",
"lgdt",
"lidt",
"smsw",
"",
"lmsw",
"invlpg"
};
char * db_Grp8[] = {
"",
"",
"",
"",
"bt",
"bts",
"btr",
"btc"
};
struct inst db_inst_0f0x[] = {
{ "", TRUE, NONE, op1(Ew), (char *)db_Grp6 },
{ "", TRUE, NONE, op1(Ew), (char *)db_Grp7 },
{ "lar", TRUE, LONG, op2(E,R), 0 },
{ "lsl", TRUE, LONG, op2(E,R), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "clts", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "invd", FALSE, NONE, 0, 0 },
{ "wbinvd",FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "ud2", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
};
struct inst db_inst_0f2x[] = {
{ "mov", TRUE, LONG, op2(CR,El), 0 },
{ "mov", TRUE, LONG, op2(DR,El), 0 },
{ "mov", TRUE, LONG, op2(El,CR), 0 },
{ "mov", TRUE, LONG, op2(El,DR), 0 },
{ "mov", TRUE, LONG, op2(TR,El), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "mov", TRUE, LONG, op2(El,TR), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
};
struct inst db_inst_0f8x[] = {
{ "jo", FALSE, NONE, op1(Dl), 0 },
{ "jno", FALSE, NONE, op1(Dl), 0 },
{ "jb", FALSE, NONE, op1(Dl), 0 },
{ "jnb", FALSE, NONE, op1(Dl), 0 },
{ "jz", FALSE, NONE, op1(Dl), 0 },
{ "jnz", FALSE, NONE, op1(Dl), 0 },
{ "jbe", FALSE, NONE, op1(Dl), 0 },
{ "jnbe", FALSE, NONE, op1(Dl), 0 },
{ "js", FALSE, NONE, op1(Dl), 0 },
{ "jns", FALSE, NONE, op1(Dl), 0 },
{ "jp", FALSE, NONE, op1(Dl), 0 },
{ "jnp", FALSE, NONE, op1(Dl), 0 },
{ "jl", FALSE, NONE, op1(Dl), 0 },
{ "jnl", FALSE, NONE, op1(Dl), 0 },
{ "jle", FALSE, NONE, op1(Dl), 0 },
{ "jnle", FALSE, NONE, op1(Dl), 0 },
};
struct inst db_inst_0f9x[] = {
{ "seto", TRUE, NONE, op1(Eb), 0 },
{ "setno", TRUE, NONE, op1(Eb), 0 },
{ "setb", TRUE, NONE, op1(Eb), 0 },
{ "setnb", TRUE, NONE, op1(Eb), 0 },
{ "setz", TRUE, NONE, op1(Eb), 0 },
{ "setnz", TRUE, NONE, op1(Eb), 0 },
{ "setbe", TRUE, NONE, op1(Eb), 0 },
{ "setnbe",TRUE, NONE, op1(Eb), 0 },
{ "sets", TRUE, NONE, op1(Eb), 0 },
{ "setns", TRUE, NONE, op1(Eb), 0 },
{ "setp", TRUE, NONE, op1(Eb), 0 },
{ "setnp", TRUE, NONE, op1(Eb), 0 },
{ "setl", TRUE, NONE, op1(Eb), 0 },
{ "setnl", TRUE, NONE, op1(Eb), 0 },
{ "setle", TRUE, NONE, op1(Eb), 0 },
{ "setnle",TRUE, NONE, op1(Eb), 0 },
};
struct inst db_inst_0fax[] = {
{ "push", FALSE, NONE, op1(Si), 0 },
{ "pop", FALSE, NONE, op1(Si), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "bt", TRUE, LONG, op2(R,E), 0 },
{ "shld", TRUE, LONG, op3(Ib,E,R), 0 },
{ "shld", TRUE, LONG, op3(CL,E,R), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "push", FALSE, NONE, op1(Si), 0 },
{ "pop", FALSE, NONE, op1(Si), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "bts", TRUE, LONG, op2(R,E), 0 },
{ "shrd", TRUE, LONG, op3(Ib,E,R), 0 },
{ "shrd", TRUE, LONG, op3(CL,E,R), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "imul", TRUE, LONG, op2(E,R), 0 },
};
struct inst db_inst_0fbx[] = {
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "lss", TRUE, LONG, op2(E, R), 0 },
{ "btr", TRUE, LONG, op2(R, E), 0 },
{ "lfs", TRUE, LONG, op2(E, R), 0 },
{ "lgs", TRUE, LONG, op2(E, R), 0 },
{ "movzb", TRUE, LONG, op2(Eb,R), 0 },
{ "movzw", TRUE, LONG, op2(Ew,R), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", TRUE, LONG, op2(Ibs,E), (char *)db_Grp8 },
{ "btc", TRUE, LONG, op2(R, E), 0 },
{ "bsf", TRUE, LONG, op2(E, R), 0 },
{ "bsr", TRUE, LONG, op2(E, R), 0 },
{ "movsb", TRUE, LONG, op2(Eb,R), 0 },
{ "movsw", TRUE, LONG, op2(Ew,R), 0 },
};
struct inst db_inst_0fcx[] = {
{ "xadd", TRUE, BYTE, op2(R, E), 0 },
{ "xadd", TRUE, LONG, op2(R, E), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
{ "bswap", FALSE, LONG, op1(Ri), 0 },
};
struct inst db_inst_0fdx[] = {
{ "cmpxchg",TRUE, BYTE, op2(R, E), 0 },
{ "cmpxchg",TRUE, LONG, op2(R, E), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
};
struct inst *db_inst_0f[] = {
db_inst_0f0x,
0,
db_inst_0f2x,
0,
0,
0,
0,
0,
db_inst_0f8x,
db_inst_0f9x,
db_inst_0fax,
db_inst_0fbx,
db_inst_0fcx,
db_inst_0fdx,
0,
0
};
char * db_Esc92[] = {
"fnop", "", "", "", "", "", "", ""
};
char * db_Esc93[] = {
"", "", "", "", "", "", "", ""
};
char * db_Esc94[] = {
"fchs", "fabs", "", "", "ftst", "fxam", "", ""
};
char * db_Esc95[] = {
"fld1", "fldl2t","fldl2e","fldpi","fldlg2","fldln2","fldz",""
};
char * db_Esc96[] = {
"f2xm1","fyl2x","fptan","fpatan","fxtract","fprem1","fdecstp",
"fincstp"
};
char * db_Esc97[] = {
"fprem","fyl2xp1","fsqrt","fsincos","frndint","fscale","fsin","fcos"
};
char * db_Esca4[] = {
"", "fucompp","", "", "", "", "", ""
};
char * db_Escb4[] = {
"", "", "fnclex","fninit","", "", "", ""
};
char * db_Esce3[] = {
"", "fcompp","", "", "", "", "", ""
};
char * db_Escf4[] = {
"fnstsw","", "", "", "", "", "", ""
};
struct finst db_Esc8[] = {
{ "fadd", SNGL, op2(STI,ST), 0 },
{ "fmul", SNGL, op2(STI,ST), 0 },
{ "fcom", SNGL, op2(STI,ST), 0 },
{ "fcomp", SNGL, op2(STI,ST), 0 },
{ "fsub", SNGL, op2(STI,ST), 0 },
{ "fsubr", SNGL, op2(STI,ST), 0 },
{ "fdiv", SNGL, op2(STI,ST), 0 },
{ "fdivr", SNGL, op2(STI,ST), 0 },
};
struct finst db_Esc9[] = {
{ "fld", SNGL, op1(STI), 0 },
{ "", NONE, op1(STI), "fxch" },
{ "fst", SNGL, op1(X), (char *)db_Esc92 },
{ "fstp", SNGL, op1(X), (char *)db_Esc93 },
{ "fldenv", NONE, op1(X), (char *)db_Esc94 },
{ "fldcw", NONE, op1(X), (char *)db_Esc95 },
{ "fnstenv",NONE, op1(X), (char *)db_Esc96 },
{ "fnstcw", NONE, op1(X), (char *)db_Esc97 },
};
struct finst db_Esca[] = {
{ "fiadd", WORD, 0, 0 },
{ "fimul", WORD, 0, 0 },
{ "ficom", WORD, 0, 0 },
{ "ficomp", WORD, 0, 0 },
{ "fisub", WORD, op1(X), (char *)db_Esca4 },
{ "fisubr", WORD, 0, 0 },
{ "fidiv", WORD, 0, 0 },
{ "fidivr", WORD, 0, 0 }
};
struct finst db_Escb[] = {
{ "fild", WORD, 0, 0 },
{ "", NONE, 0, 0 },
{ "fist", WORD, 0, 0 },
{ "fistp", WORD, 0, 0 },
{ "", WORD, op1(X), (char *)db_Escb4 },
{ "fld", EXTR, 0, 0 },
{ "", WORD, 0, 0 },
{ "fstp", EXTR, 0, 0 },
};
struct finst db_Escc[] = {
{ "fadd", DBLR, op2(ST,STI), 0 },
{ "fmul", DBLR, op2(ST,STI), 0 },
{ "fcom", DBLR, op2(ST,STI), 0 },
{ "fcomp", DBLR, op2(ST,STI), 0 },
{ "fsub", DBLR, op2(ST,STI), "fsubr" },
{ "fsubr", DBLR, op2(ST,STI), "fsub" },
{ "fdiv", DBLR, op2(ST,STI), "fdivr" },
{ "fdivr", DBLR, op2(ST,STI), "fdiv" },
};
struct finst db_Escd[] = {
{ "fld", DBLR, op1(STI), "ffree" },
{ "", NONE, 0, 0 },
{ "fst", DBLR, op1(STI), 0 },
{ "fstp", DBLR, op1(STI), 0 },
{ "frstor", NONE, op1(STI), "fucom" },
{ "", NONE, op1(STI), "fucomp" },
{ "fnsave", NONE, 0, 0 },
{ "fnstsw", NONE, 0, 0 },
};
struct finst db_Esce[] = {
{ "fiadd", LONG, op2(ST,STI), "faddp" },
{ "fimul", LONG, op2(ST,STI), "fmulp" },
{ "ficom", LONG, 0, 0 },
{ "ficomp", LONG, op1(X), (char *)db_Esce3 },
{ "fisub", LONG, op2(ST,STI), "fsubrp" },
{ "fisubr", LONG, op2(ST,STI), "fsubp" },
{ "fidiv", LONG, op2(ST,STI), "fdivrp" },
{ "fidivr", LONG, op2(ST,STI), "fdivp" },
};
struct finst db_Escf[] = {
{ "fild", LONG, 0, 0 },
{ "", LONG, 0, 0 },
{ "fist", LONG, 0, 0 },
{ "fistp", LONG, 0, 0 },
{ "fbld", NONE, op1(XA), (char *)db_Escf4 },
{ "fld", QUAD, 0, 0 },
{ "fbstp", NONE, 0, 0 },
{ "fstp", QUAD, 0, 0 },
};
struct finst *db_Esc_inst[] = {
db_Esc8, db_Esc9, db_Esca, db_Escb,
db_Escc, db_Escd, db_Esce, db_Escf
};
char * db_Grp1[] = {
"add",
"or",
"adc",
"sbb",
"and",
"sub",
"xor",
"cmp"
};
char * db_Grp2[] = {
"rol",
"ror",
"rcl",
"rcr",
"shl",
"shr",
"shl",
"sar"
};
struct inst db_Grp3[] = {
{ "test", TRUE, NONE, op2(I,E), 0 },
{ "test", TRUE, NONE, op2(I,E), 0 },
{ "not", TRUE, NONE, op1(E), 0 },
{ "neg", TRUE, NONE, op1(E), 0 },
{ "mul", TRUE, NONE, op2(E,A), 0 },
{ "imul", TRUE, NONE, op2(E,A), 0 },
{ "div", TRUE, NONE, op2(E,A), 0 },
{ "idiv", TRUE, NONE, op2(E,A), 0 },
};
struct inst db_Grp4[] = {
{ "inc", TRUE, BYTE, op1(E), 0 },
{ "dec", TRUE, BYTE, op1(E), 0 },
{ "", TRUE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, 0 }
};
struct inst db_Grp5[] = {
{ "inc", TRUE, LONG, op1(E), 0 },
{ "dec", TRUE, LONG, op1(E), 0 },
{ "call", TRUE, NONE, op1(Eind),0 },
{ "lcall", TRUE, NONE, op1(Eind),0 },
{ "jmp", TRUE, NONE, op1(Eind),0 },
{ "ljmp", TRUE, NONE, op1(Eind),0 },
{ "push", TRUE, LONG, op1(E), 0 },
{ "", TRUE, NONE, 0, 0 }
};
struct inst db_inst_table[256] = {
{ "add", TRUE, BYTE, op2(R, E), 0 },
{ "add", TRUE, LONG, op2(R, E), 0 },
{ "add", TRUE, BYTE, op2(E, R), 0 },
{ "add", TRUE, LONG, op2(E, R), 0 },
{ "add", FALSE, BYTE, op2(Is, A), 0 },
{ "add", FALSE, LONG, op2(Is, A), 0 },
{ "push", FALSE, NONE, op1(Si), 0 },
{ "pop", FALSE, NONE, op1(Si), 0 },
{ "or", TRUE, BYTE, op2(R, E), 0 },
{ "or", TRUE, LONG, op2(R, E), 0 },
{ "or", TRUE, BYTE, op2(E, R), 0 },
{ "or", TRUE, LONG, op2(E, R), 0 },
{ "or", FALSE, BYTE, op2(I, A), 0 },
{ "or", FALSE, LONG, op2(I, A), 0 },
{ "push", FALSE, NONE, op1(Si), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "adc", TRUE, BYTE, op2(R, E), 0 },
{ "adc", TRUE, LONG, op2(R, E), 0 },
{ "adc", TRUE, BYTE, op2(E, R), 0 },
{ "adc", TRUE, LONG, op2(E, R), 0 },
{ "adc", FALSE, BYTE, op2(Is, A), 0 },
{ "adc", FALSE, LONG, op2(Is, A), 0 },
{ "push", FALSE, NONE, op1(Si), 0 },
{ "pop", FALSE, NONE, op1(Si), 0 },
{ "sbb", TRUE, BYTE, op2(R, E), 0 },
{ "sbb", TRUE, LONG, op2(R, E), 0 },
{ "sbb", TRUE, BYTE, op2(E, R), 0 },
{ "sbb", TRUE, LONG, op2(E, R), 0 },
{ "sbb", FALSE, BYTE, op2(Is, A), 0 },
{ "sbb", FALSE, LONG, op2(Is, A), 0 },
{ "push", FALSE, NONE, op1(Si), 0 },
{ "pop", FALSE, NONE, op1(Si), 0 },
{ "and", TRUE, BYTE, op2(R, E), 0 },
{ "and", TRUE, LONG, op2(R, E), 0 },
{ "and", TRUE, BYTE, op2(E, R), 0 },
{ "and", TRUE, LONG, op2(E, R), 0 },
{ "and", FALSE, BYTE, op2(I, A), 0 },
{ "and", FALSE, LONG, op2(I, A), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "aaa", FALSE, NONE, 0, 0 },
{ "sub", TRUE, BYTE, op2(R, E), 0 },
{ "sub", TRUE, LONG, op2(R, E), 0 },
{ "sub", TRUE, BYTE, op2(E, R), 0 },
{ "sub", TRUE, LONG, op2(E, R), 0 },
{ "sub", FALSE, BYTE, op2(Is, A), 0 },
{ "sub", FALSE, LONG, op2(Is, A), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "das", FALSE, NONE, 0, 0 },
{ "xor", TRUE, BYTE, op2(R, E), 0 },
{ "xor", TRUE, LONG, op2(R, E), 0 },
{ "xor", TRUE, BYTE, op2(E, R), 0 },
{ "xor", TRUE, LONG, op2(E, R), 0 },
{ "xor", FALSE, BYTE, op2(I, A), 0 },
{ "xor", FALSE, LONG, op2(I, A), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "daa", FALSE, NONE, 0, 0 },
{ "cmp", TRUE, BYTE, op2(R, E), 0 },
{ "cmp", TRUE, LONG, op2(R, E), 0 },
{ "cmp", TRUE, BYTE, op2(E, R), 0 },
{ "cmp", TRUE, LONG, op2(E, R), 0 },
{ "cmp", FALSE, BYTE, op2(Is, A), 0 },
{ "cmp", FALSE, LONG, op2(Is, A), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "aas", FALSE, NONE, 0, 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "inc", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "dec", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "push", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pop", FALSE, LONG, op1(Ri), 0 },
{ "pusha", FALSE, LONG, 0, 0 },
{ "popa", FALSE, LONG, 0, 0 },
{ "bound", TRUE, LONG, op2(E, R), 0 },
{ "arpl", TRUE, NONE, op2(Ew,Rw), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "push", FALSE, LONG, op1(I), 0 },
{ "imul", TRUE, LONG, op3(I,E,R), 0 },
{ "push", FALSE, LONG, op1(Ib), 0 },
{ "imul", TRUE, LONG, op3(Ibs,E,R),0 },
{ "ins", FALSE, BYTE, op2(DX, DI), 0 },
{ "ins", FALSE, LONG, op2(DX, DI), 0 },
{ "outs", FALSE, BYTE, op2(SI, DX), 0 },
{ "outs", FALSE, LONG, op2(SI, DX), 0 },
{ "jo", FALSE, NONE, op1(Db), 0 },
{ "jno", FALSE, NONE, op1(Db), 0 },
{ "jb", FALSE, NONE, op1(Db), 0 },
{ "jnb", FALSE, NONE, op1(Db), 0 },
{ "jz", FALSE, NONE, op1(Db), 0 },
{ "jnz", FALSE, NONE, op1(Db), 0 },
{ "jbe", FALSE, NONE, op1(Db), 0 },
{ "jnbe", FALSE, NONE, op1(Db), 0 },
{ "js", FALSE, NONE, op1(Db), 0 },
{ "jns", FALSE, NONE, op1(Db), 0 },
{ "jp", FALSE, NONE, op1(Db), 0 },
{ "jnp", FALSE, NONE, op1(Db), 0 },
{ "jl", FALSE, NONE, op1(Db), 0 },
{ "jnl", FALSE, NONE, op1(Db), 0 },
{ "jle", FALSE, NONE, op1(Db), 0 },
{ "jnle", FALSE, NONE, op1(Db), 0 },
{ "", TRUE, BYTE, op2(I, E), (char *)db_Grp1 },
{ "", TRUE, LONG, op2(I, E), (char *)db_Grp1 },
{ "", TRUE, BYTE, op2(Is,E), (char *)db_Grp1 },
{ "", TRUE, LONG, op2(Ibs,E), (char *)db_Grp1 },
{ "test", TRUE, BYTE, op2(R, E), 0 },
{ "test", TRUE, LONG, op2(R, E), 0 },
{ "xchg", TRUE, BYTE, op2(R, E), 0 },
{ "xchg", TRUE, LONG, op2(R, E), 0 },
{ "mov", TRUE, BYTE, op2(R, E), 0 },
{ "mov", TRUE, LONG, op2(R, E), 0 },
{ "mov", TRUE, BYTE, op2(E, R), 0 },
{ "mov", TRUE, LONG, op2(E, R), 0 },
{ "mov", TRUE, NONE, op2(S, Ew), 0 },
{ "lea", TRUE, LONG, op2(E, R), 0 },
{ "mov", TRUE, NONE, op2(Ew, S), 0 },
{ "pop", TRUE, LONG, op1(E), 0 },
{ "nop", FALSE, NONE, 0, 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "xchg", FALSE, LONG, op2(A, Ri), 0 },
{ "cbw", FALSE, SDEP, 0, "cwde" },
{ "cwd", FALSE, SDEP, 0, "cdq" },
{ "lcall", FALSE, NONE, op1(OS), 0 },
{ "wait", FALSE, NONE, 0, 0 },
{ "pushf", FALSE, LONG, 0, 0 },
{ "popf", FALSE, LONG, 0, 0 },
{ "sahf", FALSE, NONE, 0, 0 },
{ "lahf", FALSE, NONE, 0, 0 },
{ "mov", FALSE, BYTE, op2(O, A), 0 },
{ "mov", FALSE, LONG, op2(O, A), 0 },
{ "mov", FALSE, BYTE, op2(A, O), 0 },
{ "mov", FALSE, LONG, op2(A, O), 0 },
{ "movs", FALSE, BYTE, op2(SI,DI), 0 },
{ "movs", FALSE, LONG, op2(SI,DI), 0 },
{ "cmps", FALSE, BYTE, op2(SI,DI), 0 },
{ "cmps", FALSE, LONG, op2(SI,DI), 0 },
{ "test", FALSE, BYTE, op2(I, A), 0 },
{ "test", FALSE, LONG, op2(I, A), 0 },
{ "stos", FALSE, BYTE, op1(DI), 0 },
{ "stos", FALSE, LONG, op1(DI), 0 },
{ "lods", FALSE, BYTE, op1(SI), 0 },
{ "lods", FALSE, LONG, op1(SI), 0 },
{ "scas", FALSE, BYTE, op1(DI), 0 },
{ "scas", FALSE, LONG, op1(DI), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, BYTE, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "mov", FALSE, LONG, op2(I, Ri), 0 },
{ "", TRUE, BYTE, op2(Ib, E), (char *)db_Grp2 },
{ "", TRUE, LONG, op2(Ib, E), (char *)db_Grp2 },
{ "ret", FALSE, NONE, op1(Iw), 0 },
{ "ret", FALSE, NONE, 0, 0 },
{ "les", TRUE, LONG, op2(E, R), 0 },
{ "lds", TRUE, LONG, op2(E, R), 0 },
{ "mov", TRUE, BYTE, op2(I, E), 0 },
{ "mov", TRUE, LONG, op2(I, E), 0 },
{ "enter", FALSE, NONE, op2(Ib, Iw), 0 },
{ "leave", FALSE, NONE, 0, 0 },
{ "lret", FALSE, NONE, op1(Iw), 0 },
{ "lret", FALSE, NONE, 0, 0 },
{ "int", FALSE, NONE, op1(o3), 0 },
{ "int", FALSE, NONE, op1(Ib), 0 },
{ "into", FALSE, NONE, 0, 0 },
{ "iret", FALSE, NONE, 0, 0 },
{ "", TRUE, BYTE, op2(o1, E), (char *)db_Grp2 },
{ "", TRUE, LONG, op2(o1, E), (char *)db_Grp2 },
{ "", TRUE, BYTE, op2(CL, E), (char *)db_Grp2 },
{ "", TRUE, LONG, op2(CL, E), (char *)db_Grp2 },
{ "aam", FALSE, NONE, op1(Iba), 0 },
{ "aad", FALSE, NONE, op1(Iba), 0 },
{ "", FALSE, NONE, 0, 0 },
{ "xlat", FALSE, BYTE, op1(BX), 0 },
{ "", TRUE, NONE, 0, (char *)db_Esc8 },
{ "", TRUE, NONE, 0, (char *)db_Esc9 },
{ "", TRUE, NONE, 0, (char *)db_Esca },
{ "", TRUE, NONE, 0, (char *)db_Escb },
{ "", TRUE, NONE, 0, (char *)db_Escc },
{ "", TRUE, NONE, 0, (char *)db_Escd },
{ "", TRUE, NONE, 0, (char *)db_Esce },
{ "", TRUE, NONE, 0, (char *)db_Escf },
{ "loopne",FALSE, NONE, op1(Db), 0 },
{ "loope", FALSE, NONE, op1(Db), 0 },
{ "loop", FALSE, NONE, op1(Db), 0 },
{ "jcxz", FALSE, SDEP, op1(Db), "jecxz" },
{ "in", FALSE, BYTE, op2(Ib, A), 0 },
{ "in", FALSE, LONG, op2(Ib, A) , 0 },
{ "out", FALSE, BYTE, op2(A, Ib), 0 },
{ "out", FALSE, LONG, op2(A, Ib) , 0 },
{ "call", FALSE, NONE, op1(Dl), 0 },
{ "jmp", FALSE, NONE, op1(Dl), 0 },
{ "ljmp", FALSE, NONE, op1(OS), 0 },
{ "jmp", FALSE, NONE, op1(Db), 0 },
{ "in", FALSE, BYTE, op2(DX, A), 0 },
{ "in", FALSE, LONG, op2(DX, A) , 0 },
{ "out", FALSE, BYTE, op2(A, DX), 0 },
{ "out", FALSE, LONG, op2(A, DX) , 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "", FALSE, NONE, 0, 0 },
{ "hlt", FALSE, NONE, 0, 0 },
{ "cmc", FALSE, NONE, 0, 0 },
{ "", TRUE, BYTE, 0, (char *)db_Grp3 },
{ "", TRUE, LONG, 0, (char *)db_Grp3 },
{ "clc", FALSE, NONE, 0, 0 },
{ "stc", FALSE, NONE, 0, 0 },
{ "cli", FALSE, NONE, 0, 0 },
{ "sti", FALSE, NONE, 0, 0 },
{ "cld", FALSE, NONE, 0, 0 },
{ "std", FALSE, NONE, 0, 0 },
{ "", TRUE, NONE, 0, (char *)db_Grp4 },
{ "", TRUE, NONE, 0, (char *)db_Grp5 },
};
struct inst db_bad_inst =
{ "???", FALSE, NONE, 0, 0 }
;
#define f_mod(byte) ((byte)>>6)
#define f_reg(byte) (((byte)>>3)&0x7)
#define f_rm(byte) ((byte)&0x7)
#define sib_ss(byte) ((byte)>>6)
#define sib_index(byte) (((byte)>>3)&0x7)
#define sib_base(byte) ((byte)&0x7)
struct i_addr {
int is_reg;
int disp;
char * base;
char * index;
int ss;
};
char * db_index_reg_16[8] = {
"%bx,%si",
"%bx,%di",
"%bp,%si",
"%bp,%di",
"%si",
"%di",
"%bp",
"%bx"
};
char * db_reg[3][8] = {
{ "%al", "%cl", "%dl", "%bl", "%ah", "%ch", "%dh", "%bh" },
{ "%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di" },
{ "%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi" }
};
char * db_seg_reg[8] = {
"%es", "%cs", "%ss", "%ds", "%fs", "%gs", "", ""
};
int db_lengths[] = {
1,
2,
4,
8,
4,
8,
10,
};
#define get_value_inc(result, loc, size, is_signed, task) \
MACRO_BEGIN \
result = db_get_task_value((loc), (size), (is_signed), (task)); \
(loc) += (size); \
MACRO_END
static db_addr_t
db_read_address(
db_addr_t loc,
int short_addr,
int regmodrm,
struct i_addr *addrp,
task_t task)
{
int mod, rm, sib, index, disp;
mod = f_mod(regmodrm);
rm = f_rm(regmodrm);
if (mod == 3) {
addrp->is_reg = TRUE;
addrp->disp = rm;
return loc;
}
addrp->is_reg = FALSE;
addrp->index = 0;
if (short_addr) {
addrp->index = 0;
addrp->ss = 0;
switch (mod) {
case 0:
if (rm == 6) {
get_value_inc(disp, loc, 2, TRUE, task);
addrp->disp = disp;
addrp->base = 0;
}
else {
addrp->disp = 0;
addrp->base = db_index_reg_16[rm];
}
break;
case 1:
get_value_inc(disp, loc, 1, TRUE, task);
addrp->disp = disp;
addrp->base = db_index_reg_16[rm];
break;
case 2:
get_value_inc(disp, loc, 2, TRUE, task);
addrp->disp = disp;
addrp->base = db_index_reg_16[rm];
break;
}
}
else {
if (mod != 3 && rm == 4) {
get_value_inc(sib, loc, 1, FALSE, task);
rm = sib_base(sib);
index = sib_index(sib);
if (index != 4)
addrp->index = db_reg[LONG][index];
addrp->ss = sib_ss(sib);
}
switch (mod) {
case 0:
if (rm == 5) {
get_value_inc(addrp->disp, loc, 4, FALSE, task);
addrp->base = 0;
}
else {
addrp->disp = 0;
addrp->base = db_reg[LONG][rm];
}
break;
case 1:
get_value_inc(disp, loc, 1, TRUE, task);
addrp->disp = disp;
addrp->base = db_reg[LONG][rm];
break;
case 2:
get_value_inc(disp, loc, 4, FALSE, task);
addrp->disp = disp;
addrp->base = db_reg[LONG][rm];
break;
}
}
return loc;
}
static void
db_print_address(
const char * seg,
int size,
const struct i_addr *addrp,
task_t task)
{
if (addrp->is_reg) {
db_printf("%s", db_reg[size][addrp->disp]);
return;
}
if (seg) {
db_printf("%s:", seg);
}
if (addrp->base != 0 || addrp->index != 0) {
db_printf("%#n", addrp->disp);
db_printf("(");
if (addrp->base)
db_printf("%s", addrp->base);
if (addrp->index)
db_printf(",%s,%d", addrp->index, 1<<addrp->ss);
db_printf(")");
} else
db_task_printsym((db_addr_t)addrp->disp, DB_STGY_ANY, task);
}
static db_addr_t
db_disasm_esc(
db_addr_t loc,
int inst,
int short_addr,
int size,
const char * seg,
task_t task)
{
int regmodrm;
struct finst *fp;
int mod;
struct i_addr address;
char * name;
get_value_inc(regmodrm, loc, 1, FALSE, task);
fp = &db_Esc_inst[inst - 0xd8][f_reg(regmodrm)];
mod = f_mod(regmodrm);
if (mod != 3) {
loc = db_read_address(loc, short_addr, regmodrm, &address, task);
db_printf(fp->f_name);
switch(fp->f_size) {
case SNGL:
db_printf("s");
break;
case DBLR:
db_printf("l");
break;
case EXTR:
db_printf("t");
break;
case WORD:
db_printf("s");
break;
case LONG:
db_printf("l");
break;
case QUAD:
db_printf("q");
break;
default:
break;
}
db_printf("\t");
db_print_address(seg, BYTE, &address, task);
}
else {
switch (fp->f_rrmode) {
case op2(ST,STI):
name = (fp->f_rrname) ? fp->f_rrname : fp->f_name;
db_printf("%s\t%%st,%%st(%d)",name,f_rm(regmodrm));
break;
case op2(STI,ST):
name = (fp->f_rrname) ? fp->f_rrname : fp->f_name;
db_printf("%s\t%%st(%d),%%st",name, f_rm(regmodrm));
break;
case op1(STI):
name = (fp->f_rrname) ? fp->f_rrname : fp->f_name;
db_printf("%s\t%%st(%d)",name, f_rm(regmodrm));
break;
case op1(X):
db_printf("%s", ((char **)fp->f_rrname)[f_rm(regmodrm)]);
break;
case op1(XA):
db_printf("%s\t%%ax",
((char **)fp->f_rrname)[f_rm(regmodrm)]);
break;
default:
db_printf("<bad instruction>");
break;
}
}
return loc;
}
db_addr_t
db_disasm(
db_addr_t loc,
boolean_t altfmt,
task_t task)
{
int inst;
int size;
int short_addr;
char * seg;
struct inst * ip;
char * i_name;
int i_size;
int i_mode;
int regmodrm;
boolean_t first;
int displ;
int prefix;
int imm;
int imm2;
int len;
struct i_addr address;
#ifdef __x86_64__
db_printf("TODO\n");
return loc+1;
#endif
get_value_inc(inst, loc, 1, FALSE, task);
if (db_disasm_16) {
short_addr = TRUE;
size = WORD;
}
else {
short_addr = FALSE;
size = LONG;
}
seg = 0;
regmodrm = 0;
prefix = TRUE;
do {
switch (inst) {
case 0x66:
if (size == LONG)
size = WORD;
else
size = LONG;
break;
case 0x67:
short_addr = !short_addr;
break;
case 0x26:
seg = "%es";
break;
case 0x36:
seg = "%ss";
break;
case 0x2e:
seg = "%cs";
break;
case 0x3e:
seg = "%ds";
break;
case 0x64:
seg = "%fs";
break;
case 0x65:
seg = "%gs";
break;
case 0xf0:
db_printf("lock ");
break;
case 0xf2:
db_printf("repne ");
break;
case 0xf3:
db_printf("repe ");
break;
default:
prefix = FALSE;
break;
}
if (prefix) {
get_value_inc(inst, loc, 1, FALSE, task);
}
} while (prefix);
if (inst >= 0xd8 && inst <= 0xdf) {
loc = db_disasm_esc(loc, inst, short_addr, size, seg, task);
db_printf("\n");
return loc;
}
if (inst == 0x0f) {
get_value_inc(inst, loc, 1, FALSE, task);
ip = db_inst_0f[inst>>4];
if (ip == 0) {
ip = &db_bad_inst;
}
else {
ip = &ip[inst&0xf];
}
}
else
ip = &db_inst_table[inst];
if (ip->i_has_modrm) {
get_value_inc(regmodrm, loc, 1, FALSE, task);
loc = db_read_address(loc, short_addr, regmodrm, &address, task);
}
i_name = ip->i_name;
i_size = ip->i_size;
i_mode = ip->i_mode;
if (ip->i_extra == (char *)db_Grp1 ||
ip->i_extra == (char *)db_Grp2 ||
ip->i_extra == (char *)db_Grp6 ||
ip->i_extra == (char *)db_Grp7 ||
ip->i_extra == (char *)db_Grp8) {
i_name = ((char **)ip->i_extra)[f_reg(regmodrm)];
}
else if (ip->i_extra == (char *)db_Grp3) {
ip = (struct inst *)ip->i_extra;
ip = &ip[f_reg(regmodrm)];
i_name = ip->i_name;
i_mode = ip->i_mode;
}
else if (ip->i_extra == (char *)db_Grp4 ||
ip->i_extra == (char *)db_Grp5) {
ip = (struct inst *)ip->i_extra;
ip = &ip[f_reg(regmodrm)];
i_name = ip->i_name;
i_mode = ip->i_mode;
i_size = ip->i_size;
}
if (i_size == SDEP) {
if (size == WORD)
db_printf(i_name);
else
db_printf(ip->i_extra);
}
else {
db_printf(i_name);
if (i_size != NONE) {
if (i_size == BYTE) {
db_printf("b");
size = BYTE;
}
else if (i_size == WORD) {
db_printf("w");
size = WORD;
}
else if (size == WORD)
db_printf("w");
else
db_printf("l");
}
}
db_printf("\t");
for (first = TRUE;
i_mode != 0;
i_mode >>= 8, first = FALSE)
{
if (!first)
db_printf(",");
switch (i_mode & 0xFF) {
case E:
db_print_address(seg, size, &address, task);
break;
case Eind:
db_printf("*");
db_print_address(seg, size, &address, task);
break;
case El:
db_print_address(seg, LONG, &address, task);
break;
case Ew:
db_print_address(seg, WORD, &address, task);
break;
case Eb:
db_print_address(seg, BYTE, &address, task);
break;
case R:
db_printf("%s", db_reg[size][f_reg(regmodrm)]);
break;
case Rw:
db_printf("%s", db_reg[WORD][f_reg(regmodrm)]);
break;
case Ri:
db_printf("%s", db_reg[size][f_rm(inst)]);
break;
case S:
db_printf("%s", db_seg_reg[f_reg(regmodrm)]);
break;
case Si:
db_printf("%s", db_seg_reg[f_reg(inst)]);
break;
case A:
db_printf("%s", db_reg[size][0]);
break;
case BX:
if (seg)
db_printf("%s:", seg);
db_printf("(%s)", short_addr ? "%bx" : "%ebx");
break;
case CL:
db_printf("%%cl");
break;
case DX:
db_printf("%%dx");
break;
case SI:
if (seg)
db_printf("%s:", seg);
db_printf("(%s)", short_addr ? "%si" : "%esi");
break;
case DI:
db_printf("%%es:(%s)", short_addr ? "%di" : "%edi");
break;
case CR:
db_printf("%%cr%d", f_reg(regmodrm));
break;
case DR:
db_printf("%%dr%d", f_reg(regmodrm));
break;
case TR:
db_printf("%%tr%d", f_reg(regmodrm));
break;
case I:
len = db_lengths[size];
get_value_inc(imm, loc, len, FALSE, task);
db_printf("$%#n", imm);
break;
case Is:
len = db_lengths[size];
get_value_inc(imm, loc, len, TRUE, task);
db_printf("$%#r", imm);
break;
case Ib:
get_value_inc(imm, loc, 1, FALSE, task);
db_printf("$%#n", imm);
break;
case Iba:
get_value_inc(imm, loc, 1, FALSE, task);
if (imm != 0x0a)
db_printf("$%#r", imm);
break;
case Ibs:
get_value_inc(imm, loc, 1, TRUE, task);
db_printf("$%#r", imm);
break;
case Iw:
get_value_inc(imm, loc, 2, FALSE, task);
db_printf("$%#n", imm);
break;
case Il:
get_value_inc(imm, loc, 4, FALSE, task);
db_printf("$%#n", imm);
break;
case O:
if (short_addr) {
get_value_inc(displ, loc, 2, TRUE, task);
}
else {
get_value_inc(displ, loc, 4, TRUE, task);
}
if (seg)
db_printf("%s:%#r",seg, displ);
else
db_task_printsym((db_addr_t)displ, DB_STGY_ANY, task);
break;
case Db:
get_value_inc(displ, loc, 1, TRUE, task);
if (short_addr) {
displ = (loc & 0xffff0000)
| ((loc + displ) & 0xffff);
}
else
displ = displ + loc;
db_task_printsym((db_addr_t)displ,DB_STGY_XTRN,task);
break;
case Dl:
if (short_addr) {
get_value_inc(displ, loc, 2, TRUE, task);
displ = (loc & 0xffff0000)
| ((loc + displ) & 0xffff);
}
else {
get_value_inc(displ, loc, 4, TRUE, task);
displ = displ + loc;
}
db_task_printsym((db_addr_t)displ, DB_STGY_XTRN, task);
break;
case o1:
db_printf("$1");
break;
case o3:
db_printf("$3");
break;
case OS:
if (short_addr) {
get_value_inc(imm, loc, 2, FALSE, task);
}
else {
get_value_inc(imm, loc, 4, FALSE, task);
}
get_value_inc(imm2, loc, 2, FALSE, task);
db_printf("$%#n,%#n", imm2, imm);
break;
}
}
if (altfmt == 0 && !db_disasm_16) {
if (inst == 0xe9 || inst == 0xeb) {
loc = (loc + (4-1)) & ~(4-1);
}
}
db_printf("\n");
return loc;
}
#endif