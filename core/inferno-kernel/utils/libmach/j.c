#include <lib9.h>
#include <bio.h>
#include "uregj.h"
#include <mach.h>
#define REGOFF(x) offsetof(struct Ureg, x)
#define REGSIZE sizeof(struct Ureg)
Reglist riscv64reglist[] = {
{"PC", REGOFF(pc), RINT, 'X'},
{"SP", REGOFF(r27), RINT, 'X'},
{"R31", REGOFF(r31), RINT, 'X'},
{"R30", REGOFF(r30), RINT, 'X'},
{"R28", REGOFF(r28), RINT, 'X'},
{"R27", REGOFF(r27), RINT, 'X'},
{"R26", REGOFF(r26), RINT, 'X'},
{"R25", REGOFF(r25), RINT, 'X'},
{"R24", REGOFF(r24), RINT, 'X'},
{"R23", REGOFF(r23), RINT, 'X'},
{"R22", REGOFF(r22), RINT, 'X'},
{"R21", REGOFF(r21), RINT, 'X'},
{"R20", REGOFF(r20), RINT, 'X'},
{"R19", REGOFF(r19), RINT, 'X'},
{"R18", REGOFF(r18), RINT, 'X'},
{"R17", REGOFF(r17), RINT, 'X'},
{"R16", REGOFF(r16), RINT, 'X'},
{"R15", REGOFF(r15), RINT, 'X'},
{"R14", REGOFF(r14), RINT, 'X'},
{"R13", REGOFF(r13), RINT, 'X'},
{"R12", REGOFF(r12), RINT, 'X'},
{"R11", REGOFF(r11), RINT, 'X'},
{"R10", REGOFF(r10), RINT, 'X'},
{"R9", REGOFF(r9), RINT, 'X'},
{"R8", REGOFF(r8), RINT, 'X'},
{"R7", REGOFF(r7), RINT, 'X'},
{"R6", REGOFF(r6), RINT, 'X'},
{"R5", REGOFF(r5), RINT, 'X'},
{"R4", REGOFF(r4), RINT, 'X'},
{"R3", REGOFF(r3), RINT, 'X'},
{"R2", REGOFF(r2), RINT, 'X'},
{"R1", REGOFF(r1), RINT, 'X'},
{ 0 }
};
Mach mriscv64 =
{
"riscv64",
MRISCV64,
riscv64reglist,
REGSIZE,
0,
"PC",
"SP",
"R1",
"setSB",
0,
0x1000,
0x80000000ULL,
0xC0000000ULL,
0x3FFFFFFFULL,
2,
8,
8,
4,
8,
};