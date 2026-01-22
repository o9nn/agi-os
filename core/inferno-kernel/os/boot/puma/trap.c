#include "boot.h"
typedef struct IrqEntry {
void (*r)(Ureg*, void*);
void *a;
} IrqEntry;
IrqEntry Irq[V_MAXNUM+1];
static void dumpstk(ulong *);
void dumpregs(Ureg* ureg);
void
setvec(int v, void (*f)(Ureg*, void*), void* a)
{
if(v < 0 || v >= V_MAXNUM)
panic("setvec: interrupt source %d out of range\n", v);
Irq[v].r = f;
Irq[v].a = a;
}
ulong irqstack[64];
ulong fiqstack[64];
ulong abtstack[64];
ulong undstack[64];
static void
safeintr(Ureg*, void *a)
{
int v = (int)a;
USED(v);
}
void
trapinit(void)
{
int offset;
ulong op;
int v;
int s;
s = splhi();
setr13(PsrMirq, irqstack+nelem(irqstack)-1);
setr13(PsrMfiq, fiqstack+nelem(fiqstack)-1);
setr13(PsrMabt, abtstack+nelem(abtstack)-1);
setr13(PsrMund, undstack+nelem(undstack)-1);
for(v = 0; v <= V_MAXNUM; v++) {
Irq[v].r = safeintr;
Irq[v].a = (void *)v;
}
offset = ((((ulong) _vsvccall) - 0x0)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x0) = op;
offset = ((((ulong) _vundcall) - 0x4)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x4) = op;
offset = ((((ulong) _vsvccall) - 0x8)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x8) = op;
offset = ((((ulong) _vpabcall) - 0xc)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0xc) = op;
offset = ((((ulong) _vdabcall) - 0x10)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x10) = op;
offset = ((((ulong) _virqcall) - 0x18)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x18) = op;
offset = ((((ulong) _vfiqcall) - 0x1c)-8) >> 2;
op = ( 0xea << 24 ) | offset;
*((ulong *) 0x1c) = op;
flushIcache();
writeBackDC();
flushDcache();
flushIcache();
drainWBuffer();
splx(s);
}
void
trap(Ureg* ureg)
{
ushort mask;
IrqEntry *ip;
ureg->pc -= 4;
switch(ureg->type) {
case PsrMirq:
mask = *(uchar*)HARI1 | ((*(uchar*)HARI2) << 8);
ip = Irq;
while (mask != 0) {
if(mask&1)
ip->r(ureg, ip->a);
ip++;
mask >>= 1;
}
break;
case PsrMfiq:
mask = *(uchar*)HARI1 & HARI1_FIQ_MASK;
ip = Irq;
while (mask != 0) {
if(mask&1)
ip->r(ureg, ip->a);
ip++;
mask >>= 1;
}
break;
case PsrMund:
dumpregs(ureg);
panic("Undefined Instruction Exception\n");
break;
case PsrMsvc:
dumpregs(ureg);
panic("SVC/SWI Exception\n");
break;
case PsrMabt:
ureg->pc -= 4;
case PsrMabt+1: {
uint far =0;
uint fsr =0;
USED(far,fsr);
fsr = 0;
far = 0;
if (ureg->type == PsrMabt)
print("Prefetch Abort/");
print("Data Abort\n");
print("Data Abort: FSR %8.8uX FAR %8.8uX\n", fsr, far);
}
default:
dumpregs(ureg);
panic("exception %uX\n", ureg->type);
break;
}
splhi();
}
void
dumpregs(Ureg* ureg)
{
print("PSR %8.8uX type %2.2uX PC %8.8uX LINK %8.8uX\n",
ureg->psr, ureg->type, ureg->pc, ureg->link);
print("R14 %8.8uX R13 %8.8uX R12 %8.8uX R11 %8.8uX R10 %8.8uX\n",
ureg->r14, ureg->r13, ureg->r12, ureg->r11, ureg->r10);
print("R9  %8.8uX R8  %8.8uX R7  %8.8uX R6  %8.8uX R5  %8.8uX\n",
ureg->r9, ureg->r8, ureg->r7, ureg->r6, ureg->r5);
print("R4  %8.8uX R3  %8.8uX R2  %8.8uX R1  %8.8uX R0  %8.8uX\n",
ureg->r4, ureg->r3, ureg->r2, ureg->r1, ureg->r0);
print("Stack is at: %8.8uX\n", ureg);
}