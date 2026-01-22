#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
void intr0(void), intr1(void), intr2(void), intr3(void);
void intr4(void), intr5(void), intr6(void), intr7(void);
void intr8(void), intr9(void), intr10(void), intr11(void);
void intr12(void), intr13(void), intr14(void), intr15(void);
void intr16(void);
void intr24(void), intr25(void), intr26(void), intr27(void);
void intr28(void), intr29(void), intr30(void), intr31(void);
void intr32(void), intr33(void), intr34(void), intr35(void);
void intr36(void), intr37(void), intr38(void), intr39(void);
void intr64(void);
void intrbad(void);
enum
{
Int0ctl= 0x20,
Int0aux= 0x21,
Int1ctl= 0xA0,
Int1aux= 0xA1,
Icw1= 0x10,
Ocw2= 0x00,
Ocw3= 0x08,
EOI= 0x20,
Elcr1= 0x4D0,
Elcr2= 0x4D1,
};
int int0mask = 0xff;
int int1mask = 0xff;
int i8259elcr;
Segdesc ilt[256];
enum
{
Maxhandler= 32,
};
typedef struct Handler Handler;
struct Handler
{
void (*r)(Ureg*, void*);
void *arg;
Handler *next;
};
struct
{
Handler *ivec[256];
Handler h[Maxhandler];
int nextfree;
} halloc;
void
sethvec(int v, void (*r)(void), int type, int pri)
{
ilt[v].d0 = ((ulong)r)&0xFFFF|(KESEL<<16);
ilt[v].d1 = ((ulong)r)&0xFFFF0000|SEGP|SEGPL(pri)|type;
}
void
setvec(int v, void (*r)(Ureg*, void*), void *arg)
{
Handler *h;
if(halloc.nextfree >= Maxhandler)
panic("out of interrupt handlers");
h = &halloc.h[halloc.nextfree++];
h->next = halloc.ivec[v];
h->r = r;
h->arg = arg;
halloc.ivec[v] = h;
if((v&~0x7) == VectorPIC){
int0mask &= ~(1<<(v&7));
outb(Int0aux, int0mask);
} else if((v&~0x7) == VectorPIC+8){
int1mask &= ~(1<<(v&7));
outb(Int1aux, int1mask);
}
}
void
trapdisable(void)
{
outb(Int0aux, 0xFF);
outb(Int1aux, 0xFF);
}
void
trapenable(void)
{
outb(Int0aux, int0mask);
outb(Int1aux, int1mask);
}
void
trapinit(void)
{
int i, x;
for(i = 0; i < 256; i++)
sethvec(i, intrbad, SEGTG, 0);
sethvec(0, intr0, SEGTG, 0);
sethvec(1, intr1, SEGTG, 0);
sethvec(2, intr2, SEGTG, 0);
sethvec(3, intr3, SEGTG, 0);
sethvec(4, intr4, SEGTG, 0);
sethvec(5, intr5, SEGTG, 0);
sethvec(6, intr6, SEGTG, 0);
sethvec(7, intr7, SEGTG, 0);
sethvec(8, intr8, SEGTG, 0);
sethvec(9, intr9, SEGTG, 0);
sethvec(10, intr10, SEGTG, 0);
sethvec(11, intr11, SEGTG, 0);
sethvec(12, intr12, SEGTG, 0);
sethvec(13, intr13, SEGTG, 0);
sethvec(14, intr14, SEGTG, 0);
sethvec(15, intr15, SEGTG, 0);
sethvec(16, intr16, SEGTG, 0);
sethvec(24, intr24, SEGIG, 0);
sethvec(25, intr25, SEGIG, 0);
sethvec(26, intr26, SEGIG, 0);
sethvec(27, intr27, SEGIG, 0);
sethvec(28, intr28, SEGIG, 0);
sethvec(29, intr29, SEGIG, 0);
sethvec(30, intr30, SEGIG, 0);
sethvec(31, intr31, SEGIG, 0);
sethvec(32, intr32, SEGIG, 0);
sethvec(33, intr33, SEGIG, 0);
sethvec(34, intr34, SEGIG, 0);
sethvec(35, intr35, SEGIG, 0);
sethvec(36, intr36, SEGIG, 0);
sethvec(37, intr37, SEGIG, 0);
sethvec(38, intr38, SEGIG, 0);
sethvec(39, intr39, SEGIG, 0);
putidt(ilt, sizeof(ilt)-1);
outb(Int0ctl, Icw1|0x01);
outb(Int0aux, VectorPIC);
outb(Int0aux, 0x04);
outb(Int0aux, 0x01);
outb(Int1ctl, Icw1|0x01);
outb(Int1aux, VectorPIC+8);
outb(Int1aux, 0x02);
outb(Int1aux, 0x01);
outb(Int1aux, int1mask);
int0mask &= ~0x04;
outb(Int0aux, int0mask);
outb(Int0ctl, Ocw3|0x03);
outb(Int1ctl, Ocw3|0x03);
x = (inb(Elcr2)<<8)|inb(Elcr1);
if(!(x & 0x2107)){
outb(Elcr1, 0);
if(inb(Elcr1) == 0){
outb(Elcr1, 0x20);
if(inb(Elcr1) == 0x20)
i8259elcr = x;
outb(Elcr1, x & 0xFF);
print("ELCR: %4.4uX\n", i8259elcr);
}
}
}
static void
dumpregs(Ureg *ur)
{
print("FLAGS=%lux TRAP=%lux ECODE=%lux PC=%lux\n",
ur->flags, ur->trap, ur->ecode, ur->pc);
print("  AX %8.8lux  BX %8.8lux  CX %8.8lux  DX %8.8lux\n",
ur->ax, ur->bx, ur->cx, ur->dx);
print("  SI %8.8lux  DI %8.8lux  BP %8.8lux\n",
ur->si, ur->di, ur->bp);
print("  CS %4.4lux DS %4.4lux  ES %4.4lux  FS %4.4lux  GS %4.4lux\n",
ur->cs & 0xFF, ur->ds & 0xFFFF, ur->es & 0xFFFF, ur->fs & 0xFFFF, ur->gs & 0xFFFF);
print("  CR0 %8.8lux CR2 %8.8lux CR3 %8.8lux\n",
getcr0(), getcr2(), getcr3());
}
void
trap(Ureg *ur)
{
int v;
int c;
Handler *h;
ushort isr;
v = ur->trap;
c = v&~0x7;
isr = 0;
if(c==VectorPIC || c==VectorPIC+8){
isr = inb(Int0ctl);
outb(Int0ctl, EOI);
if(c == VectorPIC+8){
isr |= inb(Int1ctl)<<8;
outb(Int1ctl, EOI);
}
}
if(v>=256 || (h = halloc.ivec[v]) == 0){
if(v >= VectorPIC && v < VectorPIC+16){
v -= VectorPIC;
if(isr & (1<<v))
print("unknown interrupt %d pc=0x%lux\n", v, ur->pc);
return;
}
switch(v){
case 0x02:
print("NMI: nmisc=0x%2.2ux, nmiertc=0x%2.2ux, nmiesc=0x%2.2ux\n",
inb(0x61), inb(0x70), inb(0x461));
return;
default:
dumpregs(ur);
panic("exception/interrupt %d", v);
return;
}
}
do {
(*h->r)(ur, h->arg);
h = h->next;
} while(h);
}
extern void realmode0(void);
extern int realmodeintr;
extern Ureg realmoderegs;
void
realmode(int intr, Ureg *ureg)
{
realmoderegs = *ureg;
realmodeintr = intr;
trapdisable();
realmode0();
trapenable();
*ureg = realmoderegs;
}