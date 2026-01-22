#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
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
static Lock i8259lock;
static int i8259mask = 0xFFFF;
int i8259elcr;
void
i8259init(void)
{
int x;
ioalloc(Int0ctl, 2, 0, "i8259.0");
ioalloc(Int1ctl, 2, 0, "i8259.1");
ilock(&i8259lock);
outb(Int0ctl, (1<<4)|(0<<3)|(1<<0));
outb(Int0aux, VectorPIC);
outb(Int0aux, 0x04);
outb(Int0aux, 0x01);
outb(Int1ctl, (1<<4)|(0<<3)|(1<<0));
outb(Int1aux, VectorPIC+8);
outb(Int1aux, 0x02);
outb(Int1aux, 0x01);
outb(Int1aux, (i8259mask>>8) & 0xFF);
i8259mask &= ~0x04;
outb(Int0aux, i8259mask & 0xFF);
inb(Int0ctl);
outb(Int0ctl, Ocw3|0x03);
inb(Int1ctl);
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
iunlock(&i8259lock);
}
int
i8259isr(int vno)
{
int irq, isr;
if(vno < VectorPIC || vno > VectorPIC+MaxIrqPIC)
return 0;
irq = vno-VectorPIC;
ilock(&i8259lock);
isr = inb(Int0ctl);
outb(Int0ctl, EOI);
if(irq >= 8){
isr |= inb(Int1ctl)<<8;
outb(Int1ctl, EOI);
}
iunlock(&i8259lock);
return isr & (1<<irq);
}
int
i8259enable(Vctl* v)
{
int irq, irqbit;
irq = v->irq;
if(irq < 0 || irq > MaxIrqPIC){
print("i8259enable: irq %d out of range\n", irq);
return -1;
}
irqbit = 1<<irq;
ilock(&i8259lock);
if(!(i8259mask & irqbit) && !(i8259elcr & irqbit)){
print("i8259enable: irq %d shared but not level\n", irq);
iunlock(&i8259lock);
return -1;
}
i8259mask &= ~irqbit;
if(irq < 8)
outb(Int0aux, i8259mask & 0xFF);
else
outb(Int1aux, (i8259mask>>8) & 0xFF);
if(i8259elcr & irqbit)
v->eoi = i8259isr;
else
v->isr = i8259isr;
iunlock(&i8259lock);
return VectorPIC+irq;
}
int
i8259vecno(int irq)
{
return VectorPIC+irq;
}
int
i8259disable(int irq)
{
int irqbit;
if(irq < 0 || irq > MaxIrqPIC){
print("i8259disable: irq %d out of range\n", irq);
return -1;
}
irqbit = 1<<irq;
ilock(&i8259lock);
if(!(i8259mask & irqbit)){
i8259mask |= irqbit;
if(irq < 8)
outb(Int0aux, i8259mask & 0xFF);
else
outb(Int1aux, (i8259mask>>8) & 0xFF);
}
iunlock(&i8259lock);
return 0;
}