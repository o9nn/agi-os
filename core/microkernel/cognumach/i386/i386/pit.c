#include <kern/mach_clock.h>
#include <i386/ipl.h>
#include <machine/irq.h>
#include <i386/pit.h>
#include <i386/pio.h>
#include <kern/cpu_number.h>
int pitctl_port = PITCTL_PORT;
int pitctr0_port = PITCTR0_PORT;
int pit0_mode = PIT_C0|PIT_SQUAREMODE|PIT_READMODE ;
unsigned int clknumb = CLKNUM;
void
pit_prepare_sleep(int persec)
{
uint32_t val = 0;
uint8_t lsb, msb;
val = inb(PITAUX_PORT);
val &= ~PITAUX_OUT2;
val |= PITAUX_GATE2;
outb (PITAUX_PORT, val);
outb (PITCTL_PORT, PIT_C2 | PIT_LOADMODE | PIT_ONESHOTMODE);
val = CLKNUM / persec;
lsb = val & 0xff;
msb = val >> 8;
outb (PITCTR2_PORT, lsb);
val = inb(POST_PORT);
outb (PITCTR2_PORT, msb);
}
void
pit_sleep(void)
{
uint8_t val;
val = inb(PITAUX_PORT);
val &= ~PITAUX_GATE2;
outb (PITAUX_PORT, val);
val |= PITAUX_GATE2;
outb (PITAUX_PORT, val);
while ((inb(PITAUX_PORT) & PITAUX_VAL) == 0);
}
void
pit_udelay(int usec)
{
pit_prepare_sleep(1000000 / usec);
pit_sleep();
}
void
pit_mdelay(int msec)
{
pit_prepare_sleep(1000 / msec);
pit_sleep();
}
void
clkstart(void)
{
if (cpu_number() != 0)
return;
unsigned char byte;
unsigned long s;
s = sploff();
outb(pitctl_port, pit0_mode);
clknumb = (CLKNUM + hz / 2) / hz;
byte = clknumb;
outb(pitctr0_port, byte);
byte = clknumb>>8;
outb(pitctr0_port, byte);
splon(s);
}