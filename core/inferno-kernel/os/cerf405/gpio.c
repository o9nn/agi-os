#include	"u.h"
#include 	"mem.h"
#include	"../port/lib.h"
#include 	"dat.h"
#include	"fns.h"
#include	"io.h"
#define	GPIOREGS	((Gpioregs*)KADDR(PHYSGPIO))
static ulong gpioreserved;
static Lock gpiolock;
void
gpioreserve(ulong mask)
{
ilock(&gpiolock);
if(gpioreserved & mask)
panic("gpioreserve: duplicate use of 0x%.8lux", gpioreserved & mask);
gpioreserved |= mask;
iunlock(&gpiolock);
}
static ulong
inflate(ulong m)
{
m  = ((m & 0xFF00) << 8) | (m & 0x00FF);
m = ((m << 4) | m) & 0x0F0F0F0F;
m = ((m << 2) | m) & 0x33333333;
return ((m << 1) | m) & 0x55555555;
}
void
gpioconfig(ulong m, ulong cfg)
{
Gpioregs *g;
ulong h, hm, l, lm;
h = inflate(m>>16);
hm = h | (h<<1);
l = inflate(m);
lm = l | (l<<1);
ilock(&gpiolock);
g = GPIOREGS;
g->tsrh &= ~hm;
g->tsrl &= ~lm;
g->isr1h = (g->isr1h & ~hm) | h;
g->isr1l = (g->isr1l & ~lm) | l;
if(cfg & Gpio_Alt1){
g->osrh = (g->osrh & ~hm) | h;
g->osrl = (g->osrl & ~lm) | l;
}else{
g->osrh &= ~hm;
g->osrl &= ~lm;
}
if(cfg & Gpio_OD)
g->odr |= m;
else
g->odr &= ~m;
if(cfg & Gpio_in || cfg & Gpio_Tri)
g->tcr &= ~m;
else
g->tcr |= m;
iunlock(&gpiolock);
}
ulong
gpioget(ulong mask)
{
return GPIOREGS->ir & mask;
}
void
gpioset(ulong mask, ulong out)
{
Gpioregs *g;
ilock(&gpiolock);
g = GPIOREGS;
g->or = (g->or & ~mask) | (out & mask);
iunlock(&gpiolock);
}
void
gpiorelease(ulong mask)
{
ilock(&gpiolock);
if((gpioreserved & mask) != mask)
panic("gpiorelease: unexpected release of 0x%.8lux", ~gpioreserved & mask);
gpioreserved &= ~mask;
iunlock(&gpiolock);
}