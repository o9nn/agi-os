#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "ether8390.h"
enum {
Msr		= 0x00,
Icr		= 0x01,
Iar		= 0x02,
Bio		= 0x03,
Ear		= 0x03,
Irr		= 0x04,
Hcr		= 0x04,
Laar		= 0x05,
Ijr		= 0x06,
Gp2		= 0x07,
Lar		= 0x08,
Id		= 0x0E,
Cksum		= 0x0F,
};
enum {
Rst		= 0x80,
Menb		= 0x40,
};
enum {
Bit16		= 0x01,
Other		= 0x02,
Ir2		= 0x04,
Msz		= 0x08,
Rla		= 0x10,
Rx7		= 0x20,
Rio		= 0x40,
Sto		= 0x80,
};
enum {
ZeroWS16	= 0x20,
L16en		= 0x40,
M16en		= 0x80,
};
enum {
Ienable		= 0x01,
};
static int irq8003[8] = {
9, 3, 5, 7, 10, 11, 15, 4,
};
static int irq8216[8] = {
0, 9, 3, 5, 7, 10, 11, 15,
};
static void
reset8003(Ether* ether, uchar ea[Eaddrlen], uchar ic[8])
{
Dp8390 *ctlr;
ulong port;
ctlr = ether->ctlr;
port = ether->port;
if(memcmp(&ea[1], &ic[1], 5) == 0){
memset(ic, 0, sizeof(ic));
ic[Msr] = (((ulong)ether->mem)>>13) & 0x3F;
}
else{
outb(port+Gp2, 0xAA);
inb(port+Msr);
if(inb(port+Gp2) != 0xAA){
memset(ic, 0, sizeof(ic));
ic[Msr] = (((ulong)ether->mem)>>13) & 0x3F;
}
else
ether->irq = irq8003[((ic[Irr]>>5) & 0x3)|(ic[Icr] & 0x4)];
outb(port+Icr, ic[Icr]^Bit16);
inb(port+Msr);
if((inb(port+Icr) & Bit16) == (ic[Icr] & Bit16)){
ctlr->width = 2;
ic[Icr] &= ~Bit16;
}
outb(port+Icr, ic[Icr]);
if(ctlr->width == 2 && (inb(port+Icr) & Bit16) == 0)
ctlr->width = 1;
}
ether->mem = (ulong)KADDR((ic[Msr] & 0x3F)<<13);
if(ctlr->width == 2)
ether->mem |= (ic[Laar] & 0x1F)<<19;
else
ether->mem |= 0x80000;
if(ic[Icr] & (1<<3))
ether->size = 32*1024;
if(ctlr->width == 2)
ether->size <<= 1;
outb(port+Msr, ic[Msr]|Menb);
if(ctlr->width == 2)
outb(port+Laar, ic[Laar]|L16en|M16en|ZeroWS16);
}
static void
reset8216(Ether* ether, uchar[8])
{
uchar hcr, irq, x;
ulong addr, port;
Dp8390 *ctlr;
ctlr = ether->ctlr;
port = ether->port;
ctlr->width = 2;
hcr = inb(port+Hcr);
outb(port+Hcr, 0x80|hcr);
addr = inb(port+0x0B) & 0xFF;
irq = inb(port+0x0D);
outb(port+Hcr, hcr);
ether->mem = (ulong)KADDR(0xC0000+((((addr>>2) & 0x30)|(addr & 0x0F))<<13));
ether->size = 8192*(1<<((addr>>4) & 0x03));
ether->irq = irq8216[((irq>>4) & 0x04)|((irq>>2) & 0x03)];
x = inb(port+Msr) & ~Rst;
outb(port+Msr, Menb|x);
x = inb(port+Laar);
outb(port+Laar, M16en|x);
outb(port+Ijr, Ienable);
}
static int
reset(Ether* ether)
{
int i;
uchar ea[Eaddrlen], ic[8], id, nullea[Eaddrlen], sum;
ulong port;
Dp8390 *ctlr;
if(ether->port == 0)
ether->port = 0x280;
if(ether->irq == 0)
ether->irq = 3;
if(ether->mem == 0)
ether->mem = 0xD0000;
if(ether->size == 0)
ether->size = 8*1024;
if(ioalloc(ether->port, 0x20, 0, "wd8003") < 0)
return -1;
port = ether->port;
sum = 0;
for(i = 0; i < sizeof(ea); i++){
ea[i] = inb(port+Lar+i);
sum += ea[i];
ic[i] = inb(port+i);
}
id = inb(port+Id);
sum += id;
sum += inb(port+Cksum);
if(sum != 0xFF){
iofree(ether->port);
return -1;
}
ether->ctlr = malloc(sizeof(Dp8390));
ctlr = ether->ctlr;
if(ctlr == nil)
error(Enomem);
ctlr->ram = 1;
if((id & 0xFE) == 0x2A)
reset8216(ether, ic);
else
reset8003(ether, ea, ic);
ctlr->port = port+0x10;
ctlr->tstart = 0;
ctlr->pstart = HOWMANY(sizeof(Etherpkt), Dp8390BufSz);
ctlr->pstop = HOWMANY(ether->size, Dp8390BufSz);
dp8390reset(ether);
memset(nullea, 0, Eaddrlen);
if(memcmp(nullea, ether->ea, Eaddrlen) == 0){
for(i = 0; i < sizeof(ether->ea); i++)
ether->ea[i] = ea[i];
}
dp8390setea(ether);
if(umbrwmalloc(PADDR(ether->mem), ether->size, 0) == 0)
print("ether8003: warning - 0x%luX unavailable\n",
PADDR(ether->mem));
return 0;
}
void
ether8003link(void)
{
addethercard("WD8003", reset);
}