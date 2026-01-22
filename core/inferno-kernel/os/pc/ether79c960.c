#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#define	chatty 1
#define	DPRINT	if(chatty)print
enum {
Lognrdre	= 6,
Nrdre		= (1<<Lognrdre),
Logntdre	= 4,
Ntdre		= (1<<Logntdre),
Rbsize		= ETHERMAXTU+4,
};
enum {
Aprom		= 0x0000,
Rdp		= 0x0010,
Rap		= 0x0012,
Sreset		= 0x0014,
Idp		= 0x0016,
};
enum {
Isa10		= 0x0001,
Isamedia		= 0x0003,
Isaawake		= 0x0004,
};
enum {
Init		= 0x0001,
Strt		= 0x0002,
Stop		= 0x0004,
Tdmd		= 0x0008,
Txon		= 0x0010,
Rxon		= 0x0020,
Iena		= 0x0040,
Intr		= 0x0080,
Idon		= 0x0100,
Tint		= 0x0200,
Rint		= 0x0400,
Merr		= 0x0800,
Miss		= 0x1000,
Cerr		= 0x2000,
Babl		= 0x4000,
Err		= 0x8000,
};
enum {
Emba		= 0x0008,
Dxmt2pd		= 0x0010,
Lappen		= 0x0020,
Idonm		= 0x0100,
Tintm		= 0x0200,
Rintm		= 0x0400,
Merrm		= 0x0800,
Missm		= 0x1000,
Bablm		= 0x4000,
};
enum {
ApadXmt		= 0x0800,
};
enum {
Prom		= 0x8000,
TenBaseT		= 0x0080,
};
typedef struct {
ushort	mode;
uchar	padr[6];
uchar	ladr[8];
ushort	rdra0;
uchar	rdra16;
uchar	rlen;
ushort	tdra0;
uchar	tdra16;
uchar	tlen;
} Iblock;
typedef struct {
ushort	rbadr;
ushort	rmd1;
ushort	rmd2;
ushort	rmd3;
} Rdre;
typedef struct {
ushort	tbadr;
ushort	tmd1;
ushort	tmd2;
ushort	tmd3;
} Tdre;
enum {
Enp		= 0x0100,
Stp		= 0x0200,
RxBuff		= 0x0400,
TxDef		= 0x0400,
RxCrc		= 0x0800,
TxOne		= 0x0800,
RxOflo		= 0x1000,
TxMore		= 0x1000,
Fram		= 0x2000,
RxErr		= 0x4000,
TxErr		= 0x4000,
Own			= 0x8000,
};
typedef struct {
Lock;
int	init;
Iblock	iblock;
Rdre*	rdr;
void*	rrb;
int	rdrx;
Tdre*	tdr;
void*	trb;
int	tdrx;
} Ctlr;
static void
attach(Ether* ether)
{
Ctlr *ctlr;
int port;
ctlr = ether->ctlr;
ilock(ctlr);
if(ctlr->init){
iunlock(ctlr);
return;
}
port = ether->port;
outs(port+Rdp, Iena|Strt);
iunlock(ctlr);
}
static void
ringinit(Ctlr* ctlr)
{
int i, x;
if(ctlr->rdr == 0)
ctlr->rdr = xspanalloc(Nrdre*sizeof(Rdre), 0x10, 0);
if(ctlr->rrb == 0)
ctlr->rrb = xalloc(Nrdre*Rbsize);
x = PADDR(ctlr->rrb);
if ((x >> 24)&0xFF)
panic("ether79c960: address>24bit");
for(i = 0; i < Nrdre; i++){
ctlr->rdr[i].rbadr = x&0xFFFF;
ctlr->rdr[i].rmd1 = Own|(x>>16)&0xFF;
x += Rbsize;
ctlr->rdr[i].rmd2 = 0xF000|-Rbsize&0x0FFF;
ctlr->rdr[i].rmd3 = 0;
}
ctlr->rdrx = 0;
if(ctlr->tdr == 0)
ctlr->tdr = xspanalloc(Ntdre*sizeof(Tdre), 0x10, 0);
if(ctlr->trb == 0)
ctlr->trb = xalloc(Ntdre*Rbsize);
x = PADDR(ctlr->trb);
if ((x >> 24)&0xFF)
panic("ether79c960: address>24bit");
for(i = 0; i < Ntdre; i++){
ctlr->tdr[i].tbadr = x&0xFFFF;
ctlr->tdr[i].tmd1 = (x>>16)&0xFF;
x += Rbsize;
ctlr->tdr[i].tmd2 = 0xF000|-Rbsize&0x0FFF;
}
ctlr->tdrx = 0;
}
static void
promiscuous(void* arg, int on)
{
Ether *ether;
int port, x;
Ctlr *ctlr;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
ilock(ctlr);
if(ctlr->init){
iunlock(ctlr);
return;
}
ctlr->init = 1;
iunlock(ctlr);
outs(port+Rdp, Stop);
outs(port+Rap, 15);
x = ins(port+Rdp) & ~Prom;
if(on)
x |= Prom;
outs(port+Rdp, x);
outs(port+Rap, 0);
ringinit(ctlr);
ilock(ctlr);
ctlr->init = 0;
outs(port+Rdp, Iena|Strt);
iunlock(ctlr);
}
static int
owntdre(void* arg)
{
return (((Tdre*)arg)->tmd1 & Own) == 0;
}
static void
txstart(Ether *ether)
{
int port;
Ctlr *ctlr;
Tdre *tdre;
Etherpkt *pkt;
Block *bp;
int n;
port = ether->port;
ctlr = ether->ctlr;
if(ctlr->init)
return;
tdre = &ctlr->tdr[ctlr->tdrx];
if(owntdre(tdre) == 0)
return;
bp = qget(ether->oq);
if(bp == nil)
return;
n = BLEN(bp);
pkt = KADDR(tdre->tbadr|(tdre->tmd1&0xFF)<<16);
memmove(pkt->d, bp->rp, n);
memmove(pkt->s, ether->ea, sizeof(pkt->s));
freeb(bp);
tdre->tmd3 = 0;
tdre->tmd2 = 0xF000|(-n)&0x0FFF;
tdre->tmd1 |= Own|Stp|Enp;
ctlr->tdrx = NEXT(ctlr->tdrx, Ntdre);
outs(port+Rdp, Iena|Tdmd);
ether->outpackets++;
}
static void
transmit(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
txstart(ether);
iunlock(ctlr);
}
static void
interrupt(Ureg*, void* arg)
{
Ether *ether;
int port, csr0, status;
Ctlr *ctlr;
Rdre *rdre;
Etherpkt *pkt;
Block *bp;
int len;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
csr0 = ins(port+Rdp);
outs(port+Rdp, Babl|Cerr|Miss|Merr|Rint|Tint|Iena);
if(csr0 & (Babl|Miss|Merr))
print("AMD70C960#%d: csr0 = 0x%uX\n", ether->ctlrno, csr0);
if(csr0 & Rint){
rdre = &ctlr->rdr[ctlr->rdrx];
while(((status = rdre->rmd1) & Own) == 0){
if(status & RxErr){
if(status & RxBuff)
ether->buffs++;
if(status & RxCrc)
ether->crcs++;
if(status & RxOflo)
ether->overflows++;
}
else {
len = (rdre->rmd3 & 0x0FFF)-4;
if((bp = iallocb(len)) != nil){
ether->inpackets++;
pkt = KADDR(rdre->rbadr|(rdre->rmd1&0xFF)<<16);
memmove(bp->wp, pkt, len);
bp->wp += len;
etheriq(ether, bp, 1);
}
}
rdre->rmd3 = 0;
rdre->rmd2 = 0xF000|-Rbsize&0x0FFF;
rdre->rmd1 |= Own;
ctlr->rdrx = NEXT(ctlr->rdrx, Nrdre);
rdre = &ctlr->rdr[ctlr->rdrx];
}
}
if(csr0 & Tint){
lock(ctlr);
txstart(ether);
unlock(ctlr);
}
}
static int
reset(Ether* ether)
{
int port, x, i;
uchar ea[Eaddrlen];
Ctlr *ctlr;
if(ether->port == 0)
ether->port = 0x300;
if(ether->irq == 0)
ether->irq = 10;
if(ether->irq == 2)
ether->irq = 9;
if(ether->dma == 0)
ether->dma = 5;
port = ether->port;
if(port == 0 || ether->dma == 0)
return -1;
ether->ctlr = malloc(sizeof(Ctlr));
ctlr = ether->ctlr;
ilock(ctlr);
ctlr->init = 1;
ins(port+Sreset);
delay(1);
outs(port+Rap, 0);
outs(port+Rdp, Stop);
outs(port+Rap, 4);
x = ins(port+Rdp) & 0xFFFF;
outs(port+Rdp, ApadXmt|x);
outs(port+Rap, 0);
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
for(i=0; i<6; i++)
ether->ea[i] = inb(port + Aprom + i);
}
ctlr->iblock.rlen = Lognrdre<<5;
ctlr->iblock.tlen = Logntdre<<5;
memmove(ctlr->iblock.padr, ether->ea, sizeof(ctlr->iblock.padr));
ringinit(ctlr);
x = PADDR(ctlr->rdr);
ctlr->iblock.rdra0 = x&0xFFFF;
ctlr->iblock.rdra16 = (x >> 16)&0xFF;
x = PADDR(ctlr->tdr);
ctlr->iblock.tdra0 = x&0xFFFF;
ctlr->iblock.tdra16 = (x >> 16)&0xFF;
switch(ether->dma){
case 5:
outb(0xd6, 0xc1); outb(0xd4, 1); break;
case 6:
outb(0xd6, 0xc2); outb(0xd4, 2); break;
case 7:
outb(0xd6, 0xc3); outb(0xd4, 3); break;
}
ctlr->iblock.mode = TenBaseT;
outs(port+Rap, 2);
x = ins(port+Idp);
x &= ~Isamedia;
x |= Isa10;
x |= Isaawake;
outs(port+Idp, x);
x = PADDR(&ctlr->iblock);
if((x>>24)&0xFF)
panic("ether79c960: address>24bit");
outs(port+Rap, 1);
outs(port+Rdp, x & 0xFFFF);
outs(port+Rap, 2);
outs(port+Rdp, (x>>16) & 0xFF);
outs(port+Rap, 3);
outs(port+Rdp, Idonm);
outs(port+Rap, 0);
outs(port+Rdp, Init);
while((ins(port+Rdp) & Idon) == 0)
;
outs(port+Rdp, Idon|Stop);
ctlr->init = 0;
iunlock(ctlr);
ether->port = port;
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = 0;
ether->promiscuous = promiscuous;
ether->arg = ether;
return 0;
}
void
ether79c960link(void)
{
addethercard("AMD79C960",  reset);
}