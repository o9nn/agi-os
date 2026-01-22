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
Cr		= 0x00,
Clda0		= 0x01,
Clda1		= 0x02,
Bnry		= 0x03,
Tsr		= 0x04,
Ncr		= 0x05,
Fifo		= 0x06,
Isr		= 0x07,
Crda0		= 0x08,
Crda1		= 0x09,
Rsr		= 0x0C,
Ref0		= 0x0D,
Ref1		= 0x0E,
Ref2		= 0x0F,
Pstart		= 0x01,
Pstop		= 0x02,
Tpsr		= 0x04,
Tbcr0		= 0x05,
Tbcr1		= 0x06,
Rsar0		= 0x08,
Rsar1		= 0x09,
Rbcr0		= 0x0A,
Rbcr1		= 0x0B,
Rcr		= 0x0C,
Tcr		= 0x0D,
Dcr		= 0x0E,
Imr		= 0x0F,
Par0		= 0x01,
Curr		= 0x07,
Mar0		= 0x08,
};
enum {
Stp		= 0x01,
Sta		= 0x02,
Txp		= 0x04,
Rd0		= 0x08,
Rd1		= 0x10,
Rd2		= 0x20,
RdREAD		= Rd0,
RdWRITE		= Rd1,
RdSEND		= Rd1|Rd0,
RdABORT		= Rd2,
Ps0		= 0x40,
Ps1		= 0x80,
Page0		= 0x00,
Page1		= Ps0,
Page2		= Ps1,
};
enum {
Prx		= 0x01,
Ptx		= 0x02,
Rxe		= 0x04,
Txe		= 0x08,
Ovw		= 0x10,
Cnt		= 0x20,
Rdc		= 0x40,
Rst		= 0x80,
};
enum {
Wts		= 0x01,
Bos		= 0x02,
Las		= 0x04,
Ls		= 0x08,
Arm		= 0x10,
Ft0		= 0x20,
Ft1		= 0x40,
Ft1WORD		= 0x00,
Ft2WORD		= Ft0,
Ft4WORD		= Ft1,
Ft6WORD		= Ft1|Ft0,
};
enum {
Crc		= 0x01,
Lb0		= 0x02,
Lb1		= 0x04,
LpbkNORMAL	= 0x00,
LpbkNIC		= Lb0,
LpbkENDEC	= Lb1,
LpbkEXTERNAL	= Lb1|Lb0,
Atd		= 0x08,
Ofst		= 0x10,
};
enum {
Ptxok		= 0x01,
Col		= 0x04,
Abt		= 0x08,
Crs		= 0x10,
Fu		= 0x20,
Cdh		= 0x40,
Owc		= 0x80,
};
enum {
Sep		= 0x01,
Ar		= 0x02,
Ab		= 0x04,
Am		= 0x08,
Pro		= 0x10,
Mon		= 0x20,
};
enum {
Prxok		= 0x01,
Crce		= 0x02,
Fae		= 0x04,
Fo		= 0x08,
Mpa		= 0x10,
Phy		= 0x20,
Dis		= 0x40,
Dfr		= 0x80,
};
typedef struct Hdr Hdr;
struct Hdr {
uchar	status;
uchar	next;
uchar	len0;
uchar	len1;
};
void
dp8390getea(Ether* ether, uchar* ea)
{
Dp8390 *ctlr;
uchar cr;
int i;
ctlr = ether->ctlr;
ilock(ctlr);
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page1|(~(Ps1|Ps0) & cr));
for(i = 0; i < Eaddrlen; i++)
ea[i] = regr(ctlr, Par0+i);
regw(ctlr, Cr, cr);
iunlock(ctlr);
}
void
dp8390setea(Ether* ether)
{
int i;
uchar cr;
Dp8390 *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page1|(~(Ps1|Ps0) & cr));
for(i = 0; i < Eaddrlen; i++)
regw(ctlr, Par0+i, ether->ea[i]);
regw(ctlr, Cr, cr);
iunlock(ctlr);
}
static void*
_dp8390read(Dp8390* ctlr, void* to, ulong from, ulong len)
{
uchar cr;
int timo;
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page0|RdABORT|Sta);
regw(ctlr, Isr, Rdc);
len = ROUNDUP(len, ctlr->width);
regw(ctlr, Rbcr0, len & 0xFF);
regw(ctlr, Rbcr1, (len>>8) & 0xFF);
regw(ctlr, Rsar0, from & 0xFF);
regw(ctlr, Rsar1, (from>>8) & 0xFF);
regw(ctlr, Cr, Page0|RdREAD|Sta);
rdread(ctlr, to, len);
for(timo = 10000; (regr(ctlr, Isr) & Rdc) == 0 && timo; timo--)
;
regw(ctlr, Isr, Rdc);
regw(ctlr, Cr, cr);
return to;
}
void*
dp8390read(Dp8390* ctlr, void* to, ulong from, ulong len)
{
void *v;
ilock(ctlr);
v = _dp8390read(ctlr, to, from, len);
iunlock(ctlr);
return v;
}
static void*
dp8390write(Dp8390* ctlr, ulong to, void* from, ulong len)
{
ulong crda;
uchar cr;
int timo, width;
top:
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page0|RdABORT|Sta);
regw(ctlr, Isr, Rdc);
len = ROUNDUP(len, ctlr->width);
if(ctlr->dummyrr && (ctlr->width == 1 || ctlr->width == 2)){
if(ctlr->width == 2)
width = 1;
else
width = 0;
crda = to-1-width;
regw(ctlr, Rbcr0, (len+1+width) & 0xFF);
regw(ctlr, Rbcr1, ((len+1+width)>>8) & 0xFF);
regw(ctlr, Rsar0, crda & 0xFF);
regw(ctlr, Rsar1, (crda>>8) & 0xFF);
regw(ctlr, Cr, Page0|RdREAD|Sta);
for(timo=0;; timo++){
if(timo > 10000){
print("ether8390: dummyrr timeout; assuming nodummyrr\n");
ctlr->dummyrr = 0;
goto top;
}
crda = regr(ctlr, Crda0);
crda |= regr(ctlr, Crda1)<<8;
if(crda == to){
regw(ctlr, Cr, Page0|RdWRITE|Sta);
crda = regr(ctlr, Crda0);
crda |= regr(ctlr, Crda1)<<8;
if(crda != to)
panic("crda write %lud to %lud\n", crda, to);
break;
}
}
}
else{
regw(ctlr, Rsar0, to & 0xFF);
regw(ctlr, Rsar1, (to>>8) & 0xFF);
regw(ctlr, Rbcr0, len & 0xFF);
regw(ctlr, Rbcr1, (len>>8) & 0xFF);
regw(ctlr, Cr, Page0|RdWRITE|Sta);
}
rdwrite(ctlr, from, len);
for(timo = 10000; (regr(ctlr, Isr) & Rdc) == 0 && timo; timo--)
;
regw(ctlr, Isr, Rdc);
regw(ctlr, Cr, cr);
return (void*)to;
}
static void
ringinit(Dp8390* ctlr)
{
regw(ctlr, Pstart, ctlr->pstart);
regw(ctlr, Pstop, ctlr->pstop);
regw(ctlr, Bnry, ctlr->pstop-1);
regw(ctlr, Cr, Page1|RdABORT|Stp);
regw(ctlr, Curr, ctlr->pstart);
regw(ctlr, Cr, Page0|RdABORT|Stp);
ctlr->nxtpkt = ctlr->pstart;
}
static uchar
getcurr(Dp8390* ctlr)
{
uchar cr, curr;
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page1|(~(Ps1|Ps0) & cr));
curr = regr(ctlr, Curr);
regw(ctlr, Cr, cr);
return curr;
}
static void
receive(Ether* ether)
{
Dp8390 *ctlr;
uchar curr, *p;
Hdr hdr;
ulong count, data, len;
Block *bp;
ctlr = ether->ctlr;
for(curr = getcurr(ctlr); ctlr->nxtpkt != curr; curr = getcurr(ctlr)){
data = ctlr->nxtpkt*Dp8390BufSz;
if(ctlr->ram)
memmove(&hdr, (void*)(ether->mem+data), sizeof(Hdr));
else
_dp8390read(ctlr, &hdr, data, sizeof(Hdr));
if(hdr.next > ctlr->nxtpkt)
len = hdr.next - ctlr->nxtpkt - 1;
else
len = (ctlr->pstop-ctlr->nxtpkt) + (hdr.next-ctlr->pstart) - 1;
if(hdr.len0 > (Dp8390BufSz-sizeof(Hdr)))
len--;
len = ((len<<8)|hdr.len0)-4;
if(hdr.next < ctlr->pstart || hdr.next >= ctlr->pstop
|| len < 60 || len > sizeof(Etherpkt)){
print("dp8390: H%2.2ux+%2.2ux+%2.2ux+%2.2ux,%lud\n",
hdr.status, hdr.next, hdr.len0, hdr.len1, len);
regw(ctlr, Cr, Page0|RdABORT|Stp);
ringinit(ctlr);
regw(ctlr, Cr, Page0|RdABORT|Sta);
return;
}
if((hdr.status & (Fo|Fae|Crce|Prxok)) == Prxok && (bp = iallocb(len))){
p = bp->rp;
bp->wp = p+len;
data += sizeof(Hdr);
if((data+len) >= ctlr->pstop*Dp8390BufSz){
count = ctlr->pstop*Dp8390BufSz - data;
if(ctlr->ram)
memmove(p, (void*)(ether->mem+data), count);
else
_dp8390read(ctlr, p, data, count);
p += count;
data = ctlr->pstart*Dp8390BufSz;
len -= count;
}
if(len){
if(ctlr->ram)
memmove(p, (void*)(ether->mem+data), len);
else
_dp8390read(ctlr, p, data, len);
}
etheriq(ether, bp, 1);
}
ctlr->nxtpkt = hdr.next;
hdr.next--;
if(hdr.next < ctlr->pstart)
hdr.next = ctlr->pstop-1;
regw(ctlr, Bnry, hdr.next);
}
}
static void
txstart(Ether* ether)
{
int len;
Dp8390 *ctlr;
Block *bp;
uchar minpkt[ETHERMINTU], *rp;
ctlr = ether->ctlr;
if(ctlr->txbusy)
return;
bp = qget(ether->oq);
if(bp == nil)
return;
len = BLEN(bp);
rp = bp->rp;
if(len < ETHERMINTU){
rp = minpkt;
memmove(rp, bp->rp, len);
memset(rp+len, 0, ETHERMINTU-len);
len = ETHERMINTU;
}
if(ctlr->ram)
memmove((void*)(ether->mem+ctlr->tstart*Dp8390BufSz), rp, len);
else
dp8390write(ctlr, ctlr->tstart*Dp8390BufSz, rp, len);
freeb(bp);
regw(ctlr, Tbcr0, len & 0xFF);
regw(ctlr, Tbcr1, (len>>8) & 0xFF);
regw(ctlr, Cr, Page0|RdABORT|Txp|Sta);
ether->outpackets++;
ctlr->txbusy = 1;
}
static void
transmit(Ether* ether)
{
Dp8390 *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
txstart(ether);
iunlock(ctlr);
}
static void
overflow(Ether *ether)
{
Dp8390 *ctlr;
uchar txp;
int resend;
ctlr = ether->ctlr;
txp = regr(ctlr, Cr) & Txp;
regw(ctlr, Cr, Page0|RdABORT|Stp);
delay(2);
regw(ctlr, Rbcr0, 0);
regw(ctlr, Rbcr1, 0);
resend = 0;
if(txp && (regr(ctlr, Isr) & (Txe|Ptx)) == 0)
resend = 1;
regw(ctlr, Tcr, LpbkNIC);
regw(ctlr, Cr, Page0|RdABORT|Sta);
receive(ether);
regw(ctlr, Isr, Ovw);
regw(ctlr, Tcr, LpbkNORMAL);
if(resend)
regw(ctlr, Cr, Page0|RdABORT|Txp|Sta);
}
static void
interrupt(Ureg*, void* arg)
{
Ether *ether;
Dp8390 *ctlr;
uchar isr, r;
ether = arg;
ctlr = ether->ctlr;
ilock(ctlr);
regw(ctlr, Imr, 0x00);
while(isr = (regr(ctlr, Isr) & (Cnt|Ovw|Txe|Rxe|Ptx|Prx))){
if(isr & Ovw){
overflow(ether);
regw(ctlr, Isr, Ovw);
ether->overflows++;
}
if(isr & (Rxe|Prx)){
receive(ether);
regw(ctlr, Isr, Rxe|Prx);
}
if(isr & (Txe|Ptx)){
r = regr(ctlr, Tsr);
if((isr & Txe) && (r & (Cdh|Fu|Crs|Abt))){
print("dp8390: Tsr %#2.2ux", r);
ether->oerrs++;
}
regw(ctlr, Isr, Txe|Ptx);
if(isr & Ptx)
ether->outpackets++;
ctlr->txbusy = 0;
txstart(ether);
}
if(isr & Cnt){
ether->frames += regr(ctlr, Ref0);
ether->crcs += regr(ctlr, Ref1);
ether->buffs += regr(ctlr, Ref2);
regw(ctlr, Isr, Cnt);
}
}
regw(ctlr, Imr, Cnt|Ovw|Txe|Rxe|Ptx|Prx);
iunlock(ctlr);
}
static uchar allmar[8] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
static void
setfilter(Ether *ether, Dp8390 *ctlr)
{
uchar r, cr;
int i;
uchar *mar;
r = Ab;
mar = 0;
if(ether->prom){
r |= Pro|Am;
mar = allmar;
} else if(ether->nmaddr){
r |= Am;
mar = ctlr->mar;
}
if(mar){
cr = regr(ctlr, Cr) & ~Txp;
regw(ctlr, Cr, Page1|(~(Ps1|Ps0) & cr));
for(i = 0; i < 8; i++)
regw(ctlr, Mar0+i, *(mar++));
regw(ctlr, Cr, cr);
}
regw(ctlr, Rcr, r);
}
static void
promiscuous(void *arg, int )
{
Ether *ether;
Dp8390 *ctlr;
ether = arg;
ctlr = ether->ctlr;
ilock(ctlr);
setfilter(ether, ctlr);
iunlock(ctlr);
}
static void
setbit(Dp8390 *ctlr, int bit, int on)
{
int i, h;
i = bit/8;
h = bit%8;
if(on){
if(++(ctlr->mref[bit]) == 1)
ctlr->mar[i] |= 1<<h;
} else {
if(--(ctlr->mref[bit]) <= 0){
ctlr->mref[bit] = 0;
ctlr->mar[i] &= ~(1<<h);
}
}
}
static uchar reverse[64];
static void
multicast(void* arg, uchar *addr, int on)
{
Ether *ether;
Dp8390 *ctlr;
int i;
ulong h;
ether = arg;
ctlr = ether->ctlr;
if(reverse[1] == 0){
for(i = 0; i < 64; i++)
reverse[i] = ((i&1)<<5) | ((i&2)<<3) | ((i&4)<<1)
| ((i&8)>>1) | ((i&16)>>3) | ((i&32)>>5);
}
h = ethercrc(addr, 6);
ilock(ctlr);
setbit(ctlr, reverse[h&0x3f], on);
setfilter(ether, ctlr);
iunlock(ctlr);
}
static void
attach(Ether* ether)
{
Dp8390 *ctlr;
uchar r;
ctlr = ether->ctlr;
r = Ab;
if(ether->prom)
r |= Pro;
if(ether->nmaddr)
r |= Am;
ilock(ctlr);
regw(ctlr, Isr, 0xFF);
regw(ctlr, Imr, Cnt|Ovw|Txe|Rxe|Ptx|Prx);
regw(ctlr, Rcr, r);
r = regr(ctlr, Ref2);
regw(ctlr, Tcr, LpbkNORMAL);
iunlock(ctlr);
USED(r);
}
static void
disable(Dp8390* ctlr)
{
int timo;
regw(ctlr, Cr, Page0|RdABORT|Stp);
regw(ctlr, Rbcr0, 0);
regw(ctlr, Rbcr1, 0);
for(timo = 10000; (regr(ctlr, Isr) & Rst) == 0 && timo; timo--)
;
}
static void
shutdown(Ether *ether)
{
Dp8390 *ctlr;
ctlr = ether->ctlr;
disable(ctlr);
}
int
dp8390reset(Ether* ether)
{
Dp8390 *ctlr;
ctlr = ether->ctlr;
disable(ctlr);
if(ctlr->width != 1)
regw(ctlr, Dcr, Ft4WORD|Ls|Wts);
else
regw(ctlr, Dcr, Ft4WORD|Ls);
regw(ctlr, Rbcr0, 0);
regw(ctlr, Rbcr1, 0);
regw(ctlr, Tcr, LpbkNIC);
regw(ctlr, Rcr, Mon);
ringinit(ctlr);
regw(ctlr, Tpsr, ctlr->tstart);
regw(ctlr, Isr, 0xFF);
regw(ctlr, Imr, 0);
regw(ctlr, Cr, Page0|RdABORT|Sta);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->shutdown = shutdown;
ether->ifstat = 0;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
ether->arg = ether;
return 0;
}