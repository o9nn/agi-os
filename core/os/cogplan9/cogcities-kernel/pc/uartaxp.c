#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "uartaxp.i"
typedef struct Cc Cc;
typedef struct Ccb Ccb;
typedef struct Ctlr Ctlr;
typedef struct Gcb Gcb;
struct Gcb {
u16int gcw;
u16int gsw;
u16int gsr;
u16int abs;
u16int bt;
u16int cpv;
u16int ccbn;
u16int ccboff;
u16int ccbsz;
u16int gcw2;
u16int gsw2;
u16int esr;
u16int isr;
u16int osr;
u16int msr;
u16int csr;
};
struct Ccb {
u16int br;
u16int df;
u16int lp;
u16int ibs;
u16int obs;
u16int ibtr;
u16int oblw;
u8int ixon[2];
u16int ibhw;
u16int iblw;
u16int cc;
u16int cs;
u16int ibsa;
u16int ibea;
u16int obsa;
u16int obea;
u16int ibwp;
u16int ibrp;
u16int obwp;
u16int obrp;
u16int ces;
u16int bcp;
u16int mc;
u16int ms;
u16int bs;
u16int crf;
u8int ixoff[2];
u16int cs2;
u8int sec[2];
};
enum {
Br76800 = 0xFF00,
Br115200 = 0xFF01,
};
enum {
Db5 = 0x0000,
Db6 = 0x0001,
Db7 = 0x0002,
Db8 = 0x0003,
DbMASK = 0x0003,
Sb1 = 0x0000,
Sb2 = 0x0004,
SbMASK = 0x0004,
Np = 0x0000,
Op = 0x0008,
Ep = 0x0010,
Mp = 0x0020,
Sp = 0x0030,
PMASK = 0x0038,
Cmn = 0x0000,
Cme = 0x0040,
Cmll = 0x0080,
Cmrl = 0x00C0,
};
enum {
Ixon = 0x0001,
Ixany = 0x0002,
Ixgen = 0x0004,
Cts = 0x0008,
Dtr = 0x0010,
½d = 0x0020,
Rts = 0x0040,
Emcs = 0x0080,
Ecs = 0x1000,
Eia422 = 0x2000,
};
enum {
Ccu = 0x0001,
Cco = 0x0002,
Fib = 0x0004,
Fob = 0x0008,
Er = 0x0010,
Dr = 0x0020,
Et = 0x0040,
Dt = 0x0080,
};
enum {
Oe = 0x0001,
Pe = 0x0002,
Fe = 0x0004,
Br = 0x0008,
};
enum {
Adtr = 0x0001,
Arts = 0x0002,
Ab = 0x0010,
};
enum {
Scts = 0x0001,
Sdsr = 0x0002,
Sri = 0x0004,
Sdcd = 0x0008,
};
enum {
Rd = 0x0001,
Td = 0x0002,
Tbxoff = 0x0004,
Tbcts = 0x0008,
Rbxoff = 0x0010,
Rbrts = 0x0020,
};
enum {
Range = 0x00,
Remap = 0x04,
Region = 0x18,
Mb0 = 0x40,
Ldb = 0x60,
Pdb = 0x64,
Ics = 0x68,
Mcc = 0x6C,
};
enum {
Edcc = 1,
Aic = 0x10,
Cpr = 1ul << 31,
};
enum {
Rcr = 1ul << 29,
Asr = 1ul << 30,
Lis = 1ul << 31,
};
typedef struct Cc Cc;
typedef struct Ccb Ccb;
typedef struct Ctlr Ctlr;
struct Cc {
int uartno;
Ccb* ccb;
Ctlr* ctlr;
Rendez;
Uart;
};
typedef struct Ctlr {
char* name;
Pcidev* pcidev;
int ctlrno;
Ctlr* next;
u32int* reg;
uchar* mem;
Gcb* gcb;
int im;
Cc cc[16];
} Ctlr;
#define csr32r(c, r) (*((c)->reg+((r)/4)))
#define csr32w(c, r, v) (*((c)->reg+((r)/4)) = (v))
static Ctlr* axpctlrhead;
static Ctlr* axpctlrtail;
extern PhysUart axpphysuart;
static int
axpccdone(void* ccb)
{
return !((Ccb*)ccb)->cc;
}
static void
axpcc(Cc* cc, int cmd)
{
Ccb *ccb;
int timeo;
u16int cs;
ccb = cc->ccb;
ccb->cc = cmd;
if(!cc->ctlr->im)
for(timeo = 0; timeo < 1000000; timeo++){
if(!ccb->cc)
break;
microdelay(1);
}
else
tsleep(cc, axpccdone, ccb, 1000);
cs = ccb->cs;
if(ccb->cc || cs){
print("%s: cmd %#ux didn't terminate: %#ux %#ux\n",
cc->name, cmd, ccb->cc, cs);
if(cc->ctlr->im)
error(Eio);
}
}
static long
axpstatus(Uart* uart, void* buf, long n, long offset)
{
char *p;
Ccb *ccb;
u16int bs, fstat, ms;
p = malloc(READSTR);
if(p == nil)
error(Enomem);
ccb = ((Cc*)(uart->regs))->ccb;
bs = ccb->bs;
fstat = ccb->df;
ms = ccb->ms;
snprint(p, READSTR,
"b%d c%d d%d e%d l%d m%d p%c r%d s%d i%d\n"
"dev(%d) type(%d) framing(%d) overruns(%d) "
"berr(%d) serr(%d)%s%s%s%s\n",
uart->baud,
uart->hup_dcd,
ms & Sdsr,
uart->hup_dsr,
(fstat & DbMASK) + 5,
0,
(fstat & PMASK) ? ((fstat & Ep) == Ep? 'e': 'o'): 'n',
(bs & Rbrts) ? 1 : 0,
(fstat & Sb2) ? 2 : 1,
0,
uart->dev,
uart->type,
uart->ferr,
uart->oerr,
uart->berr,
uart->serr,
(ms & Scts) ? " cts" : "",
(ms & Sdsr) ? " dsr" : "",
(ms & Sdcd) ? " dcd" : "",
(ms & Sri) ? " ring" : ""
);
n = readstr(offset, buf, n, p);
free(p);
return n;
}
static void
axpfifo(Uart*, int)
{
}
static void
axpdtr(Uart* uart, int on)
{
Ccb *ccb;
u16int mc;
ccb = ((Cc*)(uart->regs))->ccb;
mc = ccb->mc;
if(on)
mc |= Adtr;
else
mc &= ~Adtr;
ccb->mc = mc;
}
static void
axprts(Uart* uart, int on)
{
Ccb *ccb;
u16int mc;
ccb = ((Cc*)(uart->regs))->ccb;
mc = ccb->mc;
if(on)
mc |= Arts;
else
mc &= ~Arts;
ccb->mc = mc;
}
static void
axpmodemctl(Uart* uart, int on)
{
Ccb *ccb;
u16int lp;
ccb = ((Cc*)(uart->regs))->ccb;
ilock(&uart->tlock);
lp = ccb->lp;
if(on){
lp |= Cts|Rts;
lp &= ~Emcs;
uart->cts = ccb->ms & Scts;
}
else{
lp &= ~(Cts|Rts);
lp |= Emcs;
uart->cts = 1;
}
uart->modem = on;
iunlock(&uart->tlock);
ccb->lp = lp;
axpcc(uart->regs, Ccu);
}
static int
axpparity(Uart* uart, int parity)
{
Ccb *ccb;
u16int df;
switch(parity){
default:
return -1;
case 'e':
parity = Ep;
break;
case 'o':
parity = Op;
break;
case 'n':
parity = Np;
break;
}
ccb = ((Cc*)(uart->regs))->ccb;
df = ccb->df & ~PMASK;
ccb->df = df|parity;
axpcc(uart->regs, Ccu);
return 0;
}
static int
axpstop(Uart* uart, int stop)
{
Ccb *ccb;
u16int df;
switch(stop){
default:
return -1;
case 1:
stop = Sb1;
break;
case 2:
stop = Sb2;
break;
}
ccb = ((Cc*)(uart->regs))->ccb;
df = ccb->df & ~SbMASK;
ccb->df = df|stop;
axpcc(uart->regs, Ccu);
return 0;
}
static int
axpbits(Uart* uart, int bits)
{
Ccb *ccb;
u16int df;
bits -= 5;
if(bits < 0 || bits > 3)
return -1;
ccb = ((Cc*)(uart->regs))->ccb;
df = ccb->df & ~DbMASK;
ccb->df = df|bits;
axpcc(uart->regs, Ccu);
return 0;
}
static int
axpbaud(Uart* uart, int baud)
{
Ccb *ccb;
int i, ibtr;
if(baud <= 0)
return -1;
uart->baud = baud;
ccb = ((Cc*)(uart->regs))->ccb;
switch(baud){
default:
ccb->br = baud;
break;
case 76800:
ccb->br = Br76800;
break;
case 115200:
ccb->br = Br115200;
break;
}
ibtr = baud/500;
i = (ccb->ibea - ccb->ibsa)/2;
if(ibtr > i)
ibtr = i;
ccb->ibtr = ibtr;
axpcc(uart->regs, Ccu);
return 0;
}
static void
axpbreak(Uart* uart, int ms)
{
Ccb *ccb;
u16int mc;
if(ms <= 0)
ms = 200;
ccb = ((Cc*)(uart->regs))->ccb;
mc = ccb->mc;
ccb->mc = Ab|mc;
tsleep(&up->sleep, return0, 0, ms);
ccb->mc = mc & ~Ab;
}
static void
axpmc(Cc* cc)
{
int old;
Ccb *ccb;
u16int ms;
ccb = cc->ccb;
ms = ccb->ms;
if(ms & Scts){
ilock(&cc->tlock);
old = cc->cts;
cc->cts = ms & Scts;
if(old == 0 && cc->cts)
cc->ctsbackoff = 2;
iunlock(&cc->tlock);
}
if(ms & Sdsr){
old = ms & Sdsr;
if(cc->hup_dsr && cc->dsr && !old)
cc->dohup = 1;
cc->dsr = old;
}
if(ms & Sdcd){
old = ms & Sdcd;
if(cc->hup_dcd && cc->dcd && !old)
cc->dohup = 1;
cc->dcd = old;
}
}
static void
axpkick(Uart* uart)
{
Cc *cc;
Ccb *ccb;
uchar *ep, *mem, *rp, *wp, *bp;
if(uart->cts == 0 || uart->blocked)
return;
cc = uart->regs;
ccb = cc->ccb;
mem = (uchar*)cc->ctlr->gcb;
bp = mem + ccb->obsa;
rp = mem + ccb->obrp;
wp = mem + ccb->obwp;
ep = mem + ccb->obea;
while(wp != rp-1 && (rp != bp || wp != ep)){
if(uart->op >= uart->oe && uartstageoutput(uart) == 0)
break;
*wp++ = *(uart->op++);
if(wp > ep)
wp = bp;
ccb->obwp = wp - mem;
}
}
static void
axprecv(Cc* cc)
{
Ccb *ccb;
uchar *ep, *mem, *rp, *wp;
ccb = cc->ccb;
mem = (uchar*)cc->ctlr->gcb;
rp = mem + ccb->ibrp;
wp = mem + ccb->ibwp;
ep = mem + ccb->ibea;
while(rp != wp){
uartrecv(cc, *rp++);
if(rp > ep)
rp = mem + ccb->ibsa;
ccb->ibrp = rp - mem;
}
}
static void
axpinterrupt(Ureg*, void* arg)
{
int work;
Cc *cc;
Ctlr *ctlr;
u32int ics;
u16int r, sr;
work = 0;
ctlr = arg;
ics = csr32r(ctlr, Ics);
if(ics & 0x0810C000)
print("%s: unexpected interrupt %#ux\n", ctlr->name, ics);
if(!(ics & 0x00002000)) {
ctlr->gcb->gcw2 = 0x0001;
return;
}
cc = ctlr->cc;
for(sr = xchgw(&ctlr->gcb->isr, 0); sr != 0; sr >>= 1){
if(sr & 0x0001)
work++, axprecv(cc);
cc++;
}
cc = ctlr->cc;
for(sr = xchgw(&ctlr->gcb->osr, 0); sr != 0; sr >>= 1){
if(sr & 0x0001)
work++, uartkick(&cc->Uart);
cc++;
}
cc = ctlr->cc;
for(sr = xchgw(&ctlr->gcb->csr, 0); sr != 0; sr >>= 1){
if(sr & 0x0001)
work++, wakeup(cc);
cc++;
}
cc = ctlr->cc;
for(sr = xchgw(&ctlr->gcb->msr, 0); sr != 0; sr >>= 1){
if(sr & 0x0001)
work++, axpmc(cc);
cc++;
}
cc = ctlr->cc;
for(sr = xchgw(&ctlr->gcb->esr, 0); sr != 0; sr >>= 1){
if(sr & 0x0001){
r = cc->ccb->ms;
if(r & Oe)
cc->oerr++;
if(r & Pe)
cc->perr++;
if(r & Fe)
cc->ferr++;
if (r & (Oe|Pe|Fe))
work++;
}
cc++;
}
if (0 && !work)
print("%s: interrupt with no work\n", ctlr->name);
csr32w(ctlr, Pdb, 1);
ctlr->gcb->gcw2 = 0x0001;
}
static void
axpdisable(Uart* uart)
{
Cc *cc;
u16int lp;
Ctlr *ctlr;
(*uart->phys->dtr)(uart, 0);
(*uart->phys->rts)(uart, 0);
cc = uart->regs;
lp = cc->ccb->lp;
cc->ccb->lp = Emcs|lp;
axpcc(cc, Dt|Dr|Fob|Fib|Ccu);
ctlr = cc->ctlr;
ctlr->im &= ~(1<<cc->uartno);
if(ctlr->im == 0)
intrdisable(ctlr->pcidev->intl, axpinterrupt, ctlr,
ctlr->pcidev->tbdf, ctlr->name);
}
static void
axpenable(Uart* uart, int ie)
{
Cc *cc;
Ctlr *ctlr;
u16int lp;
cc = uart->regs;
ctlr = cc->ctlr;
if(ie){
if(ctlr->im == 0){
intrenable(ctlr->pcidev->intl, axpinterrupt, ctlr,
ctlr->pcidev->tbdf, ctlr->name);
csr32w(ctlr, Ics, 0x00031F00);
csr32w(ctlr, Pdb, 1);
ctlr->gcb->gcw2 = 1;
}
ctlr->im |= 1<<cc->uartno;
}
(*uart->phys->dtr)(uart, 1);
(*uart->phys->rts)(uart, 1);
lp = cc->ccb->lp;
cc->ccb->lp = Emcs|lp;
cc->ccb->oblw = 64;
axpcc(cc, Et|Er|Ccu);
}
static void*
axpdealloc(Ctlr* ctlr)
{
int i;
for(i = 0; i < 16; i++){
if(ctlr->cc[i].name != nil)
free(ctlr->cc[i].name);
}
if(ctlr->reg != nil)
vunmap(ctlr->reg, ctlr->pcidev->mem[0].size);
if(ctlr->mem != nil)
vunmap(ctlr->mem, ctlr->pcidev->mem[2].size);
if(ctlr->name != nil)
free(ctlr->name);
free(ctlr);
return nil;
}
static Uart*
axpalloc(int ctlrno, Pcidev* pcidev)
{
Cc *cc;
uchar *p;
Ctlr *ctlr;
void *addr;
char name[64];
u32int bar, r;
int i, n, timeo;
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil)
error(Enomem);
seprint(name, name+sizeof(name), "uartaxp%d", ctlrno);
kstrdup(&ctlr->name, name);
ctlr->pcidev = pcidev;
ctlr->ctlrno = ctlrno;
bar = pcidev->mem[0].bar;
if((addr = vmap(bar & ~0x0F, pcidev->mem[0].size)) == 0){
print("%s: can't map registers at %#ux\n", ctlr->name, bar);
return axpdealloc(ctlr);
}
ctlr->reg = addr;
print("%s: port 0x%ux irq %d ", ctlr->name, bar, pcidev->intl);
bar = pcidev->mem[2].bar;
if((addr = vmap(bar & ~0x0F, pcidev->mem[2].size)) == 0){
print("%s: can't map memory at %#ux\n", ctlr->name, bar);
return axpdealloc(ctlr);
}
ctlr->mem = addr;
ctlr->gcb = (Gcb*)(ctlr->mem+0x10000);
print("mem 0x%ux size %d: ", bar, pcidev->mem[2].size);
r = csr32r(ctlr, Mcc);
csr32w(ctlr, Mcc, r|Asr);
microdelay(1);
csr32w(ctlr, Mcc, r&~Asr);
delay(100);
for(timeo = 0; timeo < 100000; timeo++){
if(csr32r(ctlr, Mcc) & Lis)
break;
microdelay(1);
}
if(!(csr32r(ctlr, Mcc) & Lis)){
print("%s: couldn't reset\n", ctlr->name);
return axpdealloc(ctlr);
}
print("downloading...");
if(sizeof(uartaxpcp) > 0xD000){
print("%s: control programme too big\n", ctlr->name);
return axpdealloc(ctlr);
}
csr32w(ctlr, Remap, 0xA0000001);
for(i = 0; i < sizeof(uartaxpcp); i++)
ctlr->mem[i] = uartaxpcp[i];
csr32w(ctlr, Mb0, Edcc);
delay(100);
for(timeo = 0; timeo < 10000; timeo++){
if(csr32r(ctlr, Mb0) & Cpr)
break;
microdelay(100);
}
if(!(csr32r(ctlr, Mb0) & Cpr)){
print("control programme not ready; Mb0 %#ux\n",
csr32r(ctlr, Mb0));
print("%s: distribution panel not connected or card not fully seated?\n",
ctlr->name);
return axpdealloc(ctlr);
}
print("\n");
n = ctlr->gcb->ccbn;
if(ctlr->gcb->bt != 0x12 || n > 16){
print("%s: wrong board type %#ux, %d channels\n",
ctlr->name, ctlr->gcb->bt, ctlr->gcb->ccbn);
return axpdealloc(ctlr);
}
p = ((uchar*)ctlr->gcb) + ctlr->gcb->ccboff;
for(i = 0; i < n; i++){
cc = &ctlr->cc[i];
cc->ccb = (Ccb*)p;
p += ctlr->gcb->ccbsz;
cc->uartno = i;
cc->ctlr = ctlr;
cc->regs = cc;
seprint(name, name+sizeof(name), "uartaxp%d%2.2d", ctlrno, i);
kstrdup(&cc->name, name);
cc->freq = 0;
cc->bits = 8;
cc->stop = 1;
cc->parity = 'n';
cc->baud = 9600;
cc->phys = &axpphysuart;
cc->console = 0;
cc->special = 0;
cc->next = &ctlr->cc[i+1];
}
ctlr->cc[n-1].next = nil;
ctlr->next = nil;
if(axpctlrhead != nil)
axpctlrtail->next = ctlr;
else
axpctlrhead = ctlr;
axpctlrtail = ctlr;
return ctlr->cc;
}
static Uart*
axppnp(void)
{
Pcidev *p;
int ctlrno;
Uart *head, *tail, *uart;
head = tail = nil;
ctlrno = 0;
for(p = pcimatch(nil, 0, 0); p != nil; p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x07)
continue;
switch((p->did<<16)|p->vid){
default:
continue;
case (0x6001<<16)|0x114F:
if((uart = axpalloc(ctlrno, p)) == nil)
continue;
break;
}
if(head != nil)
tail->next = uart;
else
head = uart;
for(tail = uart; tail->next != nil; tail = tail->next)
;
ctlrno++;
}
return head;
}
PhysUart axpphysuart = {
.name = "AvanstarXp",
.pnp = axppnp,
.enable = axpenable,
.disable = axpdisable,
.kick = axpkick,
.dobreak = axpbreak,
.baud = axpbaud,
.bits = axpbits,
.stop = axpstop,
.parity = axpparity,
.modemctl = axpmodemctl,
.rts = axprts,
.dtr = axpdtr,
.status = axpstatus,
.fifo = axpfifo,
.getc = nil,
.putc = nil,
};