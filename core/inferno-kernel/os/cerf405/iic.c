#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
typedef struct Ctlr Ctlr;
typedef struct IICregs IICregs;
struct IICregs {
uchar mdbuf;
uchar rsvd0;
uchar sdbuf;
uchar rsvd1;
uchar lmadr;
uchar hmadr;
uchar cntl;
uchar mdcntl;
uchar sts;
uchar extsts;
uchar lsadr;
uchar hsadr;
uchar clkdiv;
uchar intrmsk;
uchar xfrcnt;
uchar xtcntlss;
uchar directcntl;
};
enum {
Hmt= 1<<7,
Amd10= 1<<6,
Rpst= 1<<3,
Cht= 1<<2,
Write= 0<<1,
Read= 1<<1,
Pt= 1<<0,
Fsdb= 1<<7,
Fmdb= 1<<6,
Fsm= 1<<4,
Esm= 1<<3,
Eint= 1<<2,
Eubs= 1<<1,
Hscl= 1<<0,
Sss= 1<<7,
Slpr= 1<<6,
Mdbs= 1<<5,
Mdbf= 1<<4,
Scmp= 1<<3,
Err= 1<<2,
Irqa= 1<<1,
Irqp= 1<<7,
Bcs= 7<<4,
Bcs_ssel= 1<<4,
Bcs_sio= 2<<4,
Bcs_mio= 3<<4,
Bcs_free= 4<<4,
Bcs_busy= 5<<4,
Bcs_gok= 6<<4,
Irqd= 1<<3,
La= 1<<2,
Ict= 1<<1,
Xfra= 1<<0,
Eirc= 1<<7,
Eirs= 1<<6,
Eiwc= 1<<5,
Eiws= 1<<4,
Eihe= 1<<3,
Eiic= 1<<2,
Eita= 1<<1,
Eimtc= 1<<0,
Src= 1<<7,
Srs= 1<<6,
Swc= 1<<5,
Sws= 1<<4,
Sdbd= 1<<3,
Sdbf= 1<<2,
Epi= 1<<1,
Srst= 1<<0,
Sdac= 1<<3,
Scc= 1<<2,
Msda= 1<<1,
Msc= 1<<0,
Rbit = 1<<0,
FIFOsize= 4,
MaxIO = 8192,
MaxSA= 2,
Bufsize = MaxIO,
Freq = 100000,
I2Ctimeout = 125,
Chatty = 0,
};
#define DPRINT if(Chatty)print
struct Ctlr {
Lock;
QLock io;
int init;
int polling;
IICregs* regs;
int status;
int phase;
Rendez r;
int cntl;
int rdcount;
Block* b;
};
enum {
Idle,
Done,
Failed,
Busy,
Halting,
};
static Ctlr iicctlr[1];
static void interrupt(Ureg*, void*);
static int readyxfer(Ctlr*);
static void rxstart(Ctlr*);
static void txstart(Ctlr*);
static void stopxfer(Ctlr*);
static void txoffset(Ctlr*, ulong, int);
static int idlectlr(Ctlr*);
static void
iicdump(char *t, IICregs *iic)
{
iprint("iic %s: lma=%.2ux hma=%.2ux im=%.2ux mdcntl=%.2ux sts=%.2ux ests=%.2ux cntl=%.2ux\n",
t, iic->lmadr, iic->hmadr, iic->intrmsk, iic->mdcntl, iic->sts, iic->extsts, iic->cntl);
}
static void
initialise(IICregs *iic, int intrmsk)
{
int d;
d = (m->opbhz-1000000)/10000000;
if(d <= 0)
d = 1;
iic->lmadr = 0;
iic->hmadr = 0;
iic->sts = Scmp|Irqa;
iic->extsts = Irqp | Irqd | La | Ict | Xfra;
iic->clkdiv = d;
iic->intrmsk = 0;
iic->xfrcnt = 0;
iic->xtcntlss = Src | Srs | Swc | Sws;
iic->mdcntl = Fsdb | Fmdb | Eubs;
iic->cntl = 0;
eieio();
iic->mdcntl = 0;
eieio();
if(intrmsk){
iic->intrmsk = intrmsk;
iic->mdcntl = Eint;
}
}
void
i2csetup(int polling)
{
IICregs *iic;
Ctlr *ctlr;
ctlr = iicctlr;
ctlr->polling = polling;
iic = (IICregs*)KADDR(PHYSIIC);
ctlr->regs = iic;
if(!polling){
if(ctlr->init == 0){
initialise(iic, Eihe | Eiic | Eita | Eimtc);
ctlr->init = 1;
intrenable(VectorIIC, interrupt, iicctlr, BUSUNKNOWN, "iic");
}
}else
initialise(iic, 0);
}
static void
interrupt(Ureg*, void *arg)
{
int sts, nb, ext, avail;
Ctlr *ctlr;
Block *b;
IICregs *iic;
ctlr = arg;
iic = ctlr->regs;
if(0)
iicdump("intr", iic);
sts = iic->sts;
if(sts & Pt)
iprint("iic: unexpected status: %.2ux", iic->sts);
ext = iic->extsts;
if(sts & Mdbs)
nb = iic->xfrcnt & 7;
else
nb = 0;
eieio();
iic->sts = sts;
if(sts & Err && (ext & (La|Xfra)) != 0)
iprint("iic: s=%.2ux es=%.2ux (IO)\n", sts, ext);
ctlr->status = ext;
switch(ctlr->phase){
default:
iprint("iic: unexpected interrupt: p-%d s=%.2ux es=%.2ux\n", ctlr->phase, sts, ext);
break;
case Halting:
ctlr->phase = Idle;
break;
case Busy:
b = ctlr->b;
if(b == nil)
panic("iic: no buffer");
if(ctlr->cntl & Read){
avail = b->lim - b->wp;
if(nb > avail)
nb = avail;
while(--nb >= 0)
*b->wp++ = iic->mdbuf;
if(sts & Err || ctlr->rdcount <= 0){
ctlr->phase = Done;
wakeup(&ctlr->r);
break;
}
rxstart(ctlr);
}else{
if((b->rp += nb) > b->wp)
b->rp = b->wp;
if(sts & Err || BLEN(b) <= 0){
ctlr->phase = Done;
wakeup(&ctlr->r);
break;
}
txstart(ctlr);
}
}
}
static int
done(void *a)
{
return ((Ctlr*)a)->phase < Busy;
}
static int
i2cerror(char *s)
{
DPRINT("iic error: %s\n", s);
if(up)
error(s);
return -1;
}
static char*
startxfer(I2Cdev *d, int op, void (*xfer)(Ctlr*), Block *b, int n, ulong offset)
{
IICregs *iic;
Ctlr *ctlr;
int i, cntl, p, s;
ctlr = iicctlr;
if(up){
qlock(&ctlr->io);
if(waserror()){
qunlock(&ctlr->io);
nexterror();
}
}
ilock(ctlr);
if(!idlectlr(ctlr)){
iunlock(ctlr);
if(up)
error("bus confused");
return "bus confused";
}
if(ctlr->phase >= Busy)
panic("iic: ctlr busy");
cntl = op | Pt;
if(d->tenbit)
cntl |= Amd10;
ctlr->cntl = cntl;
ctlr->b = b;
ctlr->rdcount = n;
ctlr->phase = Busy;
iic = ctlr->regs;
if(d->tenbit){
iic->hmadr = 0xF0 | (d->addr>>7);
iic->lmadr = d->addr;
}else{
iic->hmadr = 0;
iic->lmadr = d->addr<<1;
}
if(d->salen)
txoffset(ctlr, offset, d->salen);
else
(*xfer)(ctlr);
iunlock(ctlr);
if(ctlr->polling){
for(i=0; !done(ctlr); i++){
delay(2);
interrupt(nil, ctlr);
}
}else
tsleep(&ctlr->r, done, ctlr, I2Ctimeout);
ilock(ctlr);
p = ctlr->phase;
s = ctlr->status;
ctlr->b = nil;
if(ctlr->phase != Done && ctlr->phase != Idle)
stopxfer(ctlr);
iunlock(ctlr);
if(up){
poperror();
qunlock(&ctlr->io);
}
if(p != Done || s & (La|Xfra)){
if(s & La)
return "iic lost arbitration";
if(s & Xfra)
return "iic transfer aborted";
if(p != Done)
return "iic timed out";
sprint(up->genbuf, "iic error: phase=%d estatus=%.2ux", p, s);
return up->genbuf;
}
return nil;
}
long
i2csend(I2Cdev *d, void *buf, long n, ulong offset)
{
Block *b;
char *e;
if(n <= 0)
return 0;
if(n > MaxIO)
n = MaxIO;
if(up){
b = allocb(n);
if(b == nil)
error(Enomem);
if(waserror()){
freeb(b);
nexterror();
}
}else{
b = iallocb(n);
if(b == nil)
return -1;
}
memmove(b->wp, buf, n);
b->wp += n;
e = startxfer(d, Write, txstart, b, 0, offset);
if(up)
poperror();
n -= BLEN(b);
freeb(b);
if(e)
return i2cerror(e);
return n;
}
long
i2crecv(I2Cdev *d, void *buf, long n, ulong offset)
{
Block *b;
long nr;
char *e;
if(n <= 0)
return 0;
if(n > MaxIO)
n = MaxIO;
if(up){
b = allocb(n);
if(b == nil)
error(Enomem);
if(waserror()){
freeb(b);
nexterror();
}
}else{
b = iallocb(n);
if(b == nil)
return -1;
}
e = startxfer(d, Read, rxstart, b, n, offset);
nr = BLEN(b);
if(nr > 0)
memmove(buf, b->rp, nr);
if(up)
poperror();
freeb(b);
if(e)
return i2cerror(e);
return nr;
}
static int
readyxfer(Ctlr *ctlr)
{
IICregs *iic;
iic = ctlr->regs;
iic->sts = Scmp | Err;
if((iic->sts & Pt) != 0){
ctlr->phase = Failed;
wakeup(&ctlr->r);
return 0;
}
iic->mdcntl |= Fmdb;
return 1;
}
static void
rxstart(Ctlr *ctlr)
{
Block *b;
int cntl;
long nb;
b = ctlr->b;
if(b == nil || (nb = ctlr->rdcount) <= 0){
ctlr->phase = Done;
wakeup(&ctlr->r);
return;
}
if(!readyxfer(ctlr))
return;
cntl = ctlr->cntl;
if(nb > FIFOsize){
nb = FIFOsize;
cntl |= Cht;
}
ctlr->rdcount -= nb;
ctlr->regs->cntl = cntl | ((nb-1)<<4);
}
static void
txstart(Ctlr *ctlr)
{
Block *b;
int cntl, i;
long nb;
IICregs *iic;
b = ctlr->b;
if(b == nil || (nb = BLEN(b)) <= 0){
ctlr->phase = Done;
wakeup(&ctlr->r);
return;
}
if(!readyxfer(ctlr))
return;
cntl = ctlr->cntl;
if(nb > FIFOsize){
nb = FIFOsize;
cntl |= Cht;
}
iic = ctlr->regs;
for(i=0; i<nb; i++)
iic->mdbuf = *b->rp++;
iic->cntl = cntl | ((nb-1)<<4);
}
static void
txoffset(Ctlr *ctlr, ulong offset, int len)
{
int i, cntl;
IICregs *iic;
if(!readyxfer(ctlr))
return;
iic = ctlr->regs;
for(i=len*8; (i -= 8) >= 0;)
iic->mdbuf = offset>>i;
cntl = ctlr->cntl & Amd10;
if(ctlr->cntl & Read)
cntl |= Rpst;
else
cntl |= Cht;
iic->cntl = cntl | ((len-1)<<4) | Write | Pt;
}
static void
stopxfer(Ctlr *ctlr)
{
IICregs *iic;
int ext;
iic = ctlr->regs;
ext = iic->extsts;
eieio();
iic->sts = Scmp | Irqa;
eieio();
if((iic->sts & Pt) == 0){
ctlr->phase = Idle;
return;
}
if((ext & Bcs) == Bcs_mio && ctlr->phase != Halting){
ctlr->phase = Halting;
iic->cntl = Hmt;
}
}
static int
idlectlr(Ctlr *ctlr)
{
IICregs *iic;
iic = ctlr->regs;
if((iic->extsts & Bcs) == Bcs_free){
if((iic->sts & Pt) == 0){
ctlr->phase = Idle;
return 1;
}
iprint("iic: bus free, ctlr busy: s=%.2ux es=%.2ux\n", iic->sts, iic->extsts);
}
iprint("iic: soft reset\n");
iic->xtcntlss = Srst;
iunlock(ctlr);
delay(1);
ilock(ctlr);
initialise(iic, Eihe | Eiic | Eita | Eimtc);
ctlr->phase = Idle;
return (iic->extsts & Bcs) == Bcs_free && (iic->sts & Pt) == 0;
}