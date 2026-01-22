#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
typedef struct Ctlr Ctlr;
typedef struct I2C I2C;
struct I2C {
uchar i2mod;
uchar rsv12a[3];
uchar i2add;
uchar rsv12b[3];
uchar i2brg;
uchar rsv12c[3];
uchar i2com;
uchar rsv12d[3];
uchar i2cer;
uchar rsv12e[3];
uchar i2cmr;
};
enum {
RxeOV= 1<<1,
TxS= 1<<10,
TxeNAK= 1<<2,
TxeUN= 1<<1,
TxeCL= 1<<0,
TxERR= (TxeNAK|TxeUN|TxeCL),
REVD= 1<<5,
GCD= 1<<4,
FLT= 1<<3,
PDIV= 3<<1,
EN= 1<<0,
STR= 1<<7,
I2CM= 1<<0,
I2CS= 0<<0,
TXE = 1<<4,
BSY = 1<<2,
TXB = 1<<1,
RXB = 1<<0,
I2CSDA = IBIT(27),
I2CSCL = IBIT(26),
Rbit = 1<<0,
MaxIO = 128,
MaxSA = 2,
Bufsize = (MaxIO+MaxSA+1+4)&~3,
Freq = 100000,
I2CTimeout = 250,
Chatty = 0,
};
#define DPRINT if(Chatty)print
#define DCFLUSH(a,n)
struct Ctlr {
Lock;
QLock io;
int init;
int busywait;
I2C* i2c;
IOCparam* sp;
BD* rd;
BD* td;
int phase;
Rendez r;
char* addr;
char* txbuf;
char* rxbuf;
};
static Ctlr i2ctlr[1];
static void interrupt(Ureg*, void*);
static void
enable(void)
{
I2C *i2c;
i2c = i2ctlr->i2c;
i2c->i2cer = ~0;
eieio();
i2c->i2mod |= EN;
eieio();
i2c->i2cmr = TXE|BSY|TXB|RXB;
eieio();
}
static void
disable(void)
{
I2C *i2c;
i2c = i2ctlr->i2c;
i2c->i2cmr = 0;
i2c->i2mod &= ~EN;
}
void
i2csetup(int busywait)
{
IMM *io;
I2C *i2c;
IOCparam *sp;
CPMdev *cpm;
Ctlr *ctlr;
long f, e, emin;
int p, d, dmax;
ctlr = i2ctlr;
ctlr->busywait = busywait;
if(ctlr->init)
return;
print("i2c setup...\n");
ctlr->init = 1;
cpm = cpmdev(CPi2c);
i2c = cpm->regs;
ctlr->i2c = i2c;
sp = cpm->param;
if(sp == nil)
panic("I2C: can't allocate new parameter memory\n");
ctlr->sp = sp;
disable();
if(ctlr->txbuf == nil){
ctlr->txbuf = cpmalloc(Bufsize, 2);
ctlr->addr = ctlr->txbuf+MaxIO;
}
if(ctlr->rxbuf == nil)
ctlr->rxbuf = cpmalloc(Bufsize, 2);
if(ctlr->rd == nil){
ctlr->rd = bdalloc(1);
ctlr->rd->addr = PADDR(ctlr->rxbuf);
ctlr->rd->length = 0;
ctlr->rd->status = BDWrap;
}
if(ctlr->td == nil){
ctlr->td = bdalloc(2);
ctlr->td->addr = PADDR(ctlr->txbuf);
ctlr->td->length = 0;
ctlr->td->status = BDWrap|BDLast;
}
io = ioplock();
io->pbdir |= I2CSDA | I2CSCL;
io->pbodr |= I2CSDA | I2CSCL;
io->pbpar |= I2CSDA | I2CSCL;
iopunlock();
sp = ctlr->sp;
sp->rbase = PADDR(ctlr->rd);
sp->tbase = PADDR(ctlr->td);
sp->rfcr = 0x18;
sp->tfcr = 0x18;
sp->mrblr = Bufsize;
sp->rstate = 0;
sp->rptr = 0;
sp->rbptr = sp->rbase;
sp->rcnt = 0;
sp->tstate = 0;
sp->tbptr = sp->tbase;
sp->tptr = 0;
sp->tcnt = 0;
eieio();
i2c->i2com = I2CM;
i2c->i2mod = 0;
i2c->i2add = 0;
emin = Freq;
dmax = (m->cpuhz/Freq)/2-3;
for(d=0; d < dmax; d++){
for(p=3; p>=0; p--){
f = (m->cpuhz>>(p+2))/(2*(d+3));
e = Freq - f;
if(e < 0)
e = -e;
if(e < emin){
emin = e;
i2c->i2brg = d;
i2c->i2mod = (i2c->i2mod&~PDIV)|((3-p)<<1);
}
}
}
intrenable(VectorCPIC+cpm->irq, interrupt, i2ctlr, BUSUNKNOWN, "i2c");
}
enum {
Idling,
Done,
Busy,
Sending,
Recving,
};
static void
interrupt(Ureg*, void *arg)
{
int events;
Ctlr *ctlr;
I2C *i2c;
ctlr = arg;
i2c = ctlr->i2c;
events = i2c->i2cer;
eieio();
i2c->i2cer = events;
if(events & (BSY|TXE)){
if(ctlr->phase != Idling){
ctlr->phase = Idling;
wakeup(&ctlr->r);
}
}else{
if(events & TXB){
if(ctlr->phase == Sending){
ctlr->phase = Done;
wakeup(&ctlr->r);
}
}
if(events & RXB){
if(ctlr->phase == Recving){
ctlr->phase = Done;
wakeup(&ctlr->r);
}
}
}
}
static int
done(void *a)
{
return ((Ctlr*)a)->phase < Busy;
}
static void
i2cwait(Ctlr *ctlr)
{
int i;
if(up == nil || ctlr->busywait){
for(i=0; i < 5 && !done(ctlr); i++){
delay(2);
interrupt(nil, ctlr);
}
}else
tsleep(&ctlr->r, done, ctlr, I2CTimeout);
}
static int
i2cerror(char *s)
{
if(up)
error(s);
DPRINT("i2c error: %s\n", s);
return -1;
}
long
i2csend(I2Cdev *d, void *buf, long n, ulong offset)
{
Ctlr *ctlr;
int i, p, s;
ctlr = i2ctlr;
if(up){
if(n > MaxIO)
error(Etoobig);
qlock(&ctlr->io);
if(waserror()){
qunlock(&ctlr->io);
nexterror();
}
}
ctlr->txbuf[0] = d->addr<<1;
i = 1;
if(d->salen > 1)
ctlr->txbuf[i++] = offset>>8;
if(d->salen)
ctlr->txbuf[i++] = offset;
memmove(ctlr->txbuf+i, buf, n);
if(Chatty){
print("tx: %8.8lux: ", PADDR(ctlr->txbuf));
for(s=0; s<n+i; s++)
print(" %.2ux", ctlr->txbuf[s]&0xFF);
print("\n");
}
DCFLUSH(ctlr->txbuf, Bufsize);
ilock(ctlr);
ctlr->phase = Sending;
ctlr->rd->status = BDEmpty|BDWrap|BDInt;
ctlr->td->addr = PADDR(ctlr->txbuf);
ctlr->td->length = n+i;
ctlr->td->status = BDReady|BDWrap|BDLast|BDInt;
enable();
ctlr->i2c->i2com = STR|I2CM;
eieio();
iunlock(ctlr);
i2cwait(ctlr);
disable();
p = ctlr->phase;
s = ctlr->td->status;
if(up){
poperror();
qunlock(&ctlr->io);
}
if(s & BDReady)
return i2cerror("timed out");
if(s & TxERR){
sprint(up->genbuf, "write error: status %.4ux", s);
return i2cerror(up->genbuf);
}
if(p != Done)
return i2cerror("phase error");
return n;
}
long
i2crecv(I2Cdev *d, void *buf, long n, ulong offset)
{
Ctlr *ctlr;
int p, s, flag, i;
BD *td;
long nr;
ctlr = i2ctlr;
if(up){
if(n > MaxIO)
error(Etoobig);
qlock(&ctlr->io);
if(waserror()){
qunlock(&ctlr->io);
nexterror();
}
}
ctlr->txbuf[0] = (d->addr<<1)|Rbit;
if(d->salen){
ctlr->addr[0] = d->addr<<1;
i = 1;
if(d->salen > 1)
ctlr->addr[i++] = offset >> 8;
ctlr->addr[i] = offset;
}
DCFLUSH(ctlr->txbuf, Bufsize);
DCFLUSH(ctlr->rxbuf, Bufsize);
ilock(ctlr);
ctlr->phase = Recving;
ctlr->rd->addr = PADDR(ctlr->rxbuf);
ctlr->rd->status = BDEmpty|BDWrap|BDInt;
flag = 0;
td = ctlr->td;
td[1].status = 0;
if(d->salen){
td->addr = PADDR(ctlr->addr);
i = d->salen+1;
if(i > 3)
i = 3;
td->length = i;
td++;
flag = TxS;
}
td->addr = PADDR(ctlr->txbuf);
td->length = n+1;
td->status = BDReady|BDWrap|BDLast | flag;
if(flag)
ctlr->td->status = BDReady;
enable();
ctlr->i2c->i2com = STR|I2CM;
eieio();
iunlock(ctlr);
i2cwait(ctlr);
disable();
p = ctlr->phase;
s = ctlr->td->status;
if(flag)
s |= ctlr->td[1].status;
nr = ctlr->rd->length;
if(up){
poperror();
qunlock(&ctlr->io);
}
DPRINT("nr=%ld %4.4ux %8.8lux\n", nr, ctlr->rd->status, ctlr->rd->addr);
if(nr > n)
nr = n;
if(s & TxERR){
sprint(up->genbuf, "read: tx status: %.4ux", s);
return i2cerror(up->genbuf);
}
if(s & BDReady || ctlr->rd->status & BDEmpty)
return i2cerror("timed out");
if(p != Done)
return i2cerror("phase error");
memmove(buf, ctlr->rxbuf, nr);
if(Chatty){
for(s=0; s<nr; s++)
print(" %2.2ux", ctlr->rxbuf[s]&0xFF);
print("\n");
}
return nr;
}