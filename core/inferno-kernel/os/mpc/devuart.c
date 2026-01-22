#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
#include	"../port/netif.h"
enum {
Nbuf=	2,
Rbufsize=	512,
Bufsize=	(Rbufsize+CACHELINESZ-1)&~(CACHELINESZ-1),
Nuart=	2+4,
CTLS=	's'&037,
CTLQ=	'q'&037,
};
enum {
RxBRK=	1<<7,
RxDE=	1<<7,
RxBOF=	1<<6,
RxLG=	1<<5,
RxNO=	1<<4,
RxBR=	1<<5,
RxFR=	1<<4,
RxPR=	1<<3,
RxAB=	1<<3,
RxCR=	1<<2,
RxOV=	1<<1,
RxCD=	1<<0,
TxTC=	1<<10,
};
typedef struct Uartsmc Uartsmc;
struct Uartsmc {
IOCparam;
ushort	maxidl;
ushort	idlc;
ushort	brkln;
ushort	brkec;
ushort	brkcr;
ushort	rmask;
};
enum {
SccAHDLC = 1<<0,
SccHDLC = 1<<1,
SccIR = 1<<2,
SccPPP = 1<<3,
};
typedef struct Uartscc Uartscc;
struct Uartscc {
SCCparam;
uchar	rsvd[8];
ushort	max_idl;
ushort	idlc;
ushort	brkcr;
ushort	parec;
ushort	frmec;
ushort	nosec;
ushort	brkec;
ushort	brkln;
ushort	uaddr1;
ushort	uaddr2;
ushort	rtemp;
ushort	toseq;
ushort	character[8];
ushort	rccm;
ushort	rccrp;
ushort	rlbc;
};
typedef struct UartAHDLC UartAHDLC;
struct UartAHDLC {
SCCparam;
ulong	rsvd1;
ulong	c_mask;
ulong	c_pres;
ushort	bof;
ushort	eof;
ushort	esc;
ushort	rsvd2[2];
ushort	zero;
ushort	rsvd3;
ushort	rfthr;
ushort	resvd4[2];
ulong	txctl_tbl;
ulong	rxctl_tbl;
ushort	nof;
ushort	rsvd5;
};
typedef struct UartHDLC UartHDLC;
struct UartHDLC {
SCCparam;
ulong	rsvd1;
ulong	c_mask;
ulong	c_pres;
ushort	disfc;
ushort	crcec;
ushort	abtsc;
ushort	nmarc;
ushort	retrc;
ushort	mflr;
ushort	max_cnt;
ushort	rfthr;
ushort	rfcnt;
ushort	hmask;
ushort	haddr[4];
ushort	tmp;
ushort	tmp_mb;
};
enum {
AB=	1<<9,
GRA= 1<<7,
CCR= 1<<3,
BSY=	1<<2,
TXB= 1<<1,
RXB= 1<<0,
TXE = 1<<4,
RXF = 1<<3,
ENR = 1<<5,
ENT = 1<<4,
RXD1=	SIBIT(15),
TXD1=	SIBIT(14),
RTS1B=	IBIT(19),
RTS1C=	SIBIT(15),
CTS1=	SIBIT(11),
CD1=	SIBIT(10),
};
typedef struct Uart Uart;
struct Uart
{
QLock;
Uart	*elist;
char	name[KNAMELEN];
int	x;
int	cpmid;
CPMdev*	cpm;
int	opens;
uchar	bpc;
uchar	parity;
uchar	stopb;
uchar	setup;
uchar	enabled;
int	dev;
ulong	frame;
ulong	perror;
ulong	overrun;
ulong	crcerr;
ulong	interrupts;
int	baud;
int	xonoff;
int	blocked;
int	modem;
int	cts;
int	rts;
Rendez	r;
int	(*putc)(Queue*, int);
Queue	*iq;
Queue	*oq;
Block*	istage[Nbuf];
int	rdrx;
Lock	plock;
Block*	outb;
BD*	rxb;
BD*	txb;
SMC*	smc;
SCC*	scc;
IOCparam*	param;
ushort*	brkcr;
int	brgc;
int	mode;
Block*	partial;
int	loopback;
};
static Uart *uart[Nuart];
static int nuart;
struct Uartalloc {
Lock;
Uart *elist;
} uartalloc;
static void uartintr(Uart*, int);
static void smcuintr(Ureg*, void*);
static void sccuintr(Ureg*, void*);
static void
uartsetbuf(Uart *up)
{
IOCparam *p;
BD *bd;
int i;
Block *bp;
p = up->param;
p->rfcr = 0x18;
p->tfcr = 0x18;
p->mrblr = Rbufsize;
if((bd = up->rxb) == nil){
bd = bdalloc(Nbuf);
up->rxb = bd;
}
p->rbase = (ushort)bd;
for(i=0; i<Nbuf; i++){
bd->status = BDEmpty|BDInt;
bd->length = 0;
if((bp = up->istage[i]) == nil)
up->istage[i] = bp = allocb(Bufsize);
bd->addr = PADDR(bp->wp);
dcflush(bp->wp, Bufsize);
bd++;
}
(bd-1)->status |= BDWrap;
up->rdrx = 0;
if((bd = up->txb) == nil){
bd = bdalloc(1);
up->txb = bd;
}
p->tbase = (ushort)bd;
bd->status = BDWrap|BDInt;
bd->length = 0;
bd->addr = 0;
}
static void
smcsetup(Uart *up)
{
IMM *io;
Uartsmc *p;
SMC *smc;
ulong txrx;
archdisableuart(up->cpmid);
up->brgc = brgalloc();
if(up->brgc < 0)
error(Eio);
m->iomem->brgc[up->brgc] = baudgen(up->baud, 16) | BaudEnable;
smcnmsi(up->x, up->brgc);
archenableuart(up->cpmid, 0);
if(up->x == 1)
txrx = IBIT(24)|IBIT(25);
else
txrx = IBIT(20)|IBIT(21);
io = ioplock();
io->pbpar |= txrx;
io->pbdir &= ~txrx;
iopunlock();
up->param = up->cpm->param;
uartsetbuf(up);
cpmop(up->cpm, InitRxTx, 0);
p = (Uartsmc*)up->param;
up->brkcr = &p->brkcr;
p->maxidl = 1;
p->brkln = 0;
p->brkec = 0;
p->brkcr = 1;
smc = up->cpm->regs;
smc->smce = 0xff;
smc->smcm = BSY|RXB|TXB;
up->smc = smc;
smc->smcmr = ((1+8+1-1)<<11)|(2<<4);
intrenable(VectorCPIC+up->cpm->irq, smcuintr, up, BUSUNKNOWN, up->name);
}
static void
smcuintr(Ureg*, void *a)
{
Uart *up;
int events;
up = a;
events = up->smc->smce;
eieio();
up->smc->smce = events;
uartintr(up, events&(BSY|RXB|TXB));
}
static void
sccuartpins(int x, int mode)
{
IMM *io;
int i, w;
x--;
io = ioplock();
i = 2*x;
w = (TXD1|RXD1)<<i;
io->papar |= w;
io->padir &= ~w;
if((mode & SccIR) == 0)
io->paodr |= TXD1<<i;
else
io->paodr &= ~w;
w = (CD1|CTS1)<<i;
io->pcpar &= ~w;
io->pcdir &= ~w;
if(conf.nocts2 || mode)
io->pcso &= ~w;
else
io->pcso |= w;
w = RTS1B<<x;
io->pbpar &= ~w;
io->pbdir &= ~w;
w = RTS1C<<x;
if((mode & SccIR) == 0)
io->pcpar |= w;
else
io->pcpar &= ~w;
iopunlock();
}
static void
sccsetup(Uart *up)
{
SCC *scc;
int i;
scc = up->cpm->regs;
up->scc = scc;
up->param = up->cpm->param;
sccxstop(up->cpm);
archdisableuart(up->cpmid);
if(up->brgc < 0){
up->brgc = brgalloc();
if(up->brgc < 0)
error(Eio);
}
m->iomem->brgc[up->brgc] = baudgen(up->baud, 16) | BaudEnable;
sccnmsi(up->x, up->brgc, up->brgc);
sccuartpins(up->x, up->mode);
uartsetbuf(up);
cpmop(up->cpm, InitRxTx, 0);
if((up->mode & (SccAHDLC|SccHDLC)) == 0){
Uartscc *sp;
sp = (Uartscc*)up->param;
sp->max_idl = 1;
sp->brkcr = 1;
sp->parec = 0;
sp->frmec = 0;
sp->nosec = 0;
sp->brkec = 0;
sp->brkln = 0;
sp->brkec = 0;
sp->uaddr1 = 0;
sp->uaddr2 = 0;
sp->toseq = 0;
for(i=0; i<8; i++)
sp->character[i] = 0x8000;
sp->rccm = 0xC0FF;
up->brkcr = &sp->brkcr;
scc->irmode = 0;
scc->dsr = ~0;
scc->gsmrh = 1<<5;
scc->gsmrl = 0x28004;
}else{
UartAHDLC *hp;
hp = (UartAHDLC*)up->param;
hp->c_mask = 0x0000F0B8;
hp->c_pres = 0x0000FFFF;
if(up->mode & SccIR){
hp->bof = 0xC0;
hp->eof = 0xC1;
scc->dsr = 0x7E7E;
}else{
hp->bof = 0x7E;
hp->eof = 0x7E;
scc->dsr = 0x7E7E;
}
hp->esc = 0x7D;
hp->zero = 0;
if(up->mode & SccHDLC)
hp->rfthr = 1;
else
hp->rfthr = 0;
hp->txctl_tbl = 0;
hp->rxctl_tbl = 0;
if(up->mode & SccIR){
hp->nof = 12-1;
scc->irsip = 0;
scc->irmode = (2<<8) | 1;
archsetirxcvr(0);
if(up->loopback)
scc->irmode = (3<<4)|1;
}else{
scc->irmode = 0;
hp->txctl_tbl = ~0;
hp->rxctl_tbl = ~0;
hp->nof = 1-1;
}
up->brkcr = nil;
scc->gsmrh = 1<<5;
if(up->mode & SccHDLC)
scc->gsmrl = 0x28000;
else
scc->gsmrl = 0x28006;
}
archenableuart(up->cpmid, (up->mode&SccIR)!=0);
scc->scce = ~0;
scc->sccm = TXE|BSY|RXF|TXB|RXB;
intrenable(VectorCPIC+up->cpm->irq, sccuintr, up, BUSUNKNOWN, up->name);
scc->psmr = 3<<12;
if(up->loopback && (up->mode & SccIR) == 0)
scc->gsmrl |= 1<<6;
scc->gsmrl |= ENT|ENR;
if(0){
print("gsmrl=%8.8lux gsmrh=%8.8lux dsr=%4.4ux irmode=%4.4ux\n", scc->gsmrl, scc->gsmrh, scc->dsr, scc->irmode);
for(i=0; i<sizeof(Uartscc); i+=4)
print("%2.2ux %8.8lux\n", i, *(ulong*)((uchar*)up->param+i));
}
}
static void
sccuintr(Ureg*, void *a)
{
Uart *up;
int events;
up = a;
if(up->scc == nil)
return;
events = up->scc->scce;
eieio();
up->scc->scce = events;
if(up->enabled){
if(0)
print("#%ux|", events);
uartintr(up, events);
}
}
static void
uartsetbaud(Uart *p, int rate)
{
if(rate <= 0 || p->brgc < 0)
return;
p->baud = rate;
m->iomem->brgc[p->brgc] = baudgen(rate, 16) | BaudEnable;
}
static void
uartsetmode(Uart *p)
{
int r, clen;
ilock(&p->plock);
clen = p->bpc;
if(p->parity == 'e' || p->parity == 'o')
clen++;
clen++;
if(p->stopb == 2)
clen++;
if(p->smc){
r = p->smc->smcmr & 0x3F;
r |= (clen<<11);
if(p->parity == 'e')
r |= 3<<8;
else if(p->parity == 'o')
r |= 2<<8;
if(p->stopb == 2)
r |= 1<<10;
eieio();
p->smc->smcmr = r;
}else if(p->scc && p->mode == 0){
r = p->scc->psmr & 0x8FE0;
r |= ((p->bpc-5)&3)<<12;
if(p->parity == 'e')
r |= (6<<2)|2;
else if(p->parity == 'o')
r |= (4<<2)|0;
if(p->stopb == 2)
r |= 1<<14;
eieio();
p->scc->psmr = r;
}
iunlock(&p->plock);
}
static void
uartparity(Uart *p, char type)
{
ilock(&p->plock);
p->parity = type;
iunlock(&p->plock);
uartsetmode(p);
}
static void
uartbits(Uart *p, int bits)
{
if(bits < 5 || bits > 14 || bits > 8 && p->scc)
error(Ebadarg);
ilock(&p->plock);
p->bpc = bits;
iunlock(&p->plock);
uartsetmode(p);
}
static void
uartdtr(Uart *p, int n)
{
if(p->scc == nil)
return;
USED(n);
}
static void
uartrts(Uart *p, int n)
{
p->rts = n;
if(p->scc == nil)
return;
USED(n);
}
static void
uartbreak(Uart *p, int ms)
{
if(p->brkcr == nil)
return;
if(ms <= 0)
ms = 200;
if(waserror()){
ilock(&p->plock);
*p->brkcr = 1;
cpmop(p->cpm, RestartTx, 0);
iunlock(&p->plock);
nexterror();
}
ilock(&p->plock);
*p->brkcr = ((p->baud/(p->bpc+2))*ms+500)/1000;
cpmop(p->cpm, StopTx, 0);
iunlock(&p->plock);
tsleep(&up->sleep, return0, 0, ms);
poperror();
ilock(&p->plock);
*p->brkcr = 1;
cpmop(p->cpm, RestartTx, 0);
iunlock(&p->plock);
}
static void
uartmflow(Uart *p, int n)
{
if(p->scc == nil)
return;
if(n){
p->modem = 1;
p->scc->psmr |= 1<<15;
p->cts = 1;
}else{
p->modem = 0;
p->scc->psmr &= ~(1<<15);
p->cts = 1;
}
}
void
uartenable(Uart *p)
{
Uart **l;
if(p->enabled)
return;
if(p->setup == 0){
if(p->cpmid == CPsmc1 || p->cpmid == CPsmc2)
smcsetup(p);
else
sccsetup(p);
p->setup = 1;
}
if(p->smc){
cpmop(p->cpm, RestartTx, 0);
p->smc->smcmr |= 3;
p->smc->smcm = BSY|TXB|RXB;
eieio();
}else if(p->scc){
cpmop(p->cpm, RestartTx, 0);
p->scc->gsmrl |= ENT|ENR;
p->scc->sccm = BSY|TXB|RXB;
eieio();
}
uartdtr(p, 1);
uartrts(p, 1);
p->cts = 1;
p->blocked = 0;
uartsetbaud(p, p->baud);
lock(&uartalloc);
for(l = &uartalloc.elist; *l; l = &(*l)->elist){
if(*l == p)
break;
}
if(*l == 0){
p->elist = uartalloc.elist;
uartalloc.elist = p;
}
p->enabled = 1;
unlock(&uartalloc);
p->cts = 1;
p->blocked = 0;
p->xonoff = 0;
p->enabled = 1;
}
void
uartdisable(Uart *p)
{
Uart **l;
if(p->smc)
smcxstop(p->cpm);
else if(p->scc)
sccxstop(p->cpm);
p->bpc = 8;
p->parity = 0;
p->stopb = 0;
uartdtr(p, 0);
uartrts(p, 0);
uartmflow(p, 0);
p->xonoff = p->blocked = 0;
lock(&uartalloc);
for(l = &uartalloc.elist; *l; l = &(*l)->elist){
if(*l == p){
*l = p->elist;
break;
}
}
p->enabled = 0;
unlock(&uartalloc);
}
static void
txstart(Uart *p)
{
Block *b;
int n, flags;
if(!p->cts || p->blocked || p->txb->status & BDReady)
return;
if((b = p->outb) == nil){
if((b = qget(p->oq)) == nil)
return;
if(p->mode & SccPPP &&
p->mode & SccAHDLC &&
BLEN(b) >= 8){
UartAHDLC *hp;
hp = (UartAHDLC*)p->param;
if(hp != nil && (p->mode & SccIR) == 0){
hp->txctl_tbl = nhgetl(b->rp);
hp->rxctl_tbl = nhgetl(b->rp+4);
}
b->rp += 8;
if(0)
print("tx #%lux rx #%lux\n", hp->txctl_tbl, hp->rxctl_tbl);
}
}
n = BLEN(b);
if(n <= 0)
print("txstart: 0\n");
if(p->bpc > 8){
if(PADDR(b->rp)&1){
memmove(b->base, b->rp, n);
b->rp = b->base;
b->wp = b->rp+n;
}
if(n & 1)
n++;
}
dcflush(b->rp, n);
p->outb = b;
if(n > 0xFFFF)
n = 0xFFFE;
if(p->mode & SccHDLC)
flags = BDLast | TxTC;
else if(p->mode)
flags = BDLast;
else
flags = 0;
p->txb->addr = PADDR(b->rp);
p->txb->length = n;
eieio();
p->txb->status = (p->txb->status & BDWrap) | flags | BDReady|BDInt;
eieio();
}
static void
uartkick(void *v)
{
Uart *p;
p = v;
ilock(&p->plock);
if(p->outb == nil)
txstart(p);
iunlock(&p->plock);
}
static void
uartflow(void *v)
{
Uart *p;
p = v;
if(p->modem)
uartrts(p, 1);
}
static void
uartsetup(int x, int lid, char *name)
{
Uart *p;
if(nuart >= Nuart)
return;
p = xalloc(sizeof(Uart));
uart[nuart] = p;
strcpy(p->name, name);
p->dev = nuart;
nuart++;
p->x = x;
p->cpmid = lid;
p->cpm = cpmdev(lid);
p->brgc = -1;
p->mode = 0;
p->bpc = 8;
p->parity = 0;
p->baud = 9600;
p->iq = qopen(4*1024, Qcoalesce, uartflow, p);
p->oq = qopen(4*1024, 0, uartkick, p);
}
void
uartspecial(int port, int baud, Queue **in, Queue **out, int (*putc)(Queue*, int))
{
Uart *p;
if(port < 0 || port >= nuart || (p = uart[port]) == nil)
return;
uartenable(p);
if(baud)
uartsetbaud(p, baud);
p->putc = putc;
if(in)
*in = p->iq;
if(out)
*out = p->oq;
p->opens++;
}
static int
uartinput(Uart *p, BD *bd)
{
int ch, dokick, i, l;
uchar *bp;
dokick = 0;
if(bd->status & RxFR)
p->frame++;
if(bd->status & RxOV)
p->overrun++;
l = bd->length;
if(bd->status & RxPR){
p->perror++;
l--;
}
bp = KADDR(bd->addr);
if(p->xonoff || p->putc && p->opens==1){
for(i=0; i<l; i++){
ch = bp[i];
if(p->xonoff){
if(ch == CTLS){
p->blocked = 1;
cpmop(p->cpm, StopTx, 0);
}else if (ch == CTLQ){
p->blocked = 0;
dokick = 1;
}
}
if(p->putc)
(*p->putc)(p->iq, ch);
}
}
if(l > 0 && (p->putc == nil || p->opens>1))
qproduce(p->iq, bp, l);
return dokick;
}
static void
framedinput(Uart *p, BD *bd)
{
Block *pkt;
int l;
pkt = p->partial;
p->partial = nil;
if(bd->status & RxOV){
p->overrun++;
goto Discard;
}
if(bd->status & (RxAB|RxCR|RxCD|RxLG|RxNO|RxDE|RxBOF|RxBRK)){
if(bd->status & RxCR)
p->crcerr++;
else
p->frame++;
goto Discard;
}
if(pkt == nil){
pkt = iallocb(1500);
if(pkt == nil)
return;
}
l = bd->length;
if(bd->status & BDLast)
l -= BLEN(pkt);
if(l > 0){
if(pkt->wp+l > pkt->lim)
goto Discard;
memmove(pkt->wp, KADDR(bd->addr), l);
pkt->wp += l;
}
if(0)
print("#%ux|", bd->status);
if(bd->status & BDLast){
if(p->mode & (SccHDLC|SccAHDLC)){
if(BLEN(pkt) <= 2){
p->frame++;
goto Discard;
}
pkt->wp -= 2;
}
qpass(p->iq, pkt);
}else
p->partial = pkt;
return;
Discard:
if(pkt != nil)
freeb(pkt);
}
static void
uartintr(Uart *p, int events)
{
int dokick;
BD *bd;
Block *b;
if(events & BSY)
p->overrun++;
p->interrupts++;
dokick = 0;
while(p->rxb != nil && ((bd = &p->rxb[p->rdrx])->status & BDEmpty) == 0){
dcinval(KADDR(bd->addr), bd->length);
if(p->mode)
framedinput(p, bd);
else if(uartinput(p, bd))
dokick = 1;
bd->status = (bd->status & BDWrap) | BDEmpty|BDInt;
eieio();
if(++p->rdrx >= Nbuf)
p->rdrx = 0;
}
if((bd = p->txb) != nil){
if((bd->status & BDReady) == 0){
ilock(&p->plock);
if((b = p->outb) != nil){
b->rp += bd->length;
if(b->rp >= b->wp){
p->outb = nil;
freeb(b);
}
}
txstart(p);
iunlock(&p->plock);
}
}
eieio();
if(dokick && p->cts && !p->blocked){
if(p->outb == nil){
ilock(&p->plock);
txstart(p);
iunlock(&p->plock);
}
cpmop(p->cpm, RestartTx, 0);
} else if (events & TXE)
cpmop(p->cpm, RestartTx, 0);
}
void
uartwait(void)
{
Uart *p = uart[0];
int s;
while(p && (p->outb||qlen(p->oq))){
if(islo())
continue;
s = splhi();
if((p->txb->status & BDReady) == 0){
p->blocked = 0;
p->cts = 1;
if(p->scc == nil)
smcuintr(nil, p);
else
sccuintr(nil, p);
}
splx(s);
}
}
static Dirtab *uartdir;
static int ndir;
static void
setlength(int i)
{
Uart *p;
if(i >= 0){
p = uart[i];
if(p && p->opens && p->iq)
uartdir[1+4*i].length = qlen(p->iq);
} else for(i = 0; i < nuart; i++){
p = uart[i];
if(p && p->opens && p->iq)
uartdir[1+4*i].length = qlen(p->iq);
}
}
void
uartinstall(void)
{
static int already;
int i, n;
char name[2*KNAMELEN];
if(already)
return;
already = 1;
n = 0;
for(i=0; i<2; i++)
if(conf.smcuarts & (1<<i)){
snprint(name, sizeof(name), "eia%d", n++);
uartsetup(i+1, CPsmc1+i, name);
}
n = 2;
for(i=0; i<conf.nscc; i++)
if(conf.sccuarts & (1<<i)){
snprint(name, sizeof(name), "eia%d", n++);
uartsetup(i+1, CPscc1+i, name);
}
}
static void
uartreset(void)
{
int i;
Dirtab *dp;
uartinstall();
ndir = 1+4*nuart;
uartdir = xalloc(ndir * sizeof(Dirtab));
dp = uartdir;
strcpy(dp->name, ".");
mkqid(&dp->qid, 0, 0, QTDIR);
dp->length = 0;
dp->perm = DMDIR|0555;
dp++;
for(i = 0; i < nuart; i++){
strcpy(dp->name, uart[i]->name);
dp->qid.path = NETQID(i, Ndataqid);
dp->perm = 0660;
dp++;
sprint(dp->name, "%sctl", uart[i]->name);
dp->qid.path = NETQID(i, Nctlqid);
dp->perm = 0660;
dp++;
sprint(dp->name, "%sstatus", uart[i]->name);
dp->qid.path = NETQID(i, Nstatqid);
dp->perm = 0444;
dp++;
sprint(dp->name, "%smode", uart[i]->name);
dp->qid.path = NETQID(i, Ntypeqid);
dp->perm = 0660;
dp++;
}
}
static Chan*
uartattach(char *spec)
{
return devattach('t', spec);
}
static Walkqid*
uartwalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, uartdir, ndir, devgen);
}
static int
uartstat(Chan *c, uchar *dp, int n)
{
if(NETTYPE(c->qid.path) == Ndataqid)
setlength(NETID(c->qid.path));
return devstat(c, dp, n, uartdir, ndir, devgen);
}
static Chan*
uartopen(Chan *c, int omode)
{
Uart *p;
c = devopen(c, omode, uartdir, ndir, devgen);
switch(NETTYPE(c->qid.path)){
case Nctlqid:
case Ndataqid:
p = uart[NETID(c->qid.path)];
qlock(p);
if(p->opens++ == 0){
uartenable(p);
qreopen(p->iq);
qreopen(p->oq);
}
qunlock(p);
break;
}
return c;
}
static void
uartclose(Chan *c)
{
Uart *p;
if(c->qid.type & QTDIR)
return;
if((c->flag & COPEN) == 0)
return;
switch(NETTYPE(c->qid.path)){
case Ndataqid:
case Nctlqid:
p = uart[NETID(c->qid.path)];
qlock(p);
if(--(p->opens) == 0){
uartdisable(p);
qclose(p->iq);
qclose(p->oq);
}
qunlock(p);
break;
}
}
static long
uartstatus(Chan*, Uart *p, void *buf, long n, long offset)
{
IMM *io;
char str[256];
sprint(str, "opens %d ferr %lud oerr %lud crcerr %lud baud %ud perr %lud intr %lud", p->opens,
p->frame, p->overrun, p->crcerr, p->baud, p->perror, p->interrupts);
io = m->iomem;
if(p->scc){
if((io->pcdat & SIBIT(9)) == 0)
strcat(str, " cts");
if((io->pcdat & SIBIT(8)) == 0)
strcat(str, " dcd");
if((io->pbdat & IBIT(22)) == 0)
strcat(str, " dtr");
}else if(p->smc){
if((io->pbdat & IBIT(23)) == 0)
strcat(str, " dtr");
}
strcat(str, "\n");
return readstr(offset, buf, n, str);
}
static long
uartread(Chan *c, void *buf, long n, vlong offset)
{
Uart *p;
if(c->qid.type & QTDIR){
setlength(-1);
return devdirread(c, buf, n, uartdir, ndir, devgen);
}
p = uart[NETID(c->qid.path)];
switch(NETTYPE(c->qid.path)){
case Ndataqid:
return qread(p->iq, buf, n);
case Nctlqid:
return readnum(offset, buf, n, NETID(c->qid.path), NUMSIZE);
case Nstatqid:
return uartstatus(c, p, buf, n, offset);
case Ntypeqid:
return readnum(offset, buf, n, p->mode, NUMSIZE);
}
return 0;
}
static Block*
uartbread(Chan *c, long n, ulong offset)
{
if(c->qid.type & QTDIR || NETTYPE(c->qid.path) != Ndataqid)
return devbread(c, n, offset);
return qbread(uart[NETID(c->qid.path)]->iq, n);
}
static void
uartctl(Uart *p, char *cmd)
{
int i, n;
for(i = 0; i < 16 && qlen(p->oq); i++)
tsleep(&p->r, (int(*)(void*))qlen, p->oq, 125);
if(strncmp(cmd, "break", 5) == 0){
uartbreak(p, 0);
return;
}
n = atoi(cmd+1);
switch(*cmd){
case 'B':
case 'b':
uartsetbaud(p, n);
break;
case 'D':
case 'd':
uartdtr(p, n);
break;
case 'f':
case 'F':
qflush(p->oq);
break;
case 'H':
case 'h':
qhangup(p->iq, 0);
qhangup(p->oq, 0);
break;
case 'L':
case 'l':
uartbits(p, n);
break;
case 'm':
case 'M':
uartmflow(p, n);
break;
case 'n':
case 'N':
qnoblock(p->oq, n);
break;
case 'P':
case 'p':
uartparity(p, *(cmd+1));
break;
case 'K':
case 'k':
uartbreak(p, n);
break;
case 'R':
case 'r':
uartrts(p, n);
break;
case 'Q':
case 'q':
qsetlimit(p->iq, n);
qsetlimit(p->oq, n);
break;
case 'W':
case 'w':
break;
case 'X':
case 'x':
p->xonoff = n;
break;
case 'Z':
case 'z':
p->loopback = n;
break;
}
}
static long
uartwrite(Chan *c, void *buf, long n, vlong offset)
{
Uart *p;
char cmd[32];
int m, inuse;
USED(offset);
if(c->qid.type & QTDIR)
error(Eperm);
p = uart[NETID(c->qid.path)];
switch(NETTYPE(c->qid.path)){
case Ndataqid:
return qwrite(p->oq, buf, n);
case Nctlqid:
if(n >= sizeof(cmd))
n = sizeof(cmd)-1;
memmove(cmd, buf, n);
cmd[n] = 0;
uartctl(p, cmd);
return n;
case Ntypeqid:
if(p->smc || p->putc)
error(Ebadarg);
if(n >= sizeof(cmd))
n = sizeof(cmd)-1;
memmove(cmd, buf, n);
cmd[n] = 0;
m = strtoul(cmd, nil, 0);
inuse = 0;
qlock(p);
if(p->opens == 0){
p->mode = m & 0x7F;
p->loopback = (m&0x80)!=0;
p->setup = 0;
}else
inuse = 1;
qunlock(p);
if(inuse)
error(Einuse);
return n;
}
}
static long
uartbwrite(Chan *c, Block *bp, ulong offset)
{
if(c->qid.type & QTDIR || NETTYPE(c->qid.path) != Ndataqid)
return devbwrite(c, bp, offset);
return qbwrite(uart[NETID(c->qid.path)]->oq, bp);
}
static int
uartwstat(Chan *c, uchar *dp, int n)
{
Dir d;
Dirtab *dt;
if(!iseve())
error(Eperm);
if(c->qid.type & QTDIR)
error(Eperm);
if(NETTYPE(c->qid.path) == Nstatqid)
error(Eperm);
dt = &uartdir[1+4 * NETID(c->qid.path)];
n = convM2D(dp, n, &d, nil);
if(d.mode != ~0UL){
d.mode &= 0666;
dt[0].perm = dt[1].perm = d.mode;
}
return n;
}
Dev uartdevtab = {
't',
"uart",
uartreset,
devinit,
devshutdown,
uartattach,
uartwalk,
uartstat,
uartopen,
devcreate,
uartclose,
uartread,
uartbread,
uartwrite,
uartbwrite,
devremove,
uartwstat,
};