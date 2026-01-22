#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#undef USE_KPROCS
enum {
Vid9221	= 0x9221,
Slop	= 4,
};
typedef struct Regs Regs;
struct Regs {
ulong	rxdata;
uchar	_pad0[0x20 - 4];
ulong	txdata;
uchar	_pad1[0x40 - 0x24];
ulong	rxsts;
ulong	rxstspeek;
ulong	txsts;
ulong	txstspeek;
ushort	rev;
ushort	id;
ulong	irqcfg;
ulong	intsts;
ulong	inten;
ulong	_pad2;
ulong	bytetest;
ulong	fifoint;
ulong	rxcfg;
ulong	txcfg;
ulong	hwcfg;
ulong	rxdpctl;
ulong	rxfifoinf;
ulong	txfifoinf;
ulong	pmtctl;
ulong	gpiocfg;
ulong	gptcfg;
ulong	gptcnt;
ulong	_pad3;
ulong	wordswap;
ulong	freerun;
ulong	rxdrop;
ulong	maccsrcmd;
ulong	maccsrdata;
ulong	afccfg;
ulong	eepcmd;
ulong	eepdata;
};
enum {
Nstatistics	= 128,
};
enum {
Intcompl	= 1<<31,
Bufendalign	= 3<<24,
Datastoff	= 037<<16,
Firstseg	= 1<<13,
Lastseg		= 1<<12,
Bufsize		= MASK(11),
Pkttag		= MASK(16) << 16,
Txcksumen	= 1<<14,
Addcrcdis	= 1<<13,
Framepaddis	= 1<<12,
Pktlen		= (1<<1) - 1,
Txsdump		= 1<<15,
Txddump		= 1<<14,
Txon		= 1<<1,
Stoptx		= 1<<0,
Mbo		= 1<<20,
Srstto		= 1<<1,
Srst		= 1<<0,
Rxdmacntshift	= 16,
Rxdmacntmask	= MASK(12) << Rxdmacntshift,
Rxdump		= 1<<15,
Rxpktlenshift	= 16,
Rxpktlenmask	= MASK(14) << Rxpktlenshift,
Rxerr		= 1<<15,
Rxstsusedshift	= 16,
Rxstsusedmask	= MASK(8) << Rxstsusedshift,
Rxdatausedmask	= MASK(16),
Txstsusedshift	= 16,
Txstsusedmask	= MASK(8) << Txstsusedshift,
Txdatafreemask	= MASK(16),
Dready		= 1<<0,
Csrbusy		= 1<<31,
Csrread		= 1<<30,
Csraddrshift	= 0,
Csraddrmask	= MASK(8) - 1,
Maccr		= 1,
Macaddrh,
Macaddrl,
Machashh,
Machashl,
Macmiiacc,
Macmiidata,
Macflow,
Macvlan1,
Macvlan2,
Macwuff,
Macwucsr,
Maccoe,
Rxall		= 1<<31,
Rcvown		= 1<<23,
Fdpx		= 1<<20,
Mcpas		= 1<<19,
Prms		= 1<<18,
Ho		= 1<<15,
Hpfilt		= 1<<13,
Padstr		= 1<<8,
Txen		= 1<<3,
Rxen		= 1<<2,
Irqdeasclr	= 1<<14,
Irqdeassts	= 1<<13,
Irqint		= 1<<12,
Irqen		= 1<<8,
Irqpol		= 1<<4,
Irqpushpull	= 1<<0,
Swint		= 1<<31,
Txstop		= 1<<25,
Rxstop		= 1<<24,
Txioc		= 1<<21,
Rxdma		= 1<<20,
Gptimer		= 1<<19,
Phy		= 1<<18,
Rxe		= 1<<14,
Txe		= 1<<13,
Tdfo		= 1<<10,
Tdfa		= 1<<9,
Tsff		= 1<<8,
Tsfl		= 1<<7,
Rsff		= 1<<4,
Rsfl		= 1<<3,
Epcbusy		= 1<<31,
Epccmdshift	= 28,
Epctimeout	= 1<<9,
Epcmacloaded	= 1<<8,
Epcaddrshift	= 0,
};
enum {
Rxintrs		= Rsff | Rsfl | Rxe,
Txintrs		= Tsff | Tsfl | Txe | Txioc,
};
struct Wakeup {
ulong	bytemask[4];
uchar	filt0cmd;
uchar	_pad0;
uchar	filt1cmd;
uchar	_pad1;
uchar	filt2cmd;
uchar	_pad2;
uchar	filt3cmd;
uchar	_pad3;
uchar	offset[4];
ushort	crc16[4];
};
typedef struct Ctlr Ctlr;
struct Ctlr {
int	port;
Ctlr*	next;
Ether*	edev;
Regs*	regs;
int	active;
int	started;
int	inited;
int	id;
int	cls;
ushort	eeprom[0x40];
QLock	alock;
int	nrb;
int*	nic;
Lock	imlock;
int	im;
int	lim;
int	link;
QLock	slock;
uint	statistics[Nstatistics];
uint	lsleep;
uint	lintr;
uint	rsleep;
uint	rintr;
int	tsleep;
uint	tintr;
uchar	ra[Eaddrlen];
ulong	mta[128];
Rendez	rrendez;
int	gotinput;
int	rdcpydone;
Rendez	trendez;
int	gotoutput;
int	wrcpydone;
Lock	tlock;
};
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static Ctlr *smcctlrhead, *smcctlrtail;
static char* statistics[Nstatistics] = { "dummy", };
static uchar mymac[] = { 0xb0, 0x0f, 0xba, 0xbe, 0x00, 0x00, };
static void etherclock(void);
static void smcreceive(Ether *edev);
static void smcinterrupt(Ureg*, void* arg);
static Ether *thisether;
static int attached;
static void
smconce(Ether *edev)
{
static int beenhere;
static Lock l;
ilock(&l);
if (!beenhere && edev != nil) {
beenhere = 1;
if (edev->irq < 0) {
thisether = edev;
addclock0link(etherclock, 1000/HZ);
iprint(" polling");
}
}
iunlock(&l);
}
static void
macwait(Regs *regs)
{
long bound;
for (bound = 400*Mhz; regs->maccsrcmd & Csrbusy && bound > 0; bound--)
;
if (bound <= 0)
iprint("smc: mac registers didn't come ready\n");
}
static ulong
macrd(Regs *regs, uchar index)
{
macwait(regs);
regs->maccsrcmd = Csrbusy | Csrread | index;
coherence();
macwait(regs);
return regs->maccsrdata;
}
static void
macwr(Regs *regs, uchar index, ulong val)
{
macwait(regs);
regs->maccsrdata = val;
regs->maccsrcmd = Csrbusy | index;
macwait(regs);
}
static long
smcifstat(Ether* edev, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *p, *s;
int i, l, r;
ctlr = edev->ctlr;
qlock(&ctlr->slock);
p = malloc(READSTR);
l = 0;
for(i = 0; i < Nstatistics; i++){
r = 0;
if((s = statistics[i]) == nil)
continue;
switch(i){
default:
ctlr->statistics[i] += r;
if(ctlr->statistics[i] == 0)
continue;
l += snprint(p+l, READSTR-l, "%s: %ud %ud\n",
s, ctlr->statistics[i], r);
break;
}
}
l += snprint(p+l, READSTR-l, "lintr: %ud %ud\n",
ctlr->lintr, ctlr->lsleep);
l += snprint(p+l, READSTR-l, "rintr: %ud %ud\n",
ctlr->rintr, ctlr->rsleep);
l += snprint(p+l, READSTR-l, "tintr: %ud %ud\n",
ctlr->tintr, ctlr->tsleep);
l += snprint(p+l, READSTR-l, "eeprom:");
for(i = 0; i < 0x40; i++){
if(i && ((i & 0x07) == 0))
l += snprint(p+l, READSTR-l, "\n       ");
l += snprint(p+l, READSTR-l, " %4.4uX", ctlr->eeprom[i]);
}
l += snprint(p+l, READSTR-l, "\n");
USED(l);
n = readstr(offset, a, n, p);
free(p);
qunlock(&ctlr->slock);
return n;
}
static void
smcpromiscuous(void* arg, int on)
{
int rctl;
Ctlr *ctlr;
Ether *edev;
Regs *regs;
edev = arg;
ctlr = edev->ctlr;
regs = ctlr->regs;
rctl = macrd(regs, Maccr);
if(on)
rctl |= Prms;
else
rctl &= ~Prms;
macwr(regs, Maccr, rctl);
}
static void
smcmulticast(void*, uchar*, int)
{
}
static int
iswrcpydone(void *arg)
{
return ((Ctlr *)arg)->wrcpydone;
}
static int
smctxstart(Ctlr *ctlr, uchar *ubuf, uint len)
{
uint wds, ruplen;
ulong *wdp, *txdp;
Regs *regs;
static ulong buf[ROUNDUP(ETHERMAXTU, sizeof(ulong)) / sizeof(ulong)];
if (!ctlr->inited) {
iprint("smctxstart: too soon to send\n");
return -1;
}
regs = ctlr->regs;
if (len < ETHERMINTU)
iprint("sending too-short (%d) pkt\n", len);
else if (len > ETHERMAXTU)
iprint("sending jumbo (%d) pkt\n", len);
ruplen = ROUNDUP(len, sizeof(ulong));
coherence();
if ((regs->txfifoinf & Txdatafreemask) < ruplen + 2*sizeof(ulong))
return -1;
if ((uintptr)ubuf & MASK(2)) {
memmove(buf, ubuf, len);
ubuf = (uchar *)buf;
}
txdp = &regs->txdata;
*txdp = Intcompl | Firstseg | Lastseg | len;
*txdp = len;
wdp = (ulong *)ubuf;
for (wds = ruplen / sizeof(ulong) + 1; --wds > 0; )
*txdp = *wdp++;
regs->intsts = Txintrs;
coherence();
regs->inten |= Txintrs;
coherence();
return 0;
}
static void
smctransmit(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
ctlr = edev->ctlr;
if (ctlr == nil)
panic("smctransmit: nil ctlr");
ilock(&ctlr->tlock);
while ((bp = qget(edev->oq)) != nil)
if (smctxstart(ctlr, bp->rp, BLEN(bp)) < 0) {
qputback(edev->oq, bp);
iprint("smctransmit: tx data fifo full\n");
break;
} else
freeb(bp);
iunlock(&ctlr->tlock);
}
static void
smctransmitcall(Ether *edev)
{
Ctlr *ctlr;
ctlr = edev->ctlr;
ctlr->gotoutput = 1;
#ifdef USE_KPROCS
wakeup(&ctlr->trendez);
#else
smctransmit(edev);
#endif
}
static int
smcrim(void* ctlr)
{
return ((Ctlr*)ctlr)->gotinput;
}
static void
smcrproc(void* arg)
{
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
for(;;){
ctlr->rsleep++;
sleep(&ctlr->rrendez, smcrim, ctlr);
ctlr->gotinput = 0;
smcreceive(edev);
}
}
static int
smcgotout(void* ctlr)
{
return ((Ctlr*)ctlr)->gotoutput;
}
static void
smctproc(void* arg)
{
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
for(;;){
ctlr->tsleep++;
sleep(&ctlr->trendez, smcgotout, ctlr);
ctlr->gotoutput = 0;
smctransmit(edev);
}
}
void	gpioirqclr(void);
static void
smcattach(Ether* edev)
{
#ifdef USE_KPROCS
char name[KNAMELEN];
#endif
Ctlr *ctlr;
ctlr = edev->ctlr;
qlock(&ctlr->alock);
if(waserror()){
qunlock(&ctlr->alock);
nexterror();
}
if (!ctlr->inited) {
ctlr->inited = 1;
#ifdef USE_KPROCS
snprint(name, KNAMELEN, "#l%drproc", edev->ctlrno);
kproc(name, smcrproc, edev);
snprint(name, KNAMELEN, "#l%dtproc", edev->ctlrno);
kproc(name, smctproc, edev);
#endif
iprint("smcattach:");
#ifdef USE_KPROCS
iprint(" with kprocs");
#else
iprint(" no kprocs");
#endif
iprint(", no dma");
smconce(edev);
attached = 1;
iprint("\n");
}
qunlock(&ctlr->alock);
poperror();
}
static int
isrdcpydone(void *arg)
{
return ((Ctlr *)arg)->rdcpydone;
}
static void
smcreceive(Ether *edev)
{
uint wds, len, sts;
ulong *wdp, *rxdp;
Block *bp;
Ctlr *ctlr;
Regs *regs;
ctlr = edev->ctlr;
regs = ctlr->regs;
coherence();
while (((regs->rxfifoinf & Rxstsusedmask) >> Rxstsusedshift) != 0) {
coherence();
sts = regs->rxsts;
if(sts & Rxerr)
iprint("smcreceive: rx error\n");
len = (sts & Rxpktlenmask) >> Rxpktlenshift;
if (len > ETHERMAXTU + Slop)
iprint("smcreceive: oversized rx pkt (%d)\n", len);
else if (len < ETHERMINTU)
iprint("smcreceive: too-short (%d) pkt\n", len);
wds = ROUNDUP(len, sizeof(ulong)) / sizeof(ulong);
if (wds > 0) {
bp = iallocb(len + sizeof(ulong) );
if (bp == nil)
panic("smcreceive: nil Block*");
assert(((uintptr)bp->rp & (sizeof(ulong) - 1)) == 0);
wdp = (ulong *)bp->rp;
rxdp = &regs->rxdata;
wds = ROUNDUP(len, sizeof(ulong)) / sizeof(ulong) + 1;
while (--wds > 0)
*wdp++ = *rxdp;
bp->wp = bp->rp + len;
if (ctlr->inited)
etheriq(edev, bp, 1);
else
freeb(bp);
regs->intsts = Rxintrs;
coherence();
regs->inten |= Rxintrs;
}
coherence();
}
regs->inten |= Rxintrs;
coherence();
}
void
ackintr(Regs *regs, ulong stsclr)
{
if (stsclr == 0)
return;
regs->inten &= ~stsclr;
coherence();
}
static void
smcinterrupt(Ureg*, void* arg)
{
int junk;
unsigned intsts, intr;
Ctlr *ctlr;
Ether *edev;
Regs *regs;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->imlock);
regs = ctlr->regs;
gpioirqclr();
coherence();
intsts = regs->intsts;
coherence();
intsts &= ~MASK(3);
if (0 && intsts == 0) {
coherence();
iprint("smc: interrupt without a cause; insts %#ux (vs inten %#lux)\n",
intsts, regs->inten);
}
intr = intsts & Rxintrs;
if(intr) {
ackintr(regs, intr);
ctlr->rintr++;
ctlr->gotinput = 1;
#ifdef USE_KPROCS
wakeup(&ctlr->rrendez);
#else
smcreceive(edev);
#endif
}
while(((regs->txfifoinf & Txstsusedmask) >> Txstsusedshift) != 0) {
junk = regs->txsts;
USED(junk);
coherence();
}
intr = intsts & Txintrs;
if (ctlr->gotoutput || intr) {
ackintr(regs, intr);
ctlr->tintr++;
ctlr->gotoutput = 1;
#ifdef USE_KPROCS
wakeup(&ctlr->trendez);
#else
smctransmit(edev);
#endif
}
iunlock(&ctlr->imlock);
}
static void
etherclock(void)
{
smcinterrupt(nil, thisether);
}
static int
smcmii(Ctlr *)
{
return 0;
}
static int
smcdetach(Ctlr* ctlr)
{
Regs *regs;
if (ctlr == nil || ctlr->regs == nil)
return -1;
regs = ctlr->regs;
switch (regs->id) {
case Vid9221:
break;
default:
print("smc: unknown chip id %#ux\n", regs->id);
return -1;
}
regs->inten = 0;
regs->intsts = ~0;
regs->gptcfg = 0;
coherence();
regs->rxcfg = Rxdump;
regs->txcfg = Txsdump | Txddump;
regs->irqcfg &= ~Irqen;
coherence();
return 0;
}
static void
smcshutdown(Ether* ether)
{
smcdetach(ether->ctlr);
}
static void
powerwait(Regs *regs)
{
long bound;
regs->bytetest = 0;
for (bound = 400*Mhz; !(regs->pmtctl & Dready) && bound > 0; bound--)
;
if (bound <= 0)
iprint("smc: pmtctl didn't come ready\n");
}
static int
smcreset(Ctlr* ctlr)
{
int r;
Regs *regs;
static char zea[Eaddrlen];
regs = ctlr->regs;
powerwait(regs);
if(smcdetach(ctlr))
return -1;
switch (regs->id) {
case Vid9221:
break;
default:
print("smc: unknown chip id %#ux\n", regs->id);
return -1;
}
if (regs->bytetest != 0x87654321) {
print("smc: bytetest reg %#p (%#lux) != 0x87654321\n",
&regs->bytetest, regs->bytetest);
return -1;
}
#ifdef TODO
for(i = Ea; i < Eaddrlen/2; i++){
ctlr->ra[2*i] = ctlr->eeprom[i];
ctlr->ra[2*i+1] = ctlr->eeprom[i]>>8;
}
memset(ctlr->mta, 0, sizeof(ctlr->mta));
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta+i*4, 0);
#endif
regs->hwcfg |= Mbo;
r = ctlr->ra[3]<<24 | ctlr->ra[2]<<16 | ctlr->ra[1]<<8 | ctlr->ra[0];
macwr(regs, Macaddrl, r);
macwr(regs, Macaddrh, ctlr->ra[5]<<8 | ctlr->ra[4]);
macwr(regs, Maccoe, 0);
regs->inten = 0;
regs->intsts = ~0;
regs->gptcfg = 0;
coherence();
regs->rxcfg = Rxdump;
regs->txcfg = Txsdump | Txddump | Txon;
regs->fifoint = 72<<24;
macwr(regs, Maccr, Rxall | Rcvown | Fdpx | Mcpas | Txen | Rxen);
coherence();
regs->irqcfg = 1<<24 | Irqen | Irqpushpull;
coherence();
regs->inten = Rxintrs | Txintrs;
coherence();
if(smcmii(ctlr) < 0)
return -1;
return 0;
}
static void
smcpci(void)
{
Ctlr *ctlr;
static int beenhere;
if (beenhere)
return;
beenhere = 1;
if (probeaddr(PHYSETHER) < 0)
return;
ctlr = malloc(sizeof(Ctlr));
ctlr->id = Vid9221<<16 | 0x0424;
ctlr->port = PHYSETHER;
ctlr->nic = (int *)PHYSETHER;
ctlr->regs = (Regs *)PHYSETHER;
if(smcreset(ctlr)){
free(ctlr);
return;
}
if(smcctlrhead != nil)
smcctlrtail->next = ctlr;
else
smcctlrhead = ctlr;
smcctlrtail = ctlr;
}
static int
smcpnp(Ether* edev)
{
Ctlr *ctlr;
static char zea[Eaddrlen];
if(smcctlrhead == nil)
smcpci();
for(ctlr = smcctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
if(edev->port == 0 || edev->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
edev->ctlr = ctlr;
ctlr->edev = edev;
edev->port = ctlr->port;
edev->irq = 34;
edev->mbps = 100;
if (memcmp(edev->ea, zea, Eaddrlen) == 0)
memmove(edev->ea, ctlr->ra, Eaddrlen);
edev->attach = smcattach;
edev->transmit = smctransmitcall;
edev->interrupt = smcinterrupt;
edev->ifstat = smcifstat;
edev->arg = edev;
edev->promiscuous = smcpromiscuous;
edev->multicast = smcmulticast;
edev->shutdown = smcshutdown;
return 0;
}
void
ether9221link(void)
{
addethercard("9221", smcpnp);
}