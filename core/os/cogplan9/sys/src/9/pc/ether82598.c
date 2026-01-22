#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#define NEXTPOW2(x, m)	(((x)+1) & (m))
enum {
Rbsz	= ETHERMAXTU+32,
Descalign= 128,
Goslow	= 0,
Nrd	= 64,
Nrb	= 128,
Ntd	= 32,
};
enum {
Ctrl		= 0x00000/4,
Status		= 0x00008/4,
Ctrlext		= 0x00018/4,
Esdp		= 0x00020/4,
Esodp		= 0x00028/4,
Ledctl		= 0x00200/4,
Tcptimer	= 0x0004c/4,
Ecc		= 0x110b0/4,
Eec		= 0x10010/4,
Eerd		= 0x10014/4,
Fla		= 0x1001c/4,
Flop		= 0x1013c/4,
Grc		= 0x10200/4,
Icr		= 0x00800/4,
Ics		= 0x00808/4,
Ims		= 0x00880/4,
Imc		= 0x00888/4,
Iac		= 0x00810/4,
Iam		= 0x00890/4,
Itr		= 0x00820/4,
Ivar		= 0x00900/4,
Msixt		= 0x0000/4,
Msipba		= 0x2000/4,
Pbacl		= 0x11068/4,
Gpie		= 0x00898/4,
Pfctop		= 0x03008/4,
Fcttv		= 0x03200/4,
Fcrtl		= 0x03220/4,
Fcrth		= 0x03260/4,
Rcrtv		= 0x032a0/4,
Tfcs		= 0x0ce00/4,
Rbal		= 0x01000/4,
Rbah		= 0x01004/4,
Rdlen		= 0x01008/4,
Rdh		= 0x01010/4,
Rdt		= 0x01018/4,
Rxdctl		= 0x01028/4,
Srrctl		= 0x02100/4,
Dcarxctl	= 0x02200/4,
Rdrxctl		= 0x02f00/4,
Rxpbsize	= 0x03c00/4,
Rxctl		= 0x03000/4,
Dropen		= 0x03d04/4,
Rxcsum		= 0x05000/4,
Rfctl		= 0x05008/4,
Mta		= 0x05200/4,
Ral98		= 0x05400/4,
Rah98		= 0x05404/4,
Ral99		= 0x0a200/4,
Rah99		= 0x0a204/4,
Psrtype		= 0x05480/4,
Vfta		= 0x0a000/4,
Fctrl		= 0x05080/4,
Vlnctrl		= 0x05088/4,
Msctctrl	= 0x05090/4,
Mrqc		= 0x05818/4,
Vmdctl		= 0x0581c/4,
Imir		= 0x05a80/4,
Imirext		= 0x05aa0/4,
Imirvp		= 0x05ac0/4,
Reta		= 0x05c00/4,
Rssrk		= 0x05c80/4,
Tdbal		= 0x06000/4,
Tdbah		= 0x06004/4,
Tdlen		= 0x06008/4,
Tdh		= 0x06010/4,
Tdt		= 0x06018/4,
Txdctl		= 0x06028/4,
Tdwbal		= 0x06038/4,
Tdwbah		= 0x0603c/4,
Dtxctl98	= 0x07e00/4,
Dtxctl99	= 0x04a80/4,
Tdcatxctrl98	= 0x07200/4,
Tdcatxctrl99	= 0x0600c/4,
Tipg		= 0x0cb00/4,
Txpbsize	= 0x0cc00/4,
Hlreg0		= 0x04240/4,
Hlreg1		= 0x04244/4,
Msca		= 0x0425c/4,
Msrwd		= 0x04260/4,
Mhadd		= 0x04268/4,
Pcss1		= 0x04288/4,
Pcss2		= 0x0428c/4,
Xpcss		= 0x04290/4,
Serdesc		= 0x04298/4,
Macs		= 0x0429c/4,
Autoc		= 0x042a0/4,
Links		= 0x042a4/4,
Links2		= 0x04324/4,
Autoc2		= 0x042a8/4,
};
enum {
Factive		= 1<<0,
Enable		= 1<<31,
Rst		= 1<<26,
Ten		= 1<<25,
Te		= 1<<0,
Bam		= 1<<10,
Upe 		= 1<<9,
Mpe 		= 1<<8,
Pthresh		= 0,
Hthresh		= 8,
Wthresh		= 16,
Renable		= 1<<25,
Rxen		= 1<<0,
Dmbyps		= 1<<1,
Rdmt½		= 0,
Rdmt¼		= 1,
Rdmt⅛		= 2,
Crcstrip	= 1<<1,
Rscfrstsize	= 037<<17,
Ippcse		= 1<<12,
EEstart		= 1<<0,
EEdone		= 1<<1,
Irx0		= 1<<0,
Itx0		= 1<<1,
Lsc		= 1<<20,
Lnkup		= 1<<30,
Lnkspd		= 1<<29,
Txcrcen		= 1<<0,
Rxcrcstrip	= 1<<1,
Jumboen		= 1<<2,
Txpaden		= 1<<10,
Flu		= 1<<0,
Lmsshift	= 13,
Lmsmask		= 7,
};
typedef struct Ctlr Ctlr;
typedef struct Rd Rd;
typedef struct Td Td;
typedef struct {
uint	reg;
char	*name;
} Stat;
Stat stattab[] = {
0x4000,	"crc error",
0x4004,	"illegal byte",
0x4008,	"short packet",
0x3fa0,	"missed pkt0",
0x4034,	"mac local flt",
0x4038,	"mac rmt flt",
0x4040,	"rx length err",
0x3f60,	"xon tx",
0xcf60,	"xon rx",
0x3f68,	"xoff tx",
0xcf68,	"xoff rx",
0x405c,	"rx 040",
0x4060,	"rx 07f",
0x4064,	"rx 100",
0x4068,	"rx 200",
0x406c,	"rx 3ff",
0x4070,	"rx big",
0x4074,	"rx ok",
0x4078,	"rx bcast",
0x3fc0,	"rx no buf0",
0x40a4,	"rx runt",
0x40a8,	"rx frag",
0x40ac,	"rx ovrsz",
0x40b0,	"rx jab",
0x40d0,	"rx pkt",
0x40d4,	"tx pkt",
0x40d8,	"tx 040",
0x40dc,	"tx 07f",
0x40e0,	"tx 100",
0x40e4,	"tx 200",
0x40e8,	"tx 3ff",
0x40ec,	"tx big",
0x40f4,	"tx bcast",
0x4120,	"xsum err",
};
enum {
Pif	= 1<<7,
Ipcs	= 1<<6,
L4cs	= 1<<5,
Tcpcs	= 1<<4,
Vp	= 1<<3,
Ixsm	= 1<<2,
Reop	= 1<<1,
Rdd	= 1<<0,
};
struct Rd {
u32int	addr[2];
ushort	length;
ushort	cksum;
uchar	status;
uchar	errors;
ushort	vlan;
};
enum {
Rs	= 1<<3,
Ic	= 1<<2,
Ifcs	= 1<<1,
Teop	= 1<<0,
Tdd	= 1<<0,
};
struct Td {
u32int	addr[2];
ushort	length;
uchar	cso;
uchar	cmd;
uchar	status;
uchar	css;
ushort	vlan;
};
struct Ctlr {
Pcidev	*p;
Ether	*edev;
int	type;
u32int	*reg;
u32int	*msix;
u32int	*physreg;
u32int	*physmsix;
uchar	flag;
int	nrd;
int	ntd;
int	nrb;
uint	rbsz;
int	procsrunning;
int	attached;
Watermark wmrb;
Watermark wmrd;
Watermark wmtd;
QLock	slock;
QLock	alock;
QLock	tlock;
Rendez	lrendez;
Rendez	trendez;
Rendez	rrendez;
uint	im;
uint	lim;
uint	rim;
uint	tim;
Lock	imlock;
Rd*	rdba;
Block**	rb;
int	rdt;
int	rdfree;
Td*	tdba;
int	tdh;
int	tdt;
Block**	tb;
uchar	ra[Eaddrlen];
uchar	mta[128];
ulong	stats[nelem(stattab)];
uint	speeds[3];
};
enum {
I82598 = 1,
I82599,
};
static	Ctlr	*ctlrtab[4];
static	int	nctlr;
static	Lock	rblock;
static	Block	*rbpool;
static	int	nrbfull;
static void
readstats(Ctlr *ctlr)
{
int i;
qlock(&ctlr->slock);
for(i = 0; i < nelem(ctlr->stats); i++)
ctlr->stats[i] += ctlr->reg[stattab[i].reg >> 2];
qunlock(&ctlr->slock);
}
static int speedtab[] = {
0,
1000,
10000,
};
static long
ifstat(Ether *edev, void *a, long n, ulong offset)
{
uint i, *t;
char *s, *p, *e;
Ctlr *ctlr;
ctlr = edev->ctlr;
p = s = malloc(READSTR);
if(p == nil)
error(Enomem);
e = p + READSTR;
readstats(ctlr);
for(i = 0; i < nelem(stattab); i++)
if(ctlr->stats[i] > 0)
p = seprint(p, e, "%.10s  %uld\n", stattab[i].name,
ctlr->stats[i]);
t = ctlr->speeds;
p = seprint(p, e, "speeds: 0:%d 1000:%d 10000:%d\n", t[0], t[1], t[2]);
p = seprint(p, e, "mtu: min:%d max:%d\n", edev->minmtu, edev->maxmtu);
p = seprint(p, e, "rdfree %d rdh %d rdt %d\n", ctlr->rdfree, ctlr->reg[Rdt],
ctlr->reg[Rdh]);
p = seprintmark(p, e, &ctlr->wmrb);
p = seprintmark(p, e, &ctlr->wmrd);
p = seprintmark(p, e, &ctlr->wmtd);
USED(p);
n = readstr(offset, a, n, s);
free(s);
return n;
}
static void
ienable(Ctlr *ctlr, int i)
{
ilock(&ctlr->imlock);
ctlr->im |= i;
ctlr->reg[Ims] = ctlr->im;
iunlock(&ctlr->imlock);
}
static int
lim(void *v)
{
return ((Ctlr*)v)->lim != 0;
}
static void
lproc(void *v)
{
int r, i;
Ctlr *ctlr;
Ether *e;
e = v;
ctlr = e->ctlr;
for (;;) {
r = ctlr->reg[Links];
e->link = (r & Lnkup) != 0;
i = 0;
if(e->link)
i = 1 + ((r & Lnkspd) != 0);
ctlr->speeds[i]++;
e->mbps = speedtab[i];
ctlr->lim = 0;
ienable(ctlr, Lsc);
sleep(&ctlr->lrendez, lim, ctlr);
ctlr->lim = 0;
}
}
static long
ctl(Ether *, void *, long)
{
error(Ebadarg);
return -1;
}
static Block*
rballoc(void)
{
Block *bp;
ilock(&rblock);
if((bp = rbpool) != nil){
rbpool = bp->next;
bp->next = 0;
_xinc(&bp->ref);
}
iunlock(&rblock);
return bp;
}
void
rbfree(Block *b)
{
b->rp = b->wp = (uchar*)PGROUND((uintptr)b->base);
b->flag &= ~(Bipck | Budpck | Btcpck | Bpktck);
ilock(&rblock);
b->next = rbpool;
rbpool = b;
nrbfull--;
iunlock(&rblock);
}
static int
cleanup(Ctlr *ctlr, int tdh)
{
Block *b;
uint m, n;
m = ctlr->ntd - 1;
while(ctlr->tdba[n = NEXTPOW2(tdh, m)].status & Tdd){
tdh = n;
b = ctlr->tb[tdh];
ctlr->tb[tdh] = 0;
if (b)
freeb(b);
ctlr->tdba[tdh].status = 0;
}
return tdh;
}
void
transmit(Ether *e)
{
uint i, m, tdt, tdh;
Ctlr *ctlr;
Block *b;
Td *t;
ctlr = e->ctlr;
if(!canqlock(&ctlr->tlock)){
ienable(ctlr, Itx0);
return;
}
tdh = ctlr->tdh = cleanup(ctlr, ctlr->tdh);
tdt = ctlr->tdt;
m = ctlr->ntd - 1;
for(i = 0; ; i++){
if(NEXTPOW2(tdt, m) == tdh){
ienable(ctlr, Itx0);
break;
}
if((b = qget(e->oq)) == nil)
break;
assert(ctlr->tdba != nil);
t = ctlr->tdba + tdt;
t->addr[0] = PCIWADDR(b->rp);
t->length = BLEN(b);
t->cmd = Ifcs | Teop;
if (!Goslow)
t->cmd |= Rs;
ctlr->tb[tdt] = b;
notemark(&ctlr->wmtd, (tdt + Ntd - tdh) % Ntd);
tdt = NEXTPOW2(tdt, m);
}
if(i) {
coherence();
ctlr->reg[Tdt] = ctlr->tdt = tdt;
coherence();
ienable(ctlr, Itx0);
}
qunlock(&ctlr->tlock);
}
static int
tim(void *c)
{
return ((Ctlr*)c)->tim != 0;
}
static void
tproc(void *v)
{
Ctlr *ctlr;
Ether *e;
e = v;
ctlr = e->ctlr;
for (;;) {
sleep(&ctlr->trendez, tim, ctlr);
ctlr->tim = 0;
transmit(e);
}
}
static void
rxinit(Ctlr *ctlr)
{
int i, is598, autoc;
ulong until;
Block *b;
ctlr->reg[Rxctl] &= ~Rxen;
ctlr->reg[Rxdctl] = 0;
for(i = 0; i < ctlr->nrd; i++){
b = ctlr->rb[i];
ctlr->rb[i] = 0;
if(b)
freeb(b);
}
ctlr->rdfree = 0;
coherence();
ctlr->reg[Fctrl] |= Bam;
ctlr->reg[Fctrl] &= ~(Upe | Mpe);
ctlr->reg[Rxcsum] &= ~Ippcse;
ctlr->reg[Hlreg0] &= ~Jumboen;
ctlr->reg[Hlreg0] |= Txcrcen | Rxcrcstrip | Txpaden;
ctlr->reg[Srrctl] = (ctlr->rbsz + 1024 - 1) / 1024;
ctlr->reg[Mhadd] = ctlr->rbsz << 16;
ctlr->reg[Rbal] = PCIWADDR(ctlr->rdba);
ctlr->reg[Rbah] = 0;
ctlr->reg[Rdlen] = ctlr->nrd*sizeof(Rd);
ctlr->reg[Rdh] = 0;
ctlr->reg[Rdt] = ctlr->rdt = 0;
coherence();
is598 = (ctlr->type == I82598);
if (is598)
ctlr->reg[Rdrxctl] = Rdmt¼;
else {
ctlr->reg[Rdrxctl] |= Crcstrip;
ctlr->reg[Rdrxctl] &= ~Rscfrstsize;
}
if (Goslow && is598)
ctlr->reg[Rxdctl] = 8<<Wthresh | 8<<Pthresh | 4<<Hthresh | Renable;
else
ctlr->reg[Rxdctl] = Renable;
coherence();
until = TK2MS(MACHP(0)->ticks) + 250;
while (!(ctlr->reg[Rxdctl] & Renable) && TK2MS(MACHP(0)->ticks) < until)
;
if(!(ctlr->reg[Rxdctl] & Renable))
print("#l%d: Renable didn't come on, might be disconnected\n",
ctlr->edev->ctlrno);
ctlr->reg[Rxctl] |= Rxen | (is598? Dmbyps: 0);
if (is598){
autoc = ctlr->reg[Autoc];
print("#l%d: autoc %#ux; lms %d (3 is 10g sfp)\n",
ctlr->edev->ctlrno, autoc, (autoc>>Lmsshift) & Lmsmask);
ctlr->reg[Autoc] |= Flu;
coherence();
delay(50);
}
}
static void
replenish(Ctlr *ctlr, uint rdh)
{
int rdt, m, i;
Block *b;
Rd *r;
m = ctlr->nrd - 1;
i = 0;
for(rdt = ctlr->rdt; NEXTPOW2(rdt, m) != rdh; rdt = NEXTPOW2(rdt, m)){
r = ctlr->rdba + rdt;
if((b = rballoc()) == nil){
print("#l%d: no buffers\n", ctlr->edev->ctlrno);
break;
}
ctlr->rb[rdt] = b;
r->addr[0] = PCIWADDR(b->rp);
r->status = 0;
ctlr->rdfree++;
i++;
}
if(i) {
coherence();
ctlr->reg[Rdt] = ctlr->rdt = rdt;
coherence();
}
}
static int
rim(void *v)
{
return ((Ctlr*)v)->rim != 0;
}
void
rproc(void *v)
{
int passed;
uint m, rdh;
Block *bp;
Ctlr *ctlr;
Ether *e;
Rd *r;
e = v;
ctlr = e->ctlr;
m = ctlr->nrd - 1;
for (rdh = 0; ; ) {
replenish(ctlr, rdh);
ienable(ctlr, Irx0);
sleep(&ctlr->rrendez, rim, ctlr);
passed = 0;
for (;;) {
ctlr->rim = 0;
r = ctlr->rdba + rdh;
if(!(r->status & Rdd))
break;
bp = ctlr->rb[rdh];
ctlr->rb[rdh] = 0;
if (r->length > ETHERMAXTU)
print("#l%d: got jumbo of %d bytes\n",
e->ctlrno, r->length);
bp->wp += r->length;
bp->lim = bp->wp;
ilock(&rblock);
nrbfull++;
iunlock(&rblock);
notemark(&ctlr->wmrb, nrbfull);
etheriq(e, bp, 1);
passed++;
ctlr->rdfree--;
rdh = NEXTPOW2(rdh, m);
if (ctlr->rdfree <= ctlr->nrd - 16)
replenish(ctlr, rdh);
}
notemark(&ctlr->wmrd, passed);
}
}
static void
promiscuous(void *a, int on)
{
Ctlr *ctlr;
Ether *e;
e = a;
ctlr = e->ctlr;
if(on)
ctlr->reg[Fctrl] |= Upe | Mpe;
else
ctlr->reg[Fctrl] &= ~(Upe | Mpe);
}
static void
multicast(void *a, uchar *ea, int on)
{
int b, i;
Ctlr *ctlr;
Ether *e;
e = a;
ctlr = e->ctlr;
i = ea[5] >> 1;
b = (ea[5]&1)<<4 | ea[4]>>4;
b = 1 << b;
if(on)
ctlr->mta[i] |= b;
ctlr->reg[Mta+i] = ctlr->mta[i];
}
static void
freemem(Ctlr *ctlr)
{
Block *b;
while(b = rballoc()){
b->free = 0;
freeb(b);
}
free(ctlr->rdba);
ctlr->rdba = nil;
free(ctlr->tdba);
ctlr->tdba = nil;
free(ctlr->rb);
ctlr->rb = nil;
free(ctlr->tb);
ctlr->tb = nil;
}
static int
detach(Ctlr *ctlr)
{
int i, is598;
ctlr->reg[Imc] = ~0;
ctlr->reg[Ctrl] |= Rst;
for(i = 0; i < 100; i++){
delay(1);
if((ctlr->reg[Ctrl] & Rst) == 0)
break;
}
if (i >= 100)
return -1;
is598 = (ctlr->type == I82598);
if (is598) {
delay(50);
ctlr->reg[Ecc] &= ~(1<<21 | 1<<18 | 1<<9 | 1<<6);
}
for(i = 1; i < 16; i++)
ctlr->reg[is598? Rah98: Rah99] &= ~Enable;
for(i = 0; i < 128; i++)
ctlr->reg[Mta + i] = 0;
for(i = 1; i < (is598? 640: 128); i++)
ctlr->reg[Vfta + i] = 0;
ctlr->attached = 0;
return 0;
}
static void
shutdown(Ether *e)
{
detach(e->ctlr);
}
static ushort
eeread(Ctlr *ctlr, int i)
{
ctlr->reg[Eerd] = EEstart | i<<2;
while((ctlr->reg[Eerd] & EEdone) == 0)
;
return ctlr->reg[Eerd] >> 16;
}
static int
eeload(Ctlr *ctlr)
{
ushort u, v, p, l, i, j;
if((eeread(ctlr, 0) & 0xc0) != 0x40)
return -1;
u = 0;
for(i = 0; i < 0x40; i++)
u +=  eeread(ctlr, i);
for(i = 3; i < 0xf; i++){
p = eeread(ctlr, i);
l = eeread(ctlr, p++);
if((int)p + l + 1 > 0xffff)
continue;
for(j = p; j < p + l; j++)
u += eeread(ctlr, j);
}
if(u != 0xbaba)
return -1;
if(ctlr->reg[Status] & (1<<3))
u = eeread(ctlr, 10);
else
u = eeread(ctlr, 9);
u++;
for(i = 0; i < Eaddrlen;){
v = eeread(ctlr, u + i/2);
ctlr->ra[i++] = v;
ctlr->ra[i++] = v>>8;
}
ctlr->ra[5] += (ctlr->reg[Status] & 0xc) >> 2;
return 0;
}
static int
reset(Ctlr *ctlr)
{
int i, is598;
uchar *p;
if(detach(ctlr)){
print("82598: reset timeout\n");
return -1;
}
if(eeload(ctlr)){
print("82598: eeprom failure\n");
return -1;
}
p = ctlr->ra;
is598 = (ctlr->type == I82598);
ctlr->reg[is598? Ral98: Ral99] = p[3]<<24 | p[2]<<16 | p[1]<<8 | p[0];
ctlr->reg[is598? Rah98: Rah99] = p[5]<<8 | p[4] | Enable;
readstats(ctlr);
for(i = 0; i<nelem(ctlr->stats); i++)
ctlr->stats[i] = 0;
ctlr->reg[Ctrlext] |= 1 << 16;
if (Goslow) {
ctlr->reg[Fcrtl] = 0x10000 | Enable;
ctlr->reg[Fcrth] = 0x40000 | Enable;
ctlr->reg[Rcrtv] = 0x6000;
} else
ctlr->reg[Fcrtl] = ctlr->reg[Fcrth] = ctlr->reg[Rcrtv] = 0;
ctlr->reg[Ivar+0] =     0 | 1<<7;
ctlr->reg[Ivar+64/4] =  1 | 1<<7;
if (Goslow) {
for(i = Itr; i < Itr + 20; i++)
ctlr->reg[i] = 128;
ctlr->reg[Itr + Itx0] = 256;
} else {
for(i = Itr; i < Itr + 20; i++)
ctlr->reg[i] = 0;
ctlr->reg[Itr + Itx0] = 0;
}
return 0;
}
static void
txinit(Ctlr *ctlr)
{
Block *b;
int i;
if (Goslow)
ctlr->reg[Txdctl] = 16<<Wthresh | 16<<Pthresh;
else
ctlr->reg[Txdctl] = 0;
if (ctlr->type == I82599)
ctlr->reg[Dtxctl99] = 0;
coherence();
for(i = 0; i < ctlr->ntd; i++){
b = ctlr->tb[i];
ctlr->tb[i] = 0;
if(b)
freeb(b);
}
assert(ctlr->tdba != nil);
memset(ctlr->tdba, 0, ctlr->ntd * sizeof(Td));
ctlr->reg[Tdbal] = PCIWADDR(ctlr->tdba);
ctlr->reg[Tdbah] = 0;
ctlr->reg[Tdlen] = ctlr->ntd*sizeof(Td);
ctlr->reg[Tdh] = 0;
ctlr->tdh = ctlr->ntd - 1;
ctlr->reg[Tdt] = ctlr->tdt = 0;
coherence();
if (ctlr->type == I82599)
ctlr->reg[Dtxctl99] |= Te;
coherence();
ctlr->reg[Txdctl] |= Ten;
coherence();
while (!(ctlr->reg[Txdctl] & Ten))
;
}
static void
attach(Ether *e)
{
Block *b;
Ctlr *ctlr;
char buf[KNAMELEN];
ctlr = e->ctlr;
ctlr->edev = e;
qlock(&ctlr->alock);
if(waserror()){
reset(ctlr);
freemem(ctlr);
qunlock(&ctlr->alock);
nexterror();
}
if(ctlr->rdba == nil) {
ctlr->nrd = Nrd;
ctlr->ntd = Ntd;
ctlr->rdba = mallocalign(ctlr->nrd * sizeof *ctlr->rdba,
Descalign, 0, 0);
ctlr->tdba = mallocalign(ctlr->ntd * sizeof *ctlr->tdba,
Descalign, 0, 0);
ctlr->rb = malloc(ctlr->nrd * sizeof(Block *));
ctlr->tb = malloc(ctlr->ntd * sizeof(Block *));
if (ctlr->rdba == nil || ctlr->tdba == nil ||
ctlr->rb == nil || ctlr->tb == nil)
error(Enomem);
for(ctlr->nrb = 0; ctlr->nrb < 2*Nrb; ctlr->nrb++){
b = allocb(ctlr->rbsz + BY2PG);
if(b == nil)
error(Enomem);
b->free = rbfree;
freeb(b);
}
}
if (!ctlr->attached) {
rxinit(ctlr);
txinit(ctlr);
nrbfull = 0;
if (!ctlr->procsrunning) {
snprint(buf, sizeof buf, "#l%dl", e->ctlrno);
kproc(buf, lproc, e);
snprint(buf, sizeof buf, "#l%dr", e->ctlrno);
kproc(buf, rproc, e);
snprint(buf, sizeof buf, "#l%dt", e->ctlrno);
kproc(buf, tproc, e);
ctlr->procsrunning = 1;
}
initmark(&ctlr->wmrb, Nrb, "rcv bufs unprocessed");
initmark(&ctlr->wmrd, Nrd-1, "rcv descrs processed at once");
initmark(&ctlr->wmtd, Ntd-1, "xmit descr queue len");
ctlr->attached = 1;
}
qunlock(&ctlr->alock);
poperror();
}
static void
interrupt(Ureg*, void *v)
{
int icr, im;
Ctlr *ctlr;
Ether *e;
e = v;
ctlr = e->ctlr;
ilock(&ctlr->imlock);
ctlr->reg[Imc] = ~0;
im = ctlr->im;
while((icr = ctlr->reg[Icr] & ctlr->im) != 0){
if(icr & Irx0){
im &= ~Irx0;
ctlr->rim = Irx0;
wakeup(&ctlr->rrendez);
}
if(icr & Itx0){
im &= ~Itx0;
ctlr->tim = Itx0;
wakeup(&ctlr->trendez);
}
if(icr & Lsc){
im &= ~Lsc;
ctlr->lim = Lsc;
wakeup(&ctlr->lrendez);
}
}
ctlr->reg[Ims] = ctlr->im = im;
iunlock(&ctlr->imlock);
}
static void
scan(void)
{
int pciregs, pcimsix, type;
ulong io, iomsi;
void *mem, *memmsi;
Ctlr *ctlr;
Pcidev *p;
p = 0;
while(p = pcimatch(p, Vintel, 0)){
switch(p->did){
case 0x10b6:
case 0x10c6:
case 0x10c7:
case 0x10dd:
case 0x10ec:
pcimsix = 3;
type = I82598;
break;
case 0x10f7:
case 0x10f8:
case 0x10f9:
case 0x10fb:
case 0x10fc:
case 0x1557:
pcimsix = 4;
type = I82599;
break;
default:
continue;
}
pciregs = 0;
if(nctlr >= nelem(ctlrtab)){
print("i82598: too many controllers\n");
return;
}
io = p->mem[pciregs].bar & ~0xf;
mem = vmap(io, p->mem[pciregs].size);
if(mem == nil){
print("i82598: can't map regs %#p\n",
p->mem[pciregs].bar);
continue;
}
iomsi = p->mem[pcimsix].bar & ~0xf;
memmsi = vmap(iomsi, p->mem[pcimsix].size);
if(memmsi == nil){
print("i82598: can't map msi-x regs %#p\n",
p->mem[pcimsix].bar);
vunmap(mem, p->mem[pciregs].size);
continue;
}
ctlr = malloc(sizeof *ctlr);
if(ctlr == nil) {
vunmap(mem, p->mem[pciregs].size);
vunmap(memmsi, p->mem[pcimsix].size);
error(Enomem);
}
ctlr->p = p;
ctlr->type = type;
ctlr->physreg = (u32int*)io;
ctlr->physmsix = (u32int*)iomsi;
ctlr->reg = (u32int*)mem;
ctlr->msix = (u32int*)memmsi;
ctlr->rbsz = Rbsz;
if(reset(ctlr)){
print("i82598: can't reset\n");
free(ctlr);
vunmap(mem, p->mem[pciregs].size);
vunmap(memmsi, p->mem[pcimsix].size);
continue;
}
pcisetbme(p);
ctlrtab[nctlr++] = ctlr;
}
}
static int
pnp(Ether *e)
{
int i;
Ctlr *ctlr;
if(nctlr == 0)
scan();
ctlr = nil;
for(i = 0; i < nctlr; i++){
ctlr = ctlrtab[i];
if(ctlr == nil || ctlr->flag & Factive)
continue;
if(e->port == 0 || e->port == (ulong)ctlr->reg)
break;
}
if (i >= nctlr)
return -1;
ctlr->flag |= Factive;
e->ctlr = ctlr;
e->port = (uintptr)ctlr->physreg;
e->irq = ctlr->p->intl;
e->tbdf = ctlr->p->tbdf;
e->mbps = 10000;
e->maxmtu = ETHERMAXTU;
memmove(e->ea, ctlr->ra, Eaddrlen);
e->arg = e;
e->attach = attach;
e->detach = shutdown;
e->transmit = transmit;
e->interrupt = interrupt;
e->ifstat = ifstat;
e->shutdown = shutdown;
e->ctl = ctl;
e->multicast = multicast;
e->promiscuous = promiscuous;
return 0;
}
void
ether82598link(void)
{
addethercard("i82598", pnp);
addethercard("i10gbe", pnp);
}