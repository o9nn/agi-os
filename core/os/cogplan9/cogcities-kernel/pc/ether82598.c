#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#define NEXTPOW2(x, m) (((x)+1) & (m))
enum {
Rbsz = ETHERMAXTU+32,
Descalign= 128,
Nrd = 256,
Nrb = 1024,
Ntd = 128,
Goslow = 0,
};
enum {
Ctrl = 0x00000/4,
Status = 0x00008/4,
Ctrlext = 0x00018/4,
Esdp = 0x00020/4,
Esodp = 0x00028/4,
Ledctl = 0x00200/4,
Tcptimer = 0x0004c/4,
Ecc = 0x110b0/4,
Eec = 0x10010/4,
Eerd = 0x10014/4,
Fla = 0x1001c/4,
Flop = 0x1013c/4,
Grc = 0x10200/4,
Icr = 0x00800/4,
Ics = 0x00808/4,
Ims = 0x00880/4,
Imc = 0x00888/4,
Iac = 0x00810/4,
Iam = 0x00890/4,
Itr = 0x00820/4,
Ivar = 0x00900/4,
Msixt = 0x0000/4,
Msipba = 0x2000/4,
Pbacl = 0x11068/4,
Gpie = 0x00898/4,
Pfctop = 0x03008/4,
Fcttv = 0x03200/4,
Fcrtl = 0x03220/4,
Fcrth = 0x03260/4,
Rcrtv = 0x032a0/4,
Tfcs = 0x0ce00/4,
Rbal = 0x01000/4,
Rbah = 0x01004/4,
Rdlen = 0x01008/4,
Rdh = 0x01010/4,
Rdt = 0x01018/4,
Rxdctl = 0x01028/4,
Srrctl = 0x02100/4,
Dcarxctl = 0x02200/4,
Rdrxctl = 0x02f00/4,
Rxpbsize = 0x03c00/4,
Rxctl = 0x03000/4,
Dropen = 0x03d04/4,
Rxcsum = 0x05000/4,
Rfctl = 0x05008/4,
Mta = 0x05200/4,
Ral98 = 0x05400/4,
Rah98 = 0x05404/4,
Ral99 = 0x0a200/4,
Rah99 = 0x0a204/4,
Psrtype = 0x05480/4,
Vfta = 0x0a000/4,
Fctrl = 0x05080/4,
Vlnctrl = 0x05088/4,
Msctctrl = 0x05090/4,
Mrqc = 0x05818/4,
Vmdctl = 0x0581c/4,
Imir = 0x05a80/4,
Imirext = 0x05aa0/4,
Imirvp = 0x05ac0/4,
Reta = 0x05c00/4,
Rssrk = 0x05c80/4,
Tdbal = 0x06000/4,
Tdbah = 0x06004/4,
Tdlen = 0x06008/4,
Tdh = 0x06010/4,
Tdt = 0x06018/4,
Txdctl = 0x06028/4,
Tdwbal = 0x06038/4,
Tdwbah = 0x0603c/4,
Dtxctl98 = 0x07e00/4,
Dtxctl99 = 0x04a80/4,
Tdcatxctrl98 = 0x07200/4,
Tdcatxctrl99 = 0x0600c/4,
Tipg = 0x0cb00/4,
Txpbsize = 0x0cc00/4,
Hlreg0 = 0x04240/4,
Hlreg1 = 0x04244/4,
Msca = 0x0425c/4,
Msrwd = 0x04260/4,
Mhadd = 0x04268/4,
Pcss1 = 0x04288/4,
Pcss2 = 0x0428c/4,
Xpcss = 0x04290/4,
Serdesc = 0x04298/4,
Macs = 0x0429c/4,
Autoc = 0x042a0/4,
Links = 0x042a4/4,
Links2 = 0x04324/4,
Autoc2 = 0x042a8/4,
};
enum {
Factive = 1<<0,
Enable = 1<<31,
Rst = 1<<26,
Ten = 1<<25,
Te = 1<<0,
Bam = 1<<10,
Upe = 1<<9,
Mpe = 1<<8,
Pthresh = 0,
Hthresh = 8,
Wthresh = 16,
Renable = 1<<25,
Rxen = 1<<0,
Dmbyps = 1<<1,
Rdmt½ = 0,
Rdmt¼ = 1,
Rdmt⅛ = 2,
Crcstrip = 1<<1,
Rscfrstsize = 037<<17,
Ippcse = 1<<12,
EEstart = 1<<0,
EEdone = 1<<1,
Irx0 = 1<<0,
Itx0 = 1<<1,
Lsc = 1<<20,
Lnkup = 1<<30,
Lnkspd = 1<<29,
Txcrcen = 1<<0,
Rxcrcstrip = 1<<1,
Jumboen = 1<<2,
Txpaden = 1<<10,
Flu = 1<<0,
Lmsshift = 13,
Lmsmask = 7,
};
typedef struct Ctlr Ctlr;
typedef struct Rd Rd;
typedef struct Td Td;
typedef struct {
uint reg;
char *name;
} Stat;
Stat stattab[] = {
0x4000, "crc error",
0x4004, "illegal byte",
0x4008, "short packet",
0x3fa0, "missed pkt0",
0x4034, "mac local flt",
0x4038, "mac rmt flt",
0x4040, "rx length err",
0x3f60, "xon tx",
0xcf60, "xon rx",
0x3f68, "xoff tx",
0xcf68, "xoff rx",
0x405c, "rx 040",
0x4060, "rx 07f",
0x4064, "rx 100",
0x4068, "rx 200",
0x406c, "rx 3ff",
0x4070, "rx big",
0x4074, "rx ok",
0x4078, "rx bcast",
0x3fc0, "rx no buf0",
0x40a4, "rx runt",
0x40a8, "rx frag",
0x40ac, "rx ovrsz",
0x40b0, "rx jab",
0x40d0, "rx pkt",
0x40d4, "tx pkt",
0x40d8, "tx 040",
0x40dc, "tx 07f",
0x40e0, "tx 100",
0x40e4, "tx 200",
0x40e8, "tx 3ff",
0x40ec, "tx big",
0x40f4, "tx bcast",
0x4120, "xsum err",
};
enum {
Pif = 1<<7,
Ipcs = 1<<6,
L4cs = 1<<5,
Tcpcs = 1<<4,
Vp = 1<<3,
Ixsm = 1<<2,
Reop = 1<<1,
Rdd = 1<<0,
};
struct Rd {
u32int addr[2];
ushort length;
ushort cksum;
uchar status;
uchar errors;
ushort vlan;
};
enum {
Rs = 1<<3,
Ic = 1<<2,
Ifcs = 1<<1,
Teop = 1<<0,
Tdd = 1<<0,
};
struct Td {
u32int addr[2];
ushort length;
uchar cso;
uchar cmd;
uchar status;
uchar css;
ushort vlan;
};
struct Ctlr {
Pcidev *p;
Ether *edev;
int type;
u32int *reg;
u32int *msix;
u32int *physreg;
u32int *physmsix;
uchar flag;
int nrd;
int ntd;
int nrb;
uint rbsz;
int procsrunning;
int attached;
Lock slock;
Lock alock;
QLock tlock;
Rendez lrendez;
Rendez trendez;
Rendez rrendez;
uint im;
uint lim;
uint rim;
uint tim;
Lock imlock;
Rd* rdba;
Block** rb;
int rdt;
int rdfree;
Td* tdba;
int tdh;
int tdt;
Block** tb;
uchar ra[Eaddrlen];
uchar mta[128];
ulong stats[nelem(stattab)];
uint speeds[3];
};
enum {
I82598 = 1,
I82599,
};
static Ctlr *ctlrtab[4];
static int nctlr;
static Lock rblock;
static Block *rbpool;
static void
readstats(Ctlr *c)
{
int i;
lock(&c->slock);
for(i = 0; i < nelem(c->stats); i++)
c->stats[i] += c->reg[stattab[i].reg >> 2];
unlock(&c->slock);
}
static int speedtab[] = {
0,
1000,
10000,
};
static long
ifstat(Ether *e, void *a, long n, ulong offset)
{
uint i, *t;
char *s, *p, *q;
Ctlr *c;
c = e->ctlr;
p = s = malloc(READSTR);
if(p == nil)
error(Enomem);
q = p + READSTR;
readstats(c);
for(i = 0; i < nelem(stattab); i++)
if(c->stats[i] > 0)
p = seprint(p, q, "%.10s  %uld\n", stattab[i].name, c->stats[i]);
t = c->speeds;
p = seprint(p, q, "speeds: 0:%d 1000:%d 10000:%d\n", t[0], t[1], t[2]);
p = seprint(p, q, "mtu: min:%d max:%d\n", e->minmtu, e->maxmtu);
seprint(p, q, "rdfree %d rdh %d rdt %d\n", c->rdfree, c->reg[Rdt],
c->reg[Rdh]);
n = readstr(offset, a, n, s);
free(s);
return n;
}
static void
ienable(Ctlr *c, int i)
{
ilock(&c->imlock);
c->im |= i;
c->reg[Ims] = c->im;
iunlock(&c->imlock);
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
Ctlr *c;
Ether *e;
e = v;
c = e->ctlr;
for (;;) {
r = c->reg[Links];
e->link = (r & Lnkup) != 0;
i = 0;
if(e->link)
i = 1 + ((r & Lnkspd) != 0);
c->speeds[i]++;
e->mbps = speedtab[i];
c->lim = 0;
ienable(c, Lsc);
sleep(&c->lrendez, lim, c);
c->lim = 0;
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
iunlock(&rblock);
}
static int
cleanup(Ctlr *c, int tdh)
{
Block *b;
uint m, n;
m = c->ntd - 1;
while(c->tdba[n = NEXTPOW2(tdh, m)].status & Tdd){
tdh = n;
b = c->tb[tdh];
c->tb[tdh] = 0;
if (b)
freeb(b);
c->tdba[tdh].status = 0;
}
return tdh;
}
void
transmit(Ether *e)
{
uint i, m, tdt, tdh;
Ctlr *c;
Block *b;
Td *t;
c = e->ctlr;
if(!canqlock(&c->tlock)){
ienable(c, Itx0);
return;
}
tdh = c->tdh = cleanup(c, c->tdh);
tdt = c->tdt;
m = c->ntd - 1;
for(i = 0; ; i++){
if(NEXTPOW2(tdt, m) == tdh){
ienable(c, Itx0);
break;
}
if((b = qget(e->oq)) == nil)
break;
assert(c->tdba != nil);
t = c->tdba + tdt;
t->addr[0] = PCIWADDR(b->rp);
t->length = BLEN(b);
t->cmd = Ifcs | Teop;
if (!Goslow)
t->cmd |= Rs;
c->tb[tdt] = b;
tdt = NEXTPOW2(tdt, m);
}
if(i) {
coherence();
c->reg[Tdt] = c->tdt = tdt;
coherence();
ienable(c, Itx0);
}
qunlock(&c->tlock);
}
static int
tim(void *c)
{
return ((Ctlr*)c)->tim != 0;
}
static void
tproc(void *v)
{
Ctlr *c;
Ether *e;
e = v;
c = e->ctlr;
for (;;) {
sleep(&c->trendez, tim, c);
c->tim = 0;
transmit(e);
}
}
static void
rxinit(Ctlr *c)
{
int i, is598;
Block *b;
c->reg[Rxctl] &= ~Rxen;
c->reg[Rxdctl] = 0;
for(i = 0; i < c->nrd; i++){
b = c->rb[i];
c->rb[i] = 0;
if(b)
freeb(b);
}
c->rdfree = 0;
coherence();
c->reg[Fctrl] |= Bam;
c->reg[Fctrl] &= ~(Upe | Mpe);
c->reg[Rxcsum] &= ~Ippcse;
c->reg[Hlreg0] &= ~Jumboen;
c->reg[Hlreg0] |= Txcrcen | Rxcrcstrip | Txpaden;
c->reg[Srrctl] = (c->rbsz + 1024 - 1) / 1024;
c->reg[Mhadd] = c->rbsz << 16;
c->reg[Rbal] = PCIWADDR(c->rdba);
c->reg[Rbah] = 0;
c->reg[Rdlen] = c->nrd*sizeof(Rd);
c->reg[Rdh] = 0;
c->reg[Rdt] = c->rdt = 0;
coherence();
is598 = (c->type == I82598);
if (is598)
c->reg[Rdrxctl] = Rdmt¼;
else {
c->reg[Rdrxctl] |= Crcstrip;
c->reg[Rdrxctl] &= ~Rscfrstsize;
}
if (Goslow && is598)
c->reg[Rxdctl] = 8<<Wthresh | 8<<Pthresh | 4<<Hthresh | Renable;
else
c->reg[Rxdctl] = Renable;
coherence();
while (!(c->reg[Rxdctl] & Renable))
;
c->reg[Rxctl] |= Rxen | (c->type == I82598? Dmbyps: 0);
}
static void
replenish(Ctlr *c, uint rdh)
{
int rdt, m, i;
Block *b;
Rd *r;
m = c->nrd - 1;
i = 0;
for(rdt = c->rdt; NEXTPOW2(rdt, m) != rdh; rdt = NEXTPOW2(rdt, m)){
r = c->rdba + rdt;
if((b = rballoc()) == nil){
print("82598: no buffers\n");
break;
}
c->rb[rdt] = b;
r->addr[0] = PCIWADDR(b->rp);
r->status = 0;
c->rdfree++;
i++;
}
if(i) {
coherence();
c->reg[Rdt] = c->rdt = rdt;
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
uint m, rdh;
Block *b;
Ctlr *c;
Ether *e;
Rd *r;
e = v;
c = e->ctlr;
m = c->nrd - 1;
for (rdh = 0; ; ) {
replenish(c, rdh);
ienable(c, Irx0);
sleep(&c->rrendez, rim, c);
for (;;) {
c->rim = 0;
r = c->rdba + rdh;
if(!(r->status & Rdd))
break;
b = c->rb[rdh];
c->rb[rdh] = 0;
if (r->length > ETHERMAXTU)
print("82598: got jumbo of %d bytes\n", r->length);
b->wp += r->length;
b->lim = b->wp;
etheriq(e, b, 1);
c->rdfree--;
rdh = NEXTPOW2(rdh, m);
if (c->rdfree <= c->nrd - 16)
replenish(c, rdh);
}
}
}
static void
promiscuous(void *a, int on)
{
Ctlr *c;
Ether *e;
e = a;
c = e->ctlr;
if(on)
c->reg[Fctrl] |= Upe | Mpe;
else
c->reg[Fctrl] &= ~(Upe | Mpe);
}
static void
multicast(void *a, uchar *ea, int on)
{
int b, i;
Ctlr *c;
Ether *e;
e = a;
c = e->ctlr;
i = ea[5] >> 1;
b = (ea[5]&1)<<4 | ea[4]>>4;
b = 1 << b;
if(on)
c->mta[i] |= b;
c->reg[Mta+i] = c->mta[i];
}
static void
freemem(Ctlr *c)
{
Block *b;
while(b = rballoc()){
b->free = 0;
freeb(b);
}
free(c->rdba);
c->rdba = nil;
free(c->tdba);
c->tdba = nil;
free(c->rb);
c->rb = nil;
free(c->tb);
c->tb = nil;
}
static int
detach(Ctlr *c)
{
int i, is598;
c->reg[Imc] = ~0;
c->reg[Ctrl] |= Rst;
for(i = 0; i < 100; i++){
delay(1);
if((c->reg[Ctrl] & Rst) == 0)
break;
}
if (i >= 100)
return -1;
is598 = (c->type == I82598);
if (is598) {
delay(50);
c->reg[Ecc] &= ~(1<<21 | 1<<18 | 1<<9 | 1<<6);
}
for(i = 1; i < 16; i++)
c->reg[is598? Rah98: Rah99] &= ~Enable;
for(i = 0; i < 128; i++)
c->reg[Mta + i] = 0;
for(i = 1; i < (is598? 640: 128); i++)
c->reg[Vfta + i] = 0;
c->attached = 0;
return 0;
}
static void
shutdown(Ether *e)
{
detach(e->ctlr);
}
static ushort
eeread(Ctlr *c, int i)
{
c->reg[Eerd] = EEstart | i<<2;
while((c->reg[Eerd] & EEdone) == 0)
;
return c->reg[Eerd] >> 16;
}
static int
eeload(Ctlr *c)
{
ushort u, v, p, l, i, j;
if((eeread(c, 0) & 0xc0) != 0x40)
return -1;
u = 0;
for(i = 0; i < 0x40; i++)
u += eeread(c, i);
for(i = 3; i < 0xf; i++){
p = eeread(c, i);
l = eeread(c, p++);
if((int)p + l + 1 > 0xffff)
continue;
for(j = p; j < p + l; j++)
u += eeread(c, j);
}
if(u != 0xbaba)
return -1;
if(c->reg[Status] & (1<<3))
u = eeread(c, 10);
else
u = eeread(c, 9);
u++;
for(i = 0; i < Eaddrlen;){
v = eeread(c, u + i/2);
c->ra[i++] = v;
c->ra[i++] = v>>8;
}
c->ra[5] += (c->reg[Status] & 0xc) >> 2;
return 0;
}
static int
reset(Ctlr *c)
{
int i, is598;
uchar *p;
if(detach(c)){
print("82598: reset timeout\n");
return -1;
}
if(eeload(c)){
print("82598: eeprom failure\n");
return -1;
}
p = c->ra;
is598 = (c->type == I82598);
c->reg[is598? Ral98: Ral99] = p[3]<<24 | p[2]<<16 | p[1]<<8 | p[0];
c->reg[is598? Rah98: Rah99] = p[5]<<8 | p[4] | Enable;
readstats(c);
for(i = 0; i<nelem(c->stats); i++)
c->stats[i] = 0;
c->reg[Ctrlext] |= 1 << 16;
if (Goslow) {
c->reg[Fcrtl] = 0x10000 | Enable;
c->reg[Fcrth] = 0x40000 | Enable;
c->reg[Rcrtv] = 0x6000;
} else
c->reg[Fcrtl] = c->reg[Fcrth] = c->reg[Rcrtv] = 0;
c->reg[Ivar+0] = 0 | 1<<7;
c->reg[Ivar+64/4] = 1 | 1<<7;
if (Goslow) {
for(i = Itr; i < Itr + 20; i++)
c->reg[i] = 128;
c->reg[Itr + Itx0] = 256;
} else {
for(i = Itr; i < Itr + 20; i++)
c->reg[i] = 0;
c->reg[Itr + Itx0] = 0;
}
return 0;
}
static void
txinit(Ctlr *c)
{
Block *b;
int i;
if (Goslow)
c->reg[Txdctl] = 16<<Wthresh | 16<<Pthresh;
else
c->reg[Txdctl] = 0;
if (c->type == I82599)
c->reg[Dtxctl99] = 0;
coherence();
for(i = 0; i < c->ntd; i++){
b = c->tb[i];
c->tb[i] = 0;
if(b)
freeb(b);
}
assert(c->tdba != nil);
memset(c->tdba, 0, c->ntd * sizeof(Td));
c->reg[Tdbal] = PCIWADDR(c->tdba);
c->reg[Tdbah] = 0;
c->reg[Tdlen] = c->ntd*sizeof(Td);
c->reg[Tdh] = 0;
c->tdh = c->ntd - 1;
c->reg[Tdt] = c->tdt = 0;
coherence();
if (c->type == I82599)
c->reg[Dtxctl99] |= Te;
coherence();
c->reg[Txdctl] |= Ten;
coherence();
while (!(c->reg[Txdctl] & Ten))
;
}
static void
attach(Ether *e)
{
Block *b;
Ctlr *c;
char buf[KNAMELEN];
c = e->ctlr;
c->edev = e;
lock(&c->alock);
if(waserror()){
unlock(&c->alock);
freemem(c);
nexterror();
}
if(c->rdba == nil) {
c->nrd = Nrd;
c->ntd = Ntd;
c->rdba = mallocalign(c->nrd * sizeof *c->rdba, Descalign, 0, 0);
c->tdba = mallocalign(c->ntd * sizeof *c->tdba, Descalign, 0, 0);
c->rb = malloc(c->nrd * sizeof(Block *));
c->tb = malloc(c->ntd * sizeof(Block *));
if (c->rdba == nil || c->tdba == nil ||
c->rb == nil || c->tb == nil)
error(Enomem);
for(c->nrb = 0; c->nrb < 2*Nrb; c->nrb++){
b = allocb(c->rbsz + BY2PG);
if(b == nil)
error(Enomem);
b->free = rbfree;
freeb(b);
}
}
if (!c->attached) {
rxinit(c);
txinit(c);
if (!c->procsrunning) {
snprint(buf, sizeof buf, "#l%dl", e->ctlrno);
kproc(buf, lproc, e);
snprint(buf, sizeof buf, "#l%dr", e->ctlrno);
kproc(buf, rproc, e);
snprint(buf, sizeof buf, "#l%dt", e->ctlrno);
kproc(buf, tproc, e);
c->procsrunning = 1;
}
c->attached = 1;
}
unlock(&c->alock);
poperror();
}
static void
interrupt(Ureg*, void *v)
{
int icr, im;
Ctlr *c;
Ether *e;
e = v;
c = e->ctlr;
ilock(&c->imlock);
c->reg[Imc] = ~0;
im = c->im;
while((icr = c->reg[Icr] & c->im) != 0){
if(icr & Irx0){
im &= ~Irx0;
c->rim = Irx0;
wakeup(&c->rrendez);
}
if(icr & Itx0){
im &= ~Itx0;
c->tim = Itx0;
wakeup(&c->trendez);
}
if(icr & Lsc){
im &= ~Lsc;
c->lim = Lsc;
wakeup(&c->lrendez);
}
}
c->reg[Ims] = c->im = im;
iunlock(&c->imlock);
}
static void
scan(void)
{
int pciregs, pcimsix, type;
ulong io, iomsi;
void *mem, *memmsi;
Ctlr *c;
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
if(nctlr == nelem(ctlrtab)){
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
c = malloc(sizeof *c);
if(c == nil) {
vunmap(mem, p->mem[pciregs].size);
vunmap(memmsi, p->mem[pcimsix].size);
error(Enomem);
}
c->p = p;
c->type = type;
c->physreg = (u32int*)io;
c->physmsix = (u32int*)iomsi;
c->reg = (u32int*)mem;
c->msix = (u32int*)memmsi;
c->rbsz = Rbsz;
if(reset(c)){
print("i82598: can't reset\n");
free(c);
vunmap(mem, p->mem[pciregs].size);
vunmap(memmsi, p->mem[pcimsix].size);
continue;
}
pcisetbme(p);
ctlrtab[nctlr++] = c;
}
}
static int
pnp(Ether *e)
{
int i;
Ctlr *c = nil;
if(nctlr == 0)
scan();
for(i = 0; i < nctlr; i++){
c = ctlrtab[i];
if(c == nil || c->flag & Factive)
continue;
if(e->port == 0 || e->port == (ulong)c->reg)
break;
}
if (i >= nctlr)
return -1;
c->flag |= Factive;
e->ctlr = c;
e->port = (uintptr)c->physreg;
e->irq = c->p->intl;
e->tbdf = c->p->tbdf;
e->mbps = 10000;
e->maxmtu = ETHERMAXTU;
memmove(e->ea, c->ra, Eaddrlen);
e->arg = e;
e->attach = attach;
e->ctl = ctl;
e->ifstat = ifstat;
e->interrupt = interrupt;
e->multicast = multicast;
e->promiscuous = promiscuous;
e->shutdown = shutdown;
e->transmit = transmit;
return 0;
}
void
ether82598link(void)
{
addethercard("i82598", pnp);
}