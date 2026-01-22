#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#define DEBUG (0)
#define debug if(DEBUG)print
enum {
Nrde = 32,
Ntde = 4,
};
#define Rbsz ROUNDUP(sizeof(Etherpkt)+4, 4)
enum {
Swr = 0x00000001,
Bar = 0x00000002,
Dsl = 0x0000007C,
Ble = 0x00000080,
Pbl = 0x00003F00,
Cal = 0x0000C000,
Cal8 = 0x00004000,
Cal16 = 0x00008000,
Cal32 = 0x0000C000,
Tap = 0x000E0000,
Dbo = 0x00100000,
Rml = 0x00200000,
};
enum {
Ti = 0x00000001,
Tps = 0x00000002,
Tu = 0x00000004,
Tjt = 0x00000008,
Unf = 0x00000020,
Ri = 0x00000040,
Ru = 0x00000080,
Rps = 0x00000100,
Rwt = 0x00000200,
Eti = 0x00000400,
Gte = 0x00000800,
Fbe = 0x00002000,
Ais = 0x00008000,
Nis = 0x00010000,
Rs = 0x000E0000,
Ts = 0x00700000,
Eb = 0x03800000,
};
enum {
Hp = 0x00000001,
Sr = 0x00000002,
Ho = 0x00000004,
Pb = 0x00000008,
If = 0x00000010,
Sb = 0x00000020,
Pr = 0x00000040,
Pm = 0x00000080,
Fd = 0x00000200,
Om = 0x00000C00,
Fc = 0x00001000,
St = 0x00002000,
Tr = 0x0000C000,
Tr128 = 0x00000000,
Tr256 = 0x00004000,
Tr512 = 0x00008000,
Tr1024 = 0x0000C000,
Ca = 0x00020000,
Ps = 0x00040000,
Hbd = 0x00080000,
Imm = 0x00100000,
Sf = 0x00200000,
Ttm = 0x00400000,
Pcs = 0x00800000,
Scr = 0x01000000,
Mbo = 0x02000000,
Ra = 0x40000000,
Sc = 0x80000000,
TrMODE = Tr512,
};
enum {
Scs = 0x00000001,
Sclk = 0x00000002,
Sdi = 0x00000004,
Sdo = 0x00000008,
Ss = 0x00000800,
Wr = 0x00002000,
Rd = 0x00004000,
Mdc = 0x00010000,
Mdo = 0x00020000,
Mii = 0x00040000,
Mdi = 0x00080000,
};
enum {
Gpc = 0x00000100,
};
typedef struct Des {
int status;
int control;
ulong addr;
void* bp;
} Des;
enum {
Of = 0x00000001,
Ce = 0x00000002,
Db = 0x00000004,
Re = 0x00000008,
Rw = 0x00000010,
Ft = 0x00000020,
Cs = 0x00000040,
Tl = 0x00000080,
Ls = 0x00000100,
Fs = 0x00000200,
Mf = 0x00000400,
Rf = 0x00000800,
Dt = 0x00003000,
De = 0x00004000,
Fl = 0x3FFF0000,
Ff = 0x40000000,
Def = 0x00000001,
Uf = 0x00000002,
Lf = 0x00000004,
Cc = 0x00000078,
Hf = 0x00000080,
Ec = 0x00000100,
Lc = 0x00000200,
Nc = 0x00000400,
Lo = 0x00000800,
To = 0x00004000,
Es = 0x00008000,
Own = 0x80000000,
};
enum {
Bs1 = 0x000007FF,
Bs2 = 0x003FF800,
Ch = 0x01000000,
Er = 0x02000000,
Ft0 = 0x00400000,
Dpd = 0x00800000,
Ac = 0x04000000,
Set = 0x08000000,
Ft1 = 0x10000000,
Fseg = 0x20000000,
Lseg = 0x40000000,
Ic = 0x80000000,
};
enum {
Bmcr = 0,
Bmsr = 1,
Phyidr1 = 2,
Phyidr2 = 3,
Anar = 4,
Anlpar = 5,
Aner = 6,
};
enum {
Tulip0 = (0x0009<<16)|0x1011,
Tulip1 = (0x0014<<16)|0x1011,
Tulip3 = (0x0019<<16)|0x1011,
Pnic = (0x0002<<16)|0x11AD,
Pnic2 = (0xC115<<16)|0x11AD,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int port;
Pcidev* pcidev;
Ctlr* next;
int active;
int id;
uchar *srom;
int sromsz;
uchar* sromea;
uchar* leaf;
int sct;
int k;
uchar* infoblock[16];
int sctk;
int curk;
uchar* type5block;
int phy[32];
int phyreset;
int curphyad;
int fdx;
int ttm;
uchar fd;
int medium;
int csr6;
int mask;
int mbps;
Des* rdr;
int nrdr;
int rdrx;
Des* tdr;
int ntdr;
int tdrh;
int tdri;
int ntq;
Block* setupbp;
ulong of;
ulong ce;
ulong cs;
ulong tl;
ulong rf;
ulong de;
ulong uf;
ulong ec;
ulong lc;
ulong nc;
ulong lo;
ulong to;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
#define csr32r(c, r) (inl((c)->port+((r)*8)))
#define csr32w(c, r, l) (outl((c)->port+((r)*8), (ulong)(l)))
static void
attach(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
if(!(ctlr->csr6 & Sr)){
ctlr->csr6 |= Sr;
csr32w(ctlr, 6, ctlr->csr6);
}
}
static void
transmit(Ether* ether)
{
Ctlr *ctlr;
Block *bp;
Des *des;
int control;
RingBuf *tb;
ctlr = ether->ctlr;
while(ctlr->ntq < (ctlr->ntdr-1)){
if(ctlr->setupbp){
bp = ctlr->setupbp;
ctlr->setupbp = 0;
control = Ic|Set|BLEN(bp);
}
else{
if(ether->ntb == 0)
break;
tb = &ether->tb[ether->ti];
if(tb->owner != Interface)
break;
bp = allocb(tb->len);
memmove(bp->wp, tb->pkt, tb->len);
memmove(bp->wp+Eaddrlen, ether->ea, Eaddrlen);
bp->wp += tb->len;
tb->owner = Host;
ether->ti = NEXT(ether->ti, ether->ntb);
control = Ic|Lseg|Fseg|BLEN(bp);
}
ctlr->tdr[PREV(ctlr->tdrh, ctlr->ntdr)].control &= ~Ic;
des = &ctlr->tdr[ctlr->tdrh];
des->bp = bp;
des->addr = PADDR(bp->rp);
des->control |= control;
ctlr->ntq++;
des->status = Own;
csr32w(ctlr, 1, 0);
ctlr->tdrh = NEXT(ctlr->tdrh, ctlr->ntdr);
}
}
static void
interrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Ether *ether;
int len, status;
Des *des;
RingBuf *ring;
ether = arg;
ctlr = ether->ctlr;
while((status = csr32r(ctlr, 5)) & (Nis|Ais)){
csr32w(ctlr, 5, status);
status &= (ctlr->mask & ~(Nis|Ais|Ti));
if(status & Ri){
des = &ctlr->rdr[ctlr->rdrx];
while((des->status & Own) == 0){
len = ((des->status & Fl)>>16)-4;
if(des->status & Es){
if(des->status & Of)
ctlr->of++;
if(des->status & Ce)
ctlr->ce++;
if(des->status & Cs)
ctlr->cs++;
if(des->status & Tl)
ctlr->tl++;
if(des->status & Rf)
ctlr->rf++;
if(des->status & De)
ctlr->de++;
}
else{
ring = &ether->rb[ether->ri];
if(ring->owner == Interface){
ring->owner = Host;
ring->len = len;
memmove(ring->pkt, des->bp, len);
ether->ri = NEXT(ether->ri, ether->nrb);
}
}
des->control &= Er;
des->control |= Rbsz;
des->status = Own;
ctlr->rdrx = NEXT(ctlr->rdrx, ctlr->nrdr);
des = &ctlr->rdr[ctlr->rdrx];
}
status &= ~Ri;
}
if(status & Unf){
csr32w(ctlr, 6, ctlr->csr6 & ~St);
switch(ctlr->csr6 & Tr){
case Tr128:
len = Tr256;
break;
case Tr256:
len = Tr512;
break;
case Tr512:
len = Tr1024;
break;
default:
case Tr1024:
len = Sf;
break;
}
ctlr->csr6 = (ctlr->csr6 & ~Tr)|len;
csr32w(ctlr, 6, ctlr->csr6);
csr32w(ctlr, 5, Tps);
status &= ~(Unf|Tps);
}
while(ctlr->ntq){
des = &ctlr->tdr[ctlr->tdri];
if(des->status & Own)
break;
if(des->status & Es){
if(des->status & Uf)
ctlr->uf++;
if(des->status & Ec)
ctlr->ec++;
if(des->status & Lc)
ctlr->lc++;
if(des->status & Nc)
ctlr->nc++;
if(des->status & Lo)
ctlr->lo++;
if(des->status & To)
ctlr->to++;
}
freeb(des->bp);
des->control &= Er;
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, ctlr->ntdr);
}
transmit(ether);
if(status)
panic("#l%d: status %8.8uX\n", ether->ctlrno, status);
}
}
static void
ctlrinit(Ether* ether)
{
Ctlr *ctlr;
Des *des;
Block *bp;
int i;
uchar bi[Eaddrlen*2];
ctlr = ether->ctlr;
ctlr->rdr = malloc(ctlr->nrdr*sizeof(Des));
for(des = ctlr->rdr; des < &ctlr->rdr[ctlr->nrdr]; des++){
des->bp = malloc(Rbsz);
des->status = Own;
des->control = Rbsz;
des->addr = PADDR(des->bp);
}
ctlr->rdr[ctlr->nrdr-1].control |= Er;
ctlr->rdrx = 0;
csr32w(ctlr, 3, PADDR(ctlr->rdr));
ctlr->tdr = ialloc(ctlr->ntdr*sizeof(Des), 32);
ctlr->tdr[ctlr->ntdr-1].control |= Er;
ctlr->tdrh = 0;
ctlr->tdri = 0;
csr32w(ctlr, 4, PADDR(ctlr->tdr));
ctlr->mask = Nis|Ais|Fbe|Rwt|Rps|Ru|Ri|Unf|Tjt|Tps|Ti;
csr32w(ctlr, 5, ctlr->mask);
csr32w(ctlr, 7, ctlr->mask);
ctlr->csr6 |= St;
csr32w(ctlr, 6, ctlr->csr6);
for(i = 0; i < Eaddrlen/2; i++){
bi[i*4] = ether->ea[i*2];
bi[i*4+1] = ether->ea[i*2+1];
bi[i*4+2] = ether->ea[i*2+1];
bi[i*4+3] = ether->ea[i*2];
}
bp = allocb(Eaddrlen*2*16);
memset(bp->rp, 0xFF, sizeof(bi));
for(i = sizeof(bi); i < sizeof(bi)*16; i += sizeof(bi))
memmove(bp->rp+i, bi, sizeof(bi));
bp->wp += sizeof(bi)*16;
ctlr->setupbp = bp;
transmit(ether);
}
static void
csr9w(Ctlr* ctlr, int data)
{
csr32w(ctlr, 9, data);
microdelay(1);
}
static int
miimdi(Ctlr* ctlr, int n)
{
int data, i;
data = 0;
for(i = n-1; i >= 0; i--){
if(csr32r(ctlr, 9) & Mdi)
data |= (1<<i);
csr9w(ctlr, Mii|Mdc);
csr9w(ctlr, Mii);
}
csr9w(ctlr, 0);
return data;
}
static void
miimdo(Ctlr* ctlr, int bits, int n)
{
int i, mdo;
for(i = n-1; i >= 0; i--){
if(bits & (1<<i))
mdo = Mdo;
else
mdo = 0;
csr9w(ctlr, mdo);
csr9w(ctlr, mdo|Mdc);
csr9w(ctlr, mdo);
}
}
static int
miir(Ctlr* ctlr, int phyad, int regad)
{
int data, i;
if(ctlr->id == Pnic){
i = 1000;
csr32w(ctlr, 20, 0x60020000|(phyad<<23)|(regad<<18));
do{
microdelay(1);
data = csr32r(ctlr, 20);
}while((data & 0x80000000) && --i);
if(i == 0)
return -1;
return data & 0xFFFF;
}
miimdo(ctlr, 0xFFFFFFFF, 32);
miimdo(ctlr, 0x1800|(phyad<<5)|regad, 14);
data = miimdi(ctlr, 18);
if(data & 0x10000)
return -1;
return data & 0xFFFF;
}
static void
miiw(Ctlr* ctlr, int phyad, int regad, int data)
{
miimdo(ctlr, 0xFFFFFFFF, 32);
data &= 0xFFFF;
data |= (0x05<<(5+5+2+16))|(phyad<<(5+2+16))|(regad<<(2+16))|(0x02<<16);
miimdo(ctlr, data, 32);
csr9w(ctlr, Mdc);
csr9w(ctlr, 0);
}
static int
sromr(Ctlr* ctlr, int r)
{
int i, op, data, size;
if(ctlr->id == Pnic){
i = 1000;
csr32w(ctlr, 19, 0x600|r);
do{
microdelay(1);
data = csr32r(ctlr, 19);
}while((data & 0x80000000) && --i);
if(ctlr->sromsz == 0)
ctlr->sromsz = 6;
return csr32r(ctlr, 9) & 0xFFFF;
}
reread:
csr9w(ctlr, Rd|Ss);
csr9w(ctlr, Rd|Ss|Scs);
csr9w(ctlr, Rd|Ss|Sclk|Scs);
csr9w(ctlr, Rd|Ss);
op = 0x06;
for(i = 3-1; i >= 0; i--){
data = Rd|Ss|(((op>>i) & 0x01)<<2)|Scs;
csr9w(ctlr, data);
csr9w(ctlr, data|Sclk);
csr9w(ctlr, data);
}
if((size = ctlr->sromsz) == 0){
if(ctlr->id == Tulip1)
ctlr->sromsz = size = 6;
else
size = 8;
}
for(size = size-1; size >= 0; size--){
data = Rd|Ss|(((r>>size) & 0x01)<<2)|Scs;
csr9w(ctlr, data);
csr9w(ctlr, data|Sclk);
csr9w(ctlr, data);
microdelay(1);
if(ctlr->sromsz == 0 && !(csr32r(ctlr, 9) & Sdo))
break;
}
data = 0;
for(i = 16-1; i >= 0; i--){
csr9w(ctlr, Rd|Ss|Sclk|Scs);
if(csr32r(ctlr, 9) & Sdo)
data |= (1<<i);
csr9w(ctlr, Rd|Ss|Scs);
}
csr9w(ctlr, 0);
if(ctlr->sromsz == 0){
ctlr->sromsz = 8-size;
goto reread;
}
return data & 0xFFFF;
}
static void
softreset(Ctlr* ctlr)
{
csr32w(ctlr, 0, Swr);
microdelay(10);
csr32w(ctlr, 0, Rml|Cal16);
delay(1);
}
static int
type5block(Ctlr* ctlr, uchar* block)
{
int csr15, i, len;
len = *block++;
if(ctlr->id != Tulip3){
for(i = 0; i < len; i++){
csr32w(ctlr, 12, *block);
block++;
}
return len;
}
for(i = 0; i < len; i++){
csr15 = *block++<<16;
csr15 |= *block++<<24;
csr32w(ctlr, 15, csr15);
debug("%8.8uX ", csr15);
}
return 2*len;
}
static int
typephylink(Ctlr* ctlr, uchar*)
{
int an, bmcr, bmsr, csr6, x;
bmcr = miir(ctlr, ctlr->curphyad, Bmcr);
miir(ctlr, ctlr->curphyad, Bmsr);
bmsr = miir(ctlr, ctlr->curphyad, Bmsr);
debug("bmcr 0x%2.2uX bmsr 0x%2.2uX\n", bmcr, bmsr);
if(((bmcr & 0x1000) && !(bmsr & 0x0020)) || !(bmsr & 0x0004))
return 0;
if(bmcr & 0x1000){
an = miir(ctlr, ctlr->curphyad, Anar);
an &= miir(ctlr, ctlr->curphyad, Anlpar) & 0x3E0;
debug("an 0x%2.uX 0x%2.2uX 0x%2.2uX\n",
miir(ctlr, ctlr->curphyad, Anar),
miir(ctlr, ctlr->curphyad, Anlpar),
an);
if(an & 0x0100)
x = 0x4000;
else if(an & 0x0080)
x = 0x2000;
else if(an & 0x0040)
x = 0x1000;
else if(an & 0x0020)
x = 0x0800;
else
x = 0;
}
else if((bmcr & 0x2100) == 0x2100)
x = 0x4000;
else if(bmcr & 0x2000){
if((bmsr & 0x4000) && ctlr->fd){
miiw(ctlr, ctlr->curphyad, Bmcr, 0x2100);
x = 0x4000;
}
else
x = 0x2000;
}
else if(bmcr & 0x0100)
x = 0x1000;
else
x = 0x0800;
csr6 = Sc|Mbo|Hbd|Ps|Ca|TrMODE|Sb;
if(ctlr->fdx & x)
csr6 |= Fd;
if(ctlr->ttm & x)
csr6 |= Ttm;
debug("csr6 0x%8.8uX 0x%8.8uX 0x%8.8luX\n",
csr6, ctlr->csr6, csr32r(ctlr, 6));
if(csr6 != ctlr->csr6){
ctlr->csr6 = csr6;
csr32w(ctlr, 6, csr6);
}
return 1;
}
static int
typephymode(Ctlr* ctlr, uchar* block, int wait)
{
uchar *p;
int len, mc, nway, phyx, timeo;
if(DEBUG){
int i;
len = (block[0] & ~0x80)+1;
for(i = 0; i < len; i++)
debug("%2.2uX ", block[i]);
debug("\n");
}
if(block[1] == 1)
len = 1;
else if(block[1] == 3)
len = 2;
else
return -1;
p = &block[5+len*block[3]+len*block[4+len*block[3]]];
mc = *p++;
mc |= *p++<<8;
nway = *p++;
nway |= *p++<<8;
ctlr->fdx = *p++;
ctlr->fdx |= *p++<<8;
ctlr->ttm = *p++;
ctlr->ttm |= *p<<8;
debug("mc %4.4uX nway %4.4uX fdx %4.4uX ttm %4.4uX\n",
mc, nway, ctlr->fdx, ctlr->ttm);
USED(mc);
phyx = block[2];
ctlr->curphyad = ctlr->phy[phyx];
ctlr->csr6 = 0;
if(typephylink(ctlr, block))
return 0;
if(!(ctlr->phyreset & (1<<phyx))){
debug("reset seq: len %d: ", block[3]);
if(ctlr->type5block)
type5block(ctlr, &ctlr->type5block[2]);
else
type5block(ctlr, &block[4+len*block[3]]);
debug("\n");
ctlr->phyreset |= (1<<phyx);
}
debug("gpr seq: len %d: ", block[3]);
type5block(ctlr, &block[3]);
debug("\n");
ctlr->csr6 = 0;
if(typephylink(ctlr, block))
return 0;
miiw(ctlr, ctlr->curphyad, Bmcr, 0);
miiw(ctlr, ctlr->curphyad, Anar, nway|1);
miiw(ctlr, ctlr->curphyad, Bmcr, 0x1000);
if(!wait)
return 0;
for(timeo = 0; timeo < 30; timeo++){
if(typephylink(ctlr, block))
return 0;
delay(100);
}
return -1;
}
static int
typesymmode(Ctlr *ctlr, uchar *block, int wait)
{
uint gpmode, gpdata, command;
USED(wait);
gpmode = block[3] | ((uint) block[4] << 8);
gpdata = block[5] | ((uint) block[6] << 8);
command = (block[7] | ((uint) block[8] << 8)) & 0x71;
if (command & 0x8000) {
print("ether2114x.c: FIXME: handle type 4 mode blocks where cmd.active_invalid != 0\n");
return -1;
}
csr32w(ctlr, 15, gpmode);
csr32w(ctlr, 15, gpdata);
ctlr->csr6 = (command & 0x71) << 18;
csr32w(ctlr, 6, ctlr->csr6);
return 0;
}
static int
type2mode(Ctlr* ctlr, uchar* block, int)
{
uchar *p;
int csr6, csr13, csr14, csr15, gpc, gpd;
csr6 = Sc|Mbo|Ca|TrMODE|Sb;
debug("type2mode: medium 0x%2.2uX\n", block[2]);
if((block[2] & 0x3F) == 0x04){
if(!ctlr->fd)
return -1;
csr6 |= Fd;
}
p = &block[3];
if(block[2] & 0x40){
csr13 = (block[4]<<8)|block[3];
csr14 = (block[6]<<8)|block[5];
csr15 = (block[8]<<8)|block[7];
p += 6;
}
else switch(block[2] & 0x3F){
default:
return -1;
case 0x00:
csr13 = 0x00000001;
csr14 = 0x00007F3F;
csr15 = 0x00000008;
break;
case 0x01:
csr13 = 0x00000009;
csr14 = 0x00000705;
csr15 = 0x00000006;
break;
case 0x02:
csr13 = 0x00000009;
csr14 = 0x00000705;
csr15 = 0x0000000E;
break;
case 0x04:
csr13 = 0x00000001;
csr14 = 0x00007F3D;
csr15 = 0x00000008;
break;
}
gpc = *p++<<16;
gpc |= *p++<<24;
gpd = *p++<<16;
gpd |= *p<<24;
csr32w(ctlr, 13, 0);
csr32w(ctlr, 14, csr14);
csr32w(ctlr, 15, gpc|csr15);
delay(10);
csr32w(ctlr, 15, gpd|csr15);
csr32w(ctlr, 13, csr13);
ctlr->csr6 = csr6;
csr32w(ctlr, 6, ctlr->csr6);
debug("type2mode: csr13 %8.8uX csr14 %8.8uX csr15 %8.8uX\n",
csr13, csr14, csr15);
debug("type2mode: gpc %8.8uX gpd %8.8uX csr6 %8.8uX\n",
gpc, gpd, csr6);
return 0;
}
static int
type0link(Ctlr* ctlr, uchar* block)
{
int m, polarity, sense;
m = (block[3]<<8)|block[2];
sense = 1<<((m & 0x000E)>>1);
if(m & 0x0080)
polarity = sense;
else
polarity = 0;
return (csr32r(ctlr, 12) & sense)^polarity;
}
static int
type0mode(Ctlr* ctlr, uchar* block, int wait)
{
int csr6, m, timeo;
csr6 = Sc|Mbo|Hbd|Ca|TrMODE|Sb;
debug("type0: medium 0x%uX, fd %d: 0x%2.2uX 0x%2.2uX 0x%2.2uX 0x%2.2uX\n",
ctlr->medium, ctlr->fd, block[0], block[1], block[2], block[3]);
switch(block[0]){
default:
break;
case 0x04:
case 0x05:
case 0x08:
if(!ctlr->fd)
return -1;
csr6 |= Fd;
break;
}
m = (block[3]<<8)|block[2];
if(m & 0x0001)
csr6 |= Ps;
if(m & 0x0010)
csr6 |= Ttm;
if(m & 0x0020)
csr6 |= Pcs;
if(m & 0x0040)
csr6 |= Scr;
csr32w(ctlr, 12, block[1]);
microdelay(10);
csr32w(ctlr, 6, csr6);
ctlr->csr6 = csr6;
if(!wait)
return 0;
for(timeo = 0; timeo < 30; timeo++){
if(type0link(ctlr, block))
return 0;
delay(100);
}
return -1;
}
static int
media21041(Ether* ether, int wait)
{
Ctlr* ctlr;
uchar *block;
int csr6, csr13, csr14, csr15, medium, timeo;
ctlr = ether->ctlr;
block = ctlr->infoblock[ctlr->curk];
debug("media21041: block[0] %2.2uX, medium %4.4uX sct %4.4uX\n",
block[0], ctlr->medium, ctlr->sct);
medium = block[0] & 0x3F;
if(ctlr->medium >= 0 && medium != ctlr->medium)
return 0;
if(ctlr->sct != 0x0800 && (ctlr->sct & 0x3F) != medium)
return 0;
csr6 = Sc|Mbo|Ca|TrMODE|Sb;
if(block[0] & 0x40){
csr13 = (block[2]<<8)|block[1];
csr14 = (block[4]<<8)|block[3];
csr15 = (block[6]<<8)|block[5];
}
else switch(medium){
default:
return -1;
case 0x00:
csr13 = 0xEF01;
csr14 = 0xFF3F;
csr15 = 0x0008;
break;
case 0x01:
csr13 = 0xEF09;
csr14 = 0xF73D;
csr15 = 0x0006;
break;
case 0x02:
csr13 = 0xEF09;
csr14 = 0xF73D;
csr15 = 0x000E;
break;
case 0x04:
csr13 = 0xEF01;
csr14 = 0xFF3D;
csr15 = 0x0008;
break;
}
csr32w(ctlr, 13, 0);
csr32w(ctlr, 14, csr14);
csr32w(ctlr, 15, csr15);
csr32w(ctlr, 13, csr13);
delay(10);
if(medium == 0x04)
csr6 |= Fd;
ctlr->csr6 = csr6;
csr32w(ctlr, 6, ctlr->csr6);
debug("media21041: csr6 %8.8uX csr13 %4.4uX csr14 %4.4uX csr15 %4.4uX\n",
csr6, csr13, csr14, csr15);
if(!wait)
return 0;
for(timeo = 0; timeo < 30; timeo++){
if(!(csr32r(ctlr, 12) & 0x0002)){
debug("media21041: ok: csr12 %4.4luX timeo %d\n",
csr32r(ctlr, 12), timeo);
return 10;
}
delay(100);
}
debug("media21041: !ok: csr12 %4.4luX\n", csr32r(ctlr, 12));
return -1;
}
static int
mediaxx(Ether* ether, int wait)
{
Ctlr* ctlr;
uchar *block;
ctlr = ether->ctlr;
block = ctlr->infoblock[ctlr->curk];
if(block[0] & 0x80){
switch(block[1]){
default:
return -1;
case 0:
if(ctlr->medium >= 0 && block[2] != ctlr->medium)
return 0;
if(ctlr->sct != 0x0800 && (ctlr->sct & 0x3F) != block[2])
return 0;
if(type0mode(ctlr, block+2, wait))
return 0;
break;
case 1:
if(typephymode(ctlr, block, wait))
return 0;
break;
case 2:
debug("type2: medium %d block[2] %d\n",
ctlr->medium, block[2]);
if(ctlr->medium >= 0 && ((block[2] & 0x3F) != ctlr->medium))
return 0;
if(type2mode(ctlr, block, wait))
return 0;
break;
case 3:
if(typephymode(ctlr, block, wait))
return 0;
break;
case 4:
debug("type4: medium %d block[2] %d\n",
ctlr->medium, block[2]);
if(ctlr->medium >= 0 && ((block[2] & 0x3F) != ctlr->medium))
return 0;
if(typesymmode(ctlr, block, wait))
return 0;
break;
}
}
else{
if(ctlr->medium >= 0 && block[0] != ctlr->medium)
return 0;
if(ctlr->sct != 0x0800 && (ctlr->sct & 0x3F) != block[0])
return 0;
if(type0mode(ctlr, block, wait))
return 0;
}
if(ctlr->csr6){
if(!(ctlr->csr6 & Ps) || (ctlr->csr6 & Ttm))
return 10;
return 100;
}
return 0;
}
static int
media(Ether* ether, int wait)
{
Ctlr* ctlr;
int k, mbps;
ctlr = ether->ctlr;
for(k = 0; k < ctlr->k; k++){
switch(ctlr->id){
default:
mbps = mediaxx(ether, wait);
break;
case Tulip1:
mbps = media21041(ether, wait);
break;
}
if(mbps > 0)
return mbps;
if(ctlr->curk == 0)
ctlr->curk = ctlr->k-1;
else
ctlr->curk--;
}
return 0;
}
static char* mediatable[9] = {
"10BASE-T",
"10BASE-2",
"10BASE-5",
"100BASE-TX",
"10BASE-TFD",
"100BASE-TXFD",
"100BASE-T4",
"100BASE-FX",
"100BASE-FXFD",
};
static uchar en1207[] = {
0x00, 0x00, 0xE8,
0x00,
0x00, 0x08,
0x1F,
2,
0x00,
0x0B,
0x9E, 0x00,
0x03,
0x1B,
0x6D, 0x00,
};
static uchar ana6910fx[] = {
0x00, 0x00, 0x92,
0x00,
0x00, 0x08,
0x3F,
1,
0x07,
0x03,
0x2D, 0x00
};
static uchar smc9332[] = {
0x00, 0x00, 0xC0,
0x00,
0x00, 0x08,
0x1F,
2,
0x00,
0x00,
0x9E, 0x00,
0x03,
0x09,
0x6D, 0x00,
};
static uchar* leaf21140[] = {
en1207,
ana6910fx,
smc9332,
0,
};
static uchar leafpnic[] = {
0x00, 0x00, 0x00, 0x00,
0x00, 0x00,
0x00,
0x1E, 0x00,
0x00,
0x00, 0x08,
0x00,
0x01,
0x8C,
0x01,
0x00,
0x00,
0x00,
0x00, 0x78,
0xE0, 0x01,
0x00, 0x50,
0x00, 0x18,
};
static int
srom(Ctlr* ctlr)
{
int i, k, oui, phy, x;
uchar *p;
sromr(ctlr, 0);
if(ctlr->srom == nil)
ctlr->srom = malloc((1<<ctlr->sromsz)*sizeof(ushort));
for(i = 0; i < (1<<ctlr->sromsz); i++){
x = sromr(ctlr, i);
ctlr->srom[2*i] = x;
ctlr->srom[2*i+1] = x>>8;
}
if(DEBUG){
print("srom:");
for(i = 0; i < ((1<<ctlr->sromsz)*sizeof(ushort)); i++){
if(i && ((i & 0x0F) == 0))
print("\n     ");
print(" %2.2uX", ctlr->srom[i]);
}
print("\n");
}
ctlr->sromea = ctlr->srom;
for(i = 0; i < 8; i++){
x = ctlr->srom[i];
if(x != ctlr->srom[15-i] || x != ctlr->srom[16+i]){
ctlr->sromea = &ctlr->srom[20];
break;
}
}
if(ctlr->id == Pnic){
memmove(&ctlr->srom[20], leafpnic, sizeof(leafpnic));
for(i = 0; i < Eaddrlen; i += 2){
ctlr->srom[20+i] = ctlr->srom[i+1];
ctlr->srom[20+i+1] = ctlr->srom[i];
}
}
if(ctlr->sromea == ctlr->srom){
p = nil;
for(i = 0; leaf21140[i] != nil; i++){
if(memcmp(leaf21140[i], ctlr->sromea, 3) == 0){
p = &leaf21140[i][4];
break;
}
}
if(p == nil)
return -1;
}
else
p = &ctlr->srom[(ctlr->srom[28]<<8)|ctlr->srom[27]];
ctlr->leaf = p;
ctlr->sct = *p++;
ctlr->sct |= *p++<<8;
if(ctlr->id != Tulip3 && ctlr->id != Tulip1){
csr32w(ctlr, 12, Gpc|*p++);
delay(200);
}
ctlr->k = *p++;
if(ctlr->k >= nelem(ctlr->infoblock))
ctlr->k = nelem(ctlr->infoblock)-1;
ctlr->sctk = ctlr->k-1;
phy = 0;
for(k = 0; k < ctlr->k; k++){
ctlr->infoblock[k] = p;
if(ctlr->id == Tulip1){
debug("type21041: 0x%2.2uX\n", p[0]);
if(ctlr->sct != 0x0800 && *p == (ctlr->sct & 0xFF))
ctlr->sctk = k;
if(*p & 0x40)
p += 7;
else
p += 1;
}
else if((*p & 0x80) || (ctlr->id == Tulip3 && *(p+1) == 3)){
*p |= 0x80;
if(*(p+1) == 1 || *(p+1) == 3)
phy = 1;
if(*(p+1) == 5)
ctlr->type5block = p;
p += (*p & ~0x80)+1;
}
else{
debug("type0: 0x%2.2uX 0x%2.2uX 0x%2.2uX 0x%2.2uX\n",
p[0], p[1], p[2], p[3]);
if(ctlr->sct != 0x0800 && *p == (ctlr->sct & 0xFF))
ctlr->sctk = k;
p += 4;
}
}
ctlr->curk = ctlr->sctk;
debug("sct 0x%uX medium 0x%uX k %d curk %d phy %d\n",
ctlr->sct, ctlr->medium, ctlr->k, ctlr->curk, phy);
if(phy){
x = 0;
for(k = 0; k < nelem(ctlr->phy); k++){
if((oui = miir(ctlr, k, 2)) == -1 || oui == 0)
continue;
if(DEBUG){
oui = (oui & 0x3FF)<<6;
oui |= miir(ctlr, k, 3)>>10;
miir(ctlr, k, 1);
debug("phy%d: index %d oui %uX reg1 %uX\n",
x, k, oui, miir(ctlr, k, 1));
USED(oui);
}
ctlr->phy[x] = k;
}
}
ctlr->fd = 0;
ctlr->medium = -1;
return 0;
}
static void
dec2114xpci(void)
{
Ctlr *ctlr;
Pcidev *p;
int x;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
default:
continue;
case Tulip3:
x = pcicfgr32(p, 0x40);
x &= ~0xC0000000;
pcicfgw32(p, 0x40, x);
case Pnic:
case Pnic2:
case Tulip0:
case Tulip1:
break;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = p->mem[0].bar & ~0x01;
ctlr->pcidev = p;
ctlr->id = (p->did<<16)|p->vid;
debug("2114x: type 0x%8.8uX rev 0x%4.4uX at port 0x%4.4uX\n",
ctlr->id, p->rid, ctlr->port);
csr32w(ctlr, 6, Mbo|Ps);
softreset(ctlr);
if(srom(ctlr)){
free(ctlr);
break;
}
switch(ctlr->id){
default:
break;
case Pnic:
csr32w(ctlr, 15, 0x00000001);
break;
}
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
static void
detach(Ether* ether)
{
softreset(ether->ctlr);
}
int
ether2114xreset(Ether* ether)
{
Ctlr *ctlr;
int i, x;
uchar ea[Eaddrlen];
static int scandone;
if(scandone == 0){
dec2114xpci();
scandone = 1;
}
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
if(ether->port == 0 || ether->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
ether->ctlr = ctlr;
ether->port = ctlr->port;
ether->irq = ctlr->pcidev->intl;
ether->tbdf = ctlr->pcidev->tbdf;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0)
memmove(ether->ea, ctlr->sromea, Eaddrlen);
for(i = 0; i < ether->nopt; i++){
if(cistrcmp(ether->opt[i], "FD") == 0){
ctlr->fd = 1;
continue;
}
for(x = 0; x < nelem(mediatable); x++){
debug("compare <%s> <%s>\n", mediatable[x],
ether->opt[i]);
if(cistrcmp(mediatable[x], ether->opt[i]))
continue;
ctlr->medium = x;
switch(ctlr->medium){
default:
ctlr->fd = 0;
break;
case 0x04:
case 0x05:
case 0x08:
ctlr->fd = 1;
break;
}
break;
}
}
ctlr->mbps = media(ether, 1);
ctlr->nrdr = Nrde;
ctlr->ntdr = Ntde;
pcisetbme(ctlr->pcidev);
ctlrinit(ether);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->detach = detach;
return 0;
}