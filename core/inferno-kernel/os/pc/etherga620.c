#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#define malign(n) xspanalloc((n), 32, 0)
#include "etherif.h"
#include "etherga620fw.h"
enum {
Mhc = 0x0040,
Mlc = 0x0044,
Mc = 0x0050,
Ps = 0x005C,
Wba = 0x0068,
Wd = 0x006C,
DMAas = 0x011C,
CPUAstate = 0x0140,
CPUApc = 0x0144,
CPUBstate = 0x0240,
Hi = 0x0504,
Cpi = 0x050C,
Spi = 0x0514,
Rspi = 0x051C,
Rjpi = 0x0524,
Rmpi = 0x052C,
Mac = 0x0600,
Gip = 0x0608,
Om = 0x0618,
DMArc = 0x061C,
DMAwc = 0x0620,
Tbr = 0x0624,
Eci = 0x0628,
Cci = 0x062C,
Rct = 0x0630,
Sct = 0x0634,
St = 0x0638,
SmcBD = 0x063C,
RmcBD = 0x0640,
Nt = 0x0644,
Gln = 0x0648,
Fln = 0x064C,
Ifx = 0x065C,
IfMTU = 0x0660,
Mi = 0x0664,
Gls = 0x0668,
Fls = 0x066C,
Cr = 0x0700,
Lmw = 0x0800,
};
enum {
Is = 0x00000001,
Ci = 0x00000002,
Hr = 0x00000008,
Eebs = 0x00000010,
Eews = 0x00000020,
Mpio = 0x00000040,
};
enum {
SRAM512 = 0x00000200,
SRAMmask = 0x00000300,
EEclk = 0x00100000,
EEdoe = 0x00200000,
EEdo = 0x00400000,
EEdi = 0x00800000,
};
enum {
SyncSRAM = 0x00100000,
};
enum {
PCIwm32 = 0x000000C0,
PCImrm = 0x00020000,
PCI66 = 0x00080000,
PCI32 = 0x00100000,
PCIrcmd = 0x06000000,
PCIwcmd = 0x70000000,
};
enum {
CPUrf = 0x00000010,
CPUhalt = 0x00010000,
CPUhie = 0x00040000,
};
enum {
BswapBD = 0x00000002,
WswapBD = 0x00000004,
Warn = 0x00000008,
BswapDMA = 0x00000010,
Only1DMA = 0x00000040,
NoJFrag = 0x00000200,
Fatal = 0x40000000,
};
enum {
Lmwsz = 2*1024,
Sr = 0x2000,
};
enum {
Lpref = 0x00008000,
L10MB = 0x00010000,
L100MB = 0x00020000,
L1000MB = 0x00040000,
Lfd = 0x00080000,
Lhd = 0x00100000,
Lefc = 0x00200000,
Lofc = 0x00800000,
Lean = 0x20000000,
Le = 0x40000000,
};
typedef struct Host64 {
uint hi;
uint lo;
} Host64;
typedef struct Ere {
int event;
int unused;
} Ere;
typedef int Cmd;
typedef struct Rbd {
Host64 addr;
int indexlen;
int flags;
int checksum;
int error;
int reserved;
void* opaque;
} Rbd;
typedef struct Sbd {
Host64 addr;
int lenflags;
int reserved;
} Sbd;
enum {
Fend = 0x00000004,
Frjr = 0x00000010,
Funicast = 0x00000020,
Fmulticast = 0x00000040,
Fbroadcast = 0x00000060,
Ferror = 0x00000400,
Frmr = 0x00001000,
};
enum {
Ecrc = 0x00010000,
Ecollision = 0x00020000,
Elink = 0x00040000,
Ephy = 0x00080000,
Eodd = 0x00100000,
Emac = 0x00200000,
Elen64 = 0x00400000,
Eresources = 0x00800000,
Egiant = 0x01000000,
};
typedef struct Rcb {
Host64 addr;
int control;
int unused;
} Rcb;
enum {
TcpUdpCksum = 0x0001,
IpCksum = 0x0002,
NoPseudoHdrCksum= 0x0008,
VlanAssist = 0x0010,
CoalUpdateOnly = 0x0020,
HostRing = 0x0040,
SnapCksum = 0x0080,
UseExtRxBd = 0x0100,
RingDisabled = 0x0200,
};
typedef struct Gib {
int statistics[256];
Rcb ercb;
Rcb crcb;
Rcb srcb;
Rcb rsrcb;
Rcb rjrcb;
Rcb rmrcb;
Rcb rrrcb;
Host64 epp;
Host64 rrrpp;
Host64 scp;
Host64 rsp;
} Gib;
enum {
Ner = 256,
Ncr = 64,
Nsr = 512,
Nrsr = 512,
Nrjr = 256,
Nrmr = 1024,
Nrrr = 2048,
};
enum {
NrsrHI = 72,
NrsrLO = 54,
NrjrHI = 0,
NrjrLO = 0,
NrmrHI = 0,
NrmrLO = 0,
};
typedef struct Ctlr Ctlr;
struct Ctlr {
int port;
Pcidev* pcidev;
Ctlr* next;
int active;
int id;
uchar ea[Eaddrlen];
int* nic;
Gib* gib;
Ere* er;
Lock srlock;
Sbd* sr;
Block** srb;
int nsr;
Rbd* rsr;
int nrsr;
Rbd* rjr;
int nrjr;
Rbd* rmr;
int nrmr;
Rbd* rrr;
int rrrci;
int epi[2];
int rrrpi[2];
int sci[3];
int interrupts;
int mi;
uvlong ticks;
int coalupdateonly;
int hardwarecksum;
int rct;
int sct;
int st;
int smcbd;
int rmcbd;
};
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
#define csr32r(c, r) (*((c)->nic+((r)/4)))
#define csr32w(c, r, v) (*((c)->nic+((r)/4)) = (v))
static void
sethost64(Host64* host64, void* addr)
{
uvlong uvl;
uvl = PCIWADDR(addr);
host64->hi = uvl>>32;
host64->lo = uvl & 0xFFFFFFFFL;
}
static void
ga620command(Ctlr* ctlr, int cmd, int flags, int index)
{
int cpi;
cpi = csr32r(ctlr, Cpi);
csr32w(ctlr, Cr+(cpi*4), cmd<<24 | flags<<12 | index);
cpi = NEXT(cpi, Ncr);
csr32w(ctlr, Cpi, cpi);
}
static void
ga620attach(Ether* edev)
{
Ctlr *ctlr;
ctlr = edev->ctlr;
USED(ctlr);
}
static long
ga620ifstat(Ether* edev, void* a, long n, ulong offset)
{
char *p;
Ctlr *ctlr;
int i, l, r;
ctlr = edev->ctlr;
if(n == 0)
return 0;
p = malloc(READSTR);
l = 0;
for(i = 0; i < 256; i++){
if((r = ctlr->gib->statistics[i]) == 0)
continue;
l += snprint(p+l, READSTR-l, "%d: %ud\n", i, r);
}
l += snprint(p+l, READSTR-l, "interrupts: %ud\n", ctlr->interrupts);
l += snprint(p+l, READSTR-l, "mi: %ud\n", ctlr->mi);
l += snprint(p+l, READSTR-l, "ticks: %llud\n", ctlr->ticks);
l += snprint(p+l, READSTR-l, "coalupdateonly: %d\n", ctlr->coalupdateonly);
l += snprint(p+l, READSTR-l, "hardwarecksum: %d\n", ctlr->hardwarecksum);
l += snprint(p+l, READSTR-l, "rct: %d\n", ctlr->rct);
l += snprint(p+l, READSTR-l, "sct: %d\n", ctlr->sct);
l += snprint(p+l, READSTR-l, "smcbd: %d\n", ctlr->smcbd);
snprint(p+l, READSTR-l, "rmcbd: %d\n", ctlr->rmcbd);
n = readstr(offset, a, n, p);
free(p);
return n;
}
static long
ga620ctl(Ether* edev, void* buf, long n)
{
char *p;
Cmdbuf *cb;
Ctlr *ctlr;
int control, i, r;
ctlr = edev->ctlr;
if(ctlr == nil)
error(Enonexist);
r = 0;
cb = parsecmd(buf, n);
if(cb->nf < 2)
r = -1;
else if(cistrcmp(cb->f[0], "coalupdateonly") == 0){
if(cistrcmp(cb->f[1], "off") == 0){
control = ctlr->gib->srcb.control;
control &= ~CoalUpdateOnly;
ctlr->gib->srcb.control = control;
ctlr->coalupdateonly = 0;
}
else if(cistrcmp(cb->f[1], "on") == 0){
control = ctlr->gib->srcb.control;
control |= CoalUpdateOnly;
ctlr->gib->srcb.control = control;
ctlr->coalupdateonly = 1;
}
else
r = -1;
}
else if(cistrcmp(cb->f[0], "hardwarecksum") == 0){
if(cistrcmp(cb->f[1], "off") == 0){
control = ctlr->gib->srcb.control;
control &= ~(TcpUdpCksum|NoPseudoHdrCksum);
ctlr->gib->srcb.control = control;
control = ctlr->gib->rsrcb.control;
control &= ~(TcpUdpCksum|NoPseudoHdrCksum);
ctlr->gib->rsrcb.control = control;
ctlr->hardwarecksum = 0;
}
else if(cistrcmp(cb->f[1], "on") == 0){
control = ctlr->gib->srcb.control;
control |= (TcpUdpCksum|NoPseudoHdrCksum);
ctlr->gib->srcb.control = control;
control = ctlr->gib->rsrcb.control;
control |= (TcpUdpCksum|NoPseudoHdrCksum);
ctlr->gib->rsrcb.control = control;
ctlr->hardwarecksum = 1;
}
else
r = -1;
}
else if(cistrcmp(cb->f[0], "rct") == 0){
i = strtol(cb->f[1], &p, 0);
if(i < 0 || p == cb->f[1])
r = -1;
else{
ctlr->rct = i;
csr32w(ctlr, Rct, ctlr->rct);
}
}
else if(cistrcmp(cb->f[0], "sct") == 0){
i = strtol(cb->f[1], &p, 0);
if(i < 0 || p == cb->f[1])
r = -1;
else{
ctlr->sct = i;
csr32w(ctlr, Sct, ctlr->sct);
}
}
else if(cistrcmp(cb->f[0], "st") == 0){
i = strtol(cb->f[1], &p, 0);
if(i < 0 || p == cb->f[1])
r = -1;
else{
ctlr->st = i;
csr32w(ctlr, St, ctlr->st);
}
}
else if(cistrcmp(cb->f[0], "smcbd") == 0){
i = strtol(cb->f[1], &p, 0);
if(i < 0 || p == cb->f[1])
r = -1;
else{
ctlr->smcbd = i;
csr32w(ctlr, SmcBD, ctlr->smcbd);
}
}
else if(cistrcmp(cb->f[0], "rmcbd") == 0){
i = strtol(cb->f[1], &p, 0);
if(i < 0 || p == cb->f[1])
r = -1;
else{
ctlr->rmcbd = i;
csr32w(ctlr, RmcBD, ctlr->rmcbd);
}
}
else
r = -1;
free(cb);
if(r == 0)
return n;
return r;
}
static int
_ga620transmit(Ether* edev)
{
Sbd *sbd;
Block *bp;
Ctlr *ctlr;
int sci, spi, work;
ctlr = edev->ctlr;
ilock(&ctlr->srlock);
work = 0;
for(sci = ctlr->sci[2]; sci != ctlr->sci[0]; sci = NEXT(sci, Nsr)){
if(ctlr->srb[sci] == nil)
continue;
freeb(ctlr->srb[sci]);
ctlr->srb[sci] = nil;
work++;
}
ctlr->sci[2] = sci;
sci = PREV(sci, Nsr);
for(spi = csr32r(ctlr, Spi); spi != sci; spi = NEXT(spi, Nsr)){
if((bp = qget(edev->oq)) == nil)
break;
sbd = &ctlr->sr[spi];
sethost64(&sbd->addr, bp->rp);
sbd->lenflags = BLEN(bp)<<16 | Fend;
ctlr->srb[spi] = bp;
work++;
}
csr32w(ctlr, Spi, spi);
iunlock(&ctlr->srlock);
return work;
}
static void
ga620transmit(Ether* edev)
{
_ga620transmit(edev);
}
static void
ga620replenish(Ctlr* ctlr)
{
Rbd *rbd;
int rspi;
Block *bp;
rspi = csr32r(ctlr, Rspi);
while(ctlr->nrsr < NrsrHI){
if((bp = iallocb(ETHERMAXTU+4)) == nil)
break;
rbd = &ctlr->rsr[rspi];
sethost64(&rbd->addr, bp->rp);
rbd->indexlen = rspi<<16 | (ETHERMAXTU+4);
rbd->flags = 0;
rbd->opaque = bp;
rspi = NEXT(rspi, Nrsr);
ctlr->nrsr++;
}
csr32w(ctlr, Rspi, rspi);
}
static void
ga620event(Ether *edev, int eci, int epi)
{
unsigned event, code;
Ctlr *ctlr;
ctlr = edev->ctlr;
while(eci != epi){
event = ctlr->er[eci].event;
code = (event >> 12) & ((1<<12)-1);
switch(event>>24){
case 0x01:
ga620command(ctlr, 0x01, 0x01, 0x00);
ga620command(ctlr, 0x0B, 0x00, 0x00);
print("#l%d: ga620: port %8.8uX: firmware is up\n",
edev->ctlrno, ctlr->port);
break;
case 0x04:
break;
case 0x06:
switch (code) {
case 1:
edev->mbps = 1000;
break;
case 2:
print("#l%d: link down\n", edev->ctlrno);
break;
case 3:
edev->mbps = 100;
break;
}
if (code != 2)
print("#l%d: %dMbps link up\n",
edev->ctlrno, edev->mbps);
break;
case 0x07:
default:
print("#l%d: ga620: er[%d] = %8.8uX\n", edev->ctlrno,
eci, event);
break;
}
eci = NEXT(eci, Ner);
}
csr32w(ctlr, Eci, eci);
}
static void
ga620receive(Ether* edev)
{
int len;
Rbd *rbd;
Block *bp;
Ctlr* ctlr;
ctlr = edev->ctlr;
while(ctlr->rrrci != ctlr->rrrpi[0]){
rbd = &ctlr->rrr[ctlr->rrrci];
len = rbd->indexlen & 0xFFFF;
if(!(rbd->flags & Ferror) && len != 0){
bp = rbd->opaque;
bp->wp = bp->rp+len;
etheriq(edev, bp, 1);
}
else
freeb(rbd->opaque);
rbd->opaque = nil;
if(rbd->flags & Frjr)
ctlr->nrjr--;
else if(rbd->flags & Frmr)
ctlr->nrmr--;
else
ctlr->nrsr--;
ctlr->rrrci = NEXT(ctlr->rrrci, Nrrr);
}
}
static void
ga620interrupt(Ureg*, void* arg)
{
int csr, ie, work;
Ctlr *ctlr;
Ether *edev;
uvlong tsc0, tsc1;
edev = arg;
ctlr = edev->ctlr;
if(!(csr32r(ctlr, Mhc) & Is))
return;
cycles(&tsc0);
ctlr->interrupts++;
csr32w(ctlr, Hi, 1);
ie = 0;
work = 0;
while(ie < 2){
if(ctlr->rrrci != ctlr->rrrpi[0]){
ga620receive(edev);
work = 1;
}
if(_ga620transmit(edev) != 0)
work = 1;
csr = csr32r(ctlr, Eci);
if(csr != ctlr->epi[0]){
ga620event(edev, csr, ctlr->epi[0]);
work = 1;
}
if(ctlr->nrsr <= NrsrLO)
ga620replenish(ctlr);
if(work == 0){
if(ie == 0)
csr32w(ctlr, Hi, 0);
ie++;
}
work = 0;
}
cycles(&tsc1);
ctlr->ticks += tsc1-tsc0;
}
static void
ga620lmw(Ctlr* ctlr, int addr, int* data, int len)
{
int i, l, lmw, v;
v = 0;
while(len > 0){
csr32w(ctlr, Wba, addr);
l = ROUNDUP(addr+1, Lmwsz) - addr;
if(l > len)
l = len;
lmw = Lmw + (addr & (Lmwsz-1));
for(i = 0; i < l; i += 4){
if(data != nil)
v = *data++;
csr32w(ctlr, lmw+i, v);
}
len -= l;
addr += l;
}
}
static int
ga620init(Ether* edev)
{
Ctlr *ctlr;
Host64 host64;
int csr, ea, i, flags;
ctlr = edev->ctlr;
ea = edev->ea[0]<<8 | edev->ea[1];
csr32w(ctlr, Mac, ea);
ea = edev->ea[2]<<24 | edev->ea[3]<<16 | edev->ea[4]<<8 | edev->ea[5];
csr32w(ctlr, Mac+4, ea);
ctlr->gib = malloc(sizeof(Gib));
sethost64(&host64, ctlr->gib);
csr32w(ctlr, Gip, host64.hi);
csr32w(ctlr, Gip+4, host64.lo);
ctlr->er = malign(sizeof(Ere)*Ner);
sethost64(&ctlr->gib->ercb.addr, ctlr->er);
sethost64(&ctlr->gib->epp, ctlr->epi);
csr32w(ctlr, Eci, 0);
ctlr->gib->crcb.addr.lo = Cr-0x400;
for(i = 0; i < Ncr*4; i += 4)
csr32w(ctlr, Cr+i, 0);
csr32w(ctlr, Cpi, 0);
csr32w(ctlr, Cci, 0);
ctlr->sr = malign(sizeof(Sbd)*Nsr);
sethost64(&ctlr->gib->srcb.addr, ctlr->sr);
if(ctlr->hardwarecksum)
flags = TcpUdpCksum|NoPseudoHdrCksum|HostRing;
else
flags = HostRing;
if(ctlr->coalupdateonly)
flags |= CoalUpdateOnly;
ctlr->gib->srcb.control = Nsr<<16 | flags;
sethost64(&ctlr->gib->scp, ctlr->sci);
csr32w(ctlr, Spi, 0);
ctlr->srb = malloc(sizeof(Block*)*Nsr);
ctlr->rsr = malign(sizeof(Rbd)*Nrsr);
sethost64(&ctlr->gib->rsrcb.addr, ctlr->rsr);
if(ctlr->hardwarecksum)
flags = TcpUdpCksum|NoPseudoHdrCksum;
else
flags = 0;
ctlr->gib->rsrcb.control = (ETHERMAXTU+4)<<16 | flags;
csr32w(ctlr, Rspi, 0);
ctlr->gib->rjrcb.control = RingDisabled;
ctlr->gib->rmrcb.control = RingDisabled;
ctlr->rrr = malign(sizeof(Rbd)*Nrrr);
sethost64(&ctlr->gib->rrrcb.addr, ctlr->rrr);
ctlr->gib->rrrcb.control = Nrrr<<16 | 0;
sethost64(&ctlr->gib->rrrpp, ctlr->rrrpi);
ctlr->rrrci = 0;
sethost64(&ctlr->gib->rsp, ctlr->gib->statistics);
csr32w(ctlr, DMArc, 0x80);
csr32w(ctlr, DMAwc, 0x80);
if(NrjrHI > 0 || Nsr > 128)
csr32w(ctlr, Tbr, 64/3);
else
csr32w(ctlr, Tbr, 4);
ctlr->rct = 1;
csr32w(ctlr, Rct, ctlr->rct);
ctlr->sct = 0;
csr32w(ctlr, Sct, ctlr->sct);
ctlr->st = 1000000;
csr32w(ctlr, St, ctlr->st);
ctlr->smcbd = Nsr/4;
csr32w(ctlr, SmcBD, ctlr->smcbd);
ctlr->rmcbd = 4;
csr32w(ctlr, RmcBD, ctlr->rmcbd);
csr = csr32r(ctlr, DMAas) & ~0x03;
csr32w(ctlr, DMAas, csr|0x01);
csr32w(ctlr, Gln, Le|Lean|Lofc|Lfd|L1000MB|Lpref);
csr32w(ctlr, Fln, Le|Lean|Lhd|Lfd|L100MB|L10MB);
csr32w(ctlr, Ifx, 1);
csr32w(ctlr, IfMTU, ETHERMAXTU+4);
csr32w(ctlr, Mi, 0);
csr32w(ctlr, Hi, 0);
csr32w(ctlr, CPUApc, tigon2FwStartAddr);
csr = csr32r(ctlr, CPUAstate) & ~CPUhalt;
csr32w(ctlr, CPUAstate, csr);
return 0;
}
static int
at24c32io(Ctlr* ctlr, char* op, int data)
{
char *lp, *p;
int i, loop, mlc, r;
mlc = csr32r(ctlr, Mlc);
r = 0;
loop = -1;
lp = nil;
for(p = op; *p != '\0'; p++){
switch(*p){
default:
return -1;
case ' ':
continue;
case ':':
if(lp != nil)
return -1;
lp = p;
loop = 7;
continue;
case ';':
if(lp == nil)
return -1;
loop--;
if(loop >= 0)
p = lp;
else
lp = nil;
continue;
case 'C':
mlc |= EEclk;
break;
case 'c':
mlc &= ~EEclk;
break;
case 'D':
if(loop < 0)
return -1;
if(data & (1<<loop))
mlc |= EEdo;
else
mlc &= ~EEdo;
break;
case 'E':
mlc |= EEdoe;
break;
case 'e':
mlc &= ~EEdoe;
break;
case 'I':
i = (csr32r(ctlr, Mlc) & EEdi) != 0;
if(loop >= 0)
r |= (i<<loop);
else
r = i;
continue;
case 'O':
mlc |= EEdo;
break;
case 'o':
mlc &= ~EEdo;
break;
}
csr32w(ctlr, Mlc, mlc);
microdelay(1);
}
if(loop >= 0)
return -1;
return r;
}
static int
at24c32r(Ctlr* ctlr, int addr)
{
int data;
at24c32io(ctlr, "OECoc", 0);
data = -1;
if(at24c32io(ctlr, "oE :DCc; oeCIc", 0xA0) != 0)
goto stop;
if(at24c32io(ctlr, "oE :DCc; oeCIc", addr>>8) != 0)
goto stop;
if(at24c32io(ctlr, "oE :DCc; oeCIc", addr) != 0)
goto stop;
at24c32io(ctlr, "OECoc", 0);
if(at24c32io(ctlr, "oE :DCc; oeCIc", 0xA1) != 0)
goto stop;
data = at24c32io(ctlr, ":CIc;", 0xA1);
stop:
at24c32io(ctlr, "oECOc", 0);
return data;
}
static int
ga620detach(Ctlr* ctlr)
{
int timeo;
csr32w(ctlr, Mhc, Hr<<24 | Hr);
csr32w(ctlr, Mhc, (Eews|Ci)<<24 | Eews|Ci);
microdelay(1);
for(timeo = 0; timeo < 500000; timeo++){
if((csr32r(ctlr, CPUAstate) & (CPUhie|CPUrf)) == CPUhie)
break;
microdelay(1);
}
if((csr32r(ctlr, CPUAstate) & (CPUhie|CPUrf)) != CPUhie)
return -1;
csr32w(ctlr, CPUAstate, CPUhalt);
csr32w(ctlr, CPUBstate, CPUhalt);
return 0;
}
static void
ga620shutdown(Ether* ether)
{
print("ga620shutdown\n");
ga620detach(ether->ctlr);
}
static int
ga620reset(Ctlr* ctlr)
{
int cls, csr, i, r;
if(ga620detach(ctlr) < 0)
return -1;
csr = csr32r(ctlr, Mlc) & ~(EEdi|EEdo|EEdoe|EEclk|SRAMmask);
csr32w(ctlr, Mlc, SRAM512|csr);
csr = csr32r(ctlr, Mc);
csr32w(ctlr, Mc, SyncSRAM|csr);
csr = csr32r(ctlr, Ps) & (PCI32|PCI66);
csr |= PCIwcmd|PCIrcmd|PCImrm;
if(ctlr->pcidev->pcr & 0x0010){
cls = pcicfgr8(ctlr->pcidev, PciCLS) * 4;
if(cls != 32)
pcicfgw8(ctlr->pcidev, PciCLS, 32/4);
csr |= PCIwm32;
}
csr32w(ctlr, Ps, csr);
csr32w(ctlr, Om, Fatal|NoJFrag|BswapDMA|WswapBD);
for(i = 0; i < Eaddrlen; i++){
if((r = at24c32r(ctlr, 0x8E+i)) == -1)
return -1;
ctlr->ea[i] = r;
}
ga620lmw(ctlr, tigon2FwTextAddr, tigon2FwText, tigon2FwTextLen);
ga620lmw(ctlr, tigon2FwRodataAddr, tigon2FwRodata, tigon2FwRodataLen);
ga620lmw(ctlr, tigon2FwDataAddr, tigon2FwData, tigon2FwDataLen);
ga620lmw(ctlr, tigon2FwSbssAddr, nil, tigon2FwSbssLen);
ga620lmw(ctlr, tigon2FwBssAddr, nil, tigon2FwBssLen);
return 0;
}
static void
ga620pci(void)
{
void *mem;
Pcidev *p;
Ctlr *ctlr;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch(p->did<<16 | p->vid){
default:
continue;
case 0x620A<<16 | 0x1385:
case 0x630A<<16 | 0x1385:
case 0x0001<<16 | 0x12AE:
case 0x0002<<16 | 0x12AE:
case 0x0009<<16 | 0x10A9:
break;
}
mem = vmap(p->mem[0].bar & ~0x0F, p->mem[0].size);
if(mem == 0){
print("ga620: can't map %8.8luX\n", p->mem[0].bar);
continue;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = p->mem[0].bar & ~0x0F;
ctlr->pcidev = p;
ctlr->id = p->did<<16 | p->vid;
ctlr->nic = mem;
if(ga620reset(ctlr)){
free(ctlr);
continue;
}
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
static void
ga620promiscuous(void *arg, int on)
{
Ether *ether = arg;
ga620command(ether->ctlr, 0xa, (on? 1: 2), 0);
}
static void
ga620multicast(void *arg, uchar *addr, int on)
{
Ether *ether = arg;
USED(addr);
ga620command(ether->ctlr, 0xe, (on? 1: 2), 0);
}
static int
ga620pnp(Ether* edev)
{
Ctlr *ctlr;
uchar ea[Eaddrlen];
if(ctlrhead == nil)
ga620pci();
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
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
edev->port = ctlr->port;
edev->irq = ctlr->pcidev->intl;
edev->tbdf = ctlr->pcidev->tbdf;
edev->mbps = 1000;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0)
memmove(edev->ea, ctlr->ea, Eaddrlen);
ga620init(edev);
edev->attach = ga620attach;
edev->transmit = ga620transmit;
edev->interrupt = ga620interrupt;
edev->ifstat = ga620ifstat;
edev->ctl = ga620ctl;
edev->arg = edev;
edev->promiscuous = ga620promiscuous;
edev->multicast = ga620multicast;
edev->shutdown = ga620shutdown;
return 0;
}
void
etherga620link(void)
{
addethercard("GA620", ga620pnp);
}