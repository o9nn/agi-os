#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
typedef struct Chanuse Chanuse;
struct Chanuse {
Lock;
void*	owner;
} ;
enum {
BDSIZE=	1024,
CPMSIZE=	1024,
SCC1ID=	0,
I2CID=	1,
IDMA1ID= 1,
SCC2ID=	4,
SPIID=	5,
IDMA2ID= 5,
TIMERID=	5,
SCC3ID=	8,
SMC1ID=	9,
DSP1ID=	9,
SCC4ID=	12,
SMC2ID=	13,
DSP2ID=	13,
NCPMID=	16,
NSCC = 4,
ENR = 1<<5,
ENT = 1<<4,
NSMC = 2,
TEN = 1<<1,
REN = 1<<0,
};
static	Map	bdmapv[BDSIZE/sizeof(BD)];
static	RMap	bdmap = {"buffer descriptors"};
static	Map	cpmmapv[CPMSIZE/sizeof(ulong)];
static	RMap	cpmmap = {"CPM memory"};
static	Lock	cpmlock;
static struct {
Lock;
ulong	avail;
} brgens;
static	Chanuse	cpmids[NCPMID];
static	CPMdev	cpmdevinfo[] = {
[CPscc1] {SCC1ID, 0x1E, 0xA00, 0x3C00},
[CPscc2] {SCC2ID, 0x1D, 0xA20, 0x3D00},
[CPscc3] {SCC3ID, 0x1C, 0xA40, 0x3E00},
[CPscc4] {SCC4ID, 0x1B, 0xA60, 0x3F00},
[CPsmc1] {SMC1ID, 0x04, 0xA80, 0x3E80},
[CPsmc2] {SMC2ID, 0x03, 0xA90, 0x3F80},
[CPdsp1] {DSP1ID, 0x16, 0, 0x3EC0},
[CPdsp2] {DSP2ID, 0x16, 0, 0x3FC0},
[CPidma1] {IDMA1ID, 0x15, 0, 0x3CC0},
[CPidma2] {IDMA2ID, 0x14, 0, 0x3DC0},
[CPtimer] {TIMERID, 0x11, 0, 0x3DB0},
[CPspi] {SPIID, 0x05, 0xAA0, 0x3D80},
[CPi2c] {I2CID, 0x10, 0x860, 0x3C80},
};
static	void	i2cspireloc(void);
static	void*	relocateparam(ulong, int);
void
cpminit(void)
{
IMM *io;
io = m->iomem;
io->sdcr = 1;
io->rccr = 0;
io->rmds = 0;
io->lccr = 0;
io->vccr = 0;
io->i2mod = 0;
io->pcint = 0;
io->pcso = 0;
io->pcdir =0;
io->pcpar = 0;
io->pcdat = 0;
io->papar = 0;
io->padir = 0;
io->paodr = 0;
io->padat = 0;
io->pbpar = 0;
io->pbdir = 0;
io->pbodr = 0;
io->pbdat = 0;
io->tgcr = 0x2222;
eieio();
for(io->cpcr = 0x8001; io->cpcr & 1;)
eieio();
mapinit(&bdmap, bdmapv, sizeof(bdmapv));
mapfree(&bdmap, DPBASE, BDSIZE);
mapinit(&cpmmap, cpmmapv, sizeof(cpmmapv));
mapfree(&cpmmap, DPBASE+BDSIZE, CPMSIZE);
if(m->cputype == 0x50 && (getimmr() & 0xFFFF) <= 0x2001)
brgens.avail = 0x3;
else
brgens.avail = 0xF;
i2cspireloc();
}
CPMdev*
cpmdev(int n)
{
CPMdev *d;
if(n < 0 || n >= nelem(cpmdevinfo))
panic("cpmdev");
d = &cpmdevinfo[n];
if(d->param == nil && d->pbase != 0){
if((n == CPi2c || n == CPspi)){
d->param = relocateparam(d->pbase, 0xB0-0x80);
if(d->param == nil)
return nil;
} else
d->param = (char*)m->iomem+d->pbase;
}
if(d->rbase != 0)
d->regs = (char*)m->iomem+d->rbase;
return d;
}
void
cpmop(CPMdev *cpd, int op, int param)
{
IMM *io;
ilock(&cpmlock);
io = m->iomem;
while(io->cpcr & 1)
eieio();
io->cpcr = (op<<8)|(cpd->id<<4)|(param<<1)|1;
eieio();
while(io->cpcr & 1)
eieio();
iunlock(&cpmlock);
}
IMM*
ioplock(void)
{
ilock(&cpmlock);
return m->iomem;
}
void
iopunlock(void)
{
eieio();
iunlock(&cpmlock);
}
void
sccnmsi(int x, int rcs, int tcs)
{
IMM *io;
ulong v;
int sh;
sh = (x-1)*8;
v = (((rcs&7)<<3) | (tcs&7)) << sh;
io = ioplock();
io->sicr = (io->sicr & ~(0xFF<<sh)) | v;
iopunlock();
}
void
smcnmsi(int x, int cs)
{
IMM *io;
ulong v;
int sh;
if(x == 1)
sh = 0;
else
sh = 16;
v = cs << (12+sh);
io = ioplock();
io->simode = (io->simode & ~(0xF000<<sh)) | v;
iopunlock();
}
int
cpmidopen(int id, void *owner)
{
Chanuse *use;
use = &cpmids[id];
ilock(use);
if(use->owner != nil && use->owner != owner){
iunlock(use);
return -1;
}
use->owner = owner;
iunlock(use);
return 0;
}
void
cpmidclose(int id)
{
Chanuse *use;
use = &cpmids[id];
ilock(use);
use->owner = nil;
iunlock(use);
}
void
sccxstop(CPMdev *d)
{
SCC *scc;
if(d == nil)
return;
scc = d->regs;
if(scc->gsmrl & (ENT|ENR)){
if(scc->gsmrl & ENT)
cpmop(d, GracefulStopTx, 0);
if(scc->gsmrl & ENR)
cpmop(d, CloseRxBD, 0);
delay(1);
scc->gsmrl &= ~(ENT|ENR);
eieio();
}
scc->sccm = 0;
}
void
smcxstop(CPMdev *d)
{
SMC *smc;
if(d == nil)
return;
smc = d->regs;
if(smc->smcmr & (TEN|REN)){
if(smc->smcmr & TEN)
cpmop(d, StopTx, 0);
if(smc->smcmr & REN)
cpmop(d, CloseRxBD, 0);
delay(1);
smc->smcmr &= ~(TEN|REN);
eieio();
}
smc->smcm = 0;
}
BD *
bdalloc(int n)
{
ulong a;
a = rmapalloc(&bdmap, 0, n*sizeof(BD), sizeof(BD));
if(a == 0)
panic("bdalloc");
return KADDR(a);
}
void
bdfree(BD *b, int n)
{
if(b){
eieio();
mapfree(&bdmap, PADDR(b), n*sizeof(BD));
}
}
void
dumpbd(char *name, BD *b, int maxn)
{
uchar *d;
int i;
print("%s #%4.4lux: s=#%4.4ux l=%ud a=#%8.8lux", name, PADDR(b)&0xFFFF, b->status, b->length, b->addr);
if(maxn > b->length)
maxn = b->length;
if(b->addr != 0){
d = KADDR(b->addr);
for(i=0; i<maxn; i++)
print(" %2.2ux", d[i]);
if(i < b->length)
print(" ...");
}
print("\n");
}
void *
cpmalloc(int n, int align)
{
ulong a;
a = rmapalloc(&cpmmap, 0, n, align);
if(a == 0)
panic("cpmalloc");
return KADDR(a);
}
void
cpmfree(void *p, int n)
{
if(p != nil && n > 0){
eieio();
mapfree(&cpmmap, PADDR(p), n);
}
}
int
brgalloc(void)
{
int n;
lock(&brgens);
for(n=0; brgens.avail!=0; n++)
if(brgens.avail & (1<<n)){
brgens.avail &= ~(1<<n);
unlock(&brgens);
return n;
}
unlock(&brgens);
return -1;
}
void
brgfree(int n)
{
if(n >= 0){
if(n > 3 || brgens.avail & (1<<n))
panic("brgfree");
lock(&brgens);
brgens.avail |= 1 << n;
unlock(&brgens);
}
}
ulong
baudgen(int rate, int scale)
{
int d;
rate *= scale;
d = (2*m->cpuhz+rate)/(2*rate) - 1;
if(d < 0)
d = 0;
if(d >= (1<<12))
return ((d+15)>>(4-1))|1;
return d<<1;
}
int
ioringinit(Ring* r, int nrdre, int ntdre, int bufsize)
{
int i, x;
r->nrdre = nrdre;
if(r->rdr == nil)
r->rdr = bdalloc(nrdre);
bufsize = (bufsize+CACHELINESZ-1)&~(CACHELINESZ-1);
if(r->rrb == nil)
r->rrb = malloc(nrdre*bufsize);
if(r->rdr == nil || r->rrb == nil)
return -1;
dcflush(r->rrb, nrdre*bufsize);
x = PADDR(r->rrb);
for(i = 0; i < nrdre; i++){
r->rdr[i].length = 0;
r->rdr[i].addr = x;
r->rdr[i].status = BDEmpty|BDInt;
x += bufsize;
}
r->rdr[i-1].status |= BDWrap;
r->rdrx = 0;
r->ntdre = ntdre;
if(r->tdr == nil)
r->tdr = bdalloc(ntdre);
if(r->txb == nil)
r->txb = malloc(ntdre*sizeof(Block*));
if(r->tdr == nil || r->txb == nil)
return -1;
for(i = 0; i < ntdre; i++){
r->txb[i] = nil;
r->tdr[i].addr = 0;
r->tdr[i].length = 0;
r->tdr[i].status = 0;
}
r->tdr[i-1].status |= BDWrap;
r->tdrh = 0;
r->tdri = 0;
r->ntq = 0;
return 0;
}
static void*
relocateparam(ulong olda, int nb)
{
void *p;
if(olda < (ulong)m->iomem)
olda += (ulong)m->iomem;
p = cpmalloc(nb, 32);
if(p == nil)
return p;
*(ushort*)KADDR(olda+0x2C) = PADDR(p);
eieio();
return p;
}
static	ulong	ubase1 = 0x2000;
static	ulong	ucode1[] = {
0x7FFFEFD9,
0x3FFD0000,
0x7FFB49F7,
0x7FF90000,
0x5FEFADF7,
0x5F89ADF7,
0x5FEFAFF7,
0x5F89AFF7,
0x3A9CFBC8,
0xE7C0EDF0,
0x77C1E1BB,
0xF4DC7F1D,
0xABAD932F,
0x4E08FDCF,
0x6E0FAFF8,
0x7CCF76CF,
0xFD1FF9CF,
0xABF88DC6,
0xAB5679F7,
0xB0937383,
0xDFCE79F7,
0xB091E6BB,
0xE5BBE74F,
0xB3FA6F0F,
0x6FFB76CE,
0xEE0DF9CF,
0x2BFBEFEF,
0xCFEEF9CF,
0x76CEAD24,
0x90B2DF9A,
0x7FDDD0BF,
0x4BF847FD,
0x7CCF76CE,
0xCFEF7E1F,
0x7F1D7DFD,
0xF0B6EF71,
0x7FC177C1,
0xFBC86079,
0xE722FBC8,
0x5FFFDFFF,
0x5FB2FFFB,
0xFBC8F3C8,
0x94A67F01,
0x7F1D5F39,
0xAFE85F5E,
0xFFDFDF96,
0xCB9FAF7D,
0x5FC1AFED,
0x8C1C5FC1,
0xAFDD5FC3,
0xDF9A7EFD,
0xB0B25FB2,
0xFFFEABAD,
0x5FB2FFFE,
0x5FCE600B,
0xE6BB600B,
0x5FCEDFC6,
0x27FBEFDF,
0x5FC8CFDE,
0x3A9CE7C0,
0xEDF0F3C8,
0x7F0154CD,
0x7F1D2D3D,
0x363A7570,
0x7E0AF1CE,
0x37EF2E68,
0x7FEE10EC,
0xADF8EFDE,
0xCFEAE52F,
0x7D0FE12B,
0xF1CE5F65,
0x7E0A4DF8,
0xCFEA5F72,
0x7D0BEFEE,
0xCFEA5F74,
0xE522EFDE,
0x5F74CFDA,
0x0B627385,
0xDF627E0A,
0x30D8145B,
0xBFFFF3C8,
0x5FFFDFFF,
0xA7F85F5E,
0xBFFE7F7D,
0x10D31450,
0x5F36BFFF,
0xAF785F5E,
0xBFFDA7F8,
0x5F36BFFE,
0x77FD30C0,
0x4E08FDCF,
0xE5FF6E0F,
0xAFF87E1F,
0x7E0FFD1F,
0xF1CF5F1B,
0xABF80D5E,
0x5F5EFFEF,
0x79F730A2,
0xAFDD5F34,
0x47F85F34,
0xAFED7FDD,
0x50B24978,
0x47FD7F1D,
0x7DFD70AD,
0xEF717EC1,
0x6BA47F01,
0x2D267EFD,
0x30DE5F5E,
0xFFFD5F5E,
0xFFEF5F5E,
0xFFDF0CA0,
0xAFED0A9E,
0xAFDD0C3A,
0x5F3AAFBD,
0x7FBDB082,
0x5F8247F8,
};
static	ulong	ubase2 = 0x2F00;
static	ulong	ucode2[] = {
0x3E303430,
0x34343737,
0xABF7BF9B,
0x994B4FBD,
0xBD599493,
0x349FFF37,
0xFB9B177D,
0xD9936956,
0xBBFDD697,
0xBDD2FD11,
0x31DB9BB3,
0x63139637,
0x93733693,
0x193137F7,
0x331737AF,
0x7BB9B999,
0xBB197957,
0x7FDFD3D5,
0x73B773F7,
0x37933B99,
0x1D115316,
0x99315315,
0x31694BF4,
0xFBDBD359,
0x31497353,
0x76956D69,
0x7B9D9693,
0x13131979,
0x79376935,
};
static void
i2cspireloc(void)
{
IMM *io;
static int done;
if(done)
return;
io = m->iomem;
io->rccr &= ~3;
memmove((uchar*)m->iomem+ubase1, ucode1, sizeof(ucode1));
memmove((uchar*)m->iomem+ubase2, ucode2, sizeof(ucode2));
io->rctr1 = 0x802a;
io->rctr2 = 0x8028;
io->rctr3 = 0x802e;
io->rctr4 = 0x802c;
io->rccr |= 1;
done = 1;
}