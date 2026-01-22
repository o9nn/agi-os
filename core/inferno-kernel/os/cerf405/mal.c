#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
enum {
Nrxchan= 2,
Ntxchan= 4,
Maxchan = 4
};
enum {
Cfg= 0x180,
Esr= 0x181,
Ier= 0x182,
Txcasr= 0x184,
Txcarr= 0x185,
Txeobisr= 0x186,
Txdeir= 0x187,
Rxcasr= 0x190,
Rxcarr= 0x191,
Rxeobisr= 0x192,
Rxdeir= 0x193,
};
#define TXCTPR(n) (0x1A0+(n))
#define RXCTPR(n) (0x1C0+(n))
#define RCBS(n) (0x1E0+(n))
enum {
CfgSr= 1<<31,
CfgPlbp0= 0<<22,
CfgPlbp1= 1<<22,
CfgPlbp2= 2<<22,
CfgPlbp3= 3<<22,
CfgGa= 1<<21,
CfgOa= 1<<20,
CfgPlble= 1<<19,
CfgPlbt_f= 0xF<<15,
CfgPlbt_s= 15,
CfgPlbb= 1<<14,
CfgOpbbl= 1<<7,
CfgOepie= 1<<2,
CfgLea= 1<<1,
CfgSd= 1<<0,
EsrEvb= 1<<31,
EsrCid_f= 0x7F<<25,
EsrDe= 1<<20,
EsrOne= 1<<19,
EsrOte= 1<<18,
EsrOse= 1<<17,
EsrPein= 1<<16,
EsrDei= 1<<4,
EsrOnei= 1<<3,
EsrOtei= 1<<2,
EsrOsei= 1<<1,
EsrPbei= 1<<0,
};
typedef struct Malmem Malmem;
struct Malmem {
Lock;
BD* base;
BD* limit;
BD* avail;
};
static Malmem malmem;
static Mal* malchans[2][Maxchan];
static void
errorintr(Ureg*, void*)
{
ulong esr, rxdeir, txdeir;
esr = getdcr(Esr);
txdeir = getdcr(Txdeir);
rxdeir = getdcr(Rxdeir);
iprint("mal: esr=%8.8lux txdeir=%8.8lux rxdeir=%8.8lux\n", esr, txdeir, rxdeir);
putdcr(Rxdeir, rxdeir);
putdcr(Txdeir, txdeir);
putdcr(Esr, esr);
}
static void
scanintr(Ureg *ur, ulong ir, Mal *chans[])
{
Mal *ml;
int i;
for(i=0; ir != 0 && i < Maxchan; i++)
if(ir & IBIT(i)){
ir &= ~IBIT(i);
ml = chans[i];
if(ml != nil && ml->interrupt != nil)
ml->interrupt(ur, ml->arg);
}
}
static void
txinterrupt(Ureg *ur, void*)
{
ulong ir;
ir = getdcr(Txeobisr);
putdcr(Txeobisr, ir);
scanintr(ur, ir, malchans[1]);
}
static void
rxinterrupt(Ureg *ur, void*)
{
ulong ir;
ir = getdcr(Rxeobisr);
putdcr(Rxeobisr, ir);
scanintr(ur, ir, malchans[0]);
}
void
ioinit(void)
{
int i;
putdcr(Txcarr, ~0);
putdcr(Rxcarr, ~0);
putdcr(Cfg, CfgSr);
while(getdcr(Cfg) & CfgSr)
;
for(i=0; i<Nrxchan; i++){
putdcr(RCBS(i), 0);
putdcr(RXCTPR(i), 0);
}
for(i=0; i<Ntxchan; i++)
putdcr(TXCTPR(i), 0);
putdcr(Cfg, (0xF<<CfgPlbt_s)|CfgPlbb);
intrenable(VectorMALSERR, errorintr, nil, BUSUNKNOWN, "malserr");
intrenable(VectorMALTXDE, errorintr, nil, BUSUNKNOWN, "maltxde");
intrenable(VectorMALRXDE, errorintr, nil, BUSUNKNOWN, "malrxde");
intrenable(VectorMALTXEOB, txinterrupt, nil, BUSUNKNOWN, "maltxeob");
intrenable(VectorMALRXEOB, rxinterrupt, nil, BUSUNKNOWN, "malrxeob");
putdcr(Ier, EsrDei | EsrOnei | EsrOtei | EsrOsei | EsrPbei);
}
Mal*
malchannel(int n, int tx, void (*intr)(Ureg*, void*), void *arg)
{
Mal *ml;
if((ml = malchans[tx][n]) == nil){
ml = malloc(sizeof(*m));
malchans[tx][n] = ml;
}
ml->n = n;
ml->tx = tx;
ml->len = 1;
ml->arg = arg;
ml->interrupt = intr;
return ml;
}
void
maltxreset(Mal *ml)
{
putdcr(Txcarr, IBIT(ml->n));
}
void
maltxinit(Mal *ml, Ring *r)
{
putdcr(TXCTPR(ml->n), PADDR(r->tdr));
}
void
maltxenable(Mal *ml)
{
putdcr(Txcasr, getdcr(Txcasr) | IBIT(ml->n));
}
void
malrxreset(Mal *ml)
{
putdcr(Rxcarr, IBIT(ml->n));
}
void
malrxinit(Mal *ml, Ring *r, ulong limit)
{
putdcr(RXCTPR(ml->n), PADDR(r->rdr));
putdcr(RCBS(ml->n), limit);
}
void
malrxenable(Mal *ml)
{
putdcr(Rxcasr, getdcr(Rxcasr) | IBIT(ml->n));
}
void
ioringreserve(int nrx, ulong nrb, int ntx, ulong ntb)
{
ulong nb, nbd;
lock(&malmem);
if(malmem.base == nil){
nbd = nrx*nrb + ntx*ntb;
nb = mmumapsize(nbd*sizeof(BD));
malmem.base = mmucacheinhib(xspanalloc(nb, nb, 1<<19), nb);
malmem.limit = malmem.base + nbd;
malmem.avail = malmem.base;
if((PADDR(malmem.base)&~0x7FFFF) != (PADDR(malmem.base)&~0x7FFFF))
print("mal: trouble ahead?\n");
}
unlock(&malmem);
if(malmem.base == nil)
panic("ioringreserve");
}
BD*
bdalloc(ulong nd)
{
BD *b;
lock(&malmem);
b = malmem.avail;
if(b+nd > malmem.limit)
b = nil;
else
malmem.avail = b+nd;
unlock(&malmem);
return b;
}
int
ioringinit(Ring* r, int nrdre, int ntdre)
{
int i;
r->nrdre = nrdre;
if(r->rdr == nil)
r->rdr = bdalloc(nrdre);
if(r->rxb == nil)
r->rxb = malloc(nrdre*sizeof(Block*));
if(r->rdr == nil || r->rxb == nil)
return -1;
for(i = 0; i < nrdre; i++){
r->rxb[i] = nil;
r->rdr[i].length = 0;
r->rdr[i].addr = 0;
r->rdr[i].status = BDEmpty|BDInt;
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
void
dumpmal(void)
{
int i;
iprint("Cfg=%8.8lux\n", getdcr(Cfg));
iprint("Esr=%8.8lux\n", getdcr(Esr));
iprint("Ier=%8.8lux\n", getdcr(Ier));
iprint("Txcasr=%8.8lux\n", getdcr(Txcasr));
iprint("Txcarr=%8.8lux\n", getdcr(Txcarr));
iprint("Txeobisr=%8.8lux\n", getdcr(Txeobisr));
iprint("Txdeir=%8.8lux\n", getdcr(Txdeir));
iprint("Rxcasr=%8.8lux\n", getdcr(Rxcasr));
iprint("Rxcarr=%8.8lux\n", getdcr(Rxcarr));
iprint("Rxeobisr=%8.8lux\n", getdcr(Rxeobisr));
iprint("Rxdeir=%8.8lux\n", getdcr(Rxdeir));
for(i=0; i<Nrxchan; i++)
iprint("Rxctpr[%d]=%8.8lux Rcbs[%d]=%8.8lux\n", i, getdcr(RXCTPR(i)), i, getdcr(RCBS(i)));
for(i=0;i<Ntxchan; i++)
iprint("Txctpr[%d]=%8.8lux\n", i, getdcr(TXCTPR(i)));
}