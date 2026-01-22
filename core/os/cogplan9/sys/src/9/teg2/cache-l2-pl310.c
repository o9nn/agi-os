#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "arm.h"
#define NWAYS(l2p)	((l2p)->auxctl & Assoc16way? 16: 8)
#define L2P		((L2pl310 *)soc.l2cache)
enum {
L2size		= 1024 * 1024,
Wayszgran	= 16 * KiB,
};
typedef struct L2pl310 L2pl310;
typedef struct Pl310op Pl310op;
struct Pl310op {
ulong	pa;
ulong	_pad;
ulong	indexway;
ulong	way;
};
struct L2pl310 {
ulong	id;
ulong	type;
uchar	_pad0[0x100 - 0x8];
ulong	ctl;
ulong	auxctl;
uchar	_pad1[0x730 - 0x108];
ulong	sync;
uchar	_pad2[0x740 - 0x734];
ulong	r3p0sync;
uchar	_pad3[0x770 - 0x744];
Pl310op	inv;
uchar	_pad4[0x7b0 - 0x780];
Pl310op	clean;
uchar	_pad5[0x7f0 - 0x7c0];
Pl310op	cleaninv;
uchar	_pad6[0xc00 - 0x7d0];
ulong	filtstart;
ulong	filtend;
uchar	_pad6[0xf40 - 0xc08];
ulong	debug;
};
enum {
L2enable = 1,
Ipref	= 1<<29,
Dpref	= 1<<28,
Mbo	= 1<<25,
Sharovr	= 1<<22,
Parity	= 1<<21,
Waycfgshift= 17,
Waycfgmask = (1<<3) - 1,
Assoc16way = 1<<16,
Fullline0= 1<<0,
Wt	= 1<<1,
Nolinefill= 1<<0,
Basecfg = Wt | Nolinefill,
};
static Lock l2lock;
static int disallowed;
static int l2ison;
static int bg_op_running;
static ulong waysmask;
static Cacheimpl l2cacheimpl;
static void
awaitbgop(void)
{
while (bg_op_running)
;
}
static void
getlock(void)
{
awaitbgop();
ilock(&l2lock);
awaitbgop();
}
static void
l2pl310sync(void)
{
L2P->sync = 0;
coherence();
}
void
l2pl310init(void)
{
int waysz, nways;
ulong new;
L2pl310 *l2p = L2P;
static int configed;
if (getconf("*l2off") != nil) {
disallowed = 1;
return;
}
if (l2ison || configed)
return;
l2cache = &l2cacheimpl;
cachedwb();
l2pl310sync();
l2cache->inv();
l2pl310sync();
nways = NWAYS(l2p);
if (!(l2p->auxctl & Assoc16way)) {
l2p->auxctl |= Assoc16way;
coherence();
l2pl310sync();
nways = NWAYS(l2p);
}
waysmask = MASK(nways);
waysz = L2size / nways;
new = l2p->auxctl & ~(Waycfgmask << Waycfgshift) |
(log2(waysz / Wayszgran) + 1) << Waycfgshift;
l2p->auxctl = new;
coherence();
l2pl310sync();
l2cache->inv();
if (l2p->auxctl != new)
iprint("l2 config %#8.8lux didn't stick; is now %#8.8lux\n",
new, l2p->auxctl);
configed++;
}
void
l2pl310info(Memcache *cp)
{
int pow2;
ulong waysz;
L2pl310 *l2p = L2P;
memset(cp, 0, sizeof *cp);
if (!l2ison)
return;
l2pl310init();
assert((l2p->id >> 24) == 'A');
cp->level = 2;
cp->type = Unified;
cp->external = Extcache;
cp->setsways = Cara | Cawa | Cawt | Cawb;
cp->l1ip = 3<<14;
cp->setsh = cp->waysh = 0;
cp->linelen = CACHELINESZ;
cp->log2linelen = log2(CACHELINESZ);
cp->nways = NWAYS(l2p);
pow2 = ((l2p->auxctl >> Waycfgshift) & Waycfgmask) - 1;
if (pow2 < 0)
pow2 = 0;
waysz = (1 << pow2) * Wayszgran;
cp->nsets = waysz / CACHELINESZ;
}
void
l2pl310on(void)
{
ulong ctl;
L2pl310 *l2p = L2P;
if (getconf("*l2off") != nil) {
disallowed = 1;
return;
}
if (l2ison)
return;
l2pl310init();
l2cache->inv();
cacheuwbinv();
cacheiinv();
l2p->filtend = 0;
coherence();
l2p->filtstart = 0;
l2p->debug = 0;
coherence();
ctl = l2p->auxctl;
ctl &= Waycfgmask << Waycfgshift | Assoc16way;
ctl |= Sharovr;
ctl |= Mbo | Ipref | Dpref | Parity | Fullline0;
l2p->auxctl = ctl;
coherence();
l2p->ctl |= L2enable;
coherence();
l2ison = 1;
}
void
l2pl310off(void)
{
if (!l2ison)
return;
l2cache->wbinv();
getlock();
L2P->ctl &= ~L2enable;
coherence();
l2ison = 0;
iunlock(&l2lock);
}
static void
applyrange(ulong *reg, void *ava, int len)
{
uintptr va, endva;
if (disallowed || !l2ison)
return;
if (len < 0)
panic("l2cache*se called with negative length");
endva = (uintptr)ava + len;
for (va = (uintptr)ava & ~(CACHELINESZ-1); va < endva;
va += CACHELINESZ)
*reg = PADDR(va);
l2pl310sync();
}
void
l2pl310invse(void *va, int bytes)
{
uintptr start, end;
L2pl310 *l2p = L2P;
start = (uintptr)va;
end = start + bytes;
getlock();
if (start % CACHELINESZ != 0) {
applyrange(&l2p->clean.pa, va, 1);
}
if (end % CACHELINESZ != 0) {
applyrange(&l2p->clean.pa, (char *)va + bytes, 1);
}
applyrange(&l2p->inv.pa, va, bytes);
iunlock(&l2lock);
}
void
l2pl310wbse(void *va, int bytes)
{
getlock();
applyrange(&L2P->clean.pa, va, bytes);
iunlock(&l2lock);
}
void
l2pl310wbinvse(void *va, int bytes)
{
int odb;
L2pl310 *l2p = L2P;
if (!l2ison)
return;
getlock();
applyrange(&l2p->clean.pa, va, bytes);
odb = l2p->debug;
l2p->debug |= Wt | Nolinefill;
coherence();
applyrange(&l2p->cleaninv.pa, va, bytes);
l2p->debug = odb;
iunlock(&l2lock);
}
void
l2pl310inv(void)
{
L2pl310 *l2p = L2P;
if (disallowed)
return;
getlock();
bg_op_running = 1;
l2p->inv.way = waysmask;
coherence();
if (conf.nmach > 1)
iunlock(&l2lock);
while (l2p->inv.way & waysmask)
;
if (conf.nmach > 1)
ilock(&l2lock);
l2pl310sync();
bg_op_running = 0;
iunlock(&l2lock);
}
void
l2pl310wb(void)
{
L2pl310 *l2p = L2P;
if (disallowed || !l2ison)
return;
getlock();
bg_op_running = 1;
l2p->clean.way = waysmask;
coherence();
if (conf.nmach > 1)
iunlock(&l2lock);
while (l2p->clean.way & waysmask)
;
if (conf.nmach > 1)
ilock(&l2lock);
l2pl310sync();
bg_op_running = 0;
iunlock(&l2lock);
}
void
l2pl310wbinv(void)
{
int odb;
L2pl310 *l2p = L2P;
if (disallowed || !l2ison)
return;
l2pl310wb();
getlock();
bg_op_running = 1;
odb = l2p->debug;
l2p->debug |= Wt | Nolinefill;
coherence();
l2p->cleaninv.way = waysmask;
coherence();
if (conf.nmach > 1)
iunlock(&l2lock);
while (l2p->cleaninv.way & waysmask)
;
if (conf.nmach > 1)
ilock(&l2lock);
l2pl310sync();
l2p->debug = odb;
bg_op_running = 0;
iunlock(&l2lock);
}
static Cacheimpl l2cacheimpl = {
.info	= l2pl310info,
.on	= l2pl310on,
.off	= l2pl310off,
.inv	= l2pl310inv,
.wb	= l2pl310wb,
.wbinv	= l2pl310wbinv,
.invse	= l2pl310invse,
.wbse	= l2pl310wbse,
.wbinvse= l2pl310wbinvse,
};