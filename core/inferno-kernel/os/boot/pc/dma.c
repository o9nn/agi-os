#include	"u.h"
#include	"lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
typedef struct DMAport	DMAport;
typedef struct DMA	DMA;
typedef struct DMAxfer	DMAxfer;
enum
{
Dma0=		0x00,
Dma0status=	Dma0+0x8,
Dma0reset=	Dma0+0xD,
Dma1=		0xC0,
Dma1status=	Dma1+2*0x8,
Dma1reset=	Dma1+2*0xD,
};
struct DMAxfer
{
ulong	bpa;
void*	bva;
void*	va;
long	len;
int	isread;
};
struct DMAport
{
uchar	addr[4];
uchar	count[4];
uchar	page[4];
uchar	cmd;
uchar	req;
uchar	sbm;
uchar	mode;
uchar	cbp;
uchar	mc;
uchar	cmask;
uchar	wam;
};
struct DMA
{
DMAport;
int	shift;
Lock;
DMAxfer	x[4];
};
DMA dma[2] = {
{ 0x00, 0x02, 0x04, 0x06,
0x01, 0x03, 0x05, 0x07,
0x87, 0x83, 0x81, 0x82,
0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
0 },
{ 0xc0, 0xc4, 0xc8, 0xcc,
0xc2, 0xc6, 0xca, 0xce,
0x8f, 0x8b, 0x89, 0x8a,
0xd0, 0xd2, 0xd4, 0xd6, 0xd8, 0xda, 0xdc, 0xde,
1 },
};
void
dmainit(int chan)
{
DMA *dp;
DMAxfer *xp;
ulong v;
static int once;
if(once == 0){
outb(dma[0].mc, 0);
outb(dma[1].mc, 0);
outb(dma[0].cmask, 0);
outb(dma[1].cmask, 0);
outb(dma[1].mode, 0xC0);
once = 1;
}
dp = &dma[(chan>>2)&1];
chan = chan & 3;
xp = &dp->x[chan];
if(xp->bva != nil)
return;
v = (ulong)xalloc(BY2PG+BY2PG);
if(v == 0 || PADDR(v) >= 16*MB){
print("dmainit: chan %d: 0x%luX out of range\n", chan, v);
xfree((void*)v);
v = 0;
}
xp->bva = (void*)ROUND(v, BY2PG);
xp->bpa = PADDR(xp->bva);
xp->len = 0;
xp->isread = 0;
}
long
dmasetup(int chan, void *va, long len, int isread)
{
DMA *dp;
ulong pa;
uchar mode;
DMAxfer *xp;
dp = &dma[(chan>>2)&1];
chan = chan & 3;
xp = &dp->x[chan];
pa = PADDR(va);
if((((ulong)va)&0xF0000000) != KZERO
|| (pa&0xFFFF0000) != ((pa+len)&0xFFFF0000)
|| pa > 16*MB) {
if(xp->bva == nil)
return -1;
if(len > BY2PG)
len = BY2PG;
if(!isread)
memmove(xp->bva, va, len);
xp->va = va;
xp->len = len;
xp->isread = isread;
pa = xp->bpa;
}
else
xp->len = 0;
ilock(dp);
mode = (isread ? 0x44 : 0x48) | chan;
outb(dp->mode, mode);
outb(dp->page[chan], pa>>16);
outb(dp->cbp, 0);
outb(dp->addr[chan], pa>>dp->shift);
outb(dp->addr[chan], pa>>(8+dp->shift));
outb(dp->count[chan], (len>>dp->shift)-1);
outb(dp->count[chan], ((len>>dp->shift)-1)>>8);
outb(dp->sbm, chan);
iunlock(dp);
return len;
}
int
dmadone(int chan)
{
DMA *dp;
dp = &dma[(chan>>2)&1];
chan = chan & 3;
return inb(dp->cmd) & (1<<chan);
}
void
dmaend(int chan)
{
DMA *dp;
DMAxfer *xp;
dp = &dma[(chan>>2)&1];
chan = chan & 3;
ilock(dp);
outb(dp->sbm, 4|chan);
iunlock(dp);
xp = &dp->x[chan];
if(xp->len == 0 || !xp->isread)
return;
memmove(xp->va, xp->bva, xp->len);
xp->len = 0;
}