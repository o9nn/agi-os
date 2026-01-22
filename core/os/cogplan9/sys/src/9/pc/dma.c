#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
typedef struct DMAport	DMAport;
typedef struct DMA	DMA;
typedef struct DMAxfer	DMAxfer;
struct DMAxfer
{
ulong	bpa;
void*	bva;
int	blen;
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
extern int i8237dma;
static void* i8237bva[2];
static int i8237used;
void
_i8237alloc(void)
{
void* bva;
if(i8237dma <= 0)
return;
if(i8237dma > 2)
i8237dma = 2;
bva = xspanalloc(64*1024*i8237dma, BY2PG, 64*1024);
if(bva == nil || PADDR(bva)+64*1024*i8237dma > 16*MB){
return;
}
i8237bva[0] = bva;
if(i8237dma == 2)
i8237bva[1] = ((uchar*)i8237bva[0])+64*1024;
}
int
dmainit(int chan, int maxtransfer)
{
DMA *dp;
DMAxfer *xp;
static int once;
if(once == 0){
if(ioalloc(0x00, 0x10, 0, "dma") < 0
|| ioalloc(0x80, 0x10, 0, "dma") < 0
|| ioalloc(0xd0, 0x10, 0, "dma") < 0)
panic("dmainit");
once = 1;
}
if(maxtransfer > 64*1024)
maxtransfer = 64*1024;
dp = &dma[(chan>>2)&1];
chan = chan & 3;
xp = &dp->x[chan];
if(xp->bva != nil){
if(xp->blen < maxtransfer)
return 1;
return 0;
}
if(i8237used >= i8237dma || i8237bva[i8237used] == nil){
print("no i8237 DMA bounce buffer < 16MB\n");
return 1;
}
xp->bva = i8237bva[i8237used++];
xp->bpa = PADDR(xp->bva);
xp->blen = maxtransfer;
xp->len = 0;
xp->isread = 0;
return 0;
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
if((ulong)va < KZERO
|| ((pa=PADDR(va))&0xFFFF0000) != ((pa+len)&0xFFFF0000)
|| pa >= 16*MB){
if(xp->bva == nil)
return -1;
if(len > xp->blen)
len = xp->blen;
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