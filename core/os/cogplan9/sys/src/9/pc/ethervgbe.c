#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "ethermii.h"
#define DEBUG
enum
{
DumpIntr	= (1<<0),
DumpRx		= (1<<1),
DumpTx		= (1<<2),
};
#define htole16(x) (x)
#define htole32(x) (x)
#define le32toh(x) (x)
enum
{
Timeout		= 50000,
RxCount		= 256,
TxCount		= 256,
RxSize		= 2048,
EthAddr		= 0x00,
Cr0S		= 0x08,
Cr0C		= 0x0c,
Cr0_Start	= 0x01,
Cr0_Stop	= 0x02,
Cr0_EnableRx	= 0x04,
Cr0_EnableTx	= 0x08,
Cr1S		= 0x09,
Cr1C		= 0x0d,
Cr1_NoPool	= 0x08,
Cr1_reset	= 0x80,
Cr2S		= 0x0a,
Cr2_XonEnable	= 0x80,
Cr3S		= 0x0b,
Cr3C		= 0x0f,
Cr3_IntMask	= 0x02,
Eecsr		= 0x93,
Eecsr_Autold	= 0x20,
MiiStatus	= 0x6D,
MiiStatus_idle	= 0x80,
MiiCmd		= 0x70,
MiiCmd_write	= 0x20,
MiiCmd_read	= 0x40,
MiiCmd_auto	= 0x80,
MiiAddr		= 0x71,
MiiData		= 0x72,
TxDescHi	= 0x18,
DataBufHi	= 0x1d,
RxDescLo	= 0x38,
RxCsrS		= 0x32,
RxCsrC		= 0x36,
RxCsr_RunQueue	= 0x01,
RxCsr_Active	= 0x02,
RxCsr_Wakeup	= 0x04,
RxCsr_Dead	= 0x08,
RxNum		= 0x50,
RxDscIdx	= 0x3c,
RxResCnt	= 0x5e,
RxHostErr	= 0x23,
RxTimer		= 0x3e,
RxControl	= 0x06,
RxControl_BadFrame = 0x01,
RxControl_Runt = 0x02,
RxControl_MultiCast = 0x04,
RxControl_BroadCast = 0x08,
RxControl_Promisc = 0x10,
RxControl_Giant = 0x20,
RxControl_UniCast = 0x40,
RxControl_SymbolErr = 0x80,
RxConfig	= 0x7e,
RxConfig_VlanFilter = 0x01,
RxConfig_VlanOpt0 = (0<<1),
RxConfig_VlanOpt1 = (1<<1),
RxConfig_VlanOpt2 = (2<<1),
RxConfig_VlanOpt3 = (3<<1),
RxConfig_FifoLowWat = 0x08,
RxConfig_FifoTh128 = (0<<4),
RxConfig_FifoTh512 = (1<<4),
RxConfig_FifoTh1024 = (2<<4),
RxConfig_FifoThFwd = (3<<4),
RxConfig_ArbPrio = 0x80,
TxDescLo	= 0x40,
TxCsrS		= 0x30,
TxCsrC		= 0x38,
TxCsr_RunQueue	= 0x01,
TxCsr_Active	= 0x02,
TxCsr_Wakeup	= 0x04,
TxCsr_Dead	= 0x08,
TxNum		= 0x52,
TxDscIdx	= 0x54,
TxHostErr	= 0x22,
TxTimer		= 0x3f,
TxControl	= 0x07,
TxControl_LC_Off = (0<<0),
TxControl_LC_Mac = (1<<0),
TxControl_LC_Ext = (2<<0),
TxControl_Coll16 = (0<<2),
TxControl_Coll32 = (1<<2),
TxControl_Coll48 = (2<<2),
TxControl_CollInf = (3<<2),
TxConfig	= 0x7f,
TxConfig_SnapOpt = 0x01,
TxConfig_NonBlk	= 0x02,
TxConfig_Blk64	= (0<<3),
TxConfig_Blk32	= (1<<3),
TxConfig_Blk128	= (2<<3),
TxConfig_Blk8	= (3<<3),
TxConfig_ArbPrio	= 0x80,
Timer0		= 0x74,
Timer1		= 0x76,
ChipCfgA	= 0x78,
ChipCfgB	= 0x79,
ChipCfgC	= 0x7a,
ChipCfgD	= 0x7b,
DmaCfg0		= 0x7C,
DmaCfg1		= 0x7D,
IntCtl		= 0x20,
Imr		= 0x28,
Isr		= 0x24,
Isr_RxHiPrio	= (1<<0),
Isr_TxHiPrio	= (1<<1),
Isr_RxComplete	= (1<<2),
Isr_TxComplete	= (1<<3),
Isr_TxComplete0	= (1<<4),
Isr_TxComplete1	= (1<<5),
Isr_TxComplete2	= (1<<6),
Isr_TxComplete3	= (1<<7),
Isr_Reserved8	= (1<<8),
Isr_Reserver9	= (1<<9),
Isr_RxCountOvflow = (1<<10),
Isr_RxPause	= (1<<11),
Isr_RxFifoOvflow = (1<<12),
Isr_RxNoDesc	= (1<<13),
Isr_RxNoDescWar	= (1<<14),
Isr_LinkStatus	= (1<<15),
Isr_Timer0	= (1<<16),
Isr_Timer1	= (1<<17),
Isr_Power	= (1<<18),
Isr_PhyIntr	= (1<<19),
Isr_Stopped	= (1<<20),
Isr_MibOvflow	= (1<<21),
Isr_SoftIntr	= (1<<22),
Isr_HoldOffReload = (1<<23),
Isr_RxDmaStall	= (1<<24),
Isr_TxDmaStall	= (1<<25),
Isr_Reserved26	= (1<<26),
Isr_Reserved27	= (1<<27),
Isr_Source0	= (1<<28),
Isr_Source1	= (1<<29),
Isr_Source2	= (1<<30),
Isr_Source3	= (1<<31),
Isr_Mask = Isr_TxComplete0|Isr_RxComplete|Isr_Stopped|
Isr_RxFifoOvflow|Isr_PhyIntr|Isr_LinkStatus|
Isr_RxNoDesc|Isr_RxDmaStall|Isr_TxDmaStall
};
typedef struct Frag Frag;
struct Frag
{
ulong	addr_lo;
ushort	addr_hi;
ushort	length;
};
typedef struct RxDesc RxDesc;
struct RxDesc
{
ulong	status;
ulong	control;
Frag;
};
typedef struct TxDesc TxDesc;
struct TxDesc
{
ulong	status;
ulong	control;
Frag	frags[7];
};
enum
{
RxDesc_Status_VidMiss	= (1<<0),
RxDesc_Status_CrcErr	= (1<<1),
RxDesc_Status_FrAlErr	= (1<<3),
RxDesc_Status_CsumErr	= (1<<3),
RxDesc_Status_RxLenErr	= (1<<4),
RxDesc_Status_SymErr	= (1<<5),
RxDesc_Status_SnTag	= (1<<6),
RxDesc_Status_DeTag	= (1<<7),
RxDesc_Status_OneFrag	= (0<<8),
RxDesc_Status_FirstFrag	= (1<<8),
RxDesc_Status_LastFrag	= (2<<8),
RxDesc_Status_MidFrag	= (3<<8),
RxDesc_Status_Vtag	= (1<<10),
RxDesc_Status_UniCast	= (1<<11),
RxDesc_Status_BroadCast	= (1<<12),
RxDesc_Status_MultiCast	= (1<<13),
RxDesc_Status_Perfect	= (1<<14),
RxDesc_Status_Goodframe	= (1<<15),
RxDesc_Status_SizShift	= 16,
RxDesc_Status_SizMask	= 0x3FFF,
RxDesc_Status_Shutdown	= (1<<30),
RxDesc_Status_Own	= (1<<31),
TxDesc_Status_Own	= (1<<31),
TxDesc_Control_Intr	= (1<<23),
TxDesc_Control_Normal	= (3<<24),
};
typedef struct Stats Stats;
struct Stats
{
ulong	rx;
ulong	tx;
ulong	txe;
ulong	intr;
};
typedef struct Ctlr Ctlr;
struct Ctlr
{
Ctlr*	link;
Pcidev*	pdev;
int	port;
int	inited;
Lock	init_lock;
ulong	debugflags;
ulong	debugcount;
Mii*	mii;
int	active;
uchar	ea[6];
RxDesc*	rx_ring;
Block*	rx_blocks[RxCount];
Lock	tx_lock;
TxDesc*	tx_ring;
Block*	tx_blocks[TxCount];
ulong	tx_count;
Stats	stats;
};
static Ctlr* vgbehead;
static Ctlr* vgbetail;
#define riob(c, r)	inb(c->port + r)
#define riow(c, r)	ins(c->port + r)
#define riol(c, r)	inl(c->port + r)
#define wiob(c, r, d)	outb(c->port + r, d)
#define wiow(c, r, d)	outs(c->port + r, d)
#define wiol(c, r, d)	outl(c->port + r, d)
#define siob(c, r, b)	wiob(c, r, riob(c, r) | b)
#define siow(c, r, b)	wiow(c, r, riob(c, r) | b)
#define siol(c, r, b)	wiol(c, r, riob(c, r) | b)
#define ciob(c, r, b)	wiob(c, r, riob(c, r) & ~b)
#define ciow(c, r, b)	wiow(c, r, riob(c, r) & ~b)
#define ciol(c, r, b)	wiol(c, r, riob(c, r) & ~b)
static int
vgbemiiw(Mii* mii, int phy, int addr, int data)
{
Ctlr* ctlr;
int i;
if(phy != 1)
return -1;
ctlr = mii->ctlr;
wiob(ctlr, MiiAddr, addr);
wiow(ctlr, MiiData, (ushort) data);
wiob(ctlr, MiiCmd, MiiCmd_write);
for(i = 0; i < Timeout; i++)
if((riob(ctlr, MiiCmd) & MiiCmd_write) == 0)
break;
if(i >= Timeout){
print("vgbe: miiw timeout\n");
return -1;
}
return 0;
}
static int
vgbemiir(Mii* mii, int phy, int addr)
{
Ctlr* ctlr;
int i;
if(phy != 1)
return -1;
ctlr = mii->ctlr;
wiob(ctlr, MiiAddr, addr);
wiob(ctlr, MiiCmd, MiiCmd_read);
for(i = 0; i < Timeout; i++)
if((riob(ctlr, MiiCmd) & MiiCmd_read) == 0)
break;
if(i >= Timeout){
print("vgbe: miir timeout\n");
return -1;
}
return riow(ctlr, MiiData);
}
static long
vgbeifstat(Ether* edev, void* a, long n, ulong offset)
{
char* p;
Ctlr* ctlr;
int l;
ctlr = edev->ctlr;
p = malloc(READSTR);
if(p == nil)
error(Enomem);
l = 0;
l += snprint(p+l, READSTR-l, "tx: %uld\n", ctlr->stats.tx);
l += snprint(p+l, READSTR-l, "tx [errs]: %uld\n", ctlr->stats.txe);
l += snprint(p+l, READSTR-l, "rx: %uld\n", ctlr->stats.rx);
l += snprint(p+l, READSTR-l, "intr: %uld\n", ctlr->stats.intr);
snprint(p+l, READSTR-l, "\n");
n = readstr(offset, a, n, p);
free(p);
return n;
}
static char* vgbeisr_info[] = {
"hi prio Rx int",
"hi prio Tx int",
"Rx queue completed",
"One of Tx queues completed",
"Tx queue 0 completed",
"Tx queue 1 completed",
"Tx queue 2 completed",
"Tx queue 3 completed",
"reserved",
"reserved",
"Rx packet count overflow",
"pause frame Rx'ed",
"RX FIFO overflow",
"ran out of Rx descriptors",
"running out of Rx descriptors",
"link status change",
"one shot timer expired",
"periodic timer expired",
"wake up power event",
"PHY interrupt",
"software shutdown complete",
"MIB counter overflow warning",
"software interrupt",
"reload hold timer",
"Rx DMA stall",
"Tx DMA stall",
"reserved",
"reserved",
"interrupt source indication 0",
"interrupt source indication 1",
"interrupt source indication 2",
"interrupt source indication 3",
};
static void
vgbedumpisr(ulong isr)
{
int i;
for(i = 0; i < 32; i++){
ulong mask;
mask = 1<<i;
if(isr & mask)
print("vgbe: irq:  - %02d : %c %s\n", i,
Isr_Mask & mask ? '*' : '-', vgbeisr_info[i]);
}
}
static void
noop(Block *)
{
}
static int
vgbenewrx(Ctlr* ctlr, int i)
{
Block* block;
RxDesc* desc;
block = allocb(RxSize);
block->free = noop;
ctlr->rx_blocks[i] = block;
desc = &ctlr->rx_ring[i];
desc->status = htole32(RxDesc_Status_Own);
desc->control = htole32(0);
desc->addr_lo = htole32((ulong)PCIWADDR(block->rp));
desc->addr_hi = htole16(0);
desc->length = htole16(RxSize | 0x8000);
return 0;
}
static void
vgberxeof(Ether* edev)
{
Ctlr* ctlr;
int i;
Block* block;
ulong length, status;
RxDesc* desc;
ctlr = edev->ctlr;
if(ctlr->debugflags & DumpRx)
print("vgbe: rx_eof\n");
for(i = 0; i < RxCount; i++){
desc = &ctlr->rx_ring[i];
status = le32toh(desc->status);
if(status & RxDesc_Status_Own)
continue;
if(status & RxDesc_Status_Goodframe){
length = status >> RxDesc_Status_SizShift;
length &= RxDesc_Status_SizMask;
if(ctlr->debugflags & DumpRx)
print("vgbe: Rx-desc[%03d] status=%#08ulx ctl=%#08ulx len=%uld bytes\n",
i, status, desc->control, length);
block = ctlr->rx_blocks[i];
block->wp = block->rp + length;
ctlr->stats.rx++;
etheriq(edev, block, 1);
}
else
print("vgbe: Rx-desc[%#02x] *BAD FRAME* status=%#08ulx ctl=%#08ulx\n",
i, status, desc->control);
desc->status = htole32(RxDesc_Status_Own);
desc->control = htole32(0);
}
if(ctlr->debugflags & DumpRx)
print("vgbe: rx_eof: done\n");
wiow(ctlr, RxResCnt, RxCount);
wiob(ctlr, RxCsrS, RxCsr_Wakeup);
}
static void
vgbetxeof(Ether* edev)
{
Ctlr* ctlr;
int i, count;
Block* block;
ulong status;
ctlr = edev->ctlr;
ilock(&ctlr->tx_lock);
if(ctlr->debugflags & DumpTx)
print("vgbe: tx_eof\n");
for(count = 0, i = 0; i < TxCount; i++){
block = ctlr->tx_blocks[i];
if(block == nil)
continue;
status = le32toh(ctlr->tx_ring[i].status);
if(status & TxDesc_Status_Own)
continue;
ctlr->stats.tx++;
if(ctlr->debugflags & DumpTx)
print("vgbe: Block[%03d]:%#p has been sent\n", i, block);
count++;
ctlr->tx_blocks[i] = nil;
freeb(block);
if(ctlr->debugflags & DumpTx)
print("vgbe: Block[%03d]:%#p has been freed\n", i, block);
}
ctlr->tx_count -= count;
if(ctlr->debugflags & DumpTx)
print("vgbe: tx_eof: done [count=%d]\n", count);
iunlock(&ctlr->tx_lock);
if(ctlr->tx_count)
wiob(ctlr, TxCsrS, TxCsr_Wakeup);
}
static void
vgbeinterrupt(Ureg *, void* arg)
{
Ether* edev;
Ctlr* ctlr;
ulong status;
edev = (Ether *) arg;
if(edev == nil)
return;
ctlr = edev->ctlr;
if(ctlr == nil)
return;
wiol(ctlr, Imr, 0);
status = riol(ctlr, Isr);
if(status == 0xffff)
goto end;
if(status)
wiol(ctlr, Isr, status);
if((status & Isr_Mask) == 0)
goto end;
ctlr->stats.intr++;
if(ctlr->debugflags & DumpIntr)
if(ctlr->debugcount){
print("vgbe: irq: status = %#08ulx\n", status);
vgbedumpisr(status);
ctlr->debugcount--;
}
if(status & Isr_RxComplete)
vgberxeof(edev);
if(status & Isr_TxComplete0)
vgbetxeof(edev);
if(status & Isr_Stopped)
print("vgbe: irq: software shutdown complete\n");
if(status & Isr_RxFifoOvflow)
print("vgbe: irq: RX FIFO overflow\n");
if(status & Isr_PhyIntr)
print("vgbe: irq: PHY interrupt\n");
if(status & Isr_LinkStatus)
print("vgbe: irq: link status change\n");
if(status & Isr_RxNoDesc)
print("vgbe: irq: ran out of Rx descriptors\n");
if(status & Isr_RxDmaStall){
print("vgbe: irq: Rx DMA stall\n");
wiol(ctlr, Cr3C, Cr3_IntMask);
return;
}
if(status & Isr_TxDmaStall){
print("vgbe: irq: Tx DMA stall\n");
wiol(ctlr, Cr3C, Cr3_IntMask);
return;
}
end:
wiol(ctlr, Imr, ~0);
}
static void
vgbetransmit(Ether* edev)
{
Block* block;
Ctlr* ctlr;
int i, index, start, count;
TxDesc* desc;
ulong status, length;
ctlr = edev->ctlr;
ilock(&ctlr->tx_lock);
start = riow(ctlr, TxDscIdx);
if(ctlr->debugflags & DumpTx)
print("vgbe: transmit (start=%d)\n", start);
for(count = 0, i = 0; i < TxCount; i++){
index = (i + start) % TxCount;
if(ctlr->tx_blocks[index])
continue;
desc = &ctlr->tx_ring[index];
status = le32toh(desc->status);
if(status & TxDesc_Status_Own)
continue;
block = qget(edev->oq);
if(block == nil)
break;
count++;
length = BLEN(block);
if(ctlr->debugflags & DumpTx)
print("vgbe: Tx-Desc[%03d] Block:%#p, addr=%#08ulx, len:%ld\n", index, block,
PCIWADDR(block->rp), length);
ctlr->tx_blocks[index] = block;
desc->status = htole32((length<<16)|TxDesc_Status_Own);
desc->control = htole32(TxDesc_Control_Intr|TxDesc_Control_Normal|((1+1)<<28));
desc->frags[0].addr_lo = htole32((ulong) PCIWADDR(block->rp));
desc->frags[0].addr_hi = htole16(0);
desc->frags[0].length = htole16(length);
}
ctlr->tx_count += count;
if(ctlr->debugflags & DumpTx)
print("vgbe: transmit: done [count=%d]\n", count);
iunlock(&ctlr->tx_lock);
if(ctlr->tx_count)
wiob(ctlr, TxCsrS, TxCsr_Wakeup);
if((ctlr->debugflags & DumpTx) && count == 0)
print("vgbe: transmit: no Tx entry available\n");
}
static void
vgbeattach(Ether* edev)
{
Ctlr* ctlr;
RxDesc* rxdesc;
TxDesc* txdesc;
int i;
ctlr = edev->ctlr;
lock(&ctlr->init_lock);
if(ctlr->inited){
unlock(&ctlr->init_lock);
return;
}
rxdesc = mallocalign(RxCount* sizeof(RxDesc), 256, 0, 0);
if(rxdesc == nil){
print("vgbe: unable to alloc Rx ring\n");
unlock(&ctlr->init_lock);
return;
}
ctlr->rx_ring = rxdesc;
for(i = 0; i < RxCount; i++)
vgbenewrx(ctlr, i);
wiob(ctlr, RxControl,
RxControl_MultiCast|RxControl_BroadCast|RxControl_UniCast);
wiob(ctlr, RxConfig, RxConfig_VlanOpt0);
wiol(ctlr, RxDescLo, (ulong) PCIWADDR(rxdesc));
wiow(ctlr, RxNum, RxCount - 1);
wiow(ctlr, RxDscIdx, 0);
wiow(ctlr, RxResCnt, RxCount);
txdesc = mallocalign(TxCount* sizeof(TxDesc), 256, 0, 0);
if(txdesc == nil){
print("vgbe: unable to alloc Tx ring\n");
unlock(&ctlr->init_lock);
return;
}
ctlr->tx_ring = txdesc;
wiob(ctlr, DmaCfg0, 4);
wiob(ctlr, TxControl, 0);
wiob(ctlr, TxConfig, TxConfig_NonBlk|TxConfig_ArbPrio);
wiol(ctlr, TxDescLo, (ulong) PCIWADDR(txdesc));
wiow(ctlr, TxNum, TxCount - 1);
wiow(ctlr, TxDscIdx, 0);
wiob(ctlr, Cr2S, 0xb|Cr2_XonEnable);
wiob(ctlr, RxCsrS, RxCsr_RunQueue);
wiob(ctlr, TxCsrS, TxCsr_RunQueue);
ctlr->inited = 1;
unlock(&ctlr->init_lock);
wiol(ctlr, Isr, 0xffffffff);
wiob(ctlr, Cr3S, Cr3_IntMask);
wiob(ctlr, RxCsrS, RxCsr_Wakeup);
}
static void
vgbereset(Ctlr* ctlr)
{
int timeo, i;
wiob(ctlr, Cr1S, Cr1_reset);
for(timeo = 0; timeo < Timeout; timeo++)
if((riob(ctlr, Cr1S) & Cr1_reset) == 0)
break;
if(timeo >= Timeout){
print("vgbe: softreset timeout\n");
return;
}
siob(ctlr, Eecsr, Eecsr_Autold);
for(timeo = 0; timeo < Timeout; timeo++)
if((riob(ctlr, Eecsr) & Eecsr_Autold) == 0)
break;
if(timeo >= Timeout){
print("vgbe: eeprom reload timeout\n");
return;
}
for(i = 0; i < Eaddrlen; i++)
ctlr->ea[i] = riob(ctlr, EthAddr+i);
wiol(ctlr, Isr, 0xffffffff);
wiol(ctlr, Imr, 0xffffffff);
wiol(ctlr, Cr3C, Cr3_IntMask);
wiol(ctlr, TxDescHi, 0);
wiow(ctlr, DataBufHi, 0);
wiob(ctlr, Cr0C, Cr0_Stop|Cr0_EnableRx|Cr0_EnableTx);
wiob(ctlr, Cr0S, Cr0_Start);
wiow(ctlr, RxCsrC, RxCsr_RunQueue);
wiow(ctlr, TxCsrC, TxCsr_RunQueue);
wiob(ctlr, Cr0S, Cr0_EnableRx|Cr0_EnableTx);
ctlr->mii = malloc(sizeof(Mii));
if(ctlr->mii == nil){
print("vgbe: unable to alloc Mii\n");
return;
}
ctlr->mii->mir = vgbemiir;
ctlr->mii->miw = vgbemiiw;
ctlr->mii->ctlr = ctlr;
if(mii(ctlr->mii, 1<<1) == 0){
print("vgbe: no phy found\n");
return;
}
}
static void
vgbepci(void)
{
Pcidev* pdev;
pdev = nil;
while(pdev = pcimatch(pdev, 0, 0)){
Ctlr* ctlr;
int port, size;
if(pdev->ccrb != 0x02 || pdev->ccru != 0)
continue;
switch((pdev->did<<16) | pdev->vid){
default:
continue;
case (0x3119<<16)|0x1106:
break;
}
if((pdev->pcr & 1) == 0){
print("vgbe: io not enabled [pcr=%#lux]\n", (ulong)pdev->pcr);
continue;
}
pcisetbme(pdev);
pcisetpms(pdev, 0);
port = pdev->mem[0].bar;
size = pdev->mem[0].size;
if((port & 1) == 0){
print("vgbe: bar[0]=%#x is not io\n", port);
continue;
}
if(port > 0xff00){
print("vgbe: invalid port %#ux\n", port);
continue;
}
port &= 0xfffe;
if(size != 256){
print("vgbe: invalid io size: %d\n", size);
continue;
}
if(ioalloc(port, size, 0, "vge") < 0){
print("vgbe: port %#ux already in use\n", port);
continue;
}
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil){
print("vgbe: unable to alloc Ctlr\n");
iofree(port);
continue;
}
ctlr->pdev = pdev;
ctlr->port = port;
ctlr->inited = 0;
if(vgbehead != nil)
vgbetail->link = ctlr;
else
vgbehead = ctlr;
vgbetail = ctlr;
}
}
static long
vgbectl(Ether* edev, void* buf, long n)
{
Cmdbuf* cb;
Ctlr* ctlr;
ulong index;
char* rptr;
RxDesc* rd;
TxDesc* td;
uchar* p;
ctlr = edev->ctlr;
cb = parsecmd(buf, n);
if(waserror()){
free(cb);
nexterror();
}
if(cistrcmp(cb->f[0], "reset") == 0){
vgbereset(ctlr);
wiob(ctlr, Cr3S, Cr3_IntMask);
wiob(ctlr, RxCsrS, RxCsr_RunQueue);
wiob(ctlr, RxCsrS, RxCsr_Wakeup);
}
else if(cistrcmp(cb->f[0], "dumpintr") == 0){
if(cb->nf < 2)
error(Ecmdargs);
if(cistrcmp(cb->f[1], "on") == 0){
ctlr->debugflags |= DumpIntr;
ctlr->debugcount = ~0;
}
else if(cistrcmp(cb->f[1], "off") == 0)
ctlr->debugflags &= ~DumpIntr;
else{
ulong count;
char* rptr;
count = strtoul(cb->f[1], &rptr, 0);
if(rptr == cb->f[1])
error("invalid control request");
ctlr->debugflags |= DumpIntr;
ctlr->debugcount = count;
print("vgbe: debugcount set to %uld\n", count);
}
}
else if(cistrcmp(cb->f[0], "dumprx") == 0){
if(cb->nf < 2)
error(Ecmdargs);
if(cistrcmp(cb->f[1], "on") == 0)
ctlr->debugflags |= DumpRx;
else if(cistrcmp(cb->f[1], "off") == 0)
ctlr->debugflags &= ~DumpRx;
else{
index = strtoul(cb->f[1], &rptr, 0);
if((rptr == cb->f[1]) || (index >= RxCount))
error("invalid control request");
rd = &ctlr->rx_ring[index];
print("vgbe: DumpRx[%03uld] status=%#08ulx ctl=%#08ulx len=%#04ux bytes\n",
index, rd->status, rd->control, rd->length);
}
}
else if(cistrcmp(cb->f[0], "dumptx") == 0){
if(cb->nf < 2)
error(Ecmdargs);
if(cistrcmp(cb->f[1], "on") == 0)
ctlr->debugflags |= DumpTx;
else if(cistrcmp(cb->f[1], "off") == 0)
ctlr->debugflags &= ~DumpTx;
else{
index = strtoul(cb->f[1], &rptr, 0);
if((rptr == cb->f[1]) || (index >= TxCount))
error("invalid control request");
td = &ctlr->tx_ring[index];
print("vgbe: DumpTx[%03uld] status=%#08ulx ctl=%#08ulx len=%#04ux bytes",
index, td->status, td->control, td->frags[0].length);
p = (uchar*)td;
for(index = 0; index < sizeof(TxDesc); index++){
if((index % 16) == 0)
print("\nvgbe: ");
else
print(" ");
print("%#02x", p[index]);
}
}
}
else if(cistrcmp(cb->f[0], "dumpall") == 0){
if(cb->nf < 2)
error(Ecmdargs);
if(cistrcmp(cb->f[1], "on") == 0){
ctlr->debugflags = ~0;
ctlr->debugcount = ~0;
}
else if(cistrcmp(cb->f[1], "off") == 0)
ctlr->debugflags = 0;
else error("invalid control request");
}
else
error(Ebadctl);
free(cb);
poperror();
return n;
}
static void
vgbepromiscuous(void* arg, int on)
{
USED(arg, on);
}
static void
vgbemulticast(void*, uchar*, int)
{
}
static void
vgbeshutdown(Ether* ether)
{
vgbereset(ether->ctlr);
}
static int
vgbepnp(Ether* edev)
{
Ctlr* ctlr;
if(vgbehead == nil)
vgbepci();
for(ctlr = vgbehead; ctlr != nil; ctlr = ctlr->link){
if(ctlr->active)
continue;
if(edev->port == 0 || edev->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
vgbereset(ctlr);
edev->ctlr = ctlr;
edev->port = ctlr->port;
edev->irq = ctlr->pdev->intl;
edev->tbdf = ctlr->pdev->tbdf;
edev->mbps = 1000;
memmove(edev->ea, ctlr->ea, Eaddrlen);
edev->attach = vgbeattach;
edev->transmit = vgbetransmit;
edev->interrupt = vgbeinterrupt;
edev->ifstat = vgbeifstat;
edev->multicast = vgbemulticast;
edev->shutdown = vgbeshutdown;
edev->ctl = vgbectl;
edev->arg = edev;
return 0;
}
void
ethervgbelink(void)
{
addethercard("vgbe", vgbepnp);
}