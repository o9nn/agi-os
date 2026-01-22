#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
enum {
Idr0		= 0x0000,
Mar0		= 0x0008,
Tsd0		= 0x0010,
Tsad0		= 0x0020,
Rbstart		= 0x0030,
Erbcr		= 0x0034,
Ersr		= 0x0036,
Cr		= 0x0037,
Capr		= 0x0038,
Cbr		= 0x003A,
Imr		= 0x003C,
Isr		= 0x003E,
Tcr		= 0x0040,
Rcr		= 0x0044,
Tctr		= 0x0048,
Mpc		= 0x004C,
Cr9346		= 0x0050,
Config0		= 0x0051,
Config1		= 0x0052,
TimerInt	= 0x0054,
Msr		= 0x0058,
Config3		= 0x0059,
Config4		= 0x005A,
Mulint		= 0x005C,
RerID		= 0x005E,
Tsad		= 0x0060,
Bmcr		= 0x0062,
Bmsr		= 0x0064,
Anar		= 0x0066,
Anlpar		= 0x0068,
Aner		= 0x006A,
Dis		= 0x006C,
Fcsc		= 0x006E,
Nwaytr		= 0x0070,
Rec		= 0x0072,
Cscr		= 0x0074,
Phy1parm	= 0x0078,
Twparm		= 0x007C,
Phy2parm	= 0x0080,
};
enum {
Bufe		= 0x01,
Te		= 0x04,
Re		= 0x08,
Rst		= 0x10,
};
enum {
Rok		= 0x0001,
Rer		= 0x0002,
Tok		= 0x0004,
Ter		= 0x0008,
Rxovw		= 0x0010,
PunLc		= 0x0020,
Fovw		= 0x0040,
Clc		= 0x2000,
Timer		= 0x4000,
Serr		= 0x8000,
};
enum {
Clrabt		= 0x00000001,
TxrrSHIFT	= 4,
TxrrMASK	= 0x000000F0,
MtxdmaSHIFT	= 8,
MtxdmaMASK	= 0x00000700,
Mtxdma2048	= 0x00000700,
Acrc		= 0x00010000,
LbkSHIFT	= 17,
LbkMASK		= 0x00060000,
IfgSHIFT	= 24,
IfgMASK		= 0x03000000,
HwveridSHIFT	= 22,
HwveridMASK	= 0x7CC00000,
};
enum {
Aap		= 0x00000001,
Apm		= 0x00000002,
Am		= 0x00000004,
Ab		= 0x00000008,
Ar		= 0x00000010,
Aer		= 0x00000020,
Sel9356		= 0x00000040,
Wrap		= 0x00000080,
MrxdmaSHIFT	= 8,
MrxdmaMASK	= 0x00000700,
Mrxdmaunlimited	= 0x00000700,
RblenSHIFT	= 11,
RblenMASK	= 0x00001800,
Rblen8K		= 0x00000000,
Rblen16K	= 0x00000800,
Rblen32K	= 0x00001000,
Rblen64K	= 0x00001800,
RxfthSHIFT	= 13,
RxfthMASK	= 0x0000E000,
Rxfth256	= 0x00008000,
Rxfthnone	= 0x0000E000,
Rer8		= 0x00010000,
MulERINT	= 0x00020000,
ErxthSHIFT	= 24,
ErxthMASK	= 0x0F000000,
Erxthnone	= 0x00000000,
};
enum {
Rcok		= 0x0001,
Fae		= 0x0002,
Crc		= 0x0004,
Long		= 0x0008,
Runt		= 0x0010,
Ise		= 0x0020,
Bar		= 0x2000,
Pam		= 0x4000,
Mar		= 0x8000,
};
enum {
Rxpf		= 0x01,
Txpf		= 0x02,
Linkb		= 0x04,
Speed10		= 0x08,
Auxstatus	= 0x10,
Rxfce		= 0x40,
Txfce		= 0x80,
};
typedef struct {
int	tsd;
int	tsad;
uchar*	data;
} Td;
enum {
SizeSHIFT	= 0,
SizeMASK	= 0x00001FFF,
Own		= 0x00002000,
Tun		= 0x00004000,
Tcok		= 0x00008000,
EtxthSHIFT	= 16,
EtxthMASK	= 0x001F0000,
NccSHIFT	= 24,
NccMASK		= 0x0F000000,
Cdh		= 0x10000000,
Owc		= 0x20000000,
Tabt		= 0x40000000,
Crs		= 0x80000000,
};
enum {
Rblen		= Rblen64K,
Ntd		= 4,
Tdbsz		= ROUNDUP(sizeof(Etherpkt), 4),
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
int	active;
int	id;
Lock	ilock;
void*	alloc;
int	rcr;
uchar*	rbstart;
int	rblen;
int	ierrs;
Lock	tlock;
Td	td[Ntd];
int	ntd;
int	tdh;
int	tdi;
int	etxth;
int	taligned;
int	tunaligned;
int	dis;
int	fcsc;
int	rec;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
#define csr8r(c, r)	(inb((c)->port+(r)))
#define csr16r(c, r)	(ins((c)->port+(r)))
#define csr32r(c, r)	(inl((c)->port+(r)))
#define csr8w(c, r, b)	(outb((c)->port+(r), (int)(b)))
#define csr16w(c, r, w)	(outs((c)->port+(r), (ushort)(w)))
#define csr32w(c, r, l)	(outl((c)->port+(r), (ulong)(l)))
static int
rtl8139reset(Ctlr* ctlr)
{
csr8w(ctlr, Cr, Rst);
while(csr8r(ctlr, Cr) & Rst)
;
return 0;
}
static void
rtl8139detach(Ether* edev)
{
rtl8139reset(edev->ctlr);
}
static void
rtl8139halt(Ctlr* ctlr)
{
csr8w(ctlr, Cr, 0);
csr16w(ctlr, Imr, 0);
csr16w(ctlr, Isr, ~0);
}
static void
rtl8139init(Ether* edev)
{
int i;
ulong r;
Ctlr *ctlr;
uchar *alloc;
ctlr = edev->ctlr;
ilock(&ctlr->ilock);
rtl8139halt(ctlr);
r = (edev->ea[3]<<24)|(edev->ea[2]<<16)|(edev->ea[1]<<8)|edev->ea[0];
csr32w(ctlr, Idr0, r);
r = (edev->ea[5]<<8)|edev->ea[4];
csr32w(ctlr, Idr0+4, r);
alloc = (uchar*)ROUNDUP((ulong)ctlr->alloc, 32);
ctlr->rbstart = alloc;
alloc += ctlr->rblen+16;
memset(ctlr->rbstart, 0, ctlr->rblen+16);
csr32w(ctlr, Rbstart, PADDR(ctlr->rbstart));
ctlr->rcr = Rxfth256|Rblen|Mrxdmaunlimited|Ab|Apm;
for(i = 0; i < Ntd; i++){
ctlr->td[i].tsd = Tsd0+i*4;
ctlr->td[i].tsad = Tsad0+i*4;
ctlr->td[i].data = alloc;
alloc += Tdbsz;
}
ctlr->ntd = ctlr->tdh = ctlr->tdi = 0;
ctlr->etxth = 128/32;
csr32w(ctlr, TimerInt, 0);
csr16w(ctlr, Imr, Serr|Timer|Fovw|PunLc|Rxovw|Ter|Tok|Rer|Rok);
csr32w(ctlr, Mpc, 0);
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Mtxdma2048);
csr32w(ctlr, Rcr, ctlr->rcr);
iunlock(&ctlr->ilock);
}
static void
rtl8139attach(Ether* edev)
{
Ctlr *ctlr;
ctlr = edev->ctlr;
if(ctlr->alloc == nil){
ctlr->rblen = 1<<((Rblen>>RblenSHIFT)+13);
ctlr->alloc = mallocz(ctlr->rblen+16 + Ntd*Tdbsz + 32, 0);
rtl8139init(edev);
}
}
static void
rtl8139txstart(Ether* edev)
{
Td *td;
Ctlr *ctlr;
RingBuf *tb;
ctlr = edev->ctlr;
while(ctlr->ntd < Ntd){
tb = &edev->tb[edev->ti];
if(tb->owner != Interface)
break;
td = &ctlr->td[ctlr->tdh];
memmove(td->data, tb->pkt, tb->len);
csr32w(ctlr, td->tsad, PADDR(tb->pkt));
csr32w(ctlr, td->tsd, (ctlr->etxth<<EtxthSHIFT)|tb->len);
ctlr->ntd++;
ctlr->tdh = NEXT(ctlr->tdh, Ntd);
tb->owner = Host;
edev->ti = NEXT(edev->ti, edev->ntb);
}
}
static void
rtl8139transmit(Ether* edev)
{
Ctlr *ctlr;
ctlr = edev->ctlr;
ilock(&ctlr->tlock);
rtl8139txstart(edev);
iunlock(&ctlr->tlock);
}
static void
rtl8139receive(Ether* edev)
{
Ctlr *ctlr;
RingBuf *rb;
ushort capr;
uchar cr, *p;
int l, length, status;
ctlr = edev->ctlr;
capr = (csr16r(ctlr, Capr)+16) % ctlr->rblen;
while(!(csr8r(ctlr, Cr) & Bufe)){
p = ctlr->rbstart+capr;
length = (*(p+3)<<8)|*(p+2);
if(length == 0xFFF0)
break;
status = (*(p+1)<<8)|*p;
if(!(status & Rcok)){
cr = csr8r(ctlr, Cr);
csr8w(ctlr, Cr, cr & ~Re);
csr32w(ctlr, Rbstart, PADDR(ctlr->rbstart));
csr8w(ctlr, Cr, cr);
csr32w(ctlr, Rcr, ctlr->rcr);
continue;
}
capr = (capr+4) % ctlr->rblen;
p = ctlr->rbstart+capr;
capr = (capr+length) % ctlr->rblen;
rb = &edev->rb[edev->ri];
l = 0;
if(p+length >= ctlr->rbstart+ctlr->rblen){
l = ctlr->rbstart+ctlr->rblen - p;
if(rb->owner == Interface)
memmove(rb->pkt, p, l);
length -= l;
p = ctlr->rbstart;
}
if(length > 0 && rb->owner == Interface){
memmove(rb->pkt+l, p, length);
l += length;
}
if(rb->owner == Interface){
rb->owner = Host;
rb->len = l-4;
edev->ri = NEXT(edev->ri, edev->nrb);
}
capr = ROUNDUP(capr, 4);
csr16w(ctlr, Capr, capr-16);
}
}
static void
rtl8139interrupt(Ureg*, void* arg)
{
Td *td;
Ctlr *ctlr;
Ether *edev;
int isr, tsd;
edev = arg;
ctlr = edev->ctlr;
while((isr = csr16r(ctlr, Isr)) != 0){
csr16w(ctlr, Isr, isr);
if(isr & (Fovw|PunLc|Rxovw|Rer|Rok)){
rtl8139receive(edev);
if(!(isr & Rok))
ctlr->ierrs++;
isr &= ~(Fovw|Rxovw|Rer|Rok);
}
if(isr & (Ter|Tok)){
ilock(&ctlr->tlock);
while(ctlr->ntd){
td = &ctlr->td[ctlr->tdi];
tsd = csr32r(ctlr, td->tsd);
if(!(tsd & (Tabt|Tun|Tcok)))
break;
if(!(tsd & Tcok)){
if(tsd & Tun){
if(ctlr->etxth < ETHERMAXTU/32)
ctlr->etxth++;
}
}
ctlr->ntd--;
ctlr->tdi = NEXT(ctlr->tdi, Ntd);
}
rtl8139txstart(edev);
iunlock(&ctlr->tlock);
isr &= ~(Ter|Tok);
}
if(isr & PunLc)
isr &= ~(Clc|PunLc);
if((isr & (Serr|Timer)) != 0){
print("rtl8139interrupt: imr %4.4uX isr %4.4uX\n",
csr16r(ctlr, Imr), isr);
if(isr & Timer)
csr32w(ctlr, TimerInt, 0);
if(isr & Serr)
rtl8139init(edev);
}
}
}
static Ctlr*
rtl8139match(Ether* edev, int id)
{
int port;
Pcidev *p;
Ctlr *ctlr;
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
p = ctlr->pcidev;
if(((p->did<<16)|p->vid) != id)
continue;
port = p->mem[0].bar & ~0x01;
if(edev->port != 0 && edev->port != port)
continue;
ctlr->port = port;
if(rtl8139reset(ctlr))
continue;
pcisetbme(p);
ctlr->active = 1;
return ctlr;
}
return nil;
}
static struct {
char*	name;
int	id;
} rtl8139pci[] = {
{ "rtl8139",	(0x8139<<16)|0x10EC, },
{ "smc1211",	(0x1211<<16)|0x1113, },
{ "dfe-538tx",	(0x1300<<16)|0x1186, },
{ "dfe-560txd",	(0x1340<<16)|0x1186, },
{ nil },
};
int
rtl8139pnp(Ether* edev)
{
int i, id;
Pcidev *p;
Ctlr *ctlr;
uchar ea[Eaddrlen];
if(ctlrhead == nil){
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
ctlr = malloc(sizeof(Ctlr));
ctlr->pcidev = p;
ctlr->id = (p->did<<16)|p->vid;
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
id = 0;
for(i = 0; i < edev->nopt; i++){
if(cistrncmp(edev->opt[i], "id=", 3) == 0)
id = strtol(&edev->opt[i][3], nil, 0);
}
ctlr = nil;
if(id != 0)
ctlr = rtl8139match(edev, id);
else for(i = 0; rtl8139pci[i].name; i++){
if((ctlr = rtl8139match(edev, rtl8139pci[i].id)) != nil)
break;
}
if(ctlr == nil)
return -1;
edev->ctlr = ctlr;
edev->port = ctlr->port;
edev->irq = ctlr->pcidev->intl;
edev->tbdf = ctlr->pcidev->tbdf;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0){
i = csr32r(ctlr, Idr0);
edev->ea[0] = i;
edev->ea[1] = i>>8;
edev->ea[2] = i>>16;
edev->ea[3] = i>>24;
i = csr32r(ctlr, Idr0+4);
edev->ea[4] = i;
edev->ea[5] = i>>8;
}
edev->attach = rtl8139attach;
edev->transmit = rtl8139transmit;
edev->interrupt = rtl8139interrupt;
edev->detach = rtl8139detach;
return 0;
}