#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/sd.h"
#define dprint if(!0){}else iprint
#define idprint if(!0){}else iprint
#define ioprint if(!0){}else iprint
enum {
NCtlr		= 4,
NCtlrdrv	= 8,
NDrive		= NCtlr*NCtlrdrv,
Read 		= 0,
Write,
Coraiddebug	= 0,
};
enum {
SrbRing = 32,
ARcmd		= 027,
ARdev		= 026,
ARerr		= 021,
ARfea		= 021,
ARlba2		= 025,
ARlba1		= 024,
ARlba0		= 023,
ARseccnt	= 022,
ARstat		= 027,
ATAerr		= (1<<0),
ATAdrq		= (1<<3),
ATAdf 		= (1<<5),
ATAdrdy 	= (1<<6),
ATAbusy 	= (1<<7),
ATAabort	= (1<<2),
ATAobs		= (1<<1 | 1<<2 | 1<<4),
ATAeIEN		= (1<<1),
ATAsrst		= (1<<2),
ATAhob		= (1<<7),
ATAbad		= (ATAbusy|ATAdf|ATAdrq|ATAerr),
SFdone 		= (1<<0),
SFerror 	= (1<<1),
SRBident 	= 0,
SRBread,
SRBwrite,
SRBsmart,
SRBnodata = 0,
SRBdatain,
SRBdataout,
RQread		= 1,
PRDeot		= (1<<15),
ePrtDataErr	= (1<<0),
ePrtPRDErr	= (1<<1),
eDevErr		= (1<<2),
eDevDis		= (1<<3),
eDevCon		= (1<<4),
eOverrun	= (1<<5),
eUnderrun	= (1<<6),
eSelfDis	= (1<<8),
ePrtCRQBErr	= (1<<9),
ePrtCRPBErr	= (1<<10),
ePrtIntErr	= (1<<11),
eIORdyErr	= (1<<12),
eSelfDis2	= (1<<7),
SerrInt		= (1<<5),
eEnEDMA	= (1<<0),
eDsEDMA 	= (1<<1),
eAtaRst 	= (1<<2),
IEM		= (eDevDis | eDevCon | eSelfDis),
IEM2		= (eDevDis | eDevCon | eSelfDis2),
Dnull 		= 0,
Dnew,
Dready,
Derror,
Dmissing,
Dreset,
Dlast,
Dext	 	= (1<<0),
Dpio		= (1<<1),
Dwanted		= (1<<2),
Dedma		= (1<<3),
Dpiowant	= (1<<4),
Mpreamp	= 0x7e0,
Dpreamp	= 0x720,
REV60X1B2	= 0x7,
REV60X1C0	= 0x9,
};
static char* diskstates[Dlast] = {
"null",
"new",
"ready",
"error",
"missing",
"reset",
};
extern SDifc sdmv50xxifc;
typedef struct Arb Arb;
typedef struct Bridge Bridge;
typedef struct Chip Chip;
typedef struct Ctlr Ctlr;
typedef struct Drive Drive;
typedef struct Edma Edma;
typedef struct Prd Prd;
typedef struct Rx Rx;
typedef struct Srb Srb;
typedef struct Tx Tx;
struct Chip
{
Arb	*arb;
Edma	*edma;
};
enum {
DMautoneg,
DMsatai,
DMsataii,
};
struct Drive
{
Lock;
Ctlr	*ctlr;
SDunit	*unit;
char	name[10];
ulong	magic;
Bridge	*bridge;
Edma	*edma;
Chip	*chip;
int	chipx;
int	mediachange;
int	state;
int	flag;
uvlong	sectors;
ulong	pm2;
ulong	intick;
int	wait;
int	mode;
char	serial[20+1];
char	firmware[8+1];
char	model[40+1];
ushort	info[256];
Srb	*srb[SrbRing-1];
int	nsrb;
Prd	*prd;
Tx	*tx;
Rx	*rx;
Srb	*srbhead;
Srb	*srbtail;
int	driveno;
};
struct Ctlr
{
Lock;
int	irq;
int	tbdf;
int	rid;
ulong	magic;
int	enabled;
int	type;
SDev	*sdev;
Pcidev	*pcidev;
uchar	*mmio;
ulong	*lmmio;
Chip	chip[2];
int	nchip;
Drive	drive[NCtlrdrv];
int	ndrive;
};
struct Srb
{
Lock;
Rendez;
Srb	*next;
Drive	*drive;
uvlong	blockno;
int	count;
int	req;
int	flag;
uchar	*data;
uchar	cmd;
uchar	lba[6];
uchar	sectors;
int	sta;
int	err;
};
struct Bridge
{
ulong	status;
ulong	serror;
ulong	sctrl;
ulong	phyctrl;
ulong	phymode3;
ulong	phymode4;
uchar	fill0[0x14];
ulong	phymode1;
ulong	phymode2;
char	fill1[8];
ulong	ctrl;
char	fill2[0x34];
ulong	phymode;
char	fill3[0x88];
};
struct Arb
{
ulong	config;
ulong	rqop;
ulong	rqip;
ulong	ict;
ulong	itt;
ulong	ic;
ulong	btc;
ulong	bts;
ulong	bpc;
char	fill1[0xdc];
Bridge	bridge[4];
};
struct Edma
{
ulong	config;
ulong	timer;
ulong	iec;
ulong	iem;
ulong	txbasehi;
ulong	txi;
ulong	txo;
ulong	rxbasehi;
ulong	rxi;
ulong	rxo;
ulong	ctl;
ulong	testctl;
ulong	status;
ulong	iordyto;
char	fill[0x18];
ulong	sataconfig;
char	fill[0xac];
ushort	pio;
char	pad0[2];
uchar	err;
char	pad1[3];
uchar	seccnt;
char	pad2[3];
uchar	lba0;
char	pad3[3];
uchar	lba1;
char	pad4[3];
uchar	lba2;
char	pad5[3];
uchar	lba3;
char	pad6[3];
uchar	cmdstat;
char	pad7[3];
uchar	altstat;
uchar	fill2[0x1df];
Bridge	port;
char	fill3[0x1c00];
};
struct Prd
{
ulong	pa;
ushort	count;
ushort	flag;
ulong	zero;
ulong	reserved;
};
struct Tx
{
ulong	prdpa;
ulong	zero;
ushort	flag;
ushort	regs[11];
};
struct Rx
{
ushort	cid;
uchar	cEdmaSts;
uchar	cDevSts;
ulong	ts;
};
static Drive 	*mvsatadrive[NDrive];
static int	nmvsatadrive;
static ushort
lhgets(void *p)
{
uchar *a = p;
return ((ushort) a[1] << 8) | a[0];
}
static ulong
lhgetl(void *p)
{
uchar *a = p;
return ((ulong) lhgets(a+2) << 16) | lhgets(a);
}
static uvlong
lhgetv(void *p)
{
uchar *a = p;
return ((uvlong) lhgetl(a+4) << 32) | lhgetl(a);
}
static void
idmove(char *p, ushort *a, int n)
{
char *op;
int i;
op = p;
for(i=0; i<n/2; i++){
*p++ = a[i]>>8;
*p++ = a[i];
}
while(p>op && *--p == ' ')
*p = 0;
}
struct
{
Lock;
Srb *freechain;
int nalloc;
} srblist;
static Srb*
allocsrb(void)
{
Srb *p;
ilock(&srblist);
if((p = srblist.freechain) == nil){
srblist.nalloc++;
iunlock(&srblist);
p = smalloc(sizeof *p);
}else{
srblist.freechain = p->next;
iunlock(&srblist);
}
return p;
}
static void
freesrb(Srb *p)
{
ilock(&srblist);
p->next = srblist.freechain;
srblist.freechain = p;
iunlock(&srblist);
}
static int
satawait(uchar *p, uchar mask, uchar v, int ms)
{
int i;
for(i=0; i<ms && (*p & mask) != v; i++)
microdelay(1000);
return (*p & mask) == v;
}
static void
unmask(ulong *mmio, int port, int coal)
{
port &= 7;
if(coal)
coal = 1;
if (port < 4)
mmio[0x1d64/4] |= (3 << (((port&3)*2)) | (coal<<8));
else
mmio[0x1d64/4] |= (3 << (((port&3)*2+9)) | (coal<<17));
}
static void
mask(ulong *mmio, int port, int coal)
{
port &= 7;
if(coal)
coal = 1;
if (port < 4)
mmio[0x1d64/4] &= ~(3 << (((port&3)*2)) | (coal<<8));
else
mmio[0x1d64/4] &= ~(3 << (((port&3)*2+9)) | (coal<<17));
}
static void
phyerrata(Drive *d)
{
ulong n, m;
enum { BadAutoCal = 0xf << 26, };
if (d->ctlr->type == 1)
return;
microdelay(200);
n = d->bridge->phymode2;
while ((n & BadAutoCal) == BadAutoCal) {
dprint("%s: badautocal\n", d->unit->name);
n &= ~(1<<16);
n |= (1<<31);
d->bridge->phymode2 = n;
microdelay(200);
d->bridge->phymode2 &= ~((1<<16) | (1<<31));
microdelay(200);
n = d->bridge->phymode2;
}
n &= ~(1<<31);
d->bridge->phymode2 = n;
microdelay(200);
m = d->bridge->phymode3;
m &= ~0x7f800000;
m |= 0x2a800000;
d->bridge->phymode3 = m;
m = d->bridge->phymode3;
n = d->bridge->phymode4;
n &= ~(1<<1);
n |= 1;
switch(d->ctlr->rid){
case REV60X1B2:
default:
d->bridge->phymode4 = n;
d->bridge->phymode3 = m;
break;
case REV60X1C0:
d->bridge->phymode4 = n;
break;
}
n = d->bridge->phymode2;
n &= ~Mpreamp;
n |= d->pm2;
n &= ~(1<<16);
d->bridge->phymode2 = n;
}
static void
edmacleanout(Drive *d)
{
int i;
Srb *srb;
for(i=0; i<nelem(d->srb); i++){
if(srb = d->srb[i]){
d->srb[i] = nil;
d->nsrb--;
srb->flag |= SFerror|SFdone;
wakeup(srb);
}
}
while(srb = d->srbhead){
d->srbhead = srb->next;
srb->flag |= SFerror|SFdone;
wakeup(srb);
}
}
static void
resetdisk(Drive *d)
{
ulong n;
d->sectors = 0;
d->unit->sectors = 0;
if (d->ctlr->type == 2) {
n = d->edma->sataconfig;
n &= 0xff;
n |= 0x9b1100;
d->edma->sataconfig = n;
n = d->edma->sataconfig;
USED(n);
}
d->edma->ctl = eDsEDMA;
microdelay(1);
d->edma->ctl = eAtaRst;
microdelay(25);
d->edma->ctl = 0;
if (satawait((uchar *)&d->edma->ctl, eEnEDMA, 0, 3*1000) == 0)
print("%s: eEnEDMA never cleared on reset\n", d->unit->name);
edmacleanout(d);
phyerrata(d);
d->bridge->sctrl = 0x301 | (d->mode << 4);
d->state = Dmissing;
}
static void
edmainit(Drive *d)
{
int i;
if(d->tx != nil)
return;
d->tx = xspanalloc(32*sizeof(Tx), 1024, 0);
d->rx = xspanalloc(32*sizeof(Rx), 256, 0);
d->prd = xspanalloc(32*sizeof(Prd), 32, 0);
for(i = 0; i < 32; i++)
d->tx[i].prdpa = PADDR(&d->prd[i]);
coherence();
}
static int
configdrive(Ctlr *ctlr, Drive *d, SDunit *unit)
{
dprint("%s: configdrive\n", unit->name);
if(d->driveno < 0)
panic("mv50xx: configdrive: unset driveno\n");
d->unit = unit;
edmainit(d);
d->mode = DMsatai;
if(d->ctlr->type == 1){
d->edma->iem = IEM;
d->bridge = &d->chip->arb->bridge[d->chipx];
}else{
d->edma->iem = IEM2;
d->bridge = &d->chip->edma[d->chipx].port;
d->edma->iem = ~(1<<6);
d->pm2 = Dpreamp;
if(d->ctlr->lmmio[0x180d8/4] & 1)
d->pm2 = d->bridge->phymode2 & Mpreamp;
}
resetdisk(d);
unmask(ctlr->lmmio, d->driveno, 0);
delay(100);
if(d->bridge->status){
dprint("%s: configdrive: found drive %lx\n", unit->name, d->bridge->status);
return 0;
}
return -1;
}
static int
enabledrive(Drive *d)
{
Edma *edma;
dprint("%s: enabledrive..", d->unit->name);
if((d->bridge->status & 0xf) != 3){
dprint("%s: not present\n", d->unit->name);
d->state = Dmissing;
return -1;
}
edma = d->edma;
if(satawait(&edma->cmdstat, ATAbusy, 0, 5*1000) == 0){
dprint("%s: busy timeout\n", d->unit->name);
d->state = Dmissing;
return -1;
}
edma->iec = 0;
d->chip->arb->ic &= ~(0x101 << d->chipx);
edma->config = 0x51f;
if (d->ctlr->type == 2)
edma->config |= 7<<11;
edma->txi = PADDR(d->tx);
edma->txo = (ulong)d->tx & 0x3e0;
edma->rxi = (ulong)d->rx & 0xf8;
edma->rxo = PADDR(d->rx);
edma->ctl |= 1;
if(d->bridge->status = 0x113){
dprint("%s: new\n", d->unit->name);
d->state = Dnew;
}else
print("%s: status not forced (should be okay)\n", d->unit->name);
return 0;
}
static void
disabledrive(Drive *d)
{
int i;
ulong *r;
dprint("%s: disabledrive\n", d->unit->name);
if(d->tx == nil)
return;
d->edma->ctl = 0;
d->edma->iem = 0;
r = (ulong*)(d->ctlr->mmio + 0x1d64);
i = d->chipx;
if(d->chipx < 4)
*r &= ~(3 << (i*2));
else
*r |= ~(3 << (i*2+9));
}
static int
setudmamode(Drive *d, uchar mode)
{
Edma *edma;
dprint("%s: setudmamode %d\n", d->unit->name, mode);
edma = d->edma;
if (edma == nil) {
iprint("setudamode(m%d): zero d->edma\m", d->driveno);
return 0;
}
if(satawait(&edma->cmdstat, ~ATAobs, ATAdrdy, 9*1000) == 0){
iprint("%s: cmdstat 0x%.2ux ready timeout\n", d->unit->name, edma->cmdstat);
return 0;
}
edma->altstat = ATAeIEN;
edma->err = 3;
edma->seccnt = 0x40 | mode;
edma->cmdstat = 0xef;
microdelay(1);
if(satawait(&edma->cmdstat, ATAbusy, 0, 5*1000) == 0){
iprint("%s: cmdstat 0x%.2ux busy timeout\n", d->unit->name, edma->cmdstat);
return 0;
}
return 1;
}
static int
identifydrive(Drive *d)
{
int i;
ushort *id;
Edma *edma;
SDunit *unit;
dprint("%s: identifydrive\n", d->unit->name);
if(setudmamode(d, 5) == 0)
goto Error;
id = d->info;
memset(d->info, 0, sizeof d->info);
edma = d->edma;
if(satawait(&edma->cmdstat, ~ATAobs, ATAdrdy, 5*1000) == 0)
goto Error;
edma->altstat = ATAeIEN;
edma->cmdstat = 0xec;
microdelay(1);
if(satawait(&edma->cmdstat, ATAbusy, 0, 5*1000) == 0)
goto Error;
for(i = 0; i < 256; i++)
id[i] = edma->pio;
if(edma->cmdstat & ATAbad)
goto Error;
i = lhgets(id+83) | lhgets(id+86);
if(i & (1<<10)){
d->flag |= Dext;
d->sectors = lhgetv(id+100);
}else{
d->flag &= ~Dext;
d->sectors = lhgetl(id+60);
}
idmove(d->serial, id+10, 20);
idmove(d->firmware, id+23, 8);
idmove(d->model, id+27, 40);
unit = d->unit;
memset(unit->inquiry, 0, sizeof unit->inquiry);
unit->inquiry[2] = 2;
unit->inquiry[3] = 2;
unit->inquiry[4] = sizeof(unit->inquiry)-4;
idmove((char*)unit->inquiry+8, id+27, 40);
if(enabledrive(d) == 0) {
d->state = Dready;
d->mediachange = 1;
idprint("%s: LLBA %lld sectors\n", d->unit->name, d->sectors);
} else
d->state = Derror;
if(d->state == Dready)
return 0;
return -1;
Error:
dprint("error...");
d->state = Derror;
return -1;
}
static char stab[] = {
[1]	'M',
[10]	'P',
[16]	'N',
[18]	'W', 'B', 'D', 'C', 'H', 'S', 'T', 'F', 'X'
};
static ulong sbad = (7<<20)|(3<<23);
static void
serrdecode(ulong r, char *s, char *e)
{
int i;
e -= 3;
for(i = 0; i < nelem(stab) && s < e; i++){
if((r&(1<<i)) && stab[i]){
*s++ = stab[i];
if(sbad&(1<<i))
*s++ = '*';
}
}
*s = 0;
}
char *iectab[] = {
"ePrtDataErr",
"ePrtPRDErr",
"eDevErr",
"eDevDis",
"eDevCon",
"SerrInt",
"eUnderrun",
"eSelfDis2",
"eSelfDis",
"ePrtCRQBErr",
"ePrtCRPBErr",
"ePrtIntErr",
"eIORdyErr",
};
static char*
iecdecode(ulong cause)
{
int i;
for(i = 0; i < nelem(iectab); i++)
if(cause&(1<<i))
return iectab[i];
return "";
}
enum{
Cerror = ePrtDataErr|ePrtPRDErr|eDevErr|eSelfDis2|ePrtCRPBErr|ePrtIntErr,
};
static void
updatedrive(Drive *d)
{
int x;
ulong cause;
Edma *edma;
char buf[32+4+1];
edma = d->edma;
if((edma->ctl&eEnEDMA) == 0){
x = d->edma->cmdstat;
USED(x);
}
cause = edma->iec;
if(cause == 0)
return;
dprint("%s: cause %08ulx [%s]\n", d->unit->name, cause, iecdecode(cause));
if(cause & eDevCon)
d->state = Dnew;
if(cause&eDevDis && d->state == Dready)
iprint("%s: pulled: st=%08ulx\n", d->unit->name, cause);
switch(d->ctlr->type){
case 1:
if(cause&eSelfDis)
d->state = Derror;
break;
case 2:
if(cause&Cerror)
d->state = Derror;
if(cause&SerrInt){
serrdecode(d->bridge->serror, buf, buf+sizeof buf);
dprint("%s: serror %08ulx [%s]\n", d->unit->name, (ulong)d->bridge->serror, buf);
d->bridge->serror = d->bridge->serror;
}
}
edma->iec = ~cause;
}
static Srb*
srbrw(int req, Drive *d, uchar *data, uint sectors, uvlong lba)
{
int i;
Srb *srb;
static uchar cmd[2][2] = { 0xC8, 0x25, 0xCA, 0x35 };
srb = allocsrb();
srb->req = req;
srb->drive = d;
srb->blockno = lba;
srb->sectors = sectors;
srb->count = sectors*512;
srb->flag = 0;
srb->data = data;
for(i=0; i<6; i++)
srb->lba[i] = lba >> (8*i);
srb->cmd = cmd[srb->req!=SRBread][(d->flag&Dext)!=0];
return srb;
}
static uintptr
advance(uintptr pa, int shift)
{
int n, mask;
mask = 0x1F<<shift;
n = (pa & mask) + (1<<shift);
return (pa & ~mask) | (n & mask);
}
#define CMD(r, v) (((r)<<8) | ((v)&0xFF))
static void
mvsatarequest(ushort *cmd, Srb *srb, int ext)
{
*cmd++ = CMD(ARseccnt, 0);
*cmd++ = CMD(ARseccnt, srb->sectors);
*cmd++ = CMD(ARfea, 0);
if(ext){
*cmd++ = CMD(ARlba0, srb->lba[3]);
*cmd++ = CMD(ARlba0, srb->lba[0]);
*cmd++ = CMD(ARlba1, srb->lba[4]);
*cmd++ = CMD(ARlba1, srb->lba[1]);
*cmd++ = CMD(ARlba2, srb->lba[5]);
*cmd++ = CMD(ARlba2, srb->lba[2]);
*cmd++ = CMD(ARdev, 0xe0);
}else{
*cmd++ = CMD(ARlba0, srb->lba[0]);
*cmd++ = CMD(ARlba1, srb->lba[1]);
*cmd++ = CMD(ARlba2, srb->lba[2]);
*cmd++ = CMD(ARdev, srb->lba[3] | 0xe0);
}
*cmd = CMD(ARcmd, srb->cmd) | (1<<15);
}
static void
startsrb(Drive *d, Srb *srb)
{
int i;
Edma *edma;
Prd *prd;
Tx *tx;
if(d->nsrb >= nelem(d->srb)){
srb->next = nil;
if(d->srbhead)
d->srbtail->next = srb;
else
d->srbhead = srb;
d->srbtail = srb;
return;
}
d->nsrb++;
for(i=0; i<nelem(d->srb); i++)
if(d->srb[i] == nil)
break;
if(i == nelem(d->srb))
panic("sdmv50xx: no free srbs");
d->intick = MACHP(0)->ticks;
d->srb[i] = srb;
edma = d->edma;
tx = (Tx*)KADDR(edma->txi);
tx->flag = (i<<1) | (srb->req == SRBread);
prd = KADDR(tx->prdpa);
prd->pa = PADDR(srb->data);
prd->count = srb->count;
prd->flag = PRDeot;
mvsatarequest(tx->regs, srb, d->flag&Dext);
coherence();
edma->txi = advance(edma->txi, 5);
d->intick = MACHP(0)->ticks;
}
enum{
Rpidx	= 0x1f<<3,
};
static void
completesrb(Drive *d)
{
Edma *edma;
Rx *rx;
Srb *srb;
edma = d->edma;
if((edma->ctl & eEnEDMA) == 0)
return;
while((edma->rxo&Rpidx) != (edma->rxi&Rpidx)){
rx = (Rx*)KADDR(edma->rxo);
if(srb = d->srb[rx->cid]){
d->srb[rx->cid] = nil;
d->nsrb--;
if(rx->cDevSts & ATAbad)
srb->flag |= SFerror;
if (rx->cEdmaSts)
iprint("cEdmaSts: %02ux\n", rx->cEdmaSts);
srb->sta = rx->cDevSts;
srb->flag |= SFdone;
wakeup(srb);
}else
iprint("srb missing\n");
edma->rxo = advance(edma->rxo, 3);
if(srb = d->srbhead){
d->srbhead = srb->next;
startsrb(d, srb);
}
}
}
static int
srbdone(void *v)
{
Srb *srb;
srb = v;
return srb->flag & SFdone;
}
static void
mv50interrupt(Ureg*, void *a)
{
int i;
ulong cause;
Ctlr *ctlr;
Drive *drive;
ctlr = a;
ilock(ctlr);
cause = ctlr->lmmio[0x1d60/4];
for(i=0; i<ctlr->ndrive; i++)
if(cause & (3<<(i*2+i/4))){
drive = &ctlr->drive[i];
if(drive->edma == 0)
continue;
ilock(drive);
updatedrive(drive);
while(ctlr->chip[i/4].arb->ic & (0x0101 << (i%4))){
ctlr->chip[i/4].arb->ic = ~(0x101 << (i%4));
completesrb(drive);
}
iunlock(drive);
}
iunlock(ctlr);
}
enum{
Nms		= 256,
Midwait		= 16*1024/Nms-1,
Mphywait	= 512/Nms-1,
};
static void
westerndigitalhung(Drive *d)
{
Edma *e;
e = d->edma;
if(d->srb
&& TK2MS(MACHP(0)->ticks-d->intick) > 5*1000
&& (e->rxo&Rpidx) == (e->rxi&Rpidx)){
dprint("westerndigital drive hung; resetting\n");
d->state = Dreset;
}
}
static void
checkdrive(Drive *d, int i)
{
static ulong s, olds[NCtlr*NCtlrdrv];
char *name;
ilock(d);
name = d->unit->name;
s = d->bridge->status;
if(s != olds[i]){
dprint("%s: status: %08lx -> %08lx: %s\n", name, olds[i], s, diskstates[d->state]);
olds[i] = s;
}
switch(d->state){
case Dnew:
case Dmissing:
switch(s){
case 0x000:
break;
default:
dprint("%s: unknown state %8lx\n", name, s);
case 0x100:
if(++d->wait&Mphywait)
break;
reset:	d->mode ^= 1;
dprint("%s: reset; new mode %d\n", name, d->mode);
resetdisk(d);
break;
case 0x123:
case 0x113:
s = d->edma->cmdstat;
if(s == 0x7f || (s&~ATAobs) != ATAdrdy){
if((++d->wait&Midwait) == 0)
goto reset;
}else if(identifydrive(d) == -1)
goto reset;
}
break;
case Dready:
if(s != 0)
break;
iprint("%s: pulled: st=%08ulx\n", name, s);
case Dreset:
case Derror:
dprint("%s reset: mode %d\n", name, d->mode);
resetdisk(d);
break;
}
iunlock(d);
}
static void
satakproc(void*)
{
int i;
while(waserror())
;
for(;;){
tsleep(&up->sleep, return0, 0, Nms);
for(i = 0; i < nmvsatadrive; i++)
checkdrive(mvsatadrive[i], i);
}
}
static SDev*
mv50pnp(void)
{
int i, nunit;
uchar *base;
ulong io, n, *mem;
Ctlr *ctlr;
Pcidev *p;
SDev *head, *tail, *sdev;
Drive *d;
static int ctlrno, done;
dprint("mv50pnp\n");
if(done++)
return nil;
p = nil;
head = nil;
tail = nil;
while((p = pcimatch(p, 0x11ab, 0)) != nil){
switch(p->did){
case 0x5040:
case 0x5041:
case 0x5080:
case 0x5081:
case 0x6041:
case 0x6081:
break;
default:
print("mv50pnp: unknown did %ux ignored\n", (ushort)p->did);
continue;
}
if (ctlrno >= NCtlr) {
print("mv50pnp: too many controllers\n");
break;
}
nunit = (p->did&0xf0) >> 4;
print("Marvell 88SX%ux: %d SATA-%s ports with%s flash\n",
(ushort)p->did, nunit,
((p->did&0xf000)==0x6000? "II": "I"),
(p->did&1? "": "out"));
if((sdev = malloc(sizeof(SDev))) == nil)
continue;
if((ctlr = malloc(sizeof(Ctlr))) == nil){
free(sdev);
continue;
}
memset(sdev, 0, sizeof *sdev);
memset(ctlr, 0, sizeof *ctlr);
io = p->mem[0].bar & ~0x0F;
mem = (ulong*)vmap(io, p->mem[0].size);
if(mem == 0){
print("sdmv50xx: address 0x%luX in use\n", io);
free(sdev);
free(ctlr);
continue;
}
ctlr->rid = p->rid;
mem[0x104f0/4] = 0;
ctlr->type = (p->did >> 12) & 3;
if(ctlr->type == 1){
n = mem[0xc00/4];
n &= ~(3<<4);
mem[0xc00/4] = n;
}
sdev->ifc = &sdmv50xxifc;
sdev->ctlr = ctlr;
sdev->nunit = nunit;
sdev->idno = 'E';
ctlr->sdev = sdev;
ctlr->irq = p->intl;
ctlr->tbdf = p->tbdf;
ctlr->pcidev = p;
ctlr->lmmio = mem;
ctlr->mmio = (uchar*)mem;
ctlr->nchip = (nunit+3)/4;
ctlr->ndrive = nunit;
ctlr->enabled = 0;
for(i = 0; i < ctlr->nchip; i++){
base = ctlr->mmio+0x20000+0x10000*i;
ctlr->chip[i].arb = (Arb*)base;
ctlr->chip[i].edma = (Edma*)(base + 0x2000);
}
for (i = 0; i < nunit; i++) {
d = &ctlr->drive[i];
d->sectors = 0;
d->ctlr = ctlr;
d->driveno = ctlrno*NCtlrdrv + i;
d->chipx = i%4;
d->chip = &ctlr->chip[i/4];
d->edma = &d->chip->edma[d->chipx];
mvsatadrive[d->driveno] = d;
}
nmvsatadrive += nunit;
ctlrno++;
if(head)
tail->next = sdev;
else
head = sdev;
tail = sdev;
}
return head;
}
static int
mv50enable(SDev *sdev)
{
char name[32];
Ctlr *ctlr;
dprint("sd%c: enable\n", sdev->idno);
ctlr = sdev->ctlr;
if (ctlr->enabled)
return 1;
snprint(name, sizeof name, "%s (%s)", sdev->name, sdev->ifc->name);
intrenable(ctlr->irq, mv50interrupt, ctlr, ctlr->tbdf, name);
ctlr->enabled = 1;
return 1;
}
static int
mv50disable(SDev *sdev)
{
char name[32];
int i;
Ctlr *ctlr;
Drive *drive;
dprint("sd%c: disable\n", sdev->idno);
ctlr = sdev->ctlr;
ilock(ctlr);
for(i=0; i<ctlr->sdev->nunit; i++){
drive = &ctlr->drive[i];
ilock(drive);
disabledrive(drive);
iunlock(drive);
}
iunlock(ctlr);
snprint(name, sizeof name, "%s (%s)", sdev->name, sdev->ifc->name);
intrdisable(ctlr->irq, mv50interrupt, ctlr, ctlr->tbdf, name);
return 0;
}
static void
mv50clear(SDev *sdev)
{
int i;
Ctlr *ctlr;
Drive *d;
dprint("sd%c: clear\n", sdev->idno);
ctlr = sdev->ctlr;
for(i=0; i<ctlr->ndrive; i++){
d = &ctlr->drive[i];
free(d->tx);
free(d->rx);
free(d->prd);
}
free(ctlr);
}
static int
mv50verify(SDunit *unit)
{
Ctlr *ctlr;
Drive *drive;
int i;
dprint("%s: verify\n", unit->name);
ctlr = unit->dev->ctlr;
drive = &ctlr->drive[unit->subno];
ilock(ctlr);
ilock(drive);
i = configdrive(ctlr, drive, unit);
iunlock(drive);
iunlock(ctlr);
if(ctlr->type != 1 && i == 0){
if(!waserror()){
tsleep(&up->sleep, return0, 0, 1500);
poperror();
}
}
return 1;
}
static int
mv50online(SDunit *unit)
{
Ctlr *ctlr;
Drive *d;
int r, s0;
static int once;
if(once++ == 0)
kproc("mvsata", satakproc, 0);
ctlr = unit->dev->ctlr;
d = &ctlr->drive[unit->subno];
r = 0;
ilock(d);
s0 = d->state;
USED(s0);
if(d->state == Dnew)
identifydrive(d);
if(d->mediachange){
idprint("%s: online: %s -> %s\n", unit->name, diskstates[s0], diskstates[d->state]);
r = 2;
unit->sectors = d->sectors;
unit->secsize = 512;
d->mediachange = 0;
} else if(d->state == Dready)
r = 1;
iunlock(d);
return r;
}
typedef struct Regs Regs;
struct Regs
{
ulong offset;
char *name;
};
static Regs regsctlr[] =
{
0x0C28, "pci serr# mask",
0x1D40, "pci err addr low",
0x1D44, "pci err addr hi",
0x1D48, "pci err attr",
0x1D50, "pci err cmd",
0x1D58, "pci intr cause",
0x1D5C, "pci mask cause",
0x1D60, "device micr",
0x1D64, "device mimr",
};
static Regs regsarb[] =
{
0x0004,	"arb rqop",
0x0008,	"arb rqip",
0x000C,	"arb ict",
0x0010,	"arb itt",
0x0014,	"arb ic",
0x0018,	"arb btc",
0x001C,	"arb bts",
0x0020,	"arb bpc",
};
static Regs regsbridge[] =
{
0x0000,	"bridge status",
0x0004,	"bridge serror",
0x0008,	"bridge sctrl",
0x000C,	"bridge phyctrl",
0x003C,	"bridge ctrl",
0x0074,	"bridge phymode",
};
static Regs regsedma[] =
{
0x0000,	"edma config",
0x0004,	"edma timer",
0x0008,	"edma iec",
0x000C,	"edma iem",
0x0010,	"edma txbasehi",
0x0014,	"edma txi",
0x0018,	"edma txo",
0x001C,	"edma rxbasehi",
0x0020,	"edma rxi",
0x0024,	"edma rxo",
0x0028,	"edma c",
0x002C,	"edma tc",
0x0030,	"edma status",
0x0034,	"edma iordyto",
};
static char*
rdregs(char *p, char *e, void *base, Regs *r, int n, char *prefix)
{
int i;
for(i = 0; i < n; i++)
p = seprint(p, e, "%s%s%-19s %.8lux\n",
prefix? prefix: "", prefix? ": ": "",
r[i].name, *(ulong *)((uchar*)base + r[i].offset));
return p;
}
static char*
rdinfo(char *p, char *e, ushort *info)
{
int i;
p = seprint(p, e, "info");
for(i = 0; i < 256; i++)
p = seprint(p, e, "%s%.4ux%s", i%8 == 0? "\t": "", info[i],
i%8 == 7? "\n": "");
return p;
}
static int
mv50rctl(SDunit *unit, char *p, int l)
{
char *e, *op;
Ctlr *ctlr;
Drive *drive;
if((ctlr = unit->dev->ctlr) == nil)
return 0;
drive = &ctlr->drive[unit->subno];
e = p+l;
op = p;
if(drive->state == Dready){
p = seprint(p, e, "model    %s\n", drive->model);
p = seprint(p, e, "serial   %s\n", drive->serial);
p = seprint(p, e, "firmware %s\n", drive->firmware);
}else
p = seprint(p, e, "no disk present\n");
p = seprint(p, e, "geometry %llud 512\n", drive->sectors);
p = rdinfo(p, e, drive->info);
p = rdregs(p, e, drive->chip->arb, regsarb, nelem(regsarb), nil);
p = rdregs(p, e, drive->bridge, regsbridge, nelem(regsbridge), nil);
p = rdregs(p, e, drive->edma, regsedma, nelem(regsedma), nil);
return p-op;
}
static int
mv50wctl(SDunit *unit, Cmdbuf *cb)
{
Ctlr *ctlr;
Drive *drive;
USED(unit);
if(strcmp(cb->f[0], "reset") == 0){
ctlr = unit->dev->ctlr;
drive = &ctlr->drive[unit->subno];
ilock(drive);
drive->state = Dreset;
iunlock(drive);
return 0;
}
cmderror(cb, Ebadctl);
return -1;
}
static char*
mv50rtopctl(SDev *sdev, char *p, char *e)
{
char name[10];
Ctlr *ctlr;
ctlr = sdev->ctlr;
if(ctlr == nil)
return p;
snprint(name, sizeof name, "sd%c", sdev->idno);
p = rdregs(p, e, ctlr->mmio, regsctlr, nelem(regsctlr), name);
if (Coraiddebug) {
p = rdregs(p, e, ctlr->chip[0].arb,
regsarb, nelem(regsarb), name);
p = rdregs(p, e, &ctlr->chip[0].arb->bridge[0],
regsbridge, nelem(regsbridge), name);
p = rdregs(p, e, &ctlr->chip[0].edma[0],
regsedma, nelem(regsedma), name);
}
return p;
}
static int
waitready(Drive *d)
{
ulong s, i;
for(i = 0; i < 120; i++){
ilock(d);
s = d->bridge->status;
iunlock(d);
if(s == 0)
return SDeio;
if (d->state == Dready)
return SDok;
if ((i+1)%60 == 0){
ilock(d);
resetdisk(d);
iunlock(d);
}
if(!waserror()){
tsleep(&up->sleep, return0, 0, 1000);
poperror();
}
}
print("%s: not responding after 2 minutes\n", d->unit->name);
return SDeio;
}
static int
mv50rio(SDreq *r)
{
int count, max, n, status, try, flag;
uchar *cmd, *data;
uvlong lba;
Ctlr *ctlr;
Drive *drive;
SDunit *unit;
Srb *srb;
unit = r->unit;
ctlr = unit->dev->ctlr;
drive = &ctlr->drive[unit->subno];
cmd = r->cmd;
if((status = sdfakescsi(r, drive->info, sizeof drive->info)) != SDnostatus){
r->status = status;
return status;
}
switch(cmd[0]){
case 0x28:
case 0x2A:
break;
default:
iprint("%s: bad cmd 0x%.2ux\n", drive->unit->name, cmd[0]);
r->status = SDcheck;
return SDcheck;
}
lba = (cmd[2]<<24)|(cmd[3]<<16)|(cmd[4]<<8)|cmd[5];
count = (cmd[7]<<8)|cmd[8];
if(r->data == nil)
return SDok;
if(r->dlen < count*unit->secsize)
count = r->dlen/unit->secsize;
try = 0;
retry:
if(waitready(drive) != SDok)
return SDeio;
data = r->data;
while(count > 0){
max = 128;
n = count;
if(n > max)
n = max;
if((drive->edma->ctl&eEnEDMA) == 0)
goto tryagain;
srb = srbrw(cmd[0]==0x28 ? SRBread : SRBwrite, drive, data, n, lba);
ilock(drive);
startsrb(drive, srb);
iunlock(drive);
while(waserror())
;
sleep(srb, srbdone, srb);
poperror();
flag = srb->flag;
freesrb(srb);
if(flag == 0){
tryagain:
if(++try == 10){
print("%s: bad disk\n", drive->unit->name);
return SDeio;
}
dprint("%s: retry\n", drive->unit->name);
if(!waserror()){
tsleep(&up->sleep, return0, 0, 1000);
poperror();
}
goto retry;
}
if(flag & SFerror){
print("%s: i/o error\n", drive->unit->name);
return SDeio;
}
count -= n;
lba += n;
data += n*unit->secsize;
}
r->rlen = data - (uchar*)r->data;
return SDok;
}
SDifc sdmv50xxifc = {
"mv50xx",
mv50pnp,
nil,
mv50enable,
mv50disable,
mv50verify,
mv50online,
mv50rio,
mv50rctl,
mv50wctl,
scsibio,
nil,
mv50clear,
mv50rtopctl,
};