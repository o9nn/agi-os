#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "error.h"
#include "sd.h"
#define waserror()	(0)
#define poperror()
typedef struct QLock{ int r; } QLock;
typedef struct Rendez{ int r; } Rendez;
#define	intrenable(irq, f, c, tbdf, name)	setvec(VectorPIC+(irq), f, c);\
USED(tbdf);
#define K2BPA(va, tbdf)	PADDR(va)
#define BPA2K(pa, tbdf)	KADDR(pa)
extern SDifc sdmylexifc;
enum {
Rcontrol	= 0x00,
Rstatus		= 0x00,
Rcpr		= 0x01,
Rdatain		= 0x01,
Rinterrupt	= 0x02,
};
enum {
Rsbus		= 0x10,
Rint		= 0x20,
Rsoft		= 0x40,
Rhard		= 0x80,
};
enum {
Cmdinv		= 0x01,
Dirrdy		= 0x04,
Cprbsy		= 0x08,
Hardy		= 0x10,
Inreq		= 0x20,
Dfail		= 0x40,
Dact		= 0x80,
};
enum {
Cinitialise	= 0x01,
Cstart		= 0x02,
Cinquiry	= 0x04,
Ceombri		= 0x05,
Cinquire	= 0x0B,
Cextbios	= 0x28,
Cmbienable	= 0x29,
Ciem		= 0x81,
Ciesi		= 0x8D,
Cerrm		= 0x8F,
Cwide		= 0x96,
};
enum {
Imbl		= 0x01,
Mbor		= 0x02,
Cmdc		= 0x04,
Rsts		= 0x08,
Intv		= 0x80,
};
typedef struct {
uchar	code;
uchar	ccb[3];
} Mbox24;
typedef struct {
uchar	ccb[4];
uchar	btstat;
uchar	sdstat;
uchar	pad;
uchar	code;
} Mbox32;
enum {
Mbfree		= 0x00,
Mbostart	= 0x01,
Mboabort	= 0x02,
Mbiok		= 0x01,
Mbiabort	= 0x02,
Mbinx		= 0x03,
Mbierror	= 0x04,
};
typedef struct Ccb24 Ccb24;
typedef struct Ccb32 Ccb32;
typedef union Ccb Ccb;
typedef struct Ccb24 {
uchar	opcode;
uchar	datadir;
uchar	cdblen;
uchar	senselen;
uchar	datalen[3];
uchar	dataptr[3];
uchar	linkptr[3];
uchar	linkid;
uchar	btstat;
uchar	sdstat;
uchar	reserved[2];
uchar	cs[12+0xFF];
void*	data;
Rendez;
int	done;
Ccb*	ccb;
} Ccb24;
typedef struct Ccb32 {
uchar	opcode;
uchar	datadir;
uchar	cdblen;
uchar	senselen;
uchar	datalen[4];
uchar	dataptr[4];
uchar	reserved[2];
uchar	btstat;
uchar	sdstat;
uchar	targetid;
uchar	luntag;
uchar	cdb[12];
uchar	ccbctl;
uchar	linkid;
uchar	linkptr[4];
uchar	senseptr[4];
uchar	sense[0xFF];
Rendez;
int	done;
Ccb*	ccb;
} Ccb32;
typedef union Ccb {
Ccb24;
Ccb32;
} Ccb;
enum {
OInitiator	= 0x00,
Ordl		= 0x03,
};
enum {
CCBdatain	= 0x08,
CCBdataout	= 0x10,
};
enum {
Eok		= 0x00,
};
enum {
TagEnable	= 0x20,
SQTag		= 0x00,
HQTag		= 0x40,
OQTag		= 0x80,
};
enum {
NoDisc		= 0x08,
NoUnd		= 0x10,
NoData		= 0x20,
NoStat		= 0x40,
NoIntr		= 0x80,
};
typedef struct {
int	port;
int	id;
int	bus;
int	irq;
int	wide;
Pcidev*	pcidev;
SDev*	sdev;
int	spurious;
Lock	issuelock;
Lock	ccblock;
QLock	ccbq;
Rendez	ccbr;
Lock	mboxlock;
void*	mb;
int	mbox;
int	mbix;
Lock	cachelock;
Ccb*	ccb;
Ccb**	cache;
} Ctlr;
enum {
NMbox		= 8*8,
NCcb		= NMbox-1,
};
#define PADDR24(a, n)	((PADDR(a)+(n)) <= (1<<24))
static void
ccbfree(Ctlr* ctlr, Ccb* ccb)
{
lock(&ctlr->ccblock);
if(ctlr->bus == 24)
((Ccb24*)ccb)->ccb = ctlr->ccb;
else
((Ccb32*)ccb)->ccb = ctlr->ccb;
if(ctlr->ccb == nil)
wakeup(&ctlr->ccbr);
ctlr->ccb = ccb;
unlock(&ctlr->ccblock);
}
static int
ccbavailable(void* a)
{
return ((Ctlr*)a)->ccb != nil;
}
static Ccb*
ccballoc(Ctlr* ctlr)
{
Ccb *ccb;
for(;;){
lock(&ctlr->ccblock);
if((ccb = ctlr->ccb) != nil){
if(ctlr->bus == 24)
ctlr->ccb = ((Ccb24*)ccb)->ccb;
else
ctlr->ccb = ((Ccb32*)ccb)->ccb;
unlock(&ctlr->ccblock);
break;
}
unlock(&ctlr->ccblock);
qlock(&ctlr->ccbq);
if(waserror()){
qunlock(&ctlr->ccbq);
continue;
}
sleep(&ctlr->ccbr, ccbavailable, ctlr);
qunlock(&ctlr->ccbq);
poperror();
}
return ccb;
}
static int
done24(void* arg)
{
return ((Ccb24*)arg)->done;
}
static int
mylex24rio(SDreq* r)
{
ulong p;
Ctlr *ctlr;
Ccb24 *ccb;
Mbox24 *mb;
uchar *data, lun, *sense;
int d, n, btstat, sdstat, target;
ctlr = r->unit->dev->ctlr;
target = r->unit->subno;
lun = (r->cmd[1]>>5) & 0x07;
lock(&ctlr->cachelock);
if((ccb = ctlr->cache[target]) != nil){
ctlr->cache[target] = nil;
if(r->cmd[0] == 0x03
&& ccb->sdstat == SDcheck && lun == ((ccb->cs[1]>>5) & 0x07)){
unlock(&ctlr->cachelock);
if(r->dlen){
sense = &ccb->cs[ccb->cdblen];
n = 8+sense[7];
if(n > r->dlen)
n = r->dlen;
memmove(r->data, sense, n);
r->rlen = n;
}
ccbfree(ctlr, (Ccb*)ccb);
return SDok;
}
}
unlock(&ctlr->cachelock);
if(ccb == nil)
ccb = ccballoc(ctlr);
n = r->dlen;
if(n && !PADDR24(r->data, n)){
data = mallocz(n, 0);
if(data == nil || !PADDR24(data, n)){
if(data != nil){
free(data);
ccb->data = nil;
}
ccbfree(ctlr, (Ccb*)ccb);
return SDmalloc;
}
if(r->write)
memmove(data, r->data, n);
ccb->data = r->data;
}
else
data = r->data;
ccb->opcode = Ordl;
ccb->datadir = (target<<5)|lun;
if(n == 0)
ccb->datadir |= CCBdataout|CCBdatain;
else if(!r->write)
ccb->datadir |= CCBdatain;
else
ccb->datadir |= CCBdataout;
ccb->cdblen = r->clen;
ccb->senselen = 0xFF;
ccb->datalen[0] = n>>16;
ccb->datalen[1] = n>>8;
ccb->datalen[2] = n;
p = PADDR(data);
ccb->dataptr[0] = p>>16;
ccb->dataptr[1] = p>>8;
ccb->dataptr[2] = p;
ccb->linkptr[0] = ccb->linkptr[1] = ccb->linkptr[2] = 0;
ccb->linkid = 0;
ccb->btstat = ccb->sdstat = 0;
ccb->reserved[0] = ccb->reserved[1] = 0;
memmove(ccb->cs, r->cmd, r->clen);
lock(&ctlr->mboxlock);
mb = ctlr->mb;
mb += ctlr->mbox;
p = PADDR(ccb);
mb->ccb[0] = p>>16;
mb->ccb[1] = p>>8;
mb->ccb[2] = p;
mb->code = Mbostart;
ctlr->mbox++;
if(ctlr->mbox >= NMbox)
ctlr->mbox = 0;
ccb->done = 0;
outb(ctlr->port+Rcpr, Cstart);
unlock(&ctlr->mboxlock);
while(waserror())
;
tsleep(ccb, done24, ccb, 30*1000);
poperror();
if(!done24(ccb)){
print("%s: %d/%d: sd24rio timeout\n",
"sdmylex", target, r->lun);
if(ccb->data != nil){
free(data);
ccb->data = nil;
}
ccbfree(ctlr, (Ccb*)ccb);
return SDtimeout;
}
sdstat = ccb->sdstat;
btstat = ccb->btstat;
d = ccb->datalen[0]<<16;
d |= ccb->datalen[1]<<8;
d |= ccb->datalen[2];
if(ccb->cs[0] == 0x25 && sdstat == SDok)
d = 0;
n -= d;
r->rlen = n;
if(ccb->data != nil){
if(sdstat == SDok && btstat == 0 && !r->write)
memmove(ccb->data, data, n);
free(data);
ccb->data = nil;
}
if(sdstat == SDcheck){
if(r->flags & SDnosense){
lock(&ctlr->cachelock);
if(ctlr->cache[target])
ccbfree(ctlr, ctlr->cache[target]);
ctlr->cache[target] = (Ccb*)ccb;
unlock(&ctlr->cachelock);
return SDcheck;
}
sense = &ccb->cs[ccb->cdblen];
n = 8+sense[7];
if(n > sizeof(r->sense)-1)
n = sizeof(r->sense)-1;
memmove(r->sense, sense, n);
r->flags |= SDvalidsense;
}
ccbfree(ctlr, (Ccb*)ccb);
if(btstat){
if(btstat == 0x11)
return SDtimeout;
return SDeio;
}
return sdstat;
}
static void
mylex24interrupt(Ureg*, void* arg)
{
ulong pa;
Ctlr *ctlr;
Ccb24 *ccb;
Mbox24 *mb, *mbox;
int port, rinterrupt, rstatus;
ctlr = arg;
port = ctlr->port;
rinterrupt = inb(port+Rinterrupt);
rstatus = inb(port+Rstatus);
outb(port+Rcontrol, Rint);
if((rinterrupt & ~(Cmdc|Imbl)) != Intv && ctlr->spurious++)
print("%s: interrupt 0x%2.2ux\n",
"sdmylex", rinterrupt);
if((rinterrupt & Cmdc) && (rstatus & Cmdinv))
print("%s: command invalid\n", "sdmylex");
mb = ctlr->mb;
for(mbox = &mb[ctlr->mbix]; mbox->code; mbox = &mb[ctlr->mbix]){
pa = (mbox->ccb[0]<<16)|(mbox->ccb[1]<<8)|mbox->ccb[2];
ccb = BPA2K(pa, BUSUNKNOWN);
mbox->code = 0;
ccb->done = 1;
wakeup(ccb);
ctlr->mbix++;
if(ctlr->mbix >= NMbox+NMbox)
ctlr->mbix = NMbox;
}
}
static int
done32(void* arg)
{
return ((Ccb32*)arg)->done;
}
static int
mylex32rio(SDreq* r)
{
ulong p;
uchar lun;
Ctlr *ctlr;
Ccb32 *ccb;
Mbox32 *mb;
int d, n, btstat, sdstat, target;
ctlr = r->unit->dev->ctlr;
target = r->unit->subno;
lun = (r->cmd[1]>>5) & 0x07;
lock(&ctlr->cachelock);
if((ccb = ctlr->cache[target]) != nil){
ctlr->cache[target] = nil;
if(r->cmd[0] == 0x03
&& ccb->sdstat == SDcheck && lun == (ccb->luntag & 0x07)){
unlock(&ctlr->cachelock);
if(r->dlen){
n = 8+ccb->sense[7];
if(n > r->dlen)
n = r->dlen;
memmove(r->data, ccb->sense, n);
r->rlen = n;
}
ccbfree(ctlr, (Ccb*)ccb);
return SDok;
}
}
unlock(&ctlr->cachelock);
if(ccb == nil)
ccb = ccballoc(ctlr);
ccb->opcode = Ordl;
n = r->dlen;
if(n == 0)
ccb->datadir = CCBdataout|CCBdatain;
else if(!r->write)
ccb->datadir = CCBdatain;
else
ccb->datadir = CCBdataout;
ccb->cdblen = r->clen;
ccb->datalen[0] = n;
ccb->datalen[1] = n>>8;
ccb->datalen[2] = n>>16;
ccb->datalen[3] = n>>24;
p = PADDR(r->data);
ccb->dataptr[0] = p;
ccb->dataptr[1] = p>>8;
ccb->dataptr[2] = p>>16;
ccb->dataptr[3] = p>>24;
ccb->targetid = target;
ccb->luntag = lun;
if(r->unit->inquiry[7] & 0x02)
ccb->luntag |= SQTag|TagEnable;
memmove(ccb->cdb, r->cmd, r->clen);
ccb->btstat = ccb->sdstat = 0;
ccb->ccbctl = 0;
lock(&ctlr->mboxlock);
mb = ctlr->mb;
mb += ctlr->mbox;
p = PADDR(ccb);
mb->ccb[0] = p;
mb->ccb[1] = p>>8;
mb->ccb[2] = p>>16;
mb->ccb[3] = p>>24;
mb->code = Mbostart;
ctlr->mbox++;
if(ctlr->mbox >= NMbox)
ctlr->mbox = 0;
ccb->done = 0;
outb(ctlr->port+Rcpr, Cstart);
unlock(&ctlr->mboxlock);
while(waserror())
;
tsleep(ccb, done32, ccb, 30*1000);
poperror();
if(!done32(ccb)){
print("%s: %d/%d: sd32rio timeout\n",
"sdmylex", target, r->lun);
ccbfree(ctlr, (Ccb*)ccb);
return SDtimeout;
}
sdstat = ccb->sdstat;
btstat = ccb->btstat;
d = ccb->datalen[0];
d |= (ccb->datalen[1]<<8);
d |= (ccb->datalen[2]<<16);
d |= (ccb->datalen[3]<<24);
if(ccb->cdb[0] == 0x25 && sdstat == SDok)
d = 0;
n -= d;
r->rlen = n;
if(sdstat == SDcheck){
if(r->flags & SDnosense){
lock(&ctlr->cachelock);
if(ctlr->cache[target])
ccbfree(ctlr, ctlr->cache[target]);
ctlr->cache[target] = (Ccb*)ccb;
unlock(&ctlr->cachelock);
return SDcheck;
}
n = 8+ccb->sense[7];
if(n > sizeof(r->sense)-1)
n = sizeof(r->sense)-1;
memmove(r->sense, ccb->sense, n);
r->flags |= SDvalidsense;
}
ccbfree(ctlr, (Ccb*)ccb);
if(btstat){
if(btstat == 0x11)
return SDtimeout;
return SDeio;
}
return sdstat;
}
static void
mylex32interrupt(Ureg*, void* arg)
{
ulong pa;
Ctlr *ctlr;
Ccb32 *ccb;
Mbox32 *mb, *mbox;
int port, rinterrupt, rstatus;
ctlr = arg;
port = ctlr->port;
rinterrupt = inb(port+Rinterrupt);
rstatus = inb(port+Rstatus);
outb(port+Rcontrol, Rint);
if((rinterrupt & ~(Cmdc|Imbl)) != Intv && ctlr->spurious++)
print("%s: interrupt 0x%2.2ux\n",
"sdmylex", rinterrupt);
if((rinterrupt & Cmdc) && (rstatus & Cmdinv))
print("%s: command invalid\n", "sdmylex");
mb = ctlr->mb;
for(mbox = &mb[ctlr->mbix]; mbox->code; mbox = &mb[ctlr->mbix]){
pa = (mbox->ccb[3]<<24)
|(mbox->ccb[2]<<16)
|(mbox->ccb[1]<<8)
|mbox->ccb[0];
if(ctlr->pcidev)
ccb = BPA2K(pa, ctlr->pcidev->tbdf);
else
ccb = BPA2K(pa, BUSUNKNOWN);
mbox->code = 0;
ccb->done = 1;
wakeup(ccb);
ctlr->mbix++;
if(ctlr->mbix >= NMbox+NMbox)
ctlr->mbix = NMbox;
}
}
static int
mylexrio(SDreq* r)
{
int subno;
Ctlr *ctlr;
subno = r->unit->subno;
ctlr = r->unit->dev->ctlr;
if(subno == ctlr->id || (!ctlr->wide && subno >= 8))
r->status = SDtimeout;
else if(ctlr->bus == 24)
r->status = mylex24rio(r);
else
r->status = mylex32rio(r);
return r->status;
}
static void
issueio(int port, uchar* cmd, int cmdlen, uchar* data, int datalen)
{
int len;
if(cmd[0] != Cstart && cmd[0] != Ceombri){
while(!(inb(port+Rstatus) & Hardy))
;
}
outb(port+Rcpr, cmd[0]);
len = 1;
while(len < cmdlen){
if(!(inb(port+Rstatus) & Cprbsy)){
outb(port+Rcpr, cmd[len]);
len++;
}
if(inb(port+Rinterrupt) & Cmdc)
return;
}
if(datalen){
len = 0;
while(len < datalen){
if(inb(port+Rstatus) & Dirrdy){
data[len] = inb(port+Rdatain);
len++;
}
if(inb(port+Rinterrupt) & Cmdc)
return;
}
}
}
static int
issue(Ctlr* ctlr, uchar* cmd, int cmdlen, uchar* data, int datalen)
{
int port;
uchar rinterrupt, rstatus;
static Lock mylexissuelock;
port = ctlr->port;
ilock(&ctlr->issuelock);
issueio(port, cmd, cmdlen, data, datalen);
while(!((rinterrupt = inb(port+Rinterrupt)) & Cmdc))
;
rstatus = inb(port+Rstatus);
outb(port+Rcontrol, Rint);
iunlock(&ctlr->issuelock);
if((rinterrupt & Cmdc) && (rstatus & Cmdinv))
return 0;
return 1;
}
static SDev*
mylexprobe(int port, int irq)
{
SDev *sdev;
Ctlr *ctlr;
uchar cmd[6], data[256];
int clen, dlen, timeo;
if(ioalloc(port, 0x3, 0, "mylex") < 0)
return nil;
ctlr = nil;
if(getconf("*noscsireset") != nil)
outb(port+Rcontrol, Rhard);
else
outb(port+Rcontrol, Rhard|Rsbus);
for(timeo = 0; timeo < 100; timeo++){
if(inb(port+Rstatus) == (Inreq|Hardy))
break;
delay(100);
}
if(inb(port+Rstatus) != (Inreq|Hardy)){
buggery:
if(ctlr != nil)
free(ctlr);
iofree(port);
return nil;
}
if((ctlr = malloc(sizeof(Ctlr))) == nil)
goto buggery;
ctlr->port = port;
ctlr->irq = irq;
ctlr->bus = 24;
ctlr->wide = 0;
cmd[0] = Ciesi;
cmd[1] = 14;
clen = 2;
dlen = 256;
if(issue(ctlr, cmd, clen, data, dlen)){
if(data[0] == 'E')
ctlr->bus = 32;
ctlr->wide = data[0x0D] & 0x01;
}
else{
outb(port+Rcontrol, Rhard);
for(timeo = 0; timeo < 100; timeo++){
if(inb(port+Rstatus) == (Inreq|Hardy))
break;
delay(100);
}
if(inb(port+Rstatus) != (Inreq|Hardy))
goto buggery;
}
cmd[0] = Cinquiry;
clen = 1;
dlen = 4;
if(issue(ctlr, cmd, clen, data, dlen) == 0)
goto buggery;
if(data[0] >= 0x43){
cmd[0] = Cextbios;
clen = 1;
dlen = 2;
if(issue(ctlr, cmd, clen, data, dlen) == 0)
goto buggery;
if(data[1]){
cmd[0] = Cmbienable;
cmd[1] = 0;
cmd[2] = data[1];
clen = 3;
if(issue(ctlr, cmd, clen, 0, 0) == 0)
goto buggery;
}
}
cmd[0] = Cinquire;
clen = 1;
dlen = 3;
if(issue(ctlr, cmd, clen, data, dlen) == 0)
goto buggery;
ctlr->id = data[2] & 0x07;
if(ctlr->irq < 0){
switch(data[0]){
case 0x80:
outb(0xD6, 0xC3);
outb(0xD4, 0x03);
break;
case 0x40:
outb(0xD6, 0xC2);
outb(0xD4, 0x02);
break;
case 0x20:
outb(0xD6, 0xC1);
outb(0xD4, 0x01);
break;
case 0x01:
outb(0x0B, 0xC0);
outb(0x0A, 0x00);
break;
default:
if(ctlr->bus == 24)
goto buggery;
break;
}
switch(data[1]){
case 0x40:
ctlr->irq = 15;
break;
case 0x20:
ctlr->irq = 14;
break;
case 0x08:
ctlr->irq = 12;
break;
case 0x04:
ctlr->irq = 11;
break;
case 0x02:
ctlr->irq = 10;
break;
case 0x01:
ctlr->irq = 9;
break;
default:
goto buggery;
}
}
if((sdev = malloc(sizeof(SDev))) == nil)
goto buggery;
sdev->ifc = &sdmylexifc;
sdev->ctlr = ctlr;
ctlr->sdev = sdev;
if(!ctlr->wide)
sdev->nunit = 8;
else
sdev->nunit = 16;
return sdev;
}
static int mylexport[8] = {
0x330, 0x334, 0x230, 0x234, 0x130, 0x134, 0x000, 0x000,
};
static SDev*
mylexpnp(void)
{
Pcidev *p;
Ctlr *ctlr;
ISAConf isa;
int cfg, ctlrno, i, x;
SDev *sdev, *head, *tail;
p = nil;
head = tail = nil;
while(p = pcimatch(p, 0x104B, 0)){
if((sdev = mylexprobe(p->mem[0].bar & ~0x01, p->intl)) == nil)
continue;
ctlr = sdev->ctlr;
ctlr->pcidev = p;
if(head != nil)
tail->next = sdev;
else
head = sdev;
tail = sdev;
}
if(strncmp(KADDR(0xFFFD9), "EISA", 4) == 0){
for(cfg = 0x1000; cfg < MaxEISA*0x1000; cfg += 0x1000){
x = 0;
for(i = 0; i < 4; i++)
x |= inb(cfg+CfgEISA+i)<<(i*8);
if(x != 0x0142B30A && x != 0x0242B30A)
continue;
x = inb(cfg+0xC8C);
if((sdev = mylexprobe(mylexport[x & 0x07], -1)) == nil)
continue;
if(head != nil)
tail->next = sdev;
else
head = sdev;
tail = sdev;
}
}
for(ctlrno = 0; ctlrno < 4; ctlrno++){
memset(&isa, 0, sizeof(isa));
if(!isaconfig("scsi", ctlrno, &isa))
continue;
if(strcmp(isa.type, "aha1542"))
continue;
if((sdev = mylexprobe(isa.port, -1)) == nil)
continue;
if(head != nil)
tail->next = sdev;
else
head = sdev;
tail = sdev;
}
return head;
}
static SDev*
mylexid(SDev* sdev)
{
return scsiid(sdev, &sdmylexifc);
}
static int
mylex24enable(Ctlr* ctlr)
{
ulong p;
Ccb24 *ccb, *ccbp;
uchar cmd[6], *v;
int len;
len = (sizeof(Mbox24)*NMbox*2)+(sizeof(Ccb24)*NCcb);
v = xspanalloc(len, 32, 0);
if(!PADDR24(ctlr, sizeof(Ctlr)) || !PADDR24(v, len))
return 0;
ctlr->mb = v;
v += sizeof(Mbox24)*NMbox*2;
ccb = (Ccb24*)v;
for(ccbp = ccb; ccbp < &ccb[NCcb]; ccbp++){
ccbp->ccb = ctlr->ccb;
ctlr->ccb = (Ccb*)ccbp;
}
ctlr->mbix = NMbox;
cmd[0] = Cinitialise;
cmd[1] = NMbox;
p = K2BPA(ctlr->mb, BUSUNKNOWN);
cmd[2] = p>>16;
cmd[3] = p>>8;
cmd[4] = p;
return issue(ctlr, cmd, 5, 0, 0);
}
static int
mylex32enable(Ctlr* ctlr)
{
ulong p;
Ccb32 *ccb, *ccbp;
uchar cmd[6], *v;
v = xspanalloc((sizeof(Mbox32)*NMbox*2)+(sizeof(Ccb32)*NCcb), 32, 0);
ctlr->mb = v;
v += sizeof(Mbox32)*NMbox*2;
ccb = (Ccb32*)v;
for(ccbp = ccb; ccbp < &ccb[NCcb]; ccbp++){
ccbp->senselen = sizeof(ccbp->sense);
p = PADDR(ccbp->sense);
ccbp->senseptr[0] = p;
ccbp->senseptr[1] = p>>8;
ccbp->senseptr[2] = p>>16;
ccbp->senseptr[3] = p>>24;
ccbp->ccb = ctlr->ccb;
ctlr->ccb = (Ccb*)ccbp;
}
if(ctlr->wide){
cmd[0] = Cwide;
cmd[1] = 1;
if(!issue(ctlr, cmd, 2, 0, 0))
ctlr->wide = 0;
}
ctlr->mbix = NMbox;
cmd[0] = Ciem;
cmd[1] = NMbox;
if(ctlr->pcidev)
p = K2BPA(ctlr->mb, ctlr->tbdf);
else
p = K2BPA(ctlr->mb, BUSUNKNOWN);
cmd[2] = p;
cmd[3] = p>>8;
cmd[4] = p>>16;
cmd[5] = p>>24;
return issue(ctlr, cmd, 6, 0, 0);
}
static int
mylexenable(SDev* sdev)
{
int tbdf;
Ctlr *ctlr;
void (*interrupt)(Ureg*, void*);
char name[NAMELEN];
ctlr = sdev->ctlr;
if(ctlr->cache == nil){
if((ctlr->cache = malloc(sdev->nunit*sizeof(Ccb*))) == nil)
return 0;
}
tbdf = BUSUNKNOWN;
if(ctlr->bus == 32){
if(ctlr->pcidev){
tbdf = ctlr->pcidev->tbdf;
pcisetbme(ctlr->pcidev);
}
if(!mylex32enable(ctlr))
return 0;
interrupt = mylex32interrupt;
}
else if(mylex24enable(ctlr))
interrupt = mylex24interrupt;
else
return 0;
snprint(name, NAMELEN, "sd%c (%s)", sdev->idno, sdev->ifc->name);
intrenable(ctlr->irq, interrupt, ctlr, tbdf, name);
return 1;
}
static int
mylexdisable(SDev* sdev)
{
Ctlr *ctlr;
int port, timeo;
ctlr = sdev->ctlr;
port = ctlr->port;
if(getconf("*noscsireset") != nil)
outb(port+Rcontrol, Rhard);
else
outb(port+Rcontrol, Rhard|Rsbus);
for(timeo = 0; timeo < 100; timeo++){
if(inb(port+Rstatus) == (Inreq|Hardy))
break;
delay(100);
}
if(inb(port+Rstatus) != (Inreq|Hardy))
return 0;
return 1;
}
SDifc sdmylexifc = {
"mylex",
mylexpnp,
nil,
mylexid,
mylexenable,
mylexdisable,
scsiverify,
scsionline,
mylexrio,
nil,
nil,
scsibio,
};