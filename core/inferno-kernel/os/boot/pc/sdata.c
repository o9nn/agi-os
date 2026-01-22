#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "error.h"
#include "sd.h"
extern SDifc sdataifc;
enum {
DbgCONFIG	= 0x0001,
DbgIDENTIFY	= 0x0002,
DbgSTATE	= 0x0004,
DbgPROBE	= 0x0008,
DbgDEBUG	= 0x0080,
DbgINL		= 0x0100,
Dbg48BIT	= 0x0200,
DbgBsy		= 0x0400,
};
#define DEBUG		(DbgDEBUG|DbgCONFIG)
enum {
Data		= 0,
Error		= 1,
Features	= 1,
Count		= 2,
Ir		= 2,
Sector		= 3,
Lbalo		= 3,
Cyllo		= 4,
Bytelo		= 4,
Lbamid		= 4,
Cylhi		= 5,
Bytehi		= 5,
Lbahi		= 5,
Dh		= 6,
Status		= 7,
Command		= 7,
As		= 2,
Dc		= 2,
};
enum {
Med		= 0x01,
Ili		= 0x01,
Nm		= 0x02,
Eom		= 0x02,
Abrt		= 0x04,
Mcr		= 0x08,
Idnf		= 0x10,
Mc		= 0x20,
Unc		= 0x40,
Wp		= 0x40,
Icrc		= 0x80,
};
enum {
Dma		= 0x01,
Ovl		= 0x02,
};
enum {
Cd		= 0x01,
Io		= 0x02,
Rel		= 0x04,
};
enum {
Dev0		= 0xA0,
Dev1		= 0xB0,
Lba		= 0x40,
};
enum {
Err		= 0x01,
Chk		= 0x01,
Drq		= 0x08,
Dsc		= 0x10,
Serv		= 0x10,
Df		= 0x20,
Dmrd		= 0x20,
Drdy		= 0x40,
Bsy		= 0x80,
};
enum {
Cnop		= 0x00,
Cdr		= 0x08,
Crs		= 0x20,
Crs48		= 0x24,
Crd48		= 0x25,
Crdq48		= 0x26,
Crsm48		= 0x29,
Cws		= 0x30,
Cws48		= 0x34,
Cwd48		= 0x35,
Cwdq48		= 0x36,
Cwsm48		= 0x39,
Cedd		= 0x90,
Cpkt		= 0xA0,
Cidpkt		= 0xA1,
Crsm		= 0xC4,
Cwsm		= 0xC5,
Csm		= 0xC6,
Crdq		= 0xC7,
Crd		= 0xC8,
Cwd		= 0xCA,
Cwdq		= 0xCC,
Cstandby	= 0xE2,
Cid		= 0xEC,
Csf		= 0xEF,
};
enum {
Nien		= 0x02,
Srst		= 0x04,
};
enum {
Bmiba		= 0x20,
Idetim		= 0x40,
Sidetim		= 0x44,
Udmactl		= 0x48,
Udmatim		= 0x4A,
};
enum {
Bmicx		= 0,
Bmisx		= 2,
Bmidtpx		= 4,
};
enum {
Ssbm		= 0x01,
Rwcon		= 0x08,
};
enum {
Bmidea		= 0x01,
Idedmae		= 0x02,
Ideints		= 0x04,
Dma0cap		= 0x20,
Dma1cap		= 0x40,
};
enum {
PrdEOT		= 0x80000000,
};
enum {
Iconfig		= 0,
Ilcyl		= 1,
Ilhead		= 3,
Ilsec		= 6,
Iserial		= 10,
Ifirmware	= 23,
Imodel		= 27,
Imaxrwm		= 47,
Icapabilities	= 49,
Istandby	= 50,
Ipiomode	= 51,
Ivalid		= 53,
Iccyl		= 54,
Ichead		= 55,
Icsec		= 56,
Iccap		= 57,
Irwm		= 59,
Ilba		= 60,
Imwdma		= 63,
Iapiomode	= 64,
Iminmwdma	= 65,
Irecmwdma	= 66,
Iminpio		= 67,
Iminiordy	= 68,
Ipcktbr		= 71,
Iserbsy		= 72,
Iqdepth		= 75,
Imajor		= 80,
Iminor		= 81,
Icsfs		= 82,
Icsfe		= 85,
Iudma		= 88,
Ierase		= 89,
Ieerase		= 90,
Ipower		= 91,
Ilba48		= 100,
Irmsn		= 127,
Isecstat	= 128,
};
typedef struct Ctlr Ctlr;
typedef struct Drive Drive;
typedef struct Prd {
ulong	pa;
int	count;
} Prd;
enum {
Nprd		= SDmaxio/(64*1024)+2,
};
typedef struct Ctlr {
int	cmdport;
int	ctlport;
int	irq;
int	tbdf;
Pcidev*	pcidev;
void	(*ienable)(Ctlr*);
SDev*	sdev;
Drive*	drive[2];
Prd*	prdt;
Drive*	curdrive;
int	command;
int	done;
Lock;
} Ctlr;
typedef struct Drive {
Ctlr*	ctlr;
int	dev;
ushort	info[256];
int	c;
int	h;
int	s;
vlong	sectors;
int	secsize;
int	pkt;
uchar	pktcmd[16];
uchar	sense[18];
uchar	inquiry[48];
int	command;
int	write;
uchar*	data;
int	dlen;
uchar*	limit;
int	count;
int	block;
int	status;
int	error;
int	flags;
} Drive;
enum {
Lba48		= 0x1,
Lba48always	= 0x2,
};
static void
pc87415ienable(Ctlr* ctlr)
{
Pcidev *p;
int x;
p = ctlr->pcidev;
if(p == nil)
return;
x = pcicfgr32(p, 0x40);
if(ctlr->cmdport == p->mem[0].bar)
x &= ~0x00000100;
else
x &= ~0x00000200;
pcicfgw32(p, 0x40, x);
}
static int
atadebug(int cmdport, int ctlport, char* fmt, ...)
{
int i, n;
va_list arg;
char buf[PRINTSIZE];
if(!(DEBUG & DbgPROBE)){
USED(cmdport, ctlport, fmt);
return 0;
}
va_start(arg, fmt);
n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
va_end(arg);
if(cmdport){
if(buf[n-1] == '\n')
n--;
n += snprint(buf+n, PRINTSIZE-n, " ataregs 0x%uX:",
cmdport);
for(i = Features; i < Command; i++)
n += snprint(buf+n, PRINTSIZE-n, " 0x%2.2uX",
inb(cmdport+i));
if(ctlport)
n += snprint(buf+n, PRINTSIZE-n, " 0x%2.2uX",
inb(ctlport+As));
n += snprint(buf+n, PRINTSIZE-n, "\n");
}
putstrn(buf, n);
return n;
}
static int
ataready(int cmdport, int ctlport, int dev, int reset, int ready, int micro)
{
int as;
atadebug(cmdport, ctlport, "ataready: dev %uX reset %uX ready %uX",
dev, reset, ready);
for(;;){
as = inb(ctlport+As);
if(as & reset){
;
}
else if(dev){
outb(cmdport+Dh, dev);
dev = 0;
}
else if(ready == 0 || (as & ready)){
atadebug(0, 0, "ataready: %d 0x%2.2uX\n", micro, as);
return as;
}
if(micro-- <= 0){
atadebug(0, 0, "ataready: %d 0x%2.2uX\n", micro, as);
break;
}
microdelay(1);
}
atadebug(cmdport, ctlport, "ataready: timeout");
return -1;
}
static int
atacsfenabled(Drive* drive, vlong csf)
{
int cmdset, i, x;
for(i = 0; i < 3; i++){
x = (csf>>(16*i)) & 0xFFFF;
if(x == 0)
continue;
cmdset = drive->info[Icsfe+i];
if(cmdset == 0 || cmdset == 0xFFFF)
return 0;
return cmdset & x;
}
return 0;
}
static int
ataidentify(int cmdport, int ctlport, int dev, int pkt, void* info)
{
int as, command, drdy;
if(pkt){
command = Cidpkt;
drdy = 0;
}
else{
command = Cid;
drdy = Drdy;
}
as = ataready(cmdport, ctlport, dev, Bsy|Drq, drdy, 103*1000);
if(as < 0)
return as;
outb(cmdport+Command, command);
microdelay(1);
as = ataready(cmdport, ctlport, 0, Bsy, Drq|Err, 400*1000);
if(as < 0)
return -1;
if(as & Err)
return as;
memset(info, 0, 512);
inss(cmdport+Data, info, 256);
inb(cmdport+Status);
if(DEBUG & DbgIDENTIFY){
int i;
ushort *sp;
sp = (ushort*)info;
for(i = 0; i < 256; i++){
if(i && (i%16) == 0)
print("\n");
print(" %4.4uX ", *sp);
sp++;
}
print("\n");
}
return 0;
}
static Drive*
atadrive(int cmdport, int ctlport, int dev)
{
Drive *drive;
int as, i, pkt;
uchar buf[512], *p;
ushort iconfig, *sp;
atadebug(0, 0, "identify: port 0x%uX dev 0x%2.2uX\n", cmdport, dev);
pkt = 1;
retry:
as = ataidentify(cmdport, ctlport, dev, pkt, buf);
if(as < 0)
return nil;
if(as & Err){
if(pkt == 0)
return nil;
pkt = 0;
goto retry;
}
if((drive = malloc(sizeof(Drive))) == nil)
return nil;
drive->dev = dev;
memmove(drive->info, buf, sizeof(drive->info));
drive->sense[0] = 0x70;
drive->sense[7] = sizeof(drive->sense)-7;
drive->inquiry[2] = 2;
drive->inquiry[3] = 2;
drive->inquiry[4] = sizeof(drive->inquiry)-4;
p = &drive->inquiry[8];
sp = &drive->info[Imodel];
for(i = 0; i < 20; i++){
*p++ = *sp>>8;
*p++ = *sp++;
}
drive->secsize = 512;
iconfig = drive->info[Iconfig];
if(iconfig != 0x848A && (iconfig & 0xC000) == 0x8000){
if(iconfig & 0x01)
drive->pkt = 16;
else
drive->pkt = 12;
}
else{
if(drive->info[Ivalid] & 0x0001){
drive->c = drive->info[Iccyl];
drive->h = drive->info[Ichead];
drive->s = drive->info[Icsec];
}
else{
drive->c = drive->info[Ilcyl];
drive->h = drive->info[Ilhead];
drive->s = drive->info[Ilsec];
}
if(drive->info[Icapabilities] & 0x0200){
if(drive->info[Icsfs+1] & 0x0400){
drive->sectors = drive->info[Ilba48]
|(drive->info[Ilba48+1]<<16)
|((vlong)drive->info[Ilba48+2]<<32);
drive->flags |= Lba48;
}
else{
drive->sectors = (drive->info[Ilba+1]<<16)
|drive->info[Ilba];
}
drive->dev |= Lba;
}
else
drive->sectors = drive->c*drive->h*drive->s;
}
if(DEBUG & DbgCONFIG){
print("dev %2.2uX port %uX config %4.4uX capabilities %4.4uX",
dev, cmdport, iconfig, drive->info[Icapabilities]);
print(" mwdma %4.4uX", drive->info[Imwdma]);
if(drive->info[Ivalid] & 0x04)
print(" udma %4.4uX", drive->info[Iudma]);
if(drive->flags&Lba48)
print("\tLLBA sectors %lld", drive->sectors);
print("\n");
}
return drive;
}
static void
atasrst(int ctlport)
{
microdelay(5);
outb(ctlport+Dc, Srst);
microdelay(5);
outb(ctlport+Dc, 0);
microdelay(2*1000);
}
static SDev*
ataprobe(int cmdport, int ctlport, int irq)
{
Ctlr* ctlr;
SDev *sdev;
Drive *drive;
int dev, error, rhi, rlo;
dev = Dev0;
if(inb(ctlport+As) & Bsy){
outb(cmdport+Dh, dev);
microdelay(1);
trydev1:
atadebug(cmdport, ctlport, "ataprobe bsy");
outb(cmdport+Cyllo, 0xAA);
outb(cmdport+Cylhi, 0x55);
outb(cmdport+Sector, 0xFF);
rlo = inb(cmdport+Cyllo);
rhi = inb(cmdport+Cylhi);
if(rlo != 0xAA && (rlo == 0xFF || rhi != 0x55)){
if(dev == Dev1){
release:
return nil;
}
dev = Dev1;
if(ataready(cmdport, ctlport, dev, Bsy, 0, 20*1000) < 0)
goto trydev1;
}
}
outb(ctlport+Dc, Nien);
tryedd1:
if(ataready(cmdport, ctlport, dev, Bsy|Drq, 0, 105*1000) < 0){
atasrst(ctlport);
if(ataready(cmdport, ctlport, dev, Bsy|Drq, 0, 106*1000) < 0)
goto release;
}
outb(cmdport+Command, Cedd);
delay(2);
if(ataready(cmdport, ctlport, dev, Bsy|Drq, 0, 6*1000*1000) < 0)
goto release;
error = inb(cmdport+Error);
atadebug(cmdport, ctlport, "ataprobe: dev %uX", dev);
if((error & ~0x80) != 0x01){
if(dev == Dev1)
goto release;
dev = Dev1;
goto tryedd1;
}
if((drive = atadrive(cmdport, ctlport, dev)) == nil)
goto release;
if((ctlr = malloc(sizeof(Ctlr))) == nil){
free(drive);
goto release;
}
if((sdev = malloc(sizeof(SDev))) == nil){
free(ctlr);
free(drive);
goto release;
}
drive->ctlr = ctlr;
if(dev == Dev0){
ctlr->drive[0] = drive;
if(!(error & 0x80)){
drive = atadrive(cmdport, ctlport, Dev1);
if(drive != nil){
drive->ctlr = ctlr;
ctlr->drive[1] = drive;
}
else{
outb(cmdport+Dh, Dev0);
microdelay(1);
}
}
}
else
ctlr->drive[1] = drive;
ctlr->cmdport = cmdport;
ctlr->ctlport = ctlport;
ctlr->irq = irq;
ctlr->tbdf = BUSUNKNOWN;
ctlr->command = Cedd;
sdev->ifc = &sdataifc;
sdev->ctlr = ctlr;
sdev->nunit = 2;
ctlr->sdev = sdev;
return sdev;
}
static int
atasetsense(Drive* drive, int status, int key, int asc, int ascq)
{
drive->sense[2] = key;
drive->sense[12] = asc;
drive->sense[13] = ascq;
return status;
}
static int
atamodesense(Drive* drive, uchar* cmd)
{
int len;
if((cmd[2] & 0x3F) != 0 && (cmd[2] & 0x3F) != 0x3F)
return atasetsense(drive, SDcheck, 0x05, 0x24, 0);
len = (cmd[7]<<8)|cmd[8];
if(len == 0)
return SDok;
if(len < 8+sizeof(drive->info))
return atasetsense(drive, SDcheck, 0x05, 0x1A, 0);
if(drive->data == nil || drive->dlen < len)
return atasetsense(drive, SDcheck, 0x05, 0x20, 1);
memset(drive->data, 0, 8);
drive->data[0] = sizeof(drive->info)>>8;
drive->data[1] = sizeof(drive->info);
memmove(drive->data+8, drive->info, sizeof(drive->info));
drive->data += 8+sizeof(drive->info);
return SDok;
}
static void
atanop(Drive* drive, int subcommand)
{
Ctlr* ctlr;
int as, cmdport, ctlport, timeo;
ctlr = drive->ctlr;
cmdport = ctlr->cmdport;
outb(cmdport+Features, subcommand);
outb(cmdport+Dh, drive->dev);
ctlr->command = Cnop;
outb(cmdport+Command, Cnop);
microdelay(1);
ctlport = ctlr->ctlport;
for(timeo = 0; timeo < 1000; timeo++){
as = inb(ctlport+As);
if(!(as & Bsy))
break;
microdelay(1);
}
drive->error |= Abrt;
}
static void
ataabort(Drive* drive, int dolock)
{
if(dolock)
ilock(drive->ctlr);
if(atacsfenabled(drive, 0x0000000000004000LL))
atanop(drive, 0);
else{
atasrst(drive->ctlr->ctlport);
drive->error |= Abrt;
}
if(dolock)
iunlock(drive->ctlr);
}
static int
atapktiodone(void* arg)
{
return ((Ctlr*)arg)->done;
}
static void
atapktinterrupt(Drive* drive)
{
Ctlr* ctlr;
int cmdport, len;
ctlr = drive->ctlr;
cmdport = ctlr->cmdport;
switch(inb(cmdport+Ir) & (Io|Cd)){
case Cd:
outss(cmdport+Data, drive->pktcmd, drive->pkt/2);
break;
case 0:
len = (inb(cmdport+Bytehi)<<8)|inb(cmdport+Bytelo);
if(drive->data+len > drive->limit){
atanop(drive, 0);
break;
}
outss(cmdport+Data, drive->data, len/2);
drive->data += len;
break;
case Io:
len = (inb(cmdport+Bytehi)<<8)|inb(cmdport+Bytelo);
if(drive->data+len > drive->limit){
atanop(drive, 0);
break;
}
inss(cmdport+Data, drive->data, len/2);
drive->data += len;
break;
case Io|Cd:
ctlr->done = 1;
break;
}
}
static int
atapktio(Drive* drive, uchar* cmd, int clen)
{
Ctlr *ctlr;
int as, cmdport, ctlport, len, r;
if(cmd[0] == 0x5A && (cmd[2] & 0x3F) == 0)
return atamodesense(drive, cmd);
r = SDok;
drive->command = Cpkt;
memmove(drive->pktcmd, cmd, clen);
memset(drive->pktcmd+clen, 0, drive->pkt-clen);
drive->limit = drive->data+drive->dlen;
ctlr = drive->ctlr;
cmdport = ctlr->cmdport;
ctlport = ctlr->ctlport;
qlock(ctlr);
as = ataready(cmdport, ctlport, drive->dev, Bsy|Drq, 0, 107*1000);
if(as < 0 || (as&Chk)){
qunlock(ctlr);
return -1;
}
ilock(ctlr);
outb(cmdport+Features, 0);
outb(cmdport+Count, 0);
outb(cmdport+Sector, 0);
len = 16*drive->secsize;
outb(cmdport+Bytelo, len);
outb(cmdport+Bytehi, len>>8);
outb(cmdport+Dh, drive->dev);
ctlr->done = 0;
ctlr->curdrive = drive;
ctlr->command = Cpkt;
outb(cmdport+Command, Cpkt);
if((drive->info[Iconfig] & 0x0060) != 0x0020){
microdelay(1);
as = ataready(cmdport, ctlport, 0, Bsy, Drq|Chk, 4*1000);
if(as < 0 || (as & (Bsy|Chk))){
drive->status = as<0 ? 0 : as;
ctlr->curdrive = nil;
ctlr->done = 1;
r = SDtimeout;
}else
atapktinterrupt(drive);
}
iunlock(ctlr);
sleep(ctlr, atapktiodone, ctlr);
qunlock(ctlr);
if(drive->status & Chk)
r = SDcheck;
return r;
}
static int
atageniodone(void* arg)
{
return ((Ctlr*)arg)->done;
}
static uchar cmd48[256] = {
[Crs]	Crs48,
[Crd]	Crd48,
[Crdq]	Crdq48,
[Crsm]	Crsm48,
[Cws]	Cws48,
[Cwd]	Cwd48,
[Cwdq]	Cwdq48,
[Cwsm]	Cwsm48,
};
static int
atageniostart(Drive* drive, vlong lba)
{
Ctlr *ctlr;
uchar cmd;
int as, c, cmdport, ctlport, h, len, s, use48;
use48 = 0;
if((drive->flags&Lba48always) || (lba>>28) || drive->count > 256){
if(!(drive->flags & Lba48))
return -1;
use48 = 1;
c = h = s = 0;
}else if(drive->dev & Lba){
c = (lba>>8) & 0xFFFF;
h = (lba>>24) & 0x0F;
s = lba & 0xFF;
}
else{
c = lba/(drive->s*drive->h);
h = ((lba/drive->s) % drive->h);
s = (lba % drive->s) + 1;
}
ctlr = drive->ctlr;
cmdport = ctlr->cmdport;
ctlport = ctlr->ctlport;
if(ataready(cmdport, ctlport, drive->dev, Bsy|Drq, 0, 101*1000) < 0)
return -1;
ilock(ctlr);
drive->block = drive->secsize;
if(drive->write)
drive->command = Cws;
else
drive->command = Crs;
drive->limit = drive->data + drive->count*drive->secsize;
cmd = drive->command;
if(use48){
outb(cmdport+Count, (drive->count>>8) & 0xFF);
outb(cmdport+Count, drive->count & 0XFF);
outb(cmdport+Lbalo, (lba>>24) & 0xFF);
outb(cmdport+Lbalo, lba & 0xFF);
outb(cmdport+Lbamid, (lba>>32) & 0xFF);
outb(cmdport+Lbamid, (lba>>8) & 0xFF);
outb(cmdport+Lbahi, (lba>>40) & 0xFF);
outb(cmdport+Lbahi, (lba>>16) & 0xFF);
outb(cmdport+Dh, drive->dev|Lba);
cmd = cmd48[cmd];
if(DEBUG & Dbg48BIT)
print("using 48-bit commands\n");
}else{
outb(cmdport+Count, drive->count);
outb(cmdport+Sector, s);
outb(cmdport+Cyllo, c);
outb(cmdport+Cylhi, c>>8);
outb(cmdport+Dh, drive->dev|h);
}
ctlr->done = 0;
ctlr->curdrive = drive;
ctlr->command = drive->command;
outb(cmdport+Command, cmd);
switch(drive->command){
case Cws:
case Cwsm:
microdelay(1);
as = ataready(cmdport, ctlport, 0, Bsy, Drq|Err, 1000);
if(as < 0 || (as & Err)){
iunlock(ctlr);
return -1;
}
len = drive->block;
if(drive->data+len > drive->limit)
len = drive->limit-drive->data;
outss(cmdport+Data, drive->data, len/2);
break;
case Crd:
case Cwd:
break;
}
iunlock(ctlr);
return 0;
}
static int
atagenioretry(Drive* drive)
{
return atasetsense(drive, SDcheck, 4, 8, drive->error);
}
static int
atagenio(Drive* drive, uchar* cmd, int)
{
uchar *p;
Ctlr *ctlr;
int count, max;
vlong lba, len;
if((cmd[1]>>5) && cmd[0] != 0x12)
return atasetsense(drive, SDcheck, 0x05, 0x25, 0);
switch(cmd[0]){
default:
return atasetsense(drive, SDcheck, 0x05, 0x20, 0);
case 0x00:
return SDok;
case 0x03:
if(cmd[4] < sizeof(drive->sense))
len = cmd[4];
else
len = sizeof(drive->sense);
if(drive->data && drive->dlen >= len){
memmove(drive->data, drive->sense, len);
drive->data += len;
}
return SDok;
case 0x12:
if(cmd[4] < sizeof(drive->inquiry))
len = cmd[4];
else
len = sizeof(drive->inquiry);
if(drive->data && drive->dlen >= len){
memmove(drive->data, drive->inquiry, len);
drive->data += len;
}
return SDok;
case 0x1B:
return SDok;
case 0x25:
if((cmd[1] & 0x01) || cmd[2] || cmd[3])
return atasetsense(drive, SDcheck, 0x05, 0x24, 0);
if(drive->data == nil || drive->dlen < 8)
return atasetsense(drive, SDcheck, 0x05, 0x20, 1);
len = drive->sectors-1;
p = drive->data;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p++ = len;
len = drive->secsize;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p = len;
drive->data += 8;
return SDok;
case 0x9E:
if((cmd[1] & 0x01) || cmd[2] || cmd[3])
return atasetsense(drive, SDcheck, 0x05, 0x24, 0);
if(drive->data == nil || drive->dlen < 8)
return atasetsense(drive, SDcheck, 0x05, 0x20, 1);
len = drive->sectors-1;
p = drive->data;
*p++ = len>>56;
*p++ = len>>48;
*p++ = len>>40;
*p++ = len>>32;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p++ = len;
len = drive->secsize;
*p++ = len>>24;
*p++ = len>>16;
*p++ = len>>8;
*p = len;
drive->data += 8;
return SDok;
case 0x28:
case 0x2A:
break;
case 0x5A:
return atamodesense(drive, cmd);
}
ctlr = drive->ctlr;
lba = (cmd[2]<<24)|(cmd[3]<<16)|(cmd[4]<<8)|cmd[5];
count = (cmd[7]<<8)|cmd[8];
if(drive->data == nil)
return SDok;
if(drive->dlen < count*drive->secsize)
count = drive->dlen/drive->secsize;
qlock(ctlr);
while(count){
max = (drive->flags&Lba48) ? 65536 : 256;
if(count > max)
drive->count = max;
else
drive->count = count;
if(atageniostart(drive, lba)){
ilock(ctlr);
atanop(drive, 0);
iunlock(ctlr);
qunlock(ctlr);
return atagenioretry(drive);
}
tsleep(ctlr, atageniodone, ctlr, 10*1000);
if(!ctlr->done){
ataabort(drive, 1);
return atagenioretry(drive);
}
if(drive->status & Err){
qunlock(ctlr);
return atasetsense(drive, SDcheck, 4, 8, drive->error);
}
count -= drive->count;
lba += drive->count;
}
qunlock(ctlr);
return SDok;
}
static int
atario(SDreq* r)
{
Ctlr *ctlr;
Drive *drive;
SDunit *unit;
uchar cmd10[10], *cmdp, *p;
int clen, reqstatus, status;
unit = r->unit;
if((ctlr = unit->dev->ctlr) == nil || ctlr->drive[unit->subno] == nil){
r->status = SDtimeout;
return SDtimeout;
}
drive = ctlr->drive[unit->subno];
switch(r->cmd[0]){
case 0x08:
case 0x0A:
cmdp = cmd10;
memset(cmdp, 0, sizeof(cmd10));
cmdp[0] = r->cmd[0]|0x20;
cmdp[1] = r->cmd[1] & 0xE0;
cmdp[5] = r->cmd[3];
cmdp[4] = r->cmd[2];
cmdp[3] = r->cmd[1] & 0x0F;
cmdp[8] = r->cmd[4];
clen = sizeof(cmd10);
break;
default:
cmdp = r->cmd;
clen = r->clen;
break;
}
qlock(drive);
drive->write = r->write;
drive->data = r->data;
drive->dlen = r->dlen;
drive->status = 0;
drive->error = 0;
if(drive->pkt)
status = atapktio(drive, cmdp, clen);
else
status = atagenio(drive, cmdp, clen);
if(status == SDok){
atasetsense(drive, SDok, 0, 0, 0);
if(drive->data){
p = r->data;
r->rlen = drive->data - p;
}
else
r->rlen = 0;
}
else if(status == SDcheck && !(r->flags & SDnosense)){
drive->write = 0;
memset(cmd10, 0, sizeof(cmd10));
cmd10[0] = 0x03;
cmd10[1] = r->lun<<5;
cmd10[4] = sizeof(r->sense)-1;
drive->data = r->sense;
drive->dlen = sizeof(r->sense)-1;
drive->status = 0;
drive->error = 0;
if(drive->pkt)
reqstatus = atapktio(drive, cmd10, 6);
else
reqstatus = atagenio(drive, cmd10, 6);
if(reqstatus == SDok){
r->flags |= SDvalidsense;
atasetsense(drive, SDok, 0, 0, 0);
}
}
qunlock(drive);
r->status = status;
if(status != SDok)
return status;
switch(cmdp[0]){
case 0x12:
if((p = r->data) == nil)
break;
if((cmdp[1]>>5) && (!drive->pkt || (p[0] & 0x1F) == 0x05))
p[0] = 0x7F;
default:
break;
}
return SDok;
}
static void
atainterrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Drive *drive;
int cmdport, len, status;
ctlr = arg;
ilock(ctlr);
if(inb(ctlr->ctlport+As) & Bsy){
iunlock(ctlr);
if(DEBUG & DbgBsy)
print("IBsy+");
return;
}
cmdport = ctlr->cmdport;
status = inb(cmdport+Status);
if((drive = ctlr->curdrive) == nil){
iunlock(ctlr);
if((DEBUG & DbgINL) && ctlr->command != Cedd)
print("Inil%2.2uX+", ctlr->command);
return;
}
if(status & Err)
drive->error = inb(cmdport+Error);
else switch(drive->command){
default:
drive->error = Abrt;
break;
case Crs:
case Crsm:
if(!(status & Drq)){
drive->error = Abrt;
break;
}
len = drive->block;
if(drive->data+len > drive->limit)
len = drive->limit-drive->data;
inss(cmdport+Data, drive->data, len/2);
drive->data += len;
if(drive->data >= drive->limit)
ctlr->done = 1;
break;
case Cws:
case Cwsm:
len = drive->block;
if(drive->data+len > drive->limit)
len = drive->limit-drive->data;
drive->data += len;
if(drive->data >= drive->limit){
ctlr->done = 1;
break;
}
if(!(status & Drq)){
drive->error = Abrt;
break;
}
len = drive->block;
if(drive->data+len > drive->limit)
len = drive->limit-drive->data;
outss(cmdport+Data, drive->data, len/2);
break;
case Cpkt:
atapktinterrupt(drive);
break;
case Crd:
case Cwd:
break;
}
iunlock(ctlr);
if(drive->error){
status |= Err;
ctlr->done = 1;
}
if(ctlr->done){
ctlr->curdrive = nil;
drive->status = status;
wakeup(ctlr);
}
}
static SDev*
atapnp(void)
{
Ctlr *ctlr;
Pcidev *p;
int channel, ispc87415, pi, r;
SDev *legacy[2], *sdev, *head, *tail;
legacy[0] = legacy[1] = head = tail = nil;
if(sdev = ataprobe(0x1F0, 0x3F4, IrqATA0)){
head = tail = sdev;
legacy[0] = sdev;
}
if(sdev = ataprobe(0x170, 0x374, IrqATA1)){
if(head != nil)
tail->next = sdev;
else
head = sdev;
tail = sdev;
legacy[1] = sdev;
}
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x01)
continue;
if(p->ccru != 0x01 && p->ccru != 0x04 && p->ccru != 0x80)
continue;
pi = p->ccrp;
ispc87415 = 0;
switch((p->did<<16)|p->vid){
default:
continue;
case (0x0002<<16)|0x100B:
ispc87415 = 1;
pcicfgw32(p, 0x40, 0x00000300);
break;
case (0x1000<<16)|0x1042:
r = pcicfgr32(p, 0x40);
r &= ~0x2000;
pcicfgw32(p, 0x40, r);
break;
case (0x4D38<<16)|0x105A:
case (0x4D30<<16)|0x105A:
case (0x4D68<<16)|0x105A:
case (0x4D69<<16)|0x105A:
case (0x3373<<16)|0x105A:
case (0x3149<<16)|0x1106:
case (0x4379<<16)|0x1002:
case (0x3112<<16)|0x1095:
case (0x3114<<16)|0x1095:
pi = 0x85;
break;
case (0x0004<<16)|0x1103:
pi = 0x85;
if((r = pcicfgr8(p, 0x51)) & 0x80)
pcicfgw8(p, 0x51, r & ~0x80);
if((r = pcicfgr8(p, 0x55)) & 0x80)
pcicfgw8(p, 0x55, r & ~0x80);
break;
case (0x0640<<16)|0x1095:
break;
case (0x7441<<16)|0x1022:
r = pcicfgr8(p, 0x41);
pcicfgw8(p, 0x41, r|0xF0);
r = pcicfgr8(p, 0x43);
pcicfgw8(p, 0x43, (r & 0x90)|0x2A);
r = pcicfgr8(p, 0x44);
pcicfgw8(p, 0x44, r|0x08);
r = pcicfgr8(p, 0x46);
pcicfgw8(p, 0x46, (r & 0x0C)|0xF0);
case (0x7469<<16)|0x1022:
case (0x209A<<16)|0x1022:
case (0x01BC<<16)|0x10DE:
case (0x0065<<16)|0x10DE:
case (0x0085<<16)|0x10DE:
case (0x00D5<<16)|0x10DE:
case (0x00E5<<16)|0x10DE:
case (0x0035<<16)|0x10DE:
case (0x0053<<16)|0x10DE:
case (0x0054<<16)|0x10DE:
case (0x0055<<16)|0x10DE:
break;
case (0x0646<<16)|0x1095:
case (0x0571<<16)|0x1106:
case (0x0211<<16)|0x1166:
case (0x1230<<16)|0x8086:
case (0x7010<<16)|0x8086:
case (0x7111<<16)|0x8086:
case (0x2411<<16)|0x8086:
case (0x2421<<16)|0x8086:
case (0x244A<<16)|0x8086:
case (0x244B<<16)|0x8086:
case (0x248A<<16)|0x8086:
case (0x248B<<16)|0x8086:
case (0x24CA<<16)|0x8086:
case (0x24CB<<16)|0x8086:
case (0x24DB<<16)|0x8086:
case (0x266F<<16)|0x8086:
case (0x27C4<<16)|0x8086:
case (0x27C5<<16)|0x8086:
break;
}
for(channel = 0; channel < 2; channel++){
if(pi & (1<<(2*channel))){
sdev = ataprobe(p->mem[0+2*channel].bar & ~0x01,
p->mem[1+2*channel].bar & ~0x01,
p->intl);
if(sdev == nil)
continue;
ctlr = sdev->ctlr;
if(ispc87415)
ctlr->ienable = pc87415ienable;
if(head != nil)
tail->next = sdev;
else
head = sdev;
tail = sdev;
ctlr->tbdf = p->tbdf;
}
else if((sdev = legacy[channel]) == nil)
continue;
else
ctlr = sdev->ctlr;
ctlr->pcidev = p;
}
}
return head;
}
static SDev*
atalegacy(int port, int irq)
{
return ataprobe(port, port+0x204, irq);
}
static SDev*
ataid(SDev* sdev)
{
int i;
Ctlr *ctlr;
if(sdev == nil)
return nil;
ctlr = sdev->ctlr;
if(ctlr->cmdport == 0x1F0 || ctlr->cmdport == 0x170)
i = 2;
else
i = 0;
while(sdev){
if(sdev->ifc == &sdataifc){
ctlr = sdev->ctlr;
if(ctlr->cmdport == 0x1F0)
sdev->idno = 'C';
else if(ctlr->cmdport == 0x170)
sdev->idno = 'D';
else{
sdev->idno = 'C'+i;
i++;
}
}
sdev = sdev->next;
}
return nil;
}
static int
ataenable(SDev* sdev)
{
Ctlr *ctlr;
ctlr = sdev->ctlr;
setvec(ctlr->irq+VectorPIC, atainterrupt, ctlr);
outb(ctlr->ctlport+Dc, 0);
if(ctlr->ienable)
ctlr->ienable(ctlr);
return 1;
}
SDifc sdataifc = {
"ata",
atapnp,
atalegacy,
ataid,
ataenable,
nil,
scsiverify,
scsionline,
atario,
nil,
nil,
scsibio,
};