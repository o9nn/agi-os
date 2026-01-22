#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#define DBG if(0) pcilog
typedef struct Pci Pci;
struct
{
char output[PCICONSSIZE];
int ptr;
}PCICONS;
int
pcilog(char *fmt, ...)
{
int n;
va_list arg;
char buf[PRINTSIZE];
va_start(arg, fmt);
n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
va_end(arg);
memmove(PCICONS.output+PCICONS.ptr, buf, n);
PCICONS.ptr += n;
return n;
}
enum
{
MaxFNO = 7,
MaxUBN = 255,
};
enum
{
IOen = (1<<0),
MEMen = (1<<1),
MASen = (1<<2),
MemWrInv = (1<<4),
PErrEn = (1<<6),
SErrEn = (1<<8),
};
typedef struct {
ulong cap;
ulong ctl;
} Capctl;
typedef struct {
Capctl dev;
Capctl link;
Capctl slot;
} Devlinkslot;
struct Pci {
ulong id;
ulong cs;
ulong revclass;
ulong misc;
ulong bar[2];
ulong bus;
ulong ioaddrs;
ulong memaddrs;
ulong prefmem;
ulong prefbasehi;
ulong preflimhi;
ulong ioaddrhi;
ulong cfgcapoff;
ulong rom;
ulong intr;
ulong subsysid;
ulong subsyscap;
Capctl pwrmgmt;
ulong msictlcap;
ulong msimsgaddr[2];
ulong msimsgdata;
uchar _pad0[0x80-0x60];
ulong pciecap;
Devlinkslot port0;
ulong rootctl;
ulong rootsts;
Devlinkslot port1;
};
enum {
Port0 = 0,
Port1 = 0x1000,
Pads = 0x3000,
Afi = 0x3800,
Aficfg = Afi + 0xac,
Cfgspace = 0x4000,
Ecfgspace = 0x104000,
Iospace = 1<<0,
Memspace = 1<<1,
Busmaster = 1<<2,
Fpcion = 1<<0,
};
struct Pcictlr {
union {
uchar _padpci[0x1000];
Pci;
} ports[2];
uchar _padpads[0x1000];
uchar pads[0x800];
uchar afi[0x800];
ulong cfg[0x1000];
ulong extcfg[0x1000];
};
static Lock pcicfglock;
static Lock pcicfginitlock;
static int pcicfgmode = -1;
static int pcimaxbno = 1;
static int pcimaxdno;
static Pcidev* pciroot;
static Pcidev* pcilist;
static Pcidev* pcitail;
static int pcicfgrw8(int, int, int, int);
static int pcicfgrw16(int, int, int, int);
static int pcicfgrw32(int, int, int, int);
static char* bustypes[] = {
"CBUSI",
"CBUSII",
"EISA",
"FUTURE",
"INTERN",
"ISA",
"MBI",
"MBII",
"MCA",
"MPI",
"MPSA",
"NUBUS",
"PCI",
"PCMCIA",
"TC",
"VL",
"VME",
"XPRESS",
};
static int
tbdffmt(Fmt* fmt)
{
char *p;
int l, r;
uint type, tbdf;
if((p = malloc(READSTR)) == nil)
return fmtstrcpy(fmt, "(tbdfconv)");
switch(fmt->r){
case 'T':
tbdf = va_arg(fmt->args, int);
if(tbdf == BUSUNKNOWN)
snprint(p, READSTR, "unknown");
else{
type = BUSTYPE(tbdf);
if(type < nelem(bustypes))
l = snprint(p, READSTR, bustypes[type]);
else
l = snprint(p, READSTR, "%d", type);
snprint(p+l, READSTR-l, ".%d.%d.%d",
BUSBNO(tbdf), BUSDNO(tbdf), BUSFNO(tbdf));
}
break;
default:
snprint(p, READSTR, "(tbdfconv)");
break;
}
r = fmtstrcpy(fmt, p);
free(p);
return r;
}
ulong
pcibarsize(Pcidev *p, int rno)
{
ulong v, size;
v = pcicfgrw32(p->tbdf, rno, 0, 1);
pcicfgrw32(p->tbdf, rno, 0xFFFFFFF0, 0);
size = pcicfgrw32(p->tbdf, rno, 0, 1);
if(v & 1)
size |= 0xFFFF0000;
pcicfgrw32(p->tbdf, rno, v, 0);
return -(size & ~0x0F);
}
static int
pcilscan(int bno, Pcidev** list)
{
Pcidev *p, *head, *tail;
int dno, fno, i, hdt, l, maxfno, maxubn, rno, sbn, tbdf, ubn;
maxubn = bno;
head = nil;
tail = nil;
for(dno = 0; dno <= pcimaxdno; dno++){
maxfno = 0;
for(fno = 0; fno <= maxfno; fno++){
tbdf = MKBUS(BusPCI, bno, dno, fno);
l = pcicfgrw32(tbdf, PciVID, 0, 1);
if(l == 0xFFFFFFFF || l == 0)
continue;
p = malloc(sizeof(*p));
if(p == nil)
panic("pcilscan: no memory");
p->tbdf = tbdf;
p->vid = l;
p->did = l>>16;
if(pcilist != nil)
pcitail->list = p;
else
pcilist = p;
pcitail = p;
p->pcr = pcicfgr16(p, PciPCR);
p->rid = pcicfgr8(p, PciRID);
p->ccrp = pcicfgr8(p, PciCCRp);
p->ccru = pcicfgr8(p, PciCCRu);
p->ccrb = pcicfgr8(p, PciCCRb);
p->cls = pcicfgr8(p, PciCLS);
p->ltr = pcicfgr8(p, PciLTR);
p->intl = pcicfgr8(p, PciINTL);
hdt = pcicfgr8(p, PciHDT);
if(hdt & 0x80)
maxfno = MaxFNO;
switch(p->ccrb) {
case 0x03:
case 0x01:
case 0x02:
case 0x04:
case 0x07:
case 0x08:
case 0x09:
case 0x0A:
case 0x0B:
case 0x0C:
if((hdt & 0x7F) != 0)
break;
rno = PciBAR0 - 4;
for(i = 0; i < nelem(p->mem); i++) {
rno += 4;
p->mem[i].bar = pcicfgr32(p, rno);
p->mem[i].size = pcibarsize(p, rno);
}
break;
case 0x00:
case 0x05:
case 0x06:
default:
break;
}
if(head != nil)
tail->link = p;
else
head = p;
tail = p;
}
}
*list = head;
for(p = head; p != nil; p = p->link){
if(p->ccrb != 0x06 || p->ccru != 0x04)
continue;
sbn = pcicfgr8(p, PciSBN);
ubn = pcicfgr8(p, PciUBN);
if(sbn == 0 || ubn == 0) {
sbn = maxubn+1;
pcicfgw32(p, PciPCR, 0xFFFF0000);
l = (MaxUBN<<16)|(sbn<<8)|bno;
pcicfgw32(p, PciPBN, l);
pcicfgw16(p, PciSPSR, 0xFFFF);
maxubn = pcilscan(sbn, &p->bridge);
l = (maxubn<<16)|(sbn<<8)|bno;
pcicfgw32(p, PciPBN, l);
}
else {
if(ubn > maxubn)
maxubn = ubn;
pcilscan(sbn, &p->bridge);
}
}
return maxubn;
}
extern void rtl8169interrupt(Ureg*, void* arg);
static void
pciintr(Ureg *ureg, void *p)
{
rtl8169interrupt(ureg, p);
}
static void
pcicfginit(void)
{
char *p;
Pci *pci = (Pci *)soc.pci;
Pcidev **list;
int bno, n;
lock(&pcicfginitlock);
if(pcicfgmode != -1) {
unlock(&pcicfginitlock);
return;
}
n = pci->id >> 16;
if (((pci->id & MASK(16)) != Vnvidia || (n != 0xbf0 && n != 0xbf1)) &&
(pci->id & MASK(16)) != Vrealtek) {
print("no pci controller at %#p\n", pci);
unlock(&pcicfginitlock);
return;
}
if (0)
iprint("pci: %#p: nvidia, rev %#ux class %#6.6lux misc %#8.8lux\n",
pci, (uchar)pci->revclass, pci->revclass >> 8,
pci->misc);
pci->cs &= Iospace;
pci->cs |= Memspace | Busmaster;
coherence();
pcicfgmode = 1;
pcimaxdno = 15;
fmtinstall('T', tbdffmt);
if(p = getconf("*pcimaxbno")){
n = strtoul(p, 0, 0);
if(n < pcimaxbno)
pcimaxbno = n;
}
if(p = getconf("*pcimaxdno")){
n = strtoul(p, 0, 0);
if(n < pcimaxdno)
pcimaxdno = n;
}
list = &pciroot;
for(bno = 1; bno <= pcimaxbno; bno++) {
bno = pcilscan(bno, list);
while(*list)
list = &(*list)->link;
}
unlock(&pcicfginitlock);
if(getconf("*pcihinv"))
pcihinv(nil);
}
enum {
Afiintrcode = 0xb8,
};
void
pcieintrdone(void)
{
ulong *afi;
afi = (ulong *)(soc.pci + Afi);
afi[Afiintrcode/sizeof *afi] = 0;
coherence();
}
static void *
tegracfgaddr(int tbdf, int rno)
{
uintptr addr;
addr = soc.pci + (rno < 256? Cfgspace: Ecfgspace) + BUSBDF(tbdf) + rno;
return (void *)addr;
}
static int
pcicfgrw8(int tbdf, int rno, int data, int read)
{
int x;
void *addr;
if(pcicfgmode == -1)
pcicfginit();
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
addr = tegracfgaddr(tbdf, rno);
lock(&pcicfglock);
if(read)
x = *(uchar *)addr;
else
*(uchar *)addr = data;
unlock(&pcicfglock);
return x;
}
int
pcicfgr8(Pcidev* pcidev, int rno)
{
return pcicfgrw8(pcidev->tbdf, rno, 0, 1);
}
void
pcicfgw8(Pcidev* pcidev, int rno, int data)
{
pcicfgrw8(pcidev->tbdf, rno, data, 0);
}
static int
pcicfgrw16(int tbdf, int rno, int data, int read)
{
int x;
void *addr;
if(pcicfgmode == -1)
pcicfginit();
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
addr = tegracfgaddr(tbdf, rno);
lock(&pcicfglock);
if(read)
x = *(ushort *)addr;
else
*(ushort *)addr = data;
unlock(&pcicfglock);
return x;
}
int
pcicfgr16(Pcidev* pcidev, int rno)
{
return pcicfgrw16(pcidev->tbdf, rno, 0, 1);
}
void
pcicfgw16(Pcidev* pcidev, int rno, int data)
{
pcicfgrw16(pcidev->tbdf, rno, data, 0);
}
static int
pcicfgrw32(int tbdf, int rno, int data, int read)
{
int x;
vlong v;
void *addr;
if(pcicfgmode == -1)
pcicfginit();
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
addr = tegracfgaddr(tbdf, rno);
v = probeaddr((uintptr)addr);
if (v < 0)
return -1;
lock(&pcicfglock);
if(read)
x = *(ulong *)addr;
else
*(ulong *)addr = data;
unlock(&pcicfglock);
return x;
}
int
pcicfgr32(Pcidev* pcidev, int rno)
{
return pcicfgrw32(pcidev->tbdf, rno, 0, 1);
}
void
pcicfgw32(Pcidev* pcidev, int rno, int data)
{
pcicfgrw32(pcidev->tbdf, rno, data, 0);
}
Pcidev*
pcimatch(Pcidev* prev, int vid, int did)
{
if(pcicfgmode == -1)
pcicfginit();
if(prev == nil)
prev = pcilist;
else
prev = prev->list;
while(prev != nil){
if((vid == 0 || prev->vid == vid)
&& (did == 0 || prev->did == did))
break;
prev = prev->list;
}
return prev;
}
Pcidev*
pcimatchtbdf(int tbdf)
{
Pcidev *pcidev;
if(pcicfgmode == -1)
pcicfginit();
for(pcidev = pcilist; pcidev != nil; pcidev = pcidev->list) {
if(pcidev->tbdf == tbdf)
break;
}
return pcidev;
}
static void
pcilhinv(Pcidev* p)
{
int i;
Pcidev *t;
if(p == nil) {
putstrn(PCICONS.output, PCICONS.ptr);
p = pciroot;
print("bus dev type vid  did intl memory\n");
}
for(t = p; t != nil; t = t->link) {
print("%d  %2d/%d %.2ux %.2ux %.2ux %.4ux %.4ux %3d  ",
BUSBNO(t->tbdf), BUSDNO(t->tbdf), BUSFNO(t->tbdf),
t->ccrb, t->ccru, t->ccrp, t->vid, t->did, t->intl);
for(i = 0; i < nelem(p->mem); i++) {
if(t->mem[i].size == 0)
continue;
print("%d:%.8lux %d ", i,
t->mem[i].bar, t->mem[i].size);
}
if(t->bridge)
print("->%d", BUSBNO(t->bridge->tbdf));
print("\n");
}
while(p != nil) {
if(p->bridge != nil)
pcilhinv(p->bridge);
p = p->link;
}
}
void
pcihinv(Pcidev* p)
{
if(pcicfgmode == -1)
pcicfginit();
lock(&pcicfginitlock);
pcilhinv(p);
unlock(&pcicfginitlock);
}
void
pcireset(void)
{
Pcidev *p;
if(pcicfgmode == -1)
pcicfginit();
for(p = pcilist; p != nil; p = p->list) {
if(p->ccrb == 0x06)
continue;
pciclrbme(p);
}
}
void
pcisetioe(Pcidev* p)
{
p->pcr |= IOen;
pcicfgw16(p, PciPCR, p->pcr);
}
void
pciclrioe(Pcidev* p)
{
p->pcr &= ~IOen;
pcicfgw16(p, PciPCR, p->pcr);
}
void
pcisetbme(Pcidev* p)
{
p->pcr |= MASen;
pcicfgw16(p, PciPCR, p->pcr);
}
void
pciclrbme(Pcidev* p)
{
p->pcr &= ~MASen;
pcicfgw16(p, PciPCR, p->pcr);
}
void
pcisetmwi(Pcidev* p)
{
p->pcr |= MemWrInv;
pcicfgw16(p, PciPCR, p->pcr);
}
void
pciclrmwi(Pcidev* p)
{
p->pcr &= ~MemWrInv;
pcicfgw16(p, PciPCR, p->pcr);
}
static int
pcigetpmrb(Pcidev* p)
{
int ptr;
if(p->pmrb != 0)
return p->pmrb;
p->pmrb = -1;
if(!(pcicfgr16(p, PciPSR) & 0x0010))
return -1;
switch(pcicfgr8(p, PciHDT)){
default:
return -1;
case 0:
case 1:
ptr = 0x34;
break;
case 2:
ptr = 0x14;
break;
}
ptr = pcicfgr32(p, ptr);
while(ptr != 0){
if(ptr < 0x40 || (ptr & ~0xFC))
return -1;
if(pcicfgr8(p, ptr) == 0x01){
p->pmrb = ptr;
return ptr;
}
ptr = pcicfgr8(p, ptr+1);
}
return -1;
}
int
pcigetpms(Pcidev* p)
{
int pmcsr, ptr;
if((ptr = pcigetpmrb(p)) == -1)
return -1;
pmcsr = pcicfgr16(p, ptr+4);
return pmcsr & 0x0003;
}
int
pcisetpms(Pcidev* p, int state)
{
int ostate, pmc, pmcsr, ptr;
if((ptr = pcigetpmrb(p)) == -1)
return -1;
pmc = pcicfgr16(p, ptr+2);
pmcsr = pcicfgr16(p, ptr+4);
ostate = pmcsr & 0x0003;
pmcsr &= ~0x0003;
switch(state){
default:
return -1;
case 0:
break;
case 1:
if(!(pmc & 0x0200))
return -1;
break;
case 2:
if(!(pmc & 0x0400))
return -1;
break;
case 3:
break;
}
pmcsr |= state;
pcicfgw16(p, ptr+4, pmcsr);
return ostate;
}