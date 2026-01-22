#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "error.h"
enum {
PciADDR		= 0xCF8,
PciDATA		= 0xCFC,
PciCSE		= 0xCF8,
PciFORWARD	= 0xCFA,
MaxFNO		= 7,
MaxUBN		= 255,
};
enum
{
IOen		= (1<<0),
MEMen		= (1<<1),
MASen		= (1<<2),
MemWrInv	= (1<<4),
PErrEn		= (1<<6),
SErrEn		= (1<<8),
};
static Lock pcicfglock;
static Lock pcicfginitlock;
static int pcicfgmode = -1;
static int pcimaxbno = 7;
static int pcimaxdno;
static Pcidev* pciroot;
static Pcidev* pcilist;
static Pcidev* pcitail;
static int pcicfgrw32(int, int, int, int);
static int pcicfgrw8(int, int, int, int);
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
int
pciscan(int bno, Pcidev** list)
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
p->tbdf = tbdf;
p->vid = l;
p->did = l>>16;
if(pcilist != nil)
pcitail->list = p;
else
pcilist = p;
pcitail = p;
p->rid = pcicfgr8(p, PciRID);
p->ccrp = pcicfgr8(p, PciCCRp);
p->ccru = pcicfgr8(p, PciCCRu);
p->ccrb = pcicfgr8(p, PciCCRb);
p->pcr = pcicfgr32(p, PciPCR);
p->intl = pcicfgr8(p, PciINTL);
hdt = pcicfgr8(p, PciHDT);
if(hdt & 0x80)
maxfno = MaxFNO;
switch(p->ccrb){
case 0x01:
case 0x02:
case 0x03:
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
for(i = 0; i < nelem(p->mem); i++){
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
ubn = pcicfgr8(p, PciUBN);
sbn = pcicfgr8(p, PciSBN);
if(sbn == 0 || ubn == 0){
sbn = maxubn+1;
pcicfgw32(p, PciPCR, 0xFFFF0000);
l = (MaxUBN<<16)|(sbn<<8)|bno;
pcicfgw32(p, PciPBN, l);
pcicfgw16(p, PciSPSR, 0xFFFF);
maxubn = pciscan(sbn, &p->bridge);
l = (maxubn<<16)|(sbn<<8)|bno;
pcicfgw32(p, PciPBN, l);
}
else{
if(ubn > maxubn)
maxubn = ubn;
pciscan(sbn, &p->bridge);
}
}
return maxubn;
}
static uchar
null_link(Pcidev *, uchar )
{
return 0;
}
static void
null_init(Pcidev *, uchar , uchar )
{
}
static uchar
pIIx_link(Pcidev *router, uchar link)
{
uchar pirq;
pirq = pcicfgr8(router, link);
return (pirq < 16)? pirq: 0;
}
static void
pIIx_init(Pcidev *router, uchar link, uchar irq)
{
pcicfgw8(router, link, irq);
}
static uchar
via_link(Pcidev *router, uchar link)
{
uchar pirq;
pirq = (link < 6)? pcicfgr8(router, 0x55 + (link>>1)): 0;
return (link & 1)? (pirq >> 4): (pirq & 15);
}
static void
via_init(Pcidev *router, uchar link, uchar irq)
{
uchar pirq;
pirq = pcicfgr8(router, 0x55 + (link >> 1));
pirq &= (link & 1)? 0x0f: 0xf0;
pirq |= (link & 1)? (irq << 4): (irq & 15);
pcicfgw8(router, 0x55 + (link>>1), pirq);
}
static uchar
opti_link(Pcidev *router, uchar link)
{
uchar pirq = 0;
if ((link & 0xcf) == 0x02)
pirq = pcicfgr8(router, 0xb8 + (link >> 5));
return (link & 0x10)? (pirq >> 4): (pirq & 15);
}
static void
opti_init(Pcidev *router, uchar link, uchar irq)
{
uchar pirq;
pirq = pcicfgr8(router, 0xb8 + (link >> 5));
pirq &= (link & 0x10)? 0x0f : 0xf0;
pirq |= (link & 0x10)? (irq << 4): (irq & 15);
pcicfgw8(router, 0xb8 + (link >> 5), pirq);
}
static uchar
ali_link(Pcidev *router, uchar link)
{
static const uchar map[] = { 0, 9, 3, 10, 4, 5, 7, 6, 1, 11, 0, 12, 0, 14, 0, 15 };
uchar pirq;
pirq = pcicfgr8(router, 0x48 + ((link-1)>>1));
return (link & 1)? map[pirq&15]: map[pirq>>4];
}
static void
ali_init(Pcidev *router, uchar link, uchar irq)
{
static const uchar map[] = { 0, 8, 0, 2, 4, 5, 7, 6, 0, 1, 3, 9, 11, 0, 13, 15 };
uchar pirq;
pirq = pcicfgr8(router, 0x48 + ((link-1)>>1));
pirq &= (link & 1)? 0x0f: 0xf0;
pirq |= (link & 1)? (map[irq] << 4): (map[irq] & 15);
pcicfgw8(router, 0x48 + ((link-1)>>1), pirq);
}
static uchar
cyrix_link(Pcidev *router, uchar link)
{
uchar pirq;
pirq = pcicfgr8(router, 0x5c + ((link-1)>>1));
return ((link & 1)? pirq >> 4: pirq & 15);
}
static void
cyrix_init(Pcidev *router, uchar link, uchar irq)
{
uchar pirq;
pirq = pcicfgr8(router, 0x5c + (link>>1));
pirq &= (link & 1)? 0x0f: 0xf0;
pirq |= (link & 1)? (irq << 4): (irq & 15);
pcicfgw8(router, 0x5c + (link>>1), pirq);
}
typedef struct {
ushort	sb_vid, sb_did;
uchar	(*sb_translate)(Pcidev *, uchar);
void	(*sb_initialize)(Pcidev *, uchar, uchar);
} bridge_t;
static bridge_t southbridges[] = {
{ 0x8086, 0x122e, pIIx_link, pIIx_init },
{ 0x8086, 0x1234, pIIx_link, pIIx_init },
{ 0x8086, 0x7000, pIIx_link, pIIx_init },
{ 0x8086, 0x7110, pIIx_link, pIIx_init },
{ 0x8086, 0x7198, pIIx_link, pIIx_init },
{ 0x8086, 0x2410, pIIx_link, pIIx_init },
{ 0x8086, 0x2420, pIIx_link, pIIx_init },
{ 0x8086, 0x2440, pIIx_link, pIIx_init },
{ 0x8086, 0x244c, pIIx_link, pIIx_init },
{ 0x8086, 0x2480, pIIx_link, pIIx_init },
{ 0x8086, 0x248c, pIIx_link, pIIx_init },
{ 0x8086, 0x24c0, pIIx_link, pIIx_init },
{ 0x8086, 0x24cc, pIIx_link, pIIx_init },
{ 0x8086, 0x24d0, pIIx_link, pIIx_init },
{ 0x8086, 0x2640, pIIx_link, pIIx_init },
{ 0x8086, 0x27b8, pIIx_link, pIIx_init },
{ 0x8086, 0x27b9, pIIx_link, pIIx_init },
{ 0x1106, 0x0586, via_link, via_init },
{ 0x1106, 0x0596, via_link, via_init },
{ 0x1106, 0x0686, via_link, via_init },
{ 0x1106, 0x3227, via_link, via_init },
{ 0x1045, 0xc700, opti_link, opti_init },
{ 0x10b9, 0x1533, ali_link, ali_init },
{ 0x1039, 0x0008, pIIx_link, pIIx_init },
{ 0x1039, 0x0496, pIIx_link, pIIx_init },
{ 0x1078, 0x0100, cyrix_link, cyrix_init },
{ 0x1002, 0x4377, nil, nil },
{ 0x1002, 0x4372, nil, nil },
{ 0x1022, 0x746B, nil, nil },
{ 0x10DE, 0x00D1, nil, nil },
{ 0x10DE, 0x00E0, nil, nil },
{ 0x1166, 0x0200, nil, nil },
};
typedef struct {
uchar	e_bus;
uchar	e_dev;
uchar	e_maps[12];
uchar	e_slot;
uchar	e_reserved;
} slot_t;
typedef struct {
uchar	rt_signature[4];
uchar	rt_version[2];
uchar	rt_size[2];
uchar	rt_bus;
uchar	rt_devfn;
uchar	rt_pciirqs[2];
uchar	rt_compat[4];
uchar	rt_miniport[4];
uchar	rt_reserved[11];
uchar	rt_checksum;
} router_t;
static ushort pciirqs;
static bridge_t *southbridge;
static void
pcirouting(void)
{
uchar *p, pin, irq;
ulong tbdf, vdid;
ushort vid, did;
router_t *r;
slot_t *e;
int size, i, fn;
Pcidev *sbpci, *pci;
for (p = (uchar *)KADDR(0xf0000); p < (uchar *)KADDR(0xfffff); p += 16)
if (p[0] == '$' && p[1] == 'P' && p[2] == 'I' && p[3] == 'R')
break;
if (p >= (uchar *)KADDR(0xfffff))
return;
r = (router_t *)p;
tbdf = (BusPCI << 24)|(r->rt_bus << 16)|(r->rt_devfn << 8);
vdid = pcicfgrw32(tbdf, PciVID, 0, 1);
vid = vdid;
did = vdid >> 16;
for (i = 0; i != nelem(southbridges); i++)
if (vid == southbridges[i].sb_vid && did == southbridges[i].sb_did)
break;
if (i == nelem(southbridges)) {
print("pcirouting: South bridge %.4uX, %.4uX not found\n", vid, did);
return;
}
southbridge = &southbridges[i];
if ((sbpci = pcimatch(nil, vid, did)) == nil) {
print("pcirouting: Cannot match south bridge %.4uX, %.4uX\n",
vid, did);
return;
}
pciirqs = (r->rt_pciirqs[1] << 8)|r->rt_pciirqs[0];
size = (r->rt_size[1] << 8)|r->rt_size[0];
for (e = (slot_t *)&r[1]; (uchar *)e < p + size; e++) {
for (fn = 0; fn != 8; fn++) {
uchar *m;
tbdf = (BusPCI << 24)|(e->e_bus << 16)|((e->e_dev | fn) << 8);
vdid = pcicfgrw32(tbdf, PciVID, 0, 1);
if (vdid == 0xFFFFFFFF || vdid == 0)
continue;
vid = vdid;
did = vdid >> 16;
pci = nil;
while ((pci = pcimatch(pci, vid, did)) != nil) {
if (pci->intl != 0 && pci->intl != 0xFF)
continue;
pin = pcicfgr8(pci, PciINTP);
if (pin == 0 || pin == 0xff)
continue;
m = &e->e_maps[(pin - 1) * 3];
irq = southbridge->sb_translate(sbpci, m[0]);
if (irq) {
print("pcirouting: %.4uX/%.4uX at pin %d irq %d\n",
vid, did, pin, irq);
pcicfgw8(pci, PciINTL, irq);
pci->intl = irq;
}
}
}
}
}
static void
pcicfginit(void)
{
char *p;
int bno, n;
Pcidev **list;
lock(&pcicfginitlock);
if(pcicfgmode != -1)
goto out;
n = inl(PciADDR);
if(!(n & 0x7FF00000)){
outl(PciADDR, 0x80000000);
outb(PciADDR+3, 0);
if(inl(PciADDR) & 0x80000000){
pcicfgmode = 1;
pcimaxdno = 31;
}
}
outl(PciADDR, n);
if(pcicfgmode < 0){
n = inb(PciCSE);
if(!(n & 0xF0)){
outb(PciCSE, 0x0E);
if(inb(PciCSE) == 0x0E){
pcicfgmode = 2;
pcimaxdno = 15;
}
}
outb(PciCSE, n);
}
if(pcicfgmode < 0)
goto out;
if(p = getconf("*pcimaxbno"))
pcimaxbno = strtoul(p, 0, 0);
if(p = getconf("*pcimaxdno"))
pcimaxdno = strtoul(p, 0, 0);
list = &pciroot;
for(bno = 0; bno <= pcimaxbno; bno++) {
bno = pciscan(bno, list);
while(*list)
list = &(*list)->link;
}
pcirouting();
out:
unlock(&pcicfginitlock);
if(getconf("*pcihinv"))
pcihinv(nil);
}
static int
pcicfgrw8(int tbdf, int rno, int data, int read)
{
int o, type, x;
if(pcicfgmode == -1)
pcicfginit();
if(BUSBNO(tbdf))
type = 0x01;
else
type = 0x00;
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
lock(&pcicfglock);
switch(pcicfgmode){
case 1:
o = rno & 0x03;
rno &= ~0x03;
outl(PciADDR, 0x80000000|BUSBDF(tbdf)|rno|type);
if(read)
x = inb(PciDATA+o);
else
outb(PciDATA+o, data);
outl(PciADDR, 0);
break;
case 2:
outb(PciCSE, 0x80|(BUSFNO(tbdf)<<1));
outb(PciFORWARD, BUSBNO(tbdf));
if(read)
x = inb((0xC000|(BUSDNO(tbdf)<<8)) + rno);
else
outb((0xC000|(BUSDNO(tbdf)<<8)) + rno, data);
outb(PciCSE, 0);
break;
}
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
int o, type, x;
if(pcicfgmode == -1)
pcicfginit();
if(BUSBNO(tbdf))
type = 0x01;
else
type = 0x00;
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
lock(&pcicfglock);
switch(pcicfgmode){
case 1:
o = rno & 0x02;
rno &= ~0x03;
outl(PciADDR, 0x80000000|BUSBDF(tbdf)|rno|type);
if(read)
x = ins(PciDATA+o);
else
outs(PciDATA+o, data);
outl(PciADDR, 0);
break;
case 2:
outb(PciCSE, 0x80|(BUSFNO(tbdf)<<1));
outb(PciFORWARD, BUSBNO(tbdf));
if(read)
x = ins((0xC000|(BUSDNO(tbdf)<<8)) + rno);
else
outs((0xC000|(BUSDNO(tbdf)<<8)) + rno, data);
outb(PciCSE, 0);
break;
}
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
int type, x;
if(pcicfgmode == -1)
pcicfginit();
if(BUSBNO(tbdf))
type = 0x01;
else
type = 0x00;
x = -1;
if(BUSDNO(tbdf) > pcimaxdno)
return x;
lock(&pcicfglock);
switch(pcicfgmode){
case 1:
rno &= ~0x03;
outl(PciADDR, 0x80000000|BUSBDF(tbdf)|rno|type);
if(read)
x = inl(PciDATA);
else
outl(PciDATA, data);
outl(PciADDR, 0);
break;
case 2:
outb(PciCSE, 0x80|(BUSFNO(tbdf)<<1));
outb(PciFORWARD, BUSBNO(tbdf));
if(read)
x = inl((0xC000|(BUSDNO(tbdf)<<8)) + rno);
else
outl((0xC000|(BUSDNO(tbdf)<<8)) + rno, data);
outb(PciCSE, 0);
break;
}
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
while(prev != nil) {
if((vid == 0 || prev->vid == vid)
&& (did == 0 || prev->did == did))
break;
prev = prev->list;
}
return prev;
}
uchar
pciipin(Pcidev *pci, uchar pin)
{
if (pci == nil)
pci = pcilist;
while (pci) {
uchar intl;
if (pcicfgr8(pci, PciINTP) == pin && pci->intl != 0 && pci->intl != 0xff)
return pci->intl;
if (pci->bridge && (intl = pciipin(pci->bridge, pin)) != 0)
return intl;
pci = pci->list;
}
return 0;
}
static ushort
pciimask(Pcidev *pci)
{
ushort imask;
imask = 0;
while (pci) {
if (pcicfgr8(pci, PciINTP) && pci->intl < 16)
imask |= 1 << pci->intl;
if (pci->bridge)
imask |= pciimask(pci->bridge);
pci = pci->list;
}
return imask;
}
uchar
pciintl(Pcidev *pci)
{
ushort imask;
int i;
if (pci == nil)
pci = pcilist;
imask = pciimask(pci) | 1;
for (i = 0; i != 16; i++)
if ((imask & (1 << i)) == 0)
return i;
return 0;
}
void
pcihinv(Pcidev* p)
{
int i;
Pcidev *t;
if(pcicfgmode == -1)
pcicfginit();
if(p == nil) {
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
print("\n");
}
while(p != nil) {
if(p->bridge != nil)
pcihinv(p->bridge);
p = p->link;
}
}
void
pcireset(void)
{
Pcidev *p;
int pcr;
if(pcicfgmode == -1)
pcicfginit();
for(p = pcilist; p != nil; p = p->list){
pcr = pcicfgr16(p, PciPSR);
pcicfgw16(p, PciPSR, pcr & ~0x04);
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