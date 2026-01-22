#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "mp.h"
#include "apbootstrap.h"
static Bus* mpbus;
static Bus* mpbuslast;
static int mpisabus = -1;
static int mpeisabus = -1;
extern int i8259elcr;
static Apic mpapic[MaxAPICNO+1];
static int machno2apicno[MaxAPICNO+1];
static Lock mprdthilock;
static int mprdthi;
static Ref mpvnoref;
static int mpmachno = 1;
static char* buses[] = {
"CBUSI ",
"CBUSII",
"EISA  ",
"FUTURE",
"INTERN",
"ISA   ",
"MBI   ",
"MBII  ",
"MCA   ",
"MPI   ",
"MPSA  ",
"NUBUS ",
"PCI   ",
"PCMCIA",
"TC    ",
"VL    ",
"VME   ",
"XPRESS",
0,
};
static Apic*
mkprocessor(PCMPprocessor* p)
{
Apic *apic;
if(!(p->flags & PcmpEN) || p->apicno > MaxAPICNO)
return 0;
apic = &mpapic[p->apicno];
apic->type = PcmpPROCESSOR;
apic->apicno = p->apicno;
apic->flags = p->flags;
apic->lintr[0] = ApicIMASK;
apic->lintr[1] = ApicIMASK;
if(p->flags & PcmpBP){
machno2apicno[0] = p->apicno;
apic->machno = 0;
}
else{
machno2apicno[mpmachno] = p->apicno;
apic->machno = mpmachno;
mpmachno++;
}
return apic;
}
static Bus*
mkbus(PCMPbus* p)
{
Bus *bus;
int i;
for(i = 0; buses[i]; i++){
if(strncmp(buses[i], p->string, sizeof(p->string)) == 0)
break;
}
if(buses[i] == 0)
return 0;
bus = xalloc(sizeof(Bus));
if(mpbus)
mpbuslast->next = bus;
else
mpbus = bus;
mpbuslast = bus;
bus->type = i;
bus->busno = p->busno;
if(bus->type == BusEISA){
bus->po = PcmpLOW;
bus->el = PcmpLEVEL;
if(mpeisabus != -1)
print("mkbus: more than one EISA bus\n");
mpeisabus = bus->busno;
}
else if(bus->type == BusPCI){
bus->po = PcmpLOW;
bus->el = PcmpLEVEL;
}
else if(bus->type == BusISA){
bus->po = PcmpHIGH;
bus->el = PcmpEDGE;
if(mpisabus != -1)
print("mkbus: more than one ISA bus\n");
mpisabus = bus->busno;
}
else{
bus->po = PcmpHIGH;
bus->el = PcmpEDGE;
}
return bus;
}
static Bus*
mpgetbus(int busno)
{
Bus *bus;
for(bus = mpbus; bus; bus = bus->next){
if(bus->busno == busno)
return bus;
}
print("mpgetbus: can't find bus %d\n", busno);
return 0;
}
static Apic*
mkioapic(PCMPioapic* p)
{
Apic *apic;
if(!(p->flags & PcmpEN) || p->apicno > MaxAPICNO)
return 0;
if(mmukmap(p->addr, 0, 1024) == 0)
return 0;
apic = &mpapic[p->apicno];
apic->type = PcmpIOAPIC;
apic->apicno = p->apicno;
apic->addr = KADDR(p->addr);
apic->flags = p->flags;
return apic;
}
static Aintr*
mkiointr(PCMPintr* p)
{
Bus *bus;
Aintr *aintr;
if(p->apicno == 0xFF)
return 0;
if((bus = mpgetbus(p->busno)) == 0)
return 0;
aintr = xalloc(sizeof(Aintr));
aintr->intr = p;
aintr->apic = &mpapic[p->apicno];
aintr->next = bus->aintr;
bus->aintr = aintr;
return aintr;
}
static int
mpintrinit(Bus* bus, PCMPintr* intr, int vno, int )
{
int el, po, v;
v = vno;
po = intr->flags & PcmpPOMASK;
el = intr->flags & PcmpELMASK;
switch(intr->intr){
default:
v |= ApicLOWEST;
break;
case PcmpNMI:
v |= ApicNMI;
po = PcmpHIGH;
el = PcmpEDGE;
break;
case PcmpSMI:
v |= ApicSMI;
break;
case PcmpExtINT:
v |= ApicExtINT;
po = PcmpHIGH;
el = PcmpEDGE;
break;
}
if(bus->type == BusEISA && !po && !el ){
po = PcmpHIGH;
el = PcmpEDGE;
}
if(!po)
po = bus->po;
if(po == PcmpLOW)
v |= ApicLOW;
else if(po != PcmpHIGH){
print("mpintrinit: bad polarity 0x%uX\n", po);
return ApicIMASK;
}
if(!el)
el = bus->el;
if(el == PcmpLEVEL)
v |= ApicLEVEL;
else if(el != PcmpEDGE){
print("mpintrinit: bad trigger 0x%uX\n", el);
return ApicIMASK;
}
return v;
}
static int
mklintr(PCMPintr* p)
{
Apic *apic;
Bus *bus;
int intin, v;
if((bus = mpgetbus(p->busno)) == 0)
return 0;
intin = p->intin;
if(p->intr == PcmpExtINT || p->intr == PcmpNMI)
v = ApicIMASK;
else
v = mpintrinit(bus, p, VectorLAPIC+intin, p->irq);
if(p->apicno == 0xFF){
for(apic = mpapic; apic <= &mpapic[MaxAPICNO]; apic++){
if((apic->flags & PcmpEN)
&& apic->type == PcmpPROCESSOR)
apic->lintr[intin] = v;
}
}
else{
apic = &mpapic[p->apicno];
if((apic->flags & PcmpEN) && apic->type == PcmpPROCESSOR)
apic->lintr[intin] = v;
}
return v;
}
static void
checkmtrr(void)
{
int i, vcnt;
Mach *mach0;
if(!(m->cpuiddx & 0x1000))
return;
rdmsr(0x0FE, &m->mtrrcap);
rdmsr(0x2FF, &m->mtrrdef);
if(m->mtrrcap & 0x0100){
rdmsr(0x250, &m->mtrrfix[0]);
rdmsr(0x258, &m->mtrrfix[1]);
rdmsr(0x259, &m->mtrrfix[2]);
for(i = 0; i < 8; i++)
rdmsr(0x268+i, &m->mtrrfix[(i+3)]);
}
vcnt = m->mtrrcap & 0x00FF;
if(vcnt > nelem(m->mtrrvar))
vcnt = nelem(m->mtrrvar);
for(i = 0; i < vcnt; i++)
rdmsr(0x200+i, &m->mtrrvar[i]);
if(m->machno == 0)
return;
mach0 = MACHP(0);
if(mach0->mtrrcap != m->mtrrcap)
print("mtrrcap%d: %lluX %lluX\n",
m->machno, mach0->mtrrcap, m->mtrrcap);
if(mach0->mtrrdef != m->mtrrdef)
print("mtrrdef%d: %lluX %lluX\n",
m->machno, mach0->mtrrdef, m->mtrrdef);
for(i = 0; i < 11; i++){
if(mach0->mtrrfix[i] != m->mtrrfix[i])
print("mtrrfix%d: i%d: %lluX %lluX\n",
m->machno, i, mach0->mtrrfix[i], m->mtrrfix[i]);
}
for(i = 0; i < vcnt; i++){
if(mach0->mtrrvar[i] != m->mtrrvar[i])
print("mtrrvar%d: i%d: %lluX %lluX\n",
m->machno, i, mach0->mtrrvar[i], m->mtrrvar[i]);
}
}
static void
squidboy(Apic* apic)
{
machinit();
mmuinit();
cpuidentify();
cpuidprint();
checkmtrr();
lock(&mprdthilock);
mprdthi |= (1<<apic->apicno)<<24;
unlock(&mprdthilock);
lapicinit(apic);
lapiconline();
syncclock();
timersinit();
fpoff();
lock(&active);
active.machs |= 1<<m->machno;
unlock(&active);
while(!active.thunderbirdsarego)
microdelay(100);
schedinit();
}
static void
mpstartap(Apic* apic)
{
ulong *apbootp, *pdb, *pte;
Mach *mach, *mach0;
int i, machno;
uchar *p;
mach0 = MACHP(0);
p = xspanalloc(4*BY2PG, BY2PG, 0);
pdb = (ulong*)p;
memmove(pdb, mach0->pdb, BY2PG);
p += BY2PG;
if((pte = mmuwalk(pdb, MACHADDR, 1, 0)) == nil)
return;
memmove(p, KADDR(PPN(*pte)), BY2PG);
*pte = PADDR(p)|PTEWRITE|PTEVALID;
if(mach0->havepge)
*pte |= PTEGLOBAL;
p += BY2PG;
mach = (Mach*)p;
if((pte = mmuwalk(pdb, MACHADDR, 2, 0)) == nil)
return;
*pte = PADDR(mach)|PTEWRITE|PTEVALID;
if(mach0->havepge)
*pte |= PTEGLOBAL;
p += BY2PG;
machno = apic->machno;
MACHP(machno) = mach;
mach->machno = machno;
mach->pdb = pdb;
mach->gdt = (Segdesc*)p;
apbootp = (ulong*)(APBOOTSTRAP+0x08);
*apbootp++ = (ulong)squidboy;
*apbootp++ = PADDR(pdb);
*apbootp = (ulong)apic;
p = KADDR(0x467);
*p++ = PADDR(APBOOTSTRAP);
*p++ = PADDR(APBOOTSTRAP)>>8;
i = (PADDR(APBOOTSTRAP) & ~0xFFFF)/16;
*p++ = i;
*p = i>>8;
nvramwrite(0x0F, 0x0A);
lapicstartap(apic, PADDR(APBOOTSTRAP));
for(i = 0; i < 1000; i++){
lock(&mprdthilock);
if(mprdthi & ((1<<apic->apicno)<<24)){
unlock(&mprdthilock);
break;
}
unlock(&mprdthilock);
delay(10);
}
nvramwrite(0x0F, 0x00);
}
void
mpinit(void)
{
int ncpu;
char *cp;
PCMP *pcmp;
uchar *e, *p;
Apic *apic, *bpapic;
i8259init();
syncclock();
if(_mp_ == 0)
return;
pcmp = KADDR(_mp_->physaddr);
if(mmukmap(pcmp->lapicbase, 0, 1024) == 0)
return;
bpapic = nil;
p = ((uchar*)pcmp)+sizeof(PCMP);
e = ((uchar*)pcmp)+pcmp->length;
while(p < e) switch(*p){
default:
print("mpinit: unknown PCMP type 0x%uX (e-p 0x%luX)\n",
*p, e-p);
while(p < e){
print("%uX ", *p);
p++;
}
break;
case PcmpPROCESSOR:
if(apic = mkprocessor((PCMPprocessor*)p)){
apic->addr = KADDR(pcmp->lapicbase);
if(apic->flags & PcmpBP)
bpapic = apic;
}
p += sizeof(PCMPprocessor);
continue;
case PcmpBUS:
mkbus((PCMPbus*)p);
p += sizeof(PCMPbus);
continue;
case PcmpIOAPIC:
if(apic = mkioapic((PCMPioapic*)p))
ioapicinit(apic, ((PCMPioapic*)p)->apicno);
p += sizeof(PCMPioapic);
continue;
case PcmpIOINTR:
mkiointr((PCMPintr*)p);
p += sizeof(PCMPintr);
continue;
case PcmpLINTR:
mklintr((PCMPintr*)p);
p += sizeof(PCMPintr);
continue;
}
if(bpapic == 0)
return;
lapicinit(bpapic);
lock(&mprdthilock);
mprdthi |= (1<<bpapic->apicno)<<24;
unlock(&mprdthilock);
intrenable(IrqTIMER, lapicclock, 0, BUSUNKNOWN, "clock");
intrenable(IrqERROR, lapicerror, 0, BUSUNKNOWN, "lapicerror");
intrenable(IrqSPURIOUS, lapicspurious, 0, BUSUNKNOWN, "lapicspurious");
lapiconline();
checkmtrr();
if(cp = getconf("*ncpu")){
ncpu = strtol(cp, 0, 0);
if(ncpu < 1)
ncpu = 1;
}
else
ncpu = MaxAPICNO;
memmove((void*)APBOOTSTRAP, apbootstrap, sizeof(apbootstrap));
for(apic = mpapic; apic <= &mpapic[MaxAPICNO]; apic++){
if(ncpu <= 1)
break;
if((apic->flags & (PcmpBP|PcmpEN)) == PcmpEN
&& apic->type == PcmpPROCESSOR){
mpstartap(apic);
conf.nmach++;
ncpu--;
}
}
if(X86FAMILY(m->cpuidax) == 3 || conf.nmach > 1)
conf.copymode = 1;
}
static int
mpintrenablex(Vctl* v, int tbdf)
{
Bus *bus;
Aintr *aintr;
Apic *apic;
Pcidev *pcidev;
int bno, dno, irq, lo, n, type, vno;
type = BUSTYPE(tbdf);
bno = BUSBNO(tbdf);
dno = BUSDNO(tbdf);
n = 0;
for(bus = mpbus; bus != nil; bus = bus->next){
if(bus->type != type)
continue;
if(n == bno)
break;
n++;
}
if(bus == nil){
print("ioapicirq: can't find bus type %d\n", type);
return -1;
}
if(bus->type == BusPCI){
pcidev = pcimatchtbdf(tbdf);
if(pcidev != nil && (n = pcicfgr8(pcidev, PciINTP)) != 0)
irq = (dno<<2)|(n-1);
else
irq = -1;
}
else
irq = v->irq;
for(aintr = bus->aintr; aintr; aintr = aintr->next){
if(aintr->intr->irq != irq)
continue;
apic = aintr->apic;
ioapicrdtr(apic, aintr->intr->intin, 0, &lo);
if(!(lo & ApicIMASK)){
vno = lo & 0xFF;
n = mpintrinit(bus, aintr->intr, vno, v->irq);
n |= ApicLOGICAL;
if(n != lo || !(n & ApicLEVEL)){
print("mpintrenable: multiple botch irq%d, tbdf %uX, lo %8.8uX, n %8.8uX\n",
v->irq, tbdf, lo, n);
return -1;
}
v->isr = lapicisr;
v->eoi = lapiceoi;
return vno;
}
vno = VectorAPIC + (incref(&mpvnoref)-1)*8;
if(vno > MaxVectorAPIC){
print("mpintrenable: vno %d, irq %d, tbdf %uX\n",
vno, v->irq, tbdf);
return -1;
}
lo = mpintrinit(bus, aintr->intr, vno, v->irq);
if(lo & ApicIMASK)
return -1;
lo |= ApicLOGICAL;
if((apic->flags & PcmpEN) && apic->type == PcmpIOAPIC){
lock(&mprdthilock);
ioapicrdtw(apic, aintr->intr->intin, mprdthi, lo);
unlock(&mprdthilock);
}
v->isr = lapicisr;
v->eoi = lapiceoi;
return vno;
}
return -1;
}
int
mpintrenable(Vctl* v)
{
int irq, tbdf, vno;
tbdf = v->tbdf;
if(tbdf != BUSUNKNOWN && (vno = mpintrenablex(v, tbdf)) != -1)
return vno;
irq = v->irq;
if(irq >= IrqLINT0 && irq <= MaxIrqLAPIC){
if(irq != IrqSPURIOUS)
v->isr = lapiceoi;
return VectorPIC+irq;
}
if(irq < 0 || irq > MaxIrqPIC){
print("mpintrenable: irq %d out of range\n", irq);
return -1;
}
if(mpeisabus != -1){
vno = mpintrenablex(v, MKBUS(BusEISA, 0, 0, 0));
if(vno != -1)
return vno;
}
if(mpisabus != -1){
vno = mpintrenablex(v, MKBUS(BusISA, 0, 0, 0));
if(vno != -1)
return vno;
}
return -1;
}
static Lock mpshutdownlock;
void
mpshutdown(void)
{
if(!canlock(&mpshutdownlock)){
#ifdef FIXTHIS
if(lapicisr(VectorKBD))
lapiceoi(VectorKBD);
#endif
idle();
}
print("apshutdown: active = 0x%2.2uX\n", active.machs);
delay(1000);
splhi();
lapicicrw(0, 0x000C0000|ApicINIT);
#ifdef notdef
*(ushort*)KADDR(0x472) = 0x1234;
outb(0xCF9, 0x02);
outb(0xCF9, 0x06);
#else
pcireset();
i8042reset();
#endif
}