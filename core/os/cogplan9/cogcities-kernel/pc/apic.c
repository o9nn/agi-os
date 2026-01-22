#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "mp.h"
enum {
LapicID = 0x0020,
LapicVER = 0x0030,
LapicTPR = 0x0080,
LapicAPR = 0x0090,
LapicPPR = 0x00A0,
LapicEOI = 0x00B0,
LapicLDR = 0x00D0,
LapicDFR = 0x00E0,
LapicSVR = 0x00F0,
LapicISR = 0x0100,
LapicTMR = 0x0180,
LapicIRR = 0x0200,
LapicESR = 0x0280,
LapicICRLO = 0x0300,
LapicICRHI = 0x0310,
LapicTIMER = 0x0320,
LapicPCINT = 0x0340,
LapicLINT0 = 0x0350,
LapicLINT1 = 0x0360,
LapicERROR = 0x0370,
LapicTICR = 0x0380,
LapicTCCR = 0x0390,
LapicTDCR = 0x03E0,
};
enum {
LapicENABLE = 0x00000100,
LapicFOCUS = 0x00000200,
};
enum {
LapicDEASSERT = 0x00000000,
LapicASSERT = 0x00004000,
LapicINVALID = 0x00000000,
LapicWAIT = 0x00010000,
LapicVALID = 0x00020000,
LapicFIELD = 0x00000000,
LapicSELF = 0x00040000,
LapicALLINC = 0x00080000,
LapicALLEXC = 0x000C0000,
};
enum {
LapicSENDCS = 0x00000001,
LapicRCVCS = 0x00000002,
LapicSENDACCEPT = 0x00000004,
LapicRCVACCEPT = 0x00000008,
LapicSENDVECTOR = 0x00000020,
LapicRCVVECTOR = 0x00000040,
LapicREGISTER = 0x00000080,
};
enum {
LapicONESHOT = 0x00000000,
LapicPERIODIC = 0x00020000,
LapicCLKIN = 0x00000000,
LapicTMBASE = 0x00040000,
LapicDIVIDER = 0x00080000,
};
enum {
LapicX2 = 0x00000000,
LapicX4 = 0x00000001,
LapicX8 = 0x00000002,
LapicX16 = 0x00000003,
LapicX32 = 0x00000008,
LapicX64 = 0x00000009,
LapicX128 = 0x0000000A,
LapicX1 = 0x0000000B,
};
static ulong* lapicbase;
struct
{
uvlong hz;
ulong max;
ulong min;
ulong div;
} lapictimer;
static ulong
lapicr(int r)
{
if(lapicbase == 0)
panic("lapicr: no lapic");
return *(lapicbase+(r/sizeof(*lapicbase)));
}
static void
lapicw(int r, ulong data)
{
if(lapicbase == 0)
panic("lapicw: no lapic");
*(lapicbase+(r/sizeof(*lapicbase))) = data;
data = *(lapicbase+(LapicID/sizeof(*lapicbase)));
USED(data);
}
void
lapiconline(void)
{
microdelay((TK2MS(1)*1000/conf.nmach) * m->machno);
lapicw(LapicTICR, lapictimer.max);
lapicw(LapicTIMER, LapicCLKIN|LapicPERIODIC|(VectorPIC+IrqTIMER));
lapicw(LapicTPR, 0);
}
static void
lapictimerinit(void)
{
uvlong x, v, hz;
v = m->cpuhz/1000;
lapicw(LapicTDCR, LapicX1);
lapicw(LapicTIMER, ApicIMASK|LapicCLKIN|LapicONESHOT|(VectorPIC+IrqTIMER));
if(lapictimer.hz == 0ULL){
x = fastticks(&hz);
x += hz/10;
lapicw(LapicTICR, 0xffffffff);
do{
v = fastticks(nil);
}while(v < x);
lapictimer.hz = (0xffffffffUL-lapicr(LapicTCCR))*10;
lapictimer.max = lapictimer.hz/HZ;
lapictimer.min = lapictimer.hz/(100*HZ);
if(lapictimer.hz > hz-(hz/10)){
if(lapictimer.hz > hz+(hz/10))
panic("lapic clock %lld > cpu clock > %lld\n",
lapictimer.hz, hz);
lapictimer.hz = hz;
}
assert(lapictimer.hz != 0);
lapictimer.div = hz/lapictimer.hz;
}
}
void
lapicinit(Apic* apic)
{
ulong dfr, ldr, lvt;
if(lapicbase == 0)
lapicbase = apic->addr;
if(lapicbase == 0) {
print("lapicinit: no lapic\n");
return;
}
if(strncmp(m->cpuidid, "AuthenticAMD", 12) == 0)
dfr = 0xf0000000;
else
dfr = 0xffffffff;
ldr = 0x00000000;
lapicw(LapicDFR, dfr);
lapicw(LapicLDR, ldr);
lapicw(LapicTPR, 0xff);
lapicw(LapicSVR, LapicENABLE|(VectorPIC+IrqSPURIOUS));
lapictimerinit();
switch(m->cpuidax & 0xFFF){
case 0x526:
case 0x52B:
case 0x52C:
wrmsr(0x0E, 1<<14);
break;
}
lapiceoi(0);
lvt = (lapicr(LapicVER)>>16) & 0xFF;
if(lvt >= 4)
lapicw(LapicPCINT, ApicIMASK|(VectorPIC+IrqPCINT));
lapicw(LapicERROR, VectorPIC+IrqERROR);
lapicw(LapicESR, 0);
lapicr(LapicESR);
lapicw(LapicICRHI, 0);
lapicw(LapicICRLO, LapicALLINC|ApicLEVEL|LapicDEASSERT|ApicINIT);
while(lapicr(LapicICRLO) & ApicDELIVS)
;
}
void
lapicstartap(Apic* apic, int v)
{
int i;
ulong crhi;
crhi = apic->apicno<<24;
lapicw(LapicICRHI, crhi);
lapicw(LapicICRLO, LapicFIELD|ApicLEVEL|LapicASSERT|ApicINIT);
microdelay(200);
lapicw(LapicICRLO, LapicFIELD|ApicLEVEL|LapicDEASSERT|ApicINIT);
delay(10);
for(i = 0; i < 2; i++){
lapicw(LapicICRHI, crhi);
lapicw(LapicICRLO, LapicFIELD|ApicEDGE|ApicSTARTUP|(v/BY2PG));
microdelay(200);
}
}
void
lapicerror(Ureg*, void*)
{
ulong esr;
lapicw(LapicESR, 0);
esr = lapicr(LapicESR);
switch(m->cpuidax & 0xFFF){
case 0x526:
case 0x52B:
case 0x52C:
return;
}
print("cpu%d: lapicerror: 0x%8.8luX\n", m->machno, esr);
}
void
lapicspurious(Ureg*, void*)
{
print("cpu%d: lapicspurious\n", m->machno);
}
int
lapicisr(int v)
{
ulong isr;
isr = lapicr(LapicISR + (v/32));
return isr & (1<<(v%32));
}
int
lapiceoi(int v)
{
lapicw(LapicEOI, 0);
return v;
}
void
lapicicrw(ulong hi, ulong lo)
{
lapicw(LapicICRHI, hi);
lapicw(LapicICRLO, lo);
}
void
ioapicrdtr(Apic* apic, int sel, int* hi, int* lo)
{
ulong *iowin;
iowin = apic->addr+(0x10/sizeof(ulong));
sel = IoapicRDT + 2*sel;
lock(apic);
*apic->addr = sel+1;
if(hi)
*hi = *iowin;
*apic->addr = sel;
if(lo)
*lo = *iowin;
unlock(apic);
}
void
ioapicrdtw(Apic* apic, int sel, int hi, int lo)
{
ulong *iowin;
iowin = apic->addr+(0x10/sizeof(ulong));
sel = IoapicRDT + 2*sel;
lock(apic);
*apic->addr = sel+1;
*iowin = hi;
*apic->addr = sel;
*iowin = lo;
unlock(apic);
}
void
ioapicinit(Apic* apic, int apicno)
{
int hi, lo, v;
ulong *iowin;
iowin = apic->addr+(0x10/sizeof(ulong));
lock(apic);
*apic->addr = IoapicVER;
apic->mre = (*iowin>>16) & 0xFF;
*apic->addr = IoapicID;
*iowin = apicno<<24;
unlock(apic);
hi = 0;
lo = ApicIMASK;
for(v = 0; v <= apic->mre; v++)
ioapicrdtw(apic, v, hi, lo);
}
void
lapictimerset(uvlong next)
{
vlong period;
int x;
x = splhi();
lock(&m->apictimerlock);
period = lapictimer.max;
if(next != 0){
period = next - fastticks(nil);
if (lapictimer.div == 0)
panic("lapictimerset: zero lapictimer.div");
period /= lapictimer.div;
if(period < lapictimer.min)
period = lapictimer.min;
else if(period > lapictimer.max - lapictimer.min)
period = lapictimer.max;
}
lapicw(LapicTICR, period);
unlock(&m->apictimerlock);
splx(x);
}
void
lapicclock(Ureg *u, void*)
{
mtrrclock();
timerintr(u, 0);
}
void
lapicintron(void)
{
lapicw(LapicTPR, 0);
}
void
lapicintroff(void)
{
lapicw(LapicTPR, 0xFF);
}
void
lapicnmienable(void)
{
if (lapicbase)
lapicw(LapicPCINT, ApicNMI|(VectorPIC+IrqPCINT));
else
print("lapicnmienable: no lapic\n");
}
void
lapicnmidisable(void)
{
if (lapicbase)
lapicw(LapicPCINT, ApicIMASK|(VectorPIC+IrqPCINT));
else
print("lapicnmidisable: no lapic\n");
}