#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
#include	"flashif.h"
#define FLASHMEM	0xfff80000
#define FLASHPGSZ	0x40000
#define FLASHBKSZ	(FLASHPGSZ>>2)
#define LOG2FPGSZ	18
#define FLASHEND	(FLASHMEM+FLASHPGSZ)
#define SYSREG0	0x78
#define SYSREG1	0x878
enum {
DQ7 = 0x80,
DQ6 = 0x40,
DQ5 = 0x20,
DQ4 = 0x10,
DQ3 = 0x08,
DQ2 = 0x04,
DQ1 = 0x02,
DQ0 = 0x01,
};
enum {
FLRDM = 0xFF,
FLWTM = 0x10,
FLCLR = 0x50,
FLBE1 = 0x20,
FLBE2 = 0xD0,
FLRSR = 0x70,
FLDID = 0x90,
};
#define	DPRINT	if(0)print
#define	EPRINT	if(1)print
static int
zpcwait(uchar *p, ulong ticks)
{
uchar csr;
ticks += m->ticks+1;
while((*p & DQ7) != DQ7){
sched();
if(m->ticks >= ticks){
EPRINT("flash: timed out: %8.8lux\n", (ulong)*p);
return -1;
}
}
csr = *p;
if(csr & (DQ5|DQ4|DQ3)){
EPRINT("flash: DQ5 error: %8.8lux %8.8lux\n", p, (ulong)csr);
return 0;
}
return 1;
}
static int
eraseall(Flash *f)
{
uchar r;
uchar *p;
int i, j, s;
DPRINT("flash: erase all\n");
for (i = 0; i < 8; i++) {
r = inb(SYSREG0);
r &= 0x8f;
r |= i<<4;
outb(SYSREG0, r);
p = (uchar *)f->addr;
for (j = 0; j < 4; j++) {
DPRINT("erasing page %d block %d addr %lux\n", i, j, p);
s = splhi();
*p = FLBE1;
*p = FLBE2;
splx(s);
if(zpcwait(p, MS2TK(16*1000)) <= 0){
*p = FLCLR;
*p = FLRDM;
f->unusable = ~0;
return -1;
}
*p = FLCLR;
*p = FLRDM;
p += FLASHPGSZ>>2;
}
}
return 0;
}
static int
erasezone(Flash *f, int zone)
{
uchar r;
uchar *p;
int s, pg, blk;
DPRINT("flash: erase zone %d\n", zone);
if(zone & ~31) {
EPRINT("flash: bad erasezone %d\n", zone);
return -1;
}
pg = zone>>2;
blk = zone&3;
r = inb(SYSREG0);
r &= 0x8f;
r |= pg<<4;
outb(SYSREG0, r);
p = (uchar *)f->addr + blk*(FLASHPGSZ>>2);
DPRINT("erasing zone %d pg %d blk %d addr %lux\n", zone, pg, blk, p);
s = splhi();
*p = FLBE1;
*p = FLBE2;
splx(s);
if(zpcwait(p, MS2TK(8*1000)) <= 0){
*p = FLCLR;
*p = FLRDM;
f->unusable |= 1<<zone;
return -1;
}
*p = FLCLR;
*p = FLRDM;
return 0;
}
static int
readx(Flash *f, ulong offset, void *buf, long n)
{
uchar r;
ulong pg, o;
long m;
uchar *p = buf;
pg = offset>>LOG2FPGSZ;
o = offset&(FLASHPGSZ-1);
while (n > 0) {
if (pg < 0 || pg > 7) {
EPRINT("flash: bad read %ld %ld\n", offset, n);
return -1;
}
r = inb(SYSREG0);
r &= 0x8f;
r |= pg<<4;
outb(SYSREG0, r);
if (o+n > FLASHPGSZ)
m = FLASHPGSZ-o;
else
m = n;
DPRINT("flash: read page %ld offset %lux buf %lux n %ld\n", pg, o, p-(uchar*)buf, m);
memmove(p, (uchar *)f->addr + o, m);
p += m;
n -= m;
pg++;
o = 0;
}
return 0;
}
static int
writex(Flash *f, ulong offset, void *buf, long n)
{
int i, s;
uchar r;
ulong pg, o;
long m;
uchar *a, *v = buf;
DPRINT("flash: writex\n");
pg = offset>>LOG2FPGSZ;
o = offset&(FLASHPGSZ-1);
while (n > 0) {
if (pg < 0 || pg > 7) {
EPRINT("flash: bad write %ld %ld\n", offset, n);
return -1;
}
r = inb(SYSREG0);
r &= 0x8f;
r |= pg<<4;
outb(SYSREG0, r);
if (o+n > FLASHPGSZ)
m = FLASHPGSZ-o;
else
m = n;
a = (uchar *)f->addr + o;
DPRINT("flash: write page %ld offset %lux buf %lux n %ld\n", pg, o, v-(uchar*)buf, m);
for (i = 0; i < m; i++, v++, a++) {
if (~*a & *v) {
EPRINT("flash: bad write: %lux %lux -> %lux\n", (ulong)a, (ulong)*a, (ulong)*v);
return -1;
}
if (*a == *v)
continue;
s = splhi();
*a = FLWTM;
*a = *v;
splx(s);
microdelay(8);
if(zpcwait(a, 5) <= 0){
*a = FLCLR;
*a = FLRDM;
f->unusable = ~0;
return -1;
}
*a = FLCLR;
*a = FLRDM;
if (*a != *v) {
EPRINT("flash: write %lux %lux -> %lux failed\n", (ulong)a, (ulong)*a, (ulong)*v);
return -1;
}
}
n -= m;
pg++;
o = 0;
}
return 0;
}
#ifdef ZERO
static void
flashsearch(Flash *f)
{
int d, m, p, b, n, i;
uchar r, buf[64];
for (d = 0; d < 2; d++) {
r = inb(SYSREG0);
r &= 0xfb;
r |= (d<<2);
outb(SYSREG0, r);
for (m = 0; m < 2; m++) {
if (m == 0)
f->addr = (void *)FLASHMEM;
else
f->addr = (void *)FLASHEND;
for (p = 0; p < 8; p++) {
for (b = 0; b < 4; b++) {
n = readx(f, (4*p+b)*FLASHBKSZ, buf, 64);
if (n != 0) {
print("bad read in search %d\n", n);
goto end;
}
print("%d %d %d %d : ", d, m, p, b);
if (buf[0] == 0x5a && buf[1] == 0x54) {
n = 0;
for (i = 0; i < 64; i++) {
if (buf[i] == 0xff)
n++;
}
if (n == 64-28)
print("un");
print("used dos\n");
}
else if (buf[0] == 0x55 && buf[1] == 0xaa)
print("bios start\n");
else
print("bios ?\n");
}
}
}
}
end:
r = inb(SYSREG0);
r |= 4;
outb(SYSREG0, r);
f->addr = (void *)FLASHMEM;
}
#endif
static int
reset(Flash *f)
{
uchar r;
int s;
ulong pa;
Pcidev *bridge;
bridge = pcimatch(nil, 0x8086, 0x7000);
if (bridge == nil) {
EPRINT("flash : failed to find bridge device\n");
return 1;
}
s = splhi();
r = pcicfgr8(bridge, 0x4e);
r |= 0x84;
pcicfgw8(bridge, 0x4e, r);
splx(s);
r = inb(SYSREG0);
r |= 0x86;
outb(SYSREG0, r);
pa = mmukmap(FLASHMEM, FLASHMEM, FLASHPGSZ);
if (pa != FLASHEND) {
EPRINT("failed to map flash memory");
return 1;
}
f->id = 0x0089;
f->devid = 0x66a0;
f->read = readx;
f->write = writex;
f->eraseall = eraseall;
f->erasezone = erasezone;
f->suspend = nil;
f->resume = nil;
f->width = 1;
f->erasesize = 64*1024;
*(uchar*)f->addr = FLCLR;
*(uchar*)f->addr = FLRDM;
return 0;
}
void
flashzpclink(void)
{
addflashcard("DD28F032SA", reset);
}