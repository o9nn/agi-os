#include <u.h>
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "fs.h"
typedef uvlong Devbytes, Devsects;
typedef struct Biosdrive Biosdrive;
typedef struct Biosdev Biosdev;
enum {
Debug = 0,
Maxdevs = 4,
CF = 1,
Flopid = 0,
Baseid = 0x80,
Biosinit	= 0,
Biosdrvsts,
Bioschsrdsects,
Biosdrvparam	= 8,
Biosctlrinit,
Biosreset	=  0xd,
Biosdrvrdy	= 0x10,
Biosdrvtype	= 0x15,
Biosckext	= 0x41,
Biosrdsect,
Biosedrvparam	= 0x48,
Typenone = 0,
Typedisk = 3,
};
struct Biosdrive {
int	ndevs;
};
struct Biosdev {
Devbytes size;
Devbytes offset;
uchar	id;
char	type;
ushort	sectsz;
};
typedef struct Extread {
uchar	size;
uchar	unused1;
uchar	nsects;
uchar	unused2;
ulong	addr;
uvlong	stsect;
} Extread;
typedef struct Edrvparam {
ushort	size;
ushort	flags;
ulong	physcyls;
ulong	physheads;
ulong	phystracksects;
uvlong	physsects;
ushort	sectsz;
void	*dpte;
ushort	key;
uchar	dpilen;
uchar	unused1;
ushort	unused2;
char	bustype[4];
char	ifctype[8];
uvlong	ifcpath;
uvlong	devpath;
uchar	unused3;
uchar	dpicksum;
} Edrvparam;
void	realmode(int intr, Ureg *ureg);
int onlybios0;
int biosinited;
static Biosdev bdev[Maxdevs];
static Biosdrive bdrive;
static Ureg regs;
static int	dreset(uchar drive);
static Devbytes	extgetsize(Biosdev *);
static Devsects	getsize(uchar drive, char *type);
static int	islba(uchar drive);
static int
biosdiskcall(Ureg *rp, uchar op, ulong bx, ulong dx, ulong si)
{
memset(rp, 0, sizeof *rp);
rp->ax = op << 8;
rp->bx = bx;
rp->dx = dx;
rp->si = si;
realmode(0x13, rp);
if (rp->flags & CF) {
return -1;
}
return 0;
}
int
biosinit(void)
{
int devid, lba, mask, lastbit;
Devbytes size;
char type;
Biosdev *bdp;
static int beenhere;
mask = lastbit = 0;
if (beenhere)
return mask;
beenhere = 1;
if (pxe || getconf("*nobiosload") != nil || onlybios0 || !biosinited)
return mask;
for (devid = 0; devid < (1 << 8) && bdrive.ndevs < Maxdevs; devid++) {
lba = islba(devid);
if(!lba  )
continue;
type = Typedisk;
if (getsize(devid, &type) == 0) {
devid &= ~0xf;
devid += 0x10;
devid--;
continue;
}
lastbit = 1 << bdrive.ndevs;
mask |= lastbit;
bdp = &bdev[bdrive.ndevs];
bdp->id = devid;
bdp->type = type;
size = extgetsize(bdp);
bdp->size = size;
print("bios%d: drive 0x%ux: %llud bytes, type %d\n",
bdrive.ndevs, devid, size, type);
bdrive.ndevs++;
}
if (bdrive.ndevs > 0) {
if (bdrive.ndevs == 1) {
print("biosinit: sorry, only one bios drive; "
"can't read last one\n");
onlybios0 = 1;
} else
biosinited = 1;
bdrive.ndevs--;
mask &= ~lastbit;
}
return mask;
}
void
biosinitdev(int i, char *name)
{
if(i >= bdrive.ndevs)
panic("biosinitdev");
sprint(name, "bios%d", i);
}
void
biosprintdevs(int i)
{
if(i >= bdrive.ndevs){
print("got a print for %d, only got %d\n", i, bdrive.ndevs);
panic("biosprintdevs");
}
print(" bios%d", i);
}
int
biosboot(int dev, char *file, Boot *b)
{
Fs *fs;
if(strncmp(file, "dos!", 4) == 0)
file += 4;
if(strchr(file, '!') != nil || strcmp(file, "") == 0) {
print("syntax is bios0!file\n");
return -1;
}
fs = biosgetfspart(dev, "9fat", 1);
if(fs == nil)
return -1;
return fsboot(fs, file, b);
}
long
sectread(Biosdev *bdp, void *a, long n, Devsects offset)
{
uchar *biosparam, *cp;
Extread *erp;
if(n < 0 || n > bdp->sectsz)
return -1;
if(Debug)
memset((uchar *)BIOSXCHG, 'r', bdp->sectsz);
biosdiskcall(&regs, Biosdrvrdy, 0, bdp->id, 0);
biosparam = (uchar *)BIOSXCHG + 2*1024;
erp = (Extread *)biosparam;
memset(erp, 0, sizeof *erp);
erp->size = sizeof *erp;
erp->nsects = 1;
erp->addr = PADDR(BIOSXCHG);
erp->stsect = offset;
if (biosdiskcall(&regs, Biosrdsect, 0, bdp->id, PADDR(erp)) < 0) {
print("sectread: bios failed to read %ld @ sector %lld of 0x%ux\n",
n, offset, bdp->id);
return -1;
}
memmove(a, (char *)BIOSXCHG, n);
if(Debug){
cp = (uchar *)BIOSXCHG;
print("-%ux %ux %ux %ux--%16.16s-\n",
cp[0], cp[1], cp[2], cp[3], (char *)cp + 480);
}
return n;
}
static int
dreset(uchar drive)
{
if (0) {
print("devbios: resetting disk controllers...");
biosdiskcall(&regs, Biosinit, 0, drive, 0);
print("\n");
}
return regs.ax? -1: 0;
}
static int
islba(uchar drive)
{
if (biosdiskcall(&regs, Biosckext, 0x55aa, drive, 0) < 0)
return 0;
if(regs.bx != 0xaa55){
print("islba: buggy bios\n");
return 0;
}
if (Debug)
print("islba: drive 0x%ux extensions version %d.%d cx 0x%lux\n",
drive, (uchar)(regs.ax >> 8),
(uchar)regs.ax, regs.cx);
return regs.cx & 1;
}
static Devsects
getsize(uchar id, char *typep)
{
int dtype;
if (biosdiskcall(&regs, Biosdrvtype, 0x55aa, id, 0) < 0)
return 0;
dtype = (ushort)regs.ax >> 8;
if(dtype == Typenone){
print("no such device 0x%ux of type %d\n", id, dtype);
return 0;
}
if(dtype != Typedisk){
print("non-disk device 0x%ux of type %d\n", id, dtype);
return 0;
}
*typep = dtype;
return (ushort)regs.cx | regs.dx << 16;
}
static Devbytes
extgetsize(Biosdev *bdp)
{
Edrvparam *edp;
edp = (Edrvparam *)BIOSXCHG;
memset(edp, 0, sizeof *edp);
edp->size = sizeof *edp;
edp->dpilen = 36;
if (biosdiskcall(&regs, Biosedrvparam, 0, bdp->id, PADDR(edp)) < 0)
return 0;
if(Debug) {
print("extgetsize: drive 0x%ux info flags 0x%ux",
bdp->id, edp->flags);
if (edp->key == 0xbedd)
print(" %s %s", edp->bustype, edp->ifctype);
print("\n");
}
if (edp->sectsz <= 0) {
print("extgetsize: drive 0x%ux: non-positive sector size\n",
bdp->id);
edp->sectsz = 1;
}
bdp->sectsz = edp->sectsz;
return edp->physsects * edp->sectsz;
}
long
biosread(Fs *fs, void *a, long n)
{
int want, got, part;
long totnr, stuck;
Devbytes offset;
Biosdev *bdp;
if(fs->dev > bdrive.ndevs)
return -1;
if (n <= 0)
return n;
bdp = &bdev[fs->dev];
offset = bdp->offset;
stuck = 0;
for (totnr = 0; totnr < n && stuck < 4; totnr += got) {
want = bdp->sectsz;
if (totnr + want > n)
want = n - totnr;
if(Debug)
print("bios%d, read: %ld @ off %lld, want: %d, id: 0x%ux\n",
fs->dev, n, offset, want, bdp->id);
part = offset % bdp->sectsz;
if (part != 0) {
offset -= part;
totnr  -= part;
if (totnr < 0) {
print("biosread: negative count %ld\n", totnr);
return -1;
}
}
if ((vlong)offset < 0) {
print("biosread: negative offset %lld\n", offset);
return -1;
}
got = sectread(bdp, (char *)a + totnr, want, offset/bdp->sectsz);
if(got <= 0){
return -1;
}
offset += got;
bdp->offset = offset;
if (got < bdp->sectsz)
stuck++;
else
stuck = 0;
}
return totnr;
}
vlong
biosseek(Fs *fs, vlong off)
{
if (off < 0) {
print("biosseek(fs, %lld) is illegal\n", off);
return -1;
}
if(fs->dev > bdrive.ndevs) {
print("biosseek: fs->dev %d > bdrive.ndevs %d\n",
fs->dev, bdrive.ndevs);
return -1;
}
bdev[fs->dev].offset = off;
return off;
}
void *
biosgetfspart(int i, char *name, int chatty)
{
static Fs fs;
if(strcmp(name, "9fat") != 0){
if(chatty)
print("unknown partition bios%d!%s (use bios%d!9fat)\n",
i, name, i);
return nil;
}
fs.dev = i;
fs.diskread = biosread;
fs.diskseek = biosseek;
if(dosinit(&fs) < 0){
if(chatty)
print("bios%d!%s does not contain a FAT file system\n",
i, name);
return nil;
}
return &fs;
}