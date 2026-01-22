#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"pool.h"
#include	"../port/error.h"
#include	"../port/netif.h"
#include	"../port/sd.h"
#include	"dosfs.h"
#define TYPE(q)		((ulong)(q).path & 0xf)
#define UNIT(q)		(((ulong)(q).path>>4) & 0xff)
#define L(q)		(((ulong)(q).path>>12) & 0xf)
#define QID(u, t) 	((u)<<4 | (t))
typedef struct Biosdev Biosdev;
typedef struct Dap Dap;
typedef uvlong Devbytes, Devsects;
typedef uchar Devid;
typedef struct Edrvparam Edrvparam;
enum {
Debug = 0,
Pause = 0,
Minsectsz	= 512,
Maxsectsz	= 2048,
Highshort	= ((1ul<<16) - 1) << 16,
Maxdevs		= 8,
CF		= 1,
Flopid		= 0,
Baseid		= 0x80,
Diskint		= 0x13,
Fixeddisk	= 1<<0,
Drlock		= 1<<1,
Edd		= 1<<2,
Bit64ext	= 1<<3,
Biosinit	= 0,
Biosdrvsts,
Biosdrvparam	= 8,
Biosctlrinit,
Biosreset	=  0xd,
Biosdrvrdy	= 0x10,
Biosckext	= 0x41,
Biosrdsect,
Biosedrvparam	= 0x48,
Imok		= 0x55aa,
Youreok		= 0xaa55,
};
enum {
Qzero,
Qtopdir		= 1,
Qtopbase,
Qtopctl		= Qtopbase,
Qtopend,
Qunitdir,
Qunitbase,
Qctl		= Qunitbase,
Qdata,
Qtopfiles	= Qtopend-Qtopbase,
};
struct Biosdev {
Devbytes size;
Devbytes offset;
Devid	id;
ushort	sectsz;
Chan	*rootchan;
Bootfs;
};
struct Dap {
uchar	size;
uchar	_unused1;
uchar	nsects;
uchar	_unused2;
union {
ulong	addr;
struct {
ushort	addroff;
ushort	addrseg;
};
};
uvlong	stsect;
uvlong	addr64;
ulong	lnsects;
ulong	_unused3;
};
struct Edrvparam {
ushort	size;
ushort	flags;
ulong	physcyls;
ulong	physheads;
ulong	phystracksects;
uvlong	physsects;
ushort	sectsz;
ushort	dpteoff;
ushort	dpteseg;
ushort	key;
uchar	dpilen;
uchar	_unused1;
ushort	_unused2;
char	bustype[4];
char	ifctype[8];
uvlong	ifcpath;
uvlong	devpath[2];
uchar	_unused3;
uchar	dpicksum;
};
int biosinited;
int biosndevs;
void *biosgetfspart(int i, char *name, int chatty);
static Biosdev bdev[Maxdevs];
static Ureg regs;
static RWlock devs;
static int	dreset(Devid drive);
static Devbytes	extgetsize(Biosdev *);
static int	drivecap(Devid drive);
static char *
strerr(uchar err)
{
switch (err) {
case 0:
return "no error";
case 1:
return "bad command";
case 0x80:
return "disk timeout";
default:
return "unknown";
}
}
static void
assertlow64k(uintptr p, char *tag)
{
if (p & Highshort)
panic("devbios: %s address %#p not in bottom 64k", tag, p);
}
static void
initrealregs(Ureg *ureg)
{
memset(ureg, 0, sizeof *ureg);
}
static int
biosdiskcall(Ureg *ureg, uchar op, ulong bx, ulong dx, ulong si)
{
int s;
uchar err;
s = splhi();
initrealregs(ureg);
ureg->ax = op << 8;
ureg->bx = bx;
ureg->dx = dx;
assertlow64k(si, "dap");
if(si && (si & Highshort) != ((si + Maxsectsz - 1) & Highshort))
print("biosdiskcall: dap address %#lux too near segment boundary\n",
si);
ureg->si = si;
ureg->ds = 0;
ureg->es = 0;
ureg->di = 0;
ureg->flags = 0;
ureg->trap = Diskint;
realmode(ureg);
if (ureg->flags & CF) {
if (dx == Baseid) {
err = ureg->ax >> 8;
print("\nbiosdiskcall: int %#x op %#ux drive %#lux "
"failed, ah error code %#ux (%s)\n",
Diskint, op, dx, err, strerr(err));
}
splx(s);
return -1;
}
splx(s);
return 0;
}
int
biosinit0(void)
{
int cap, mask, lastbit, ndrive;
Devbytes size;
Devid devid;
Biosdev *bdp;
static int beenhere;
delay(Pause);
if (biosinited || beenhere)
return 0;
beenhere = 1;
ndrive = *(uchar *)KADDR(0x475);
if (Debug)
print("%d bios drive(s)\n", ndrive);
mask = lastbit = 0;
for (devid = Baseid, biosndevs = 0; devid != 0 && biosndevs < Maxdevs &&
biosndevs < ndrive; devid++) {
cap = drivecap(devid);
if(cap < 0 || (cap & (Fixeddisk|Edd)) != (Fixeddisk|Edd)
)
continue;
lastbit = 1 << biosndevs;
mask |= lastbit;
bdp = &bdev[biosndevs];
bdp->id = devid;
size = extgetsize(bdp);
if (size == 0)
continue;
bdp->size = size;
print("bios%d: drive %#ux: %,llud bytes, %d-byte sectors\n",
biosndevs, devid, size, bdp->sectsz);
biosndevs++;
}
USED(lastbit);
if (Debug && ndrive != biosndevs)
print("devbios: expected %d drives, found %d\n", ndrive, biosndevs);
if (biosndevs > 0)
biosinited = 1;
else
panic("devbios: no bios drives seen");
delay(Pause);
return mask;
}
static void
biosreset(void)
{
biosinit0();
}
static void
biosinit(void)
{
}
static Chan*
biosattach(char *spec)
{
ulong drive;
char *p;
Chan *chan;
drive = 0;
if(spec && *spec){
drive = strtoul(spec, &p, 0);
if((drive == 0 && p == spec) || *p || (drive >= Maxdevs))
error(Ebadarg);
}
if(bdev[drive].rootchan)
return bdev[drive].rootchan;
chan = devattach(L'☹', spec);
if(waserror()){
chanfree(chan);
nexterror();
}
chan->dev = drive;
bdev[drive].rootchan = chan;
poperror();
return chan;
}
static int
unitgen(Chan *c, ulong type, Dir *dp)
{
int perm, t;
ulong vers;
vlong size;
char *p;
Qid q;
perm = 0644;
size = 0;
vers = 0;
t = QTFILE;
switch(type){
default:
return -1;
case Qctl:
p = "ctl";
break;
case Qdata:
p = "data";
perm = 0640;
break;
}
mkqid(&q, QID(UNIT(c->qid), type), vers, t);
devdir(c, q, p, size, eve, perm, dp);
return 1;
}
static int
topgen(Chan *c, ulong type, Dir *d)
{
int perm;
vlong size;
char *p;
Qid q;
size = 0;
switch(type){
default:
return -1;
case Qdata:
p = "data";
perm = 0644;
break;
}
mkqid(&q, type, 0, QTFILE);
devdir(c, q, p, size, eve, perm, d);
return 1;
}
static int
biosgen(Chan *c, char *, Dirtab *, int, int s, Dir *dp)
{
Qid q;
if(c->qid.path == 0){
switch(s){
case DEVDOTDOT:
q.path = 0;
q.type = QTDIR;
devdir(c, q, "#☹", 0, eve, 0555, dp);
break;
case 0:
q.path = Qtopdir;
q.type = QTDIR;
devdir(c, q, "bios", 0, eve, 0555, dp);
break;
default:
return -1;
}
return 1;
}
switch(TYPE(c->qid)){
default:
return -1;
case Qtopdir:
if(s == DEVDOTDOT){
mkqid(&q, Qzero, 0, QTDIR);
devdir(c, q, "bios", 0, eve, 0555, dp);
return 1;
}
if(s < Qtopfiles)
return topgen(c, Qtopbase + s, dp);
s -= Qtopfiles;
if(s >= 1)
return -1;
mkqid(&q, QID(s, Qunitdir), 0, QTDIR);
devdir(c, q, "bios", 0, eve, 0555, dp);
return 1;
case Qdata:
return unitgen(c, TYPE(c->qid), dp);
}
}
static Walkqid*
bioswalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, nil, 0, biosgen);
}
static int
biosstat(Chan *c, uchar *db, int n)
{
return devstat(c, db, n, nil, 0, biosgen);
}
static Chan*
biosopen(Chan *c, int omode)
{
return devopen(c, omode, 0, 0, biosgen);
}
static void
biosclose(Chan *)
{
}
#ifdef UNUSED
int
biosboot(int dev, char *file, Boot *b)
{
Bootfs *fs;
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
#endif
long
sectread(Biosdev *bdp, void *a, long n, Devsects offset)
{
uchar *xch;
uintptr xchaddr;
Dap *dap;
if(bdp->sectsz <= 0 || n < 0 || n > bdp->sectsz)
return -1;
xch = (uchar *)BIOSXCHG;
assertlow64k(PADDR(xch), "biosxchg");
if(Debug)
memset(xch, 'r', bdp->sectsz);
dap = (Dap *)(xch + Maxsectsz);
assertlow64k(PADDR(dap), "Dap");
memset(dap, 0, sizeof *dap);
dap->size = sizeof *dap;
dap->nsects = 1;
dap->stsect = offset;
xchaddr = PADDR(xch);
assertlow64k(xchaddr, "sectread buffer");
dap->addr = xchaddr;
dap->addroff = xchaddr;
dap->addrseg = 0;
dap->addr64 = xchaddr;
dap->lnsects = 1;
if((dap->addr & Highshort) !=
((dap->addr + Minsectsz - 1) & Highshort))
print("devbios: sectread: address %#lux too near seg boundary\n",
dap->addr);
if (Debug)
print("reading bios drive %#ux sector %lld -> %#lux...",
bdp->id, offset, dap->addr);
delay(Pause);
if (biosdiskcall(&regs, Biosrdsect, 0, bdp->id, PADDR(dap)) < 0) {
print("devbios: sectread: bios failed to read %ld @ sector %lld of %#ux\n",
n, offset, bdp->id);
return -1;
}
if (dap->nsects != 1)
panic("devbios: sector read ok but read %d sectors",
dap->nsects);
if (Debug)
print("OK\n");
memmove(a, xch, n);
if(0 && Debug)
print("-%ux %ux %ux %ux--%16.16s-\n",
xch[0], xch[1], xch[2], xch[3], (char *)xch + 480);
delay(Pause);
return n;
}
static int
dreset(Devid drive)
{
print("devbios: resetting %#ux...", drive);
biosdiskcall(&regs, Biosinit, 0, drive, 0);
print("\n");
return regs.ax? -1: 0;
}
static int
drivecap(Devid drive)
{
int cap;
if (biosdiskcall(&regs, Biosckext, Imok, drive, 0) < 0)
return -1;
if(regs.bx != Youreok){
print("devbios: buggy bios: drive %#ux extension check "
"returned %lux in bx\n", drive, regs.bx);
return -1;
}
cap = regs.cx;
if (Debug) {
print("bios drive %#ux extensions version %#x.%d cx %#ux\n",
drive, (uchar)(regs.ax >> 8), (uchar)regs.ax, cap);
if ((uchar)(regs.ax >> 8) < 0x30) {
print("drivecap: extensions prior to 0x30\n");
return -1;
}
print("\tsubsets supported:");
if (cap & Fixeddisk)
print(" fixed disk access;");
if (cap & Drlock)
print(" drive locking;");
if (cap & Edd)
print(" enhanced disk support;");
if (cap & Bit64ext)
print(" 64-bit extensions;");
print("\n");
}
delay(Pause);
return cap;
}
static Devbytes
extgetsize(Biosdev *bdp)
{
ulong sectsz;
Edrvparam *edp;
edp = (Edrvparam *)BIOSXCHG;
memset(edp, 0, sizeof *edp);
edp->size = sizeof *edp;
edp->dpteseg = edp->dpteoff = ~0;
edp->dpilen = 44;
if (biosdiskcall(&regs, Biosedrvparam, 0, bdp->id, PADDR(edp)) < 0)
return 0;
if(Debug) {
print("bios drive %#ux info flags %#ux", bdp->id, edp->flags);
if (edp->key == 0xbedd)
print("; edd 3.0  %.4s %.8s",
edp->bustype, edp->ifctype);
else
print("; NOT edd 3.0 compliant (key %#ux)", edp->key);
print("\n");
}
if (edp->sectsz <= 0) {
print("devbios: drive %#ux: sector size <= 0\n", bdp->id);
edp->sectsz = 1;
return 0;
}
sectsz = edp->sectsz;
if (sectsz > Maxsectsz) {
print("devbios: sector size %lud > %d\n", sectsz, Maxsectsz);
return 0;
}
bdp->sectsz = sectsz;
return edp->physsects * sectsz;
}
vlong
biossize(uint dev)
{
Biosdev *bdp;
if (dev >= biosndevs)
return -1;
bdp = &bdev[dev];
if (bdp->sectsz <= 0)
return -1;
return bdp->size / bdp->sectsz;
}
long
biossectsz(uint dev)
{
Biosdev *bdp;
if (dev >= biosndevs)
return -1;
bdp = &bdev[dev];
if (bdp->sectsz <= 0)
return -1;
return bdp->sectsz;
}
long
biosread0(Bootfs *fs, void *a, long n)
{
int want, got, part, dev;
long totnr, stuck;
Devbytes offset;
Biosdev *bdp;
dev = fs->dev;
if(dev > biosndevs)
return -1;
if (n <= 0)
return n;
bdp = &bdev[dev];
offset = bdp->offset;
stuck = 0;
for (totnr = 0; totnr < n && stuck < 4; totnr += got) {
if (bdp->sectsz == 0) {
print("devbios: zero sector size\n");
return -1;
}
want = bdp->sectsz;
if (totnr + want > n)
want = n - totnr;
if(0 && Debug && debugload)
print("bios%d, read: %ld @ off %lld, want: %d, id: %#ux\n",
dev, n, offset, want, bdp->id);
part = offset % bdp->sectsz;
if (part != 0) {
offset -= part;
totnr  -= part;
if (totnr < 0) {
print("biosread0: negative count %ld\n", totnr);
return -1;
}
}
if ((vlong)offset < 0) {
print("biosread0: negative offset %lld\n", offset);
return -1;
}
got = sectread(bdp, (char *)a + totnr, want,
offset / bdp->sectsz);
if(got <= 0)
return -1;
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
biosseek(Bootfs *fs, vlong off)
{
if (off < 0) {
print("biosseek(fs, %lld) is illegal\n", off);
return -1;
}
if(fs->dev > biosndevs) {
print("biosseek: fs->dev %d > biosndevs %d\n", fs->dev, biosndevs);
return -1;
}
bdev[fs->dev].offset = off;
return off;
}
static long
biosread(Chan *c, void *db, long n, vlong off)
{
Biosdev *bp;
switch(TYPE(c->qid)){
default:
error(Eperm);
case Qzero:
case Qtopdir:
return devdirread(c, db, n, 0, 0, biosgen);
case Qdata:
bp = &bdev[UNIT(c->qid)];
if (bp->rootchan == nil)
panic("biosread: nil root chan for bios%ld",
UNIT(c->qid));
biosseek(&bp->Bootfs, off);
return biosread0(&bp->Bootfs, db, n);
}
}
void *
biosgetfspart(int i, char *name, int chatty)
{
static Bootfs fs;
if(strcmp(name, "9fat") != 0){
if(chatty)
print("unknown partition bios%d!%s (use bios%d!9fat)\n",
i, name, i);
return nil;
}
fs.dev = i;
fs.diskread = biosread0;
fs.diskseek = biosseek;
if(dosinit(&fs, "#S/sdB0/9fat") < 0){
if(chatty)
print("bios%d!%s does not contain a FAT file system\n",
i, name);
return nil;
}
return &fs;
}
static long
bioswrite(Chan *, void *, long, vlong)
{
error("bios devices are read-only in bootstrap");
return 0;
}
Dev biosdevtab = {
L'☹',
"bios",
biosreset,
biosinit,
devshutdown,
biosattach,
bioswalk,
biosstat,
biosopen,
devcreate,
biosclose,
biosread,
devbread,
bioswrite,
devbwrite,
devremove,
devwstat,
devpower,
devconfig,
};