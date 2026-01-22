#include "all.h"
#include "io.h"
enum {
SCSInone = SCSIread,
MAXDRIVE = 10,
MAXSIDE = 500,
TWORM = MINUTE(10),
THYSTER = SECOND(10),
Sectorsz = 512,
Jukemagic = 0xbabfece2,
};
typedef struct Side Side;
struct Side
{
QLock;
int elem;
int drive;
uchar status;
uchar rot;
int ord;
Timet time;
Timet stime;
long nblock;
long block;
long mult;
long max;
};
typedef struct Juke Juke;
struct Juke
{
QLock;
Side side[MAXSIDE];
int nside;
int ndrive;
Device* juke;
Device* drive[MAXDRIVE];
uchar offline[MAXDRIVE];
int isfixedsize;
long fixedsize;
int probeok;
Scsi* robot;
char* robotdir;
int mt0, nmt;
int se0, nse;
int ie0, nie;
int dt0, ndt;
int rot;
ulong magic;
Juke* link;
};
static Juke* jukelist;
enum
{
Sempty = 0,
Sunload,
Sstart,
};
static int bestdrive(Juke*, int);
static void element(Juke*, int);
static int mmove(Juke*, int, int, int, int);
static void shelves(void);
static int waitready(Juke *, Device*);
static int wormsense(Device*);
static Side* wormunit(Device*);
static void
newlabel(Device *d, Off labelblk, char *labelbuf, unsigned vord)
{
Label *label = (Label *)labelbuf;
memset(labelbuf, 0, RBUFSIZE);
label->magic = Labmagic;
label->ord = vord;
strncpy(label->service, service, sizeof label->service);
if (!okay("write new label"))
print("NOT writing new label\n");
else if (wormwrite(d, labelblk, labelbuf))
print("can't write new label on side %d\n", vord);
else
print("wrote new label on side %d\n", vord);
}
Side*
wormlabel(Device *d, Side *v)
{
int vord;
Off labelblk = v->max - 1;
char labelbuf[RBUFSIZE];
Label *label = (Label *)labelbuf;
Juke *w = d->private;
vord = v->ord;
qunlock(v);
memset(label, 0, sizeof *label);
if (wormread(d, labelblk, labelbuf)) {
print("error reading label block of side %d\n", vord);
newlabel(d, labelblk, labelbuf, vord);
} else if (label->magic != Labmagic) {
swab8(&label->magic);
if (label->magic == Labmagic) {
print(
"side %d's label magic byte-swapped; filsys should be configured with xD",
vord);
swab2(&label->ord);
} else {
print("bad magic number in label of side %d\n", vord);
newlabel(d, labelblk, labelbuf, vord);
}
}
qlock(v);
if (v->ord != vord)
panic("wormlabel: side %d switched ordinal to %d underfoot",
vord, v->ord);
if (label->ord != vord) {
print(
"labelled worm side %Z has wrong ordinal in label (%d, want %d)",
d, label->ord, vord);
qunlock(v);
cmd_wormreset(0, nil);
panic("wrong ordinal in label");
}
print("label %Z ordinal %d\n", d, v->ord);
qunlock(v);
if (w != d->private)
panic("wormlabel: w != %Z->private", d);
return wormunit(d);
}
static Side*
wormunit(Device *d)
{
int p, drive;
Device *dr;
Side *v;
Juke *w;
Dir *dir;
w = d->private;
if (w == nil)
panic("wormunit %Z nil juke", d);
if (w->magic != Jukemagic)
panic("bad magic in Juke for %Z", d);
p = d->wren.targ;
if(p < 0 || w && p >= w->nside) {
panic("wormunit: target %d out of range for %Z", p, d);
return 0;
}
v = &w->side[p];
qlock(v);
if(v->status == Sunload) {
for(;;) {
qlock(w);
drive = bestdrive(w, p);
if(drive >= 0)
break;
qunlock(w);
delay(100);
}
print("\tload   r%ld drive %Z\n", v-w->side, w->drive[drive]);
if(mmove(w, w->mt0, v->elem, w->dt0+drive, v->rot)) {
qunlock(w);
goto sbad;
}
v->drive = drive;
v->status = Sstart;
v->stime = toytime();
qunlock(w);
dr = w->drive[drive];
if (!waitready(w, dr))
goto sbad;
v->stime = toytime();
} else
dr = w->drive[v->drive];
if(v->status != Sstart) {
if(v->status == Sempty)
print("worm: unit empty %Z\n", d);
else
print("worm: not started %Z\n", d);
goto sbad;
}
v->time = toytime();
if(v->block)
return v;
if (dr->wren.file)
dr->wren.sddata = dataof(dr->wren.file);
else {
if (dr->wren.sddir == nil) {
if (dr->type == Devwren)
dr->wren.sddir = sdof(dr);
if (dr->wren.sddir == nil)
panic("wormunit: %Z for %Z not a wren", dr, d);
}
dr->wren.sddata = smprint("%s/data", dr->wren.sddir);
}
if (dr->wren.fd == 0)
dr->wren.fd = open(dr->wren.sddata, ORDWR);
if (dr->wren.fd < 0) {
print("wormunit: can't open %s for %Z: %r\n", dr->wren.sddata, d);
goto sbad;
}
v->block = inqsize(dr->wren.sddata);
if(v->block <= 0) {
print("\twormunit %Z block size %ld, setting to %d\n",
d, v->block, Sectorsz);
v->block = Sectorsz;
}
dir = dirfstat(dr->wren.fd);
v->nblock = dir->length / v->block;
free(dir);
v->mult = (RBUFSIZE + v->block - 1) / v->block;
v->max = (v->nblock + 1) / v->mult;
print("\tworm %Z: drive %Z (juke drive %d)\n",
d, w->drive[v->drive], v->drive);
print("\t\t%,ld %ld-byte sectors, ", v->nblock, v->block);
print("%,ld %d-byte blocks\n", v->max, RBUFSIZE);
print("\t\t%ld multiplier\n", v->mult);
if(d->type == Devlworm)
return wormlabel(d, v);
else
return v;
sbad:
qunlock(v);
return 0;
}
static int
waitready(Juke *w, Device *d)
{
int p, e, rv;
char *datanm;
if (w->magic != Jukemagic)
panic("waitready: bad magic in Juke (d->private) for %Z", d);
p = d->wren.targ;
if(p < 0 || p >= w->nside) {
print("waitready: target %d out of range for %Z\n", p, d);
return 0;
}
if (d->type == Devwren && d->wren.file)
datanm = strdup(d->wren.file);
else {
if (d->wren.sddir)
free(d->wren.sddir);
if (d->type == Devwren)
d->wren.sddir = sdof(d);
if (d->wren.sddir == nil)
panic("waitready: d->wren.sddir not set for %Z", d);
datanm = smprint("%s/data", d->wren.sddir);
}
rv = 0;
for(e=0; e < 100; e++) {
if (e == 10)
print("waitready: waiting for %s to exist\n", datanm);
if (access(datanm, AEXIST) >= 0) {
rv = 1;
break;
}
delay(200);
}
if (rv == 0)
print("waitready: %s for %Z didn't come ready\n", datanm, d);
free(datanm);
return rv;
}
static int
bestdrive(Juke *w, int side)
{
Side *v, *bv[MAXDRIVE];
int i, e, drive;
Timet t, t0;
loop:
for(i=0; i<w->ndt; i++)
bv[i] = 0;
v = &w->side[0];
for(i=0; i < w->nside; i++, v++)
if(v->status == Sstart) {
drive = v->drive;
if(drive >= 0 && drive < w->ndt)
bv[drive] = v;
}
e = w->side[side].elem;
t0 = toytime() - THYSTER;
t = t0;
drive = -1;
for(i=0; i<w->ndt; i++) {
v = bv[i];
if(v == 0) {
if(w->offline[i])
continue;
if(w->drive[i] != devnone) {
drive = i;
t = 0;
}
continue;
}
if(v->elem == e) {
drive = -1;
if(v->stime < t0)
drive = i;
break;
}
if(v->stime < t) {
drive = i;
t = v->stime;
}
}
if(drive >= 0) {
v = bv[drive];
if(v) {
qlock(v);
if(v->status != Sstart) {
qunlock(v);
goto loop;
}
print("\tunload r%ld drive %Z\n",
v-w->side, w->drive[drive]);
if(mmove(w, w->mt0, w->dt0+drive, v->elem, v->rot)) {
qunlock(v);
goto loop;
}
v->status = Sunload;
qunlock(v);
}
}
return drive;
}
Devsize
wormsize(Device *d)
{
Side *v;
Juke *w;
Devsize size;
w = d->private;
if (w->magic != Jukemagic)
print("wormsize: bad magic in Juke (d->private) for %Z\n", d);
if(w->isfixedsize && w->fixedsize != 0)
size = w->fixedsize;
else {
if (w != d->private)
panic("wormsize: w != %Z->private", d);
v = wormunit(d);
if(v == nil)
return 0;
size = v->max;
qunlock(v);
if(w->isfixedsize)
w->fixedsize = size;
}
if(d->type == Devlworm)
return size-1;
return size;
}
static Device *
devtojuke(Device *d, Device *top)
{
while (d != nil)
switch(d->type) {
default:
print("devtojuke: type of device %Z of %Z unknown\n",
d, top);
return nil;
case Devjuke:
case Devmcat:
case Devmlev:
case Devmirr:
return d;
case Devworm:
case Devlworm:
print("devtojuke: (l)worm %Z of %Z encountered\n",
d, top);
case Devwren:
return nil;
case Devcw:
d = d->cw.w;
break;
case Devro:
d = d->ro.parent;
break;
case Devfworm:
d = d->fw.fw;
break;
case Devpart:
d = d->part.d;
break;
case Devswab:
d = d->swab.d;
break;
}
return d;
}
static int
devisside(Device *d)
{
return d->type == Devworm || d->type == Devlworm;
}
static Device *
findside(Device *juke, int side, Device *top)
{
int i = 0;
Device *mcat = juke->j.m, *x;
Juke *w = juke->private;
for (x = mcat->cat.first; x != nil; x = x->link) {
if (!devisside(x)) {
print("wormsizeside: %Z of %Z of %Z type not (l)worm\n",
x, mcat, top);
return nil;
}
i = x->wren.targ;
if (i < 0 || i >= w->nside)
panic("wormsizeside: side %d in %Z out of range",
i, mcat);
if (i == side)
break;
}
if (x == nil)
return nil;
if (w->side[i].time == 0) {
print("wormsizeside: side %d not in jukebox %Z\n", i, juke);
return nil;
}
return x;
}
typedef struct {
int sleft;
int starget;
Device *topdev;
int sawjuke;
int sized;
} Visit;
static Off
visitsides(Device *d, Device *parentj, Visit *vp)
{
Off size = 0;
Device *x;
Juke *w;
d = devtojuke(d, vp->topdev);
if (d == nil || vp->sleft < 0)
return 0;
if (d->type == Devjuke) {
vp->sawjuke = 1;
w = d->private;
if (vp->sleft >= w->nside) {
vp->sleft -= w->nside;
return 0;
}
return visitsides(d->j.m, d, vp);
}
x = d->cat.first;
if (x == nil) {
print("visitsides: %Z of %Z: empty mcat\n", d, vp->topdev);
return 0;
}
if (!devisside(x)) {
for (; x != nil && !vp->sized; x = x->link)
size = visitsides(x, parentj, vp);
return size;
}
if (parentj == nil) {
print("visitsides: no parent juke for sides mcat %Z\n", d);
vp->sleft = -1;
return 0;
}
if (d != parentj->j.m)
panic("visitsides: mcat mismatch %Z vs %Z", d, parentj->j.m);
x = findside(parentj, vp->sleft, vp->topdev);
if (x == nil) {
vp->sleft = -1;
return 0;
}
vp->sleft = 0;
vp->sized = 1;
return wormsize(x);
}
Devsize
wormsizeside(Device *d, int side)
{
Devsize size;
Visit visit;
memset(&visit, 0, sizeof visit);
visit.starget = visit.sleft = side;
visit.topdev = d;
size = visitsides(d, nil, &visit);
if (visit.sawjuke && (visit.sleft != 0 || !visit.sized)) {
print("wormsizeside: fewer than %d sides in %Z\n", side, d);
return 0;
}
return size;
}
void
wormsidestarts(Device *dev, int side, Sidestarts *stp)
{
int s;
Devsize dstart;
for (dstart = s = 0; s < side; s++)
dstart += wormsizeside(dev, s);
stp->sstart = dstart;
stp->s1start = dstart + wormsizeside(dev, side);
}
int
wormread(Device *d, Off b, void *c)
{
int r = 0;
long max;
char name[128];
Side *v = wormunit(d);
Juke *w = d->private;
Device *dr;
if (v == nil)
panic("wormread: nil wormunit(%Z)", d);
dr = w->drive[v->drive];
if (dr->wren.fd < 0)
panic("wormread: unopened fd for %Z", d);
max = (d->type == Devlworm? v->max + 1: v->max);
if(b >= max) {
print("wormread: block out of range %Z(%lld)\n", d, (Wideoff)b);
r = 0x071;
} else if (pread(dr->wren.fd, c, RBUFSIZE, (vlong)b*RBUFSIZE) != RBUFSIZE) {
fd2path(dr->wren.fd, name, sizeof name);
print("wormread: error on %Z(%lld) on %s in %s: %r\n",
d, (Wideoff)b, name, dr->wren.sddir);
cons.nwormre++;
r = 1;
}
qunlock(v);
return r;
}
int
wormwrite(Device *d, Off b, void *c)
{
int r = 0;
long max;
char name[128];
Side *v = wormunit(d);
Juke *w = d->private;
Device *dr;
if (v == nil)
panic("wormwrite: nil wormunit(%Z)", d);
dr = w->drive[v->drive];
if (dr->wren.fd < 0)
panic("wormwrite: unopened fd for %Z", d);
max = (d->type == Devlworm? v->max + 1: v->max);
if(b >= max) {
print("wormwrite: block out of range %Z(%lld)\n",
d, (Wideoff)b);
r = 0x071;
} else if (pwrite(dr->wren.fd, c, RBUFSIZE, (vlong)b*RBUFSIZE) != RBUFSIZE) {
fd2path(dr->wren.fd, name, sizeof name);
print("wormwrwite: error on %Z(%lld) on %s in %s: %r\n",
d, (Wideoff)b, name, dr->wren.sddir);
cons.nwormwe++;
r = 1;
}
qunlock(v);
return r;
}
static int
mmove(Juke *w, int trans, int from, int to, int rot)
{
int s;
uchar cmd[12], buf[4];
static int recur = 0;
memset(cmd, 0, sizeof cmd);
cmd[0] = 0xa5;
cmd[2] = trans>>8;
cmd[3] = trans;
cmd[4] = from>>8;
cmd[5] = from;
cmd[6] = to>>8;
cmd[7] = to;
if(rot)
cmd[10] = 1;
s = scsiio(w->juke, SCSInone, cmd, sizeof cmd, buf, 0);
if(s) {
print("scsio status #%x\n", s);
print("move medium t=%d fr=%d to=%d rot=%d\n",
trans, from, to, rot);
if(recur == 0) {
recur = 1;
print("element from=%d\n", from);
element(w, from);
print("element to=%d\n", to);
element(w, to);
print("element trans=%d\n", trans);
element(w, trans);
recur = 0;
}
return 1;
}
return 0;
}
static void
geometry(Juke *w)
{
int s;
uchar cmd[6], buf[4+20];
memset(cmd, 0, sizeof cmd);
memset(buf, 0, sizeof buf);
cmd[0] = 0x1a;
cmd[2] = 0x1d;
cmd[4] = sizeof buf;
s = scsiio(w->juke, SCSIread, cmd, sizeof cmd, buf, sizeof buf);
if(s)
panic("geometry #%x", s);
w->mt0 = (buf[4+2]<<8) | buf[4+3];
w->nmt = (buf[4+4]<<8) | buf[4+5];
w->se0 = (buf[4+6]<<8) | buf[4+7];
w->nse = (buf[4+8]<<8) | buf[4+9];
w->ie0 = (buf[4+10]<<8) | buf[4+11];
w->nie = (buf[4+12]<<8) | buf[4+13];
w->dt0 = (buf[4+14]<<8) | buf[4+15];
w->ndt = (buf[4+16]<<8) | buf[4+17];
memset(cmd, 0, 6);
memset(buf, 0, sizeof buf);
cmd[0] = 0x1a;
cmd[2] = 0x1e;
cmd[4] = sizeof buf;
s = scsiio(w->juke, SCSIread, cmd, sizeof cmd, buf, sizeof buf);
if(s)
panic("geometry #%x", s);
w->rot = buf[4+2] & 1;
print("\tmt %d %d\n", w->mt0, w->nmt);
print("\tse %d %d\n", w->se0, w->nse);
print("\tie %d %d\n", w->ie0, w->nie);
print("\tdt %d %d\n", w->dt0, w->ndt);
print("\trot %d\n", w->rot);
prflush();
}
static void
element(Juke *w, int e)
{
uchar cmd[12], buf[8+8+88];
int s, t;
memset(cmd, 0, sizeof cmd);
memset(buf, 0, sizeof buf);
cmd[0] = 0xb8;
cmd[2] = e>>8;
cmd[3] = e;
cmd[5] = 1;
cmd[9] = sizeof buf;
s = scsiio(w->juke, SCSIread, cmd, sizeof cmd, buf, sizeof buf);
if(s) {
print("scsiio #%x\n", s);
goto bad;
}
s = (buf[0]<<8) | buf[1];
if(s != e) {
print("element = %d\n", s);
goto bad;
}
if(buf[3] != 1) {
print("number reported = %d\n", buf[3]);
goto bad;
}
s = (buf[8+8+0]<<8) | buf[8+8+1];
if(s != e) {
print("element1 = %d\n", s);
goto bad;
}
switch(buf[8+0]) {
default:
print("unknown element %d: %d\n", e, buf[8+0]);
goto bad;
case 1:
s = e - w->mt0;
if(s < 0 || s >= w->nmt)
goto bad;
if(buf[8+8+2] & 1)
print("transport %d full %d.%d\n", s,
(buf[8+8+10]<<8) | buf[8+8+11],
(buf[8+8+9]>>6) & 1);
break;
case 2:
s = e - w->se0;
if(s < 0 || s >= w->nse)
goto bad;
w->side[s].status = Sempty;
if(buf[8+8+2] & 1)
w->side[s].status = Sunload;
if(w->rot)
w->side[w->nse+s].status = w->side[s].status;
break;
case 3:
s = e - w->ie0;
if(s < 0 || s >= w->nie)
goto bad;
print("import/export %d #%.2x %d.%d\n", s,
buf[8+8+2],
(buf[8+8+10]<<8) | buf[8+8+11],
(buf[8+8+9]>>6) & 1);
break;
case 4:
s = e - w->dt0;
if(s < 0 || s >= w->ndt)
goto bad;
print("data transfer %d #%.2x %d.%d\n", s,
buf[8+8+2],
(buf[8+8+10]<<8) | buf[8+8+11],
(buf[8+8+9]>>6) & 1);
if(buf[8+8+2] & 1) {
t = ((buf[8+8+10]<<8) | buf[8+8+11]) - w->se0;
if (t < 0 || t >= w->nse || t >= MAXSIDE ||
s >= MAXDRIVE) {
print(
"element: juke %Z lies; claims side %d is in drive %d\n",
w->juke, t, s);
goto bad;
}
print("r%d in drive %d\n", t, s);
if(mmove(w, w->mt0, w->dt0+s, w->se0+t,
(buf[8+8+9]>>6) & 1)) {
print("mmove initial unload\n");
goto bad;
}
w->side[t].status = Sunload;
if(w->rot)
w->side[w->nse+t].status = Sunload;
}
if(buf[8+8+2] & 4) {
print("drive w%d has exception #%.2x #%.2x\n", s,
buf[8+8+4], buf[8+8+5]);
goto bad;
}
break;
}
return;
bad:
;
}
static void
positions(Juke *w)
{
int i, f;
for(i=0; i<w->nse; i++)
element(w, w->se0+i);
for(i=0; i<w->nmt; i++)
element(w, w->mt0+i);
for(i=0; i<w->nie; i++)
element(w, w->ie0+i);
for(i=0; i<w->ndt; i++)
element(w, w->dt0+i);
f = 0;
for(i=0; i<w->nse; i++)
if(w->side[i].status == Sempty) {
if(f) {
print("r%d\n", i-1);
f = 0;
}
} else {
if(!f) {
print("\tshelves r%d-", i);
f = 1;
}
}
if(f)
print("r%d\n", i-1);
}
static void
jinit(Juke *w, Device *d, int o)
{
int p;
Device *dev = d;
switch(d->type) {
default:
print("juke platter not (devmcat of) dev(l)worm: %Z\n", d);
panic("jinit: type");
case Devmcat:
for(d=d->cat.first; d; d=d->link)
jinit(w, d, o++);
dev->cat.ndev = o;
break;
case Devlworm:
p = d->wren.targ;
if(p < 0 || p >= w->nside)
panic("jinit partition %Z", d);
w->side[p].ord = o;
case Devworm:
if(d->private) {
print("juke platter private pointer set %p\n",
d->private);
panic("jinit: private");
}
d->private = w;
break;
}
}
Side*
wormi(char *arg)
{
int i, j;
Juke *w;
Side *v;
i = number(arg, -1, 10) - 1;
w = jukelist;
if(i < 0 || i >= w->nside) {
print("bad unit number %s (%d)\n", arg, i+1);
return 0;
}
j = i;
if(j >= w->nse)
j -= w->nse;
if(j < w->nside) {
v = &w->side[j];
qlock(v);
if(v->status == Sstart) {
if(mmove(w, w->mt0, w->dt0+v->drive, v->elem, v->rot)) {
qunlock(v);
return 0;
}
v->status = Sunload;
}
qunlock(v);
}
j += w->nse;
if(j < w->nside) {
v = &w->side[j];
qlock(v);
if(v->status == Sstart) {
if(mmove(w, w->mt0, w->dt0+v->drive, v->elem, v->rot)) {
qunlock(v);
return 0;
}
v->status = Sunload;
}
qunlock(v);
}
v = &w->side[i];
qlock(v);
return v;
}
static void
cmd_wormoffline(int argc, char *argv[])
{
int u, i;
Juke *w;
if(argc <= 1) {
print("usage: wormoffline drive\n");
return;
}
u = number(argv[1], -1, 10);
w = jukelist;
if(u < 0 || u >= w->ndrive) {
print("bad drive %s (0<=%d<%d)\n", argv[1], u, w->ndrive);
return;
}
if(w->offline[u])
print("drive %d already offline\n", u);
w->offline[u] = 1;
for(i=0; i<w->ndrive; i++)
if(w->offline[i] == 0)
return;
print("that would take all drives offline\n");
w->offline[u] = 0;
}
static void
cmd_wormonline(int argc, char *argv[])
{
int u;
Juke *w;
if(argc <= 1) {
print("usage: wormonline drive\n");
return;
}
u = number(argv[1], -1, 10);
w = jukelist;
if(u < 0 || u >= w->ndrive) {
print("bad drive %s (0<=%d<%d)\n", argv[1], u, w->ndrive);
return;
}
if(w->offline[u] == 0)
print("drive %d already online\n", u);
w->offline[u] = 0;
}
void
cmd_wormreset(int, char *[])
{
Juke *w;
for(w=jukelist; w; w=w->link) {
qlock(w);
positions(w);
qunlock(w);
}
}
static void
cmd_wormeject(int argc, char *argv[])
{
Juke *w;
Side *v;
if(argc <= 1) {
print("usage: wormeject unit\n");
return;
}
v = wormi(argv[1]);
if(v == 0)
return;
w = jukelist;
mmove(w, w->mt0, v->elem, w->ie0, 0);
qunlock(v);
}
static void
cmd_wormingest(int argc, char *argv[])
{
Juke *w;
Side *v;
if(argc <= 1) {
print("usage: wormingest unit\n");
return;
}
v = wormi(argv[1]);
if(v == 0)
return;
w = jukelist;
mmove(w, w->mt0, w->ie0, v->elem, 0);
qunlock(v);
}
static void
newside(Side *v, int rot, int elem)
{
qlock(v);
qunlock(v);
v->elem = elem;
v->rot = rot;
v->status = Sempty;
v->time = toytime();
}
void *
querychanger(Device *xdev)
{
Juke *w;
Side *v;
int i;
if (xdev == nil)
panic("querychanger: nil Device");
if(xdev->type != Devwren) {
print("juke changer not wren %Z\n", xdev);
goto bad;
}
for(w=jukelist; w; w=w->link)
if(xdev == w->juke)
return w;
w = malloc(sizeof(Juke));
w->magic = Jukemagic;
w->isfixedsize = FIXEDSIZE;
w->link = jukelist;
jukelist = w;
print("alloc juke %Z\n", xdev);
qlock(w);
qunlock(w);
w->juke = xdev;
w->robotdir = sdof(xdev);
w->robot = openscsi(w->robotdir);
if (w->robot == nil)
panic("can't openscsi(%s): %r", w->robotdir);
newscsi(xdev, w->robot);
geometry(w);
w->nside = w->nse;
if(w->rot)
w->nside += w->nside;
if(w->nside > MAXSIDE) {
print("too many sides: %d max %d\n", w->nside, MAXSIDE);
goto bad;
}
for(i=0; i < w->nse; i++) {
v = &w->side[i];
newside(v, 0, w->se0 + i);
if(w->rot)
newside(v + w->nse, 1, w->se0 + i);
}
positions(w);
w->ndrive = w->ndt;
if(w->ndrive > MAXDRIVE) {
print("ndrives truncated to %d\n", MAXDRIVE);
w->ndrive = MAXDRIVE;
}
for(i=0; i<w->ndrive; i++)
w->drive[i] = devnone;
return w;
bad:
panic("querychanger: %Z", xdev);
return nil;
}
void
jukeinit(Device *d)
{
Juke *w;
Device *xdev;
int i;
static int beenhere = 0;
if (d == nil)
panic("jukeinit: nil Device");
xdev = d->j.j;
if(xdev == nil || xdev->type != Devmcat) {
print("juke union not mcat\n");
goto bad;
}
xdev = xdev->cat.first;
w = querychanger(xdev);
if (!beenhere) {
beenhere = 1;
cmd_install("wormreset",
"-- put drives back where jukebox thinks they belong",
cmd_wormreset);
cmd_install("wormeject", "unit -- shelf to outside",
cmd_wormeject);
cmd_install("wormingest", "unit -- outside to shelf",
cmd_wormingest);
cmd_install("wormoffline", "unit -- disable drive",
cmd_wormoffline);
cmd_install("wormonline", "unit -- enable drive",
cmd_wormonline);
}
i = 0;
while(xdev = xdev->link) {
if(xdev->type != Devwren) {
print("drive not devwren: %Z\n", xdev);
goto bad;
}
if(w->drive[i]->type != Devnone &&
xdev != w->drive[i]) {
print("double init drive %d %Z %Z\n",
i, w->drive[i], xdev);
goto bad;
}
if(i >= w->ndrive) {
print("too many drives %Z\n", xdev);
goto bad;
}
w->drive[i++] = xdev;
}
if(i <= 0) {
print("no drives\n");
goto bad;
}
d->private = w;
jinit(w, d->j.m, 0);
w->probeok = 1;
return;
bad:
panic("juke init");
}
void
wormprobe(void)
{
int i, drive;
Timet t;
Side *v;
Juke *w;
t = toytime() - TWORM;
for(w=jukelist; w; w=w->link) {
if(w->probeok == 0 || !canqlock(w))
continue;
for(i=0; i<w->nside; i++) {
v = &w->side[i];
if(!canqlock(v))
continue;
if(v->status == Sstart && t > v->time) {
drive = v->drive;
print("\ttime   r%ld drive %Z\n",
v-w->side, w->drive[drive]);
mmove(w, w->mt0, w->dt0+drive, v->elem, v->rot);
v->status = Sunload;
}
qunlock(v);
}
qunlock(w);
}
}