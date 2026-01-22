#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
#define DPRINT if(0)iprint
typedef	struct Drive		Drive;
typedef	struct Ident		Ident;
typedef	struct Controller	Controller;
typedef struct Partition	Partition;
typedef struct Repl		Repl;
enum
{
Pbase=		0x1F0,
Pdata=		0,
Perror=		1,
Pprecomp=	1,
Pcount=		2,
Psector=	3,
Pcyllsb=	4,
Pcylmsb=	5,
Pdh=		6,
Pstatus=	7,
Sbusy=		 (1<<7),
Sready=	 (1<<6),
Sdrq=		 (1<<3),
Serr=		 (1<<0),
Pcmd=		7,
Crecal=		0x10,
Cread=		0x20,
Cwrite=		0x30,
Cident=		0xEC,
Cident2=	0xFF,
Csetbuf=	0xEF,
Cinitparam=	0x91,
Cstandby=	0xE2,
Cidle=		0xE1,
Cpowerdown=	0xE3,
Sspinning,
Sstandby,
Sidle,
Spowerdown,
DHmagic=	0xA0,
Qdir=		0,
Qfile,
Maxxfer=	BY2PG,
Npart=		8+2,
Nrepl=		64,
};
#define PART(x)		(((x)>>1)&0xF)
#define DRIVE(x)	(((x)>>5)&0x7)
#define MKQID(d,p)	(((d)<<5) | ((p)<<1) | Qfile)
struct Partition
{
ulong	start;
ulong	end;
char	name[KNAMELEN+1];
};
struct Repl
{
Partition *p;
int	nrepl;
ulong	blk[Nrepl];
};
#define PARTMAGIC	"plan9 partitions"
#define REPLMAGIC	"block replacements"
struct Drive
{
QLock;
Controller *cp;
int	drive;
int	confused;
int	online;
int	npart;
Partition p[Npart];
Repl	repl;
ulong	usetime;
int	state;
char	vol[KNAMELEN];
ulong	cap;
int	bytes;
int	sectors;
int	heads;
long	cyl;
char	lba;
char	multi;
};
struct Controller
{
QLock;
ISAConf;
Lock	reglock;
int	confused;
ulong	pbase;
int	cmd;
int	lastcmd;
Rendez	r;
char	*buf;
int	nsecs;
int	sofar;
int	status;
int	error;
Drive	*dp;
};
Controller	*atac;
Drive		*ata;
static char*	ataerr;
static int	nhard;
static int	spindowntime;
static void	ataintr(Ureg*, void*);
static long	ataxfer(Drive*, Partition*, int, long, long, char*);
static void	ataident(Drive*);
static void	atasetbuf(Drive*, int);
static void	ataparams(Drive*);
static void	atapart(Drive*);
static int	ataprobe(Drive*, int, int, int);
static int
atagen(Chan *c, char*, Dirtab*, int, int s, Dir *dirp)
{
Qid qid;
int drive;
Drive *dp;
Partition *pp;
ulong l;
if(s == DEVDOTDOT){
mkqid(&qid, 0, 0, QTDIR);
sprint(up->genbuf, "#%C", devtab[c->type]->dc);
devdir(c, qid, up->genbuf, 0, eve, 0555, dirp);
return 1;
}
qid.vers = 0;
qid.type = QTFILE;
drive = s/Npart;
s = s % Npart;
if(drive >= nhard)
return -1;
dp = &ata[drive];
if(dp->online == 0 || s >= dp->npart)
return 0;
pp = &dp->p[s];
sprint(up->genbuf, "%s%s", dp->vol, pp->name);
qid.path = MKQID(drive, s);
l = (pp->end - pp->start) * dp->bytes;
devdir(c, qid, up->genbuf, l, eve, 0660, dirp);
return 1;
}
static void
atainit(void)
{
Drive *dp;
Controller *cp;
uchar equip;
int pcmslot;
if (atac)
return;
equip = 0x10;
cp = malloc(sizeof(*cp));
if (!cp)
error(Enomem);
cp->port = Pbase;
cp->irq = 14;
cp->nopt = 1;
cp->opt[0] = "index=1";
if((pcmslot = pcmspecial("ATA FLASH", cp)) < 0) {
DPRINT("No ATA card\n");
free(cp);
ataerr = Enoifc;
return;
}
ata = malloc(2 * sizeof(*ata));
if(ata == nil) {
pcmspecialclose(pcmslot);
free(cp);
error(Enomem);
}
atac = cp;
cp->buf = 0;
cp->lastcmd = cp->cmd;
cp->cmd = 0;
cp->pbase = cp->port;
intrenable(cp->irq, ataintr, cp, cp->itype, "ata");
dp = ata;
if(equip & 0xf0){
dp->drive = 0;
dp->online = 0;
dp->cp = cp;
dp++;
}
if((equip & 0x0f)){
dp->drive = 1;
dp->online = 0;
dp->cp = cp;
dp++;
}
nhard = dp - ata;
spindowntime = 1;
}
static Chan*
ataattach(char *spec)
{
Drive *dp;
atainit();
if (!ata)
error(ataerr ? ataerr : Enoifc);
for(dp = ata; dp < &ata[nhard]; dp++){
if(waserror()){
dp->online = 0;
qunlock(dp);
continue;
}
qlock(dp);
if(!dp->online){
dp->usetime = m->ticks;
ataparams(dp);
dp->online = 1;
atasetbuf(dp, 1);
}
atapart(dp);
qunlock(dp);
poperror();
}
return devattach('H', spec);
}
static Walkqid*
atawalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, 0, 0, atagen);
}
static int
atastat(Chan *c, uchar *dp, int n)
{
return devstat(c, dp, n, 0, 0, atagen);
}
static Chan*
ataopen(Chan *c, int omode)
{
return devopen(c, omode, 0, 0, atagen);
}
static void
ataclose(Chan *c)
{
Drive *d;
Partition *p;
if(c->mode != OWRITE && c->mode != ORDWR)
return;
d = &ata[DRIVE(c->qid.path)];
p = &d->p[PART(c->qid.path)];
if(strcmp(p->name, "partition") != 0)
return;
if(waserror()){
qunlock(d);
nexterror();
}
qlock(d);
atapart(d);
qunlock(d);
poperror();
}
static long
ataread(Chan *c, void *a, long n, vlong offset)
{
Drive *dp;
long rv, i;
int skip;
uchar *aa = a;
Partition *pp;
char *buf;
if(c->qid.type & QTDIR)
return devdirread(c, a, n, 0, 0, atagen);
buf = smalloc(Maxxfer);
if(waserror()){
free(buf);
nexterror();
}
dp = &ata[DRIVE(c->qid.path)];
pp = &dp->p[PART(c->qid.path)];
skip = offset % dp->bytes;
for(rv = 0; rv < n; rv += i){
i = ataxfer(dp, pp, Cread, offset+rv-skip, n-rv+skip, buf);
if(i == 0)
break;
i -= skip;
if(i > n - rv)
i = n - rv;
memmove(aa+rv, buf + skip, i);
skip = 0;
}
free(buf);
poperror();
return rv;
}
static long
atawrite(Chan *c, void *a, long n, vlong offset)
{
Drive *dp;
long rv, i, partial;
uchar *aa = a;
Partition *pp;
char *buf;
if(c->qid.type & QTDIR)
error(Eisdir);
dp = &ata[DRIVE(c->qid.path)];
pp = &dp->p[PART(c->qid.path)];
buf = smalloc(Maxxfer);
if(waserror()){
free(buf);
nexterror();
}
partial = offset % dp->bytes;
if(partial){
ataxfer(dp, pp, Cread, offset-partial, dp->bytes, buf);
if(partial+n > dp->bytes)
rv = dp->bytes - partial;
else
rv = n;
memmove(buf+partial, aa, rv);
ataxfer(dp, pp, Cwrite, offset-partial, dp->bytes, buf);
} else
rv = 0;
partial = (n - rv) % dp->bytes;
n -= partial;
for(; rv < n; rv += i){
i = n - rv;
if(i > Maxxfer)
i = Maxxfer;
memmove(buf, aa+rv, i);
i = ataxfer(dp, pp, Cwrite, offset+rv, i, buf);
if(i == 0)
break;
}
if(partial){
ataxfer(dp, pp, Cread, offset+rv, dp->bytes, buf);
memmove(buf, aa+rv, partial);
ataxfer(dp, pp, Cwrite, offset+rv, dp->bytes, buf);
rv += partial;
}
free(buf);
poperror();
return rv;
}
static int
cmddone(void *a)
{
Controller *cp = a;
return cp->cmd == 0;
}
static void
cmdreadywait(Drive *dp)
{
long start;
int period;
Controller *cp = dp->cp;
if(dp->state == Sspinning)
period = 10;
else
period = 2000;
start = m->ticks;
while((inb(cp->pbase+Pstatus) & (Sready|Sbusy)) != Sready)
if(TK2MS(m->ticks - start) > period){
DPRINT("cmdreadywait failed\n");
error(Eio);
}
}
static void
atarepl(Drive *dp, long bblk)
{
int i;
if(dp->repl.p == 0)
return;
for(i = 0; i < dp->repl.nrepl; i++){
if(dp->repl.blk[i] == bblk)
DPRINT("found bblk %ld at offset %d\n", bblk, i);
}
}
static void
atasleep(Controller *cp, int ms)
{
tsleep(&cp->r, cmddone, cp, ms);
if(cp->cmd && cp->cmd != Cident2){
DPRINT("ata: cmd 0x%uX timeout, status=%ux\n",
cp->cmd, inb(cp->pbase+Pstatus));
error("ata drive timeout");
}
}
static long
ataxfer(Drive *dp, Partition *pp, int cmd, long start, long len, char *buf)
{
Controller *cp;
long lblk;
int cyl, sec, head;
int loop, stat;
if(dp->online == 0)
error(Eio);
start = start / dp->bytes;
if(len > Maxxfer)
len = Maxxfer;
len = (len + dp->bytes - 1) / dp->bytes;
if(len == 0)
return 0;
lblk = start + pp->start;
if(lblk >= pp->end)
return 0;
if(lblk+len > pp->end)
len = pp->end - lblk;
if(dp->lba){
sec = lblk & 0xff;
cyl = (lblk>>8) & 0xffff;
head = (lblk>>24) & 0xf;
} else {
cyl = lblk/(dp->sectors*dp->heads);
sec = (lblk % dp->sectors) + 1;
head = ((lblk/dp->sectors) % dp->heads);
}
DPRINT("<%s %ld>", (cmd == Cwrite) ? "W" : "R", lblk);
cp = dp->cp;
qlock(cp);
if(waserror()){
cp->buf = 0;
qunlock(cp);
nexterror();
}
dp->usetime = m->ticks;
cmdreadywait(dp);
ilock(&cp->reglock);
cp->sofar = 0;
cp->buf = buf;
cp->nsecs = len;
cp->cmd = cmd;
cp->dp = dp;
cp->status = 0;
outb(cp->pbase+Pcount, cp->nsecs);
outb(cp->pbase+Psector, sec);
outb(cp->pbase+Pdh, DHmagic | (dp->drive<<4) | (dp->lba<<6) | head);
outb(cp->pbase+Pcyllsb, cyl);
outb(cp->pbase+Pcylmsb, cyl>>8);
outb(cp->pbase+Pcmd, cmd);
if(cmd == Cwrite){
loop = 0;
microdelay(1);
while((stat = inb(cp->pbase+Pstatus) & (Serr|Sdrq)) == 0)
if(++loop > 10000)
panic("ataxfer");
outss(cp->pbase+Pdata, cp->buf, dp->bytes/2);
} else
stat = 0;
iunlock(&cp->reglock);
if(stat & Serr)
error(Eio);
loop = 0;
while(waserror()){
DPRINT("interrupted ataxfer\n");
if(loop++ > 10){
print("ata disk error\n");
nexterror();
}
}
atasleep(cp, 3000);
dp->state = Sspinning;
dp->usetime = m->ticks;
poperror();
if(loop)
nexterror();
if(cp->status & Serr){
DPRINT("hd%ld err: lblk %ld status %ux, err %ux\n",
dp-ata, lblk, cp->status, cp->error);
DPRINT("\tcyl %d, sec %d, head %d\n", cyl, sec, head);
DPRINT("\tnsecs %d, sofar %d\n", cp->nsecs, cp->sofar);
atarepl(dp, lblk+cp->sofar);
error(Eio);
}
cp->buf = 0;
len = cp->sofar*dp->bytes;
qunlock(cp);
poperror();
return len;
}
static void
atasetbuf(Drive *dp, int on)
{
Controller *cp = dp->cp;
qlock(cp);
if(waserror()){
qunlock(cp);
nexterror();
}
cmdreadywait(dp);
ilock(&cp->reglock);
cp->cmd = Csetbuf;
outb(cp->pbase+Pprecomp, on ? 0xAA : 0x55);
outb(cp->pbase+Pdh, DHmagic | (dp->drive<<4));
outb(cp->pbase+Pcmd, Csetbuf);
iunlock(&cp->reglock);
atasleep(cp, 5000);
poperror();
qunlock(cp);
}
struct Ident
{
ushort	config;
ushort	cyls;
ushort	reserved0;
ushort	heads;
ushort	b2t;
ushort	b2s;
ushort	s2t;
ushort	reserved1[3];
ushort	serial[10];
ushort	type;
ushort	bsize;
ushort	ecc;
ushort	firm[4];
ushort	model[20];
ushort	s2i;
ushort	dwtf;
ushort	capabilities;
ushort	reserved2;
ushort	piomode;
ushort	dmamode;
ushort	cvalid;
ushort	ccyls;
ushort	cheads;
ushort	cs2t;
ushort	ccap[2];
ushort	cs2i;
ushort	lbasecs[2];
ushort	dmasingle;
ushort	dmadouble;
ushort	reserved3[64];
ushort	vendor[32];
ushort	reserved4[96];
};
static void
ataident(Drive *dp)
{
Controller *cp;
char *buf;
Ident *ip;
char id[21];
cp = dp->cp;
buf = smalloc(Maxxfer);
qlock(cp);
if(waserror()){
cp->buf = 0;
qunlock(cp);
free(buf);
nexterror();
}
cmdreadywait(dp);
ilock(&cp->reglock);
cp->nsecs = 1;
cp->sofar = 0;
cp->cmd = Cident;
cp->dp = dp;
cp->buf = buf;
outb(cp->pbase+Pdh, DHmagic | (dp->drive<<4));
outb(cp->pbase+Pcmd, Cident);
iunlock(&cp->reglock);
atasleep(cp, 5000);
if(cp->status & Serr){
DPRINT("bad disk ident status\n");
error(Eio);
}
ip = (Ident*)buf;
if (cp->cmd == Cident2)
tsleep(&cp->r, return0, 0, 10);
memmove(id, ip->model, sizeof(id)-1);
id[sizeof(id)-1] = 0;
if(ip->capabilities & (1<<9)){
dp->lba = 1;
dp->sectors = (ip->lbasecs[0]) | (ip->lbasecs[1]<<16);
dp->cap = dp->bytes * dp->sectors;
} else {
dp->lba = 0;
dp->cyl = ip->cyls;
dp->heads = ip->heads;
dp->sectors = ip->s2t;
if(ip->cvalid&(1<<0)){
dp->cyl = ip->ccyls;
dp->heads = ip->cheads;
dp->sectors = ip->cs2t;
}
dp->cap = dp->bytes * dp->cyl * dp->heads * dp->sectors;
}
cp->lastcmd = cp->cmd;
cp->cmd = 0;
cp->buf = 0;
free(buf);
poperror();
qunlock(cp);
}
static int
ataprobe(Drive *dp, int cyl, int sec, int head)
{
Controller *cp;
char *buf;
int rv;
cp = dp->cp;
buf = smalloc(Maxxfer);
qlock(cp);
if(waserror()){
free(buf);
qunlock(cp);
nexterror();
}
cmdreadywait(dp);
ilock(&cp->reglock);
cp->cmd = Cread;
cp->dp = dp;
cp->status = 0;
cp->nsecs = 1;
cp->sofar = 0;
outb(cp->pbase+Pcount, 1);
outb(cp->pbase+Psector, sec+1);
outb(cp->pbase+Pdh, DHmagic | head | (dp->lba<<6) | (dp->drive<<4));
outb(cp->pbase+Pcyllsb, cyl);
outb(cp->pbase+Pcylmsb, cyl>>8);
outb(cp->pbase+Pcmd, Cread);
iunlock(&cp->reglock);
atasleep(cp, 5000);
if(cp->status & Serr)
rv = -1;
else
rv = 0;
cp->buf = 0;
free(buf);
poperror();
qunlock(cp);
return rv;
}
static void
ataparams(Drive *dp)
{
int i, hi, lo;
dp->bytes = 512;
ataident(dp);
if(dp->lba){
i = dp->sectors - 1;
if(ataprobe(dp, (i>>8)&0xffff, (i&0xff)-1, (i>>24)&0xf) == 0)
return;
} else {
if(ataprobe(dp, dp->cyl-1, dp->sectors-1, dp->heads-1) == 0)
return;
}
dp->lba = 0;
for(i = 0; i < 32; i++)
if(ataprobe(dp, 0, 0, i) < 0)
break;
dp->heads = i;
for(i = 0; i < 128; i++)
if(ataprobe(dp, 0, i, 0) < 0)
break;
dp->sectors = i;
for(i = 512; ; i += 512)
if(ataprobe(dp, i, dp->sectors-1, dp->heads-1) < 0)
break;
lo = i - 512;
hi = i;
for(; hi-lo > 1;){
i = lo + (hi - lo)/2;
if(ataprobe(dp, i, dp->sectors-1, dp->heads-1) < 0)
hi = i;
else
lo = i;
}
dp->cyl = lo + 1;
dp->cap = dp->bytes * dp->cyl * dp->heads * dp->sectors;
}
static void
atareplinit(Drive *dp)
{
char *line[Nrepl+1];
char *field[1];
ulong n;
int i;
char *buf;
if(dp->repl.p->end - dp->repl.p->start < Nrepl+1){
dp->repl.p = 0;
return;
}
buf = smalloc(Maxxfer);
if(waserror()){
free(buf);
nexterror();
}
ataxfer(dp, dp->repl.p, Cread, 0, dp->bytes, buf);
buf[dp->bytes-1] = 0;
n = getfields(buf, line, Nrepl+1, 1, "\n");
if(strncmp(line[0], REPLMAGIC, sizeof(REPLMAGIC)-1)){
dp->repl.p = 0;
} else {
for(dp->repl.nrepl = 0, i = 1; i < n; i++, dp->repl.nrepl++){
if(getfields(line[i], field, 1, 1, " ") != 1)
break;
dp->repl.blk[dp->repl.nrepl] = strtoul(field[0], 0, 0);
if(dp->repl.blk[dp->repl.nrepl] <= 0)
break;
}
}
free(buf);
poperror();
}
static void
atapart(Drive *dp)
{
Partition *pp;
char *line[Npart+1];
char *field[3];
ulong n;
int i;
char *buf;
sprint(dp->vol, "hd%ld", dp - ata);
pp = &dp->p[0];
strcpy(pp->name, "disk");
pp->start = 0;
pp->end = dp->cap / dp->bytes;
pp++;
strcpy(pp->name, "partition");
pp->start = dp->p[0].end - 1;
pp->end = dp->p[0].end;
pp++;
dp->npart = 2;
dp->repl.p = 0;
buf = smalloc(Maxxfer);
if(waserror()){
free(buf);
nexterror();
}
ataxfer(dp, dp->p+1, Cread, 0, dp->bytes, buf);
buf[dp->bytes-1] = 0;
n = getfields(buf, line, Npart+1, 1, "\n");
if(n == 0 || strncmp(line[0], PARTMAGIC, sizeof(PARTMAGIC)-1)){
dp->p[0].end--;
dp->p[1].start--;
dp->p[1].end--;
ataxfer(dp, dp->p+1, Cread, 0, dp->bytes, buf);
buf[dp->bytes-1] = 0;
n = getfields(buf, line, Npart+1, 1, "\n");
}
if(n > 0 && strncmp(line[0], PARTMAGIC, sizeof(PARTMAGIC)-1) == 0){
for(i = 1; i < n; i++){
switch(getfields(line[i], field, 3, 1, " ")) {
case 2:
if(strcmp(field[0], "unit") == 0)
strncpy(dp->vol, field[1], KNAMELEN);
break;
case 3:
strncpy(pp->name, field[0], KNAMELEN);
if(strncmp(pp->name, "repl", KNAMELEN) == 0)
dp->repl.p = pp;
pp->start = strtoul(field[1], 0, 0);
pp->end = strtoul(field[2], 0, 0);
if(pp->start > pp->end || pp->end > dp->p[0].end)
break;
dp->npart++;
pp++;
}
}
}
free(buf);
poperror();
if(dp->repl.p)
atareplinit(dp);
}
enum
{
Maxloop=	10000,
};
static void
ataintr(Ureg*, void *arg)
{
Controller *cp;
Drive *dp;
long loop;
char *addr;
cp = arg;
dp = cp->dp;
ilock(&cp->reglock);
loop = 0;
while((cp->status = inb(cp->pbase+Pstatus)) & Sbusy){
if(++loop > Maxloop) {
DPRINT("cmd=%ux status=%ux\n",
cp->cmd, inb(cp->pbase+Pstatus));
panic("ataintr: wait busy");
}
}
switch(cp->cmd){
case Cwrite:
if(cp->status & Serr){
cp->lastcmd = cp->cmd;
cp->cmd = 0;
cp->error = inb(cp->pbase+Perror);
wakeup(&cp->r);
break;
}
cp->sofar++;
if(cp->sofar < cp->nsecs){
loop = 0;
while(((cp->status = inb(cp->pbase+Pstatus)) & Sdrq) == 0)
if(++loop > Maxloop) {
DPRINT("cmd=%ux status=%ux\n",
cp->cmd, inb(cp->pbase+Pstatus));
panic("ataintr: write");
}
addr = cp->buf;
if(addr){
addr += cp->sofar*dp->bytes;
outss(cp->pbase+Pdata, addr, dp->bytes/2);
}
} else{
cp->lastcmd = cp->cmd;
cp->cmd = 0;
wakeup(&cp->r);
}
break;
case Cread:
case Cident:
loop = 0;
while((cp->status & (Serr|Sdrq)) == 0){
if(++loop > Maxloop) {
DPRINT("cmd=%ux status=%ux\n",
cp->cmd, inb(cp->pbase+Pstatus));
panic("ataintr: read/ident");
}
cp->status = inb(cp->pbase+Pstatus);
}
if(cp->status & Serr){
cp->lastcmd = cp->cmd;
cp->cmd = 0;
cp->error = inb(cp->pbase+Perror);
wakeup(&cp->r);
break;
}
addr = cp->buf;
if(addr){
addr += cp->sofar*dp->bytes;
inss(cp->pbase+Pdata, addr, dp->bytes/2);
}
cp->sofar++;
if(cp->sofar > cp->nsecs)
print("ataintr %d %d\n", cp->sofar, cp->nsecs);
if(cp->sofar >= cp->nsecs){
cp->lastcmd = cp->cmd;
if (cp->cmd == Cread)
cp->cmd = 0;
else
cp->cmd = Cident2;
wakeup(&cp->r);
}
break;
case Cinitparam:
case Csetbuf:
case Cidle:
case Cstandby:
case Cpowerdown:
cp->lastcmd = cp->cmd;
cp->cmd = 0;
wakeup(&cp->r);
break;
case Cident2:
cp->lastcmd = cp->cmd;
cp->cmd = 0;
break;
default:
print("weird disk interrupt, cmd=%.2ux, lastcmd= %.2ux status=%.2ux\n",
cp->cmd, cp->lastcmd, cp->status);
break;
}
iunlock(&cp->reglock);
}
void
hardclock(void)
{
int drive;
Drive *dp;
Controller *cp;
int diff;
if(spindowntime <= 0)
return;
for(drive = 0; drive < nhard; drive++){
dp = &ata[drive];
cp = dp->cp;
diff = TK2SEC(m->ticks - dp->usetime);
if((dp->state == Sspinning) && (diff >= spindowntime)){
ilock(&cp->reglock);
cp->cmd = Cstandby;
outb(cp->pbase+Pcount, 0);
outb(cp->pbase+Pdh, DHmagic | (dp->drive<<4) | 0);
outb(cp->pbase+Pcmd, cp->cmd);
iunlock(&cp->reglock);
dp->state = Sstandby;
}
}
}
Dev atadevtab = {
'H',
"ata",
devreset,
atainit,
devshutdown,
ataattach,
atawalk,
atastat,
ataopen,
devcreate,
ataclose,
ataread,
devbread,
atawrite,
devbwrite,
devremove,
devwstat,
};