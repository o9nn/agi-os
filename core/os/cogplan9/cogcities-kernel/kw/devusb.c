#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
#include	"../port/usb.h"
typedef struct Hcitype Hcitype;
enum
{
Qdir = 0,
Qusbdir,
Qctl,
Qep0dir,
Qep0io,
Qep0ctl,
Qep0dummy,
Qepdir = 0,
Qepio,
Qepctl,
CMdebug = 0,
CMdump,
CMnew = 0,
CMnewdev,
CMhub,
CMspeed,
CMmaxpkt,
CMntds,
CMclrhalt,
CMpollival,
CMhz,
CMsamplesz,
CMinfo,
CMdetach,
CMaddress,
CMdebugep,
CMname,
CMtmout,
CMpreset,
Rportenable	= 1,
Rportreset	= 4,
};
struct Hcitype
{
char*	type;
int	(*reset)(Hci*);
};
#define QID(q)	((int)(q).path)
static char Edetach[] = "device is detached";
static char Enotconf[] = "endpoint not configured";
char Estalled[] = "endpoint stalled";
static Cmdtab usbctls[] =
{
{CMdebug,	"debug",	2},
{CMdump,	"dump",		1},
};
static Cmdtab epctls[] =
{
{CMnew,		"new",		4},
{CMnewdev,	"newdev",	3},
{CMhub,		"hub",		1},
{CMspeed,	"speed",	2},
{CMmaxpkt,	"maxpkt",	2},
{CMntds,	"ntds",		2},
{CMpollival,	"pollival",	2},
{CMsamplesz,	"samplesz",	2},
{CMhz,		"hz",		2},
{CMinfo,	"info",		0},
{CMdetach,	"detach",	1},
{CMaddress,	"address",	1},
{CMdebugep,	"debug",	2},
{CMclrhalt,	"clrhalt",	1},
{CMname,	"name",		2},
{CMtmout,	"timeout",	2},
{CMpreset,	"reset",	1},
};
static Dirtab usbdir[] =
{
"ctl",		{Qctl},		0,	0666,
};
char *usbmodename[] =
{
[OREAD]	"r",
[OWRITE]	"w",
[ORDWR]	"rw",
};
static char *ttname[] =
{
[Tnone]	"none",
[Tctl]	"control",
[Tiso]	"iso",
[Tintr]	"interrupt",
[Tbulk]	"bulk",
};
static char *spname[] =
{
[Fullspeed]	"full",
[Lowspeed]	"low",
[Highspeed]	"high",
[Nospeed]	"no",
};
static int	debug;
static Hcitype	hcitypes[Nhcis];
static Hci*	hcis[Nhcis];
static QLock	epslck;
static Ep*	eps[Neps];
static int	epmax;
static int	usbidgen;
char*
seprintdata(char *s, char *se, uchar *d, int n)
{
int i, l;
s = seprint(s, se, " %#p[%d]: ", d, n);
l = n;
if(l > 10)
l = 10;
for(i=0; i<l; i++)
s = seprint(s, se, " %2.2ux", d[i]);
if(l < n)
s = seprint(s, se, "...");
return s;
}
static int
name2speed(char *name)
{
int i;
for(i = 0; i < nelem(spname); i++)
if(strcmp(name, spname[i]) == 0)
return i;
return Nospeed;
}
static int
name2ttype(char *name)
{
int i;
for(i = 0; i < nelem(ttname); i++)
if(strcmp(name, ttname[i]) == 0)
return i;
i = strtol(name, nil, 0);
switch(i+1){
case Tctl:
case Tiso:
case Tbulk:
case Tintr:
return i+1;
default:
return Tnone;
}
}
static int
name2mode(char *mode)
{
int i;
for(i = 0; i < nelem(usbmodename); i++)
if(strcmp(mode, usbmodename[i]) == 0)
return i;
return -1;
}
static int
qid2epidx(int q)
{
q = (q-Qep0dir)/4;
if(q < 0 || q >= epmax || eps[q] == nil)
return -1;
return q;
}
static int
isqtype(int q, int type)
{
if(q < Qep0dir)
return 0;
q -= Qep0dir;
return (q & 3) == type;
}
void
addhcitype(char* t, int (*r)(Hci*))
{
static int ntype;
if(ntype == Nhcis)
panic("too many USB host interface types");
hcitypes[ntype].type = t;
hcitypes[ntype].reset = r;
ntype++;
}
static char*
seprintep(char *s, char *se, Ep *ep, int all)
{
static char* dsnames[] = { "config", "enabled", "detached", "reset" };
Udev *d;
int i;
int di;
d = ep->dev;
qlock(ep);
if(waserror()){
qunlock(ep);
nexterror();
}
di = ep->dev->nb;
if(all)
s = seprint(s, se, "dev %d ep %d ", di, ep->nb);
s = seprint(s, se, "%s", dsnames[ep->dev->state]);
s = seprint(s, se, " %s", ttname[ep->ttype]);
assert(ep->mode == OREAD || ep->mode == OWRITE || ep->mode == ORDWR);
s = seprint(s, se, " %s", usbmodename[ep->mode]);
s = seprint(s, se, " speed %s", spname[d->speed]);
s = seprint(s, se, " maxpkt %ld", ep->maxpkt);
s = seprint(s, se, " pollival %ld", ep->pollival);
s = seprint(s, se, " samplesz %ld", ep->samplesz);
s = seprint(s, se, " hz %ld", ep->hz);
s = seprint(s, se, " hub %d", ep->dev->hub);
s = seprint(s, se, " port %d", ep->dev->port);
if(ep->inuse)
s = seprint(s, se, " busy");
else
s = seprint(s, se, " idle");
if(all){
s = seprint(s, se, " load %uld", ep->load);
s = seprint(s, se, " ref %ld addr %#p", ep->ref, ep);
s = seprint(s, se, " idx %d", ep->idx);
if(ep->name != nil)
s = seprint(s, se, " name '%s'", ep->name);
if(ep->tmout != 0)
s = seprint(s, se, " tmout");
if(ep == ep->ep0){
s = seprint(s, se, " ctlrno %#x", ep->hp->ctlrno);
s = seprint(s, se, " eps:");
for(i = 0; i < nelem(d->eps); i++)
if(d->eps[i] != nil)
s = seprint(s, se, " ep%d.%d", di, i);
}
}
if(ep->info != nil)
s = seprint(s, se, "\n%s %s\n", ep->info, ep->hp->type);
else
s = seprint(s, se, "\n");
qunlock(ep);
poperror();
return s;
}
static Ep*
epalloc(Hci *hp)
{
Ep *ep;
int i;
ep = smalloc(sizeof(Ep));
ep->ref = 1;
qlock(&epslck);
for(i = 0; i < Neps; i++)
if(eps[i] == nil)
break;
if(i == Neps){
qunlock(&epslck);
free(ep);
print("usb: bug: too few endpoints.\n");
return nil;
}
ep->idx = i;
if(epmax <= i)
epmax = i+1;
eps[i] = ep;
ep->hp = hp;
ep->maxpkt = 8;
ep->ntds = 1;
ep->samplesz = ep->pollival = ep->hz = 0;
qunlock(&epslck);
return ep;
}
static Ep*
getep(int i)
{
Ep *ep;
if(i < 0 || i >= epmax || eps[i] == nil)
return nil;
qlock(&epslck);
ep = eps[i];
if(ep != nil)
incref(ep);
qunlock(&epslck);
return ep;
}
static void
putep(Ep *ep)
{
Udev *d;
if(ep != nil && decref(ep) == 0){
d = ep->dev;
deprint("usb: ep%d.%d %#p released\n", d->nb, ep->nb, ep);
qlock(&epslck);
eps[ep->idx] = nil;
if(ep->idx == epmax-1)
epmax--;
if(ep == ep->ep0 && ep->dev != nil && ep->dev->nb == usbidgen)
usbidgen--;
qunlock(&epslck);
if(d != nil){
qlock(ep->ep0);
d->eps[ep->nb] = nil;
qunlock(ep->ep0);
}
if(ep->ep0 != ep){
putep(ep->ep0);
ep->ep0 = nil;
}
free(ep->info);
free(ep->name);
free(ep);
}
}
static void
dumpeps(void)
{
int i;
static char buf[512];
char *s;
char *e;
Ep *ep;
print("usb dump eps: epmax %d Neps %d (ref=1+ for dump):\n", epmax, Neps);
for(i = 0; i < epmax; i++){
s = buf;
e = buf+sizeof(buf);
ep = getep(i);
if(ep != nil){
if(waserror()){
putep(ep);
nexterror();
}
s = seprint(s, e, "ep%d.%d ", ep->dev->nb, ep->nb);
seprintep(s, e, ep, 1);
print("%s", buf);
ep->hp->seprintep(buf, e, ep);
print("%s", buf);
poperror();
putep(ep);
}
}
print("usb dump hcis:\n");
for(i = 0; i < Nhcis; i++)
if(hcis[i] != nil)
hcis[i]->dump(hcis[i]);
}
static int
newusbid(Hci *)
{
int id;
qlock(&epslck);
id = ++usbidgen;
if(id >= 0x7F)
print("#u: too many device addresses; reuse them more\n");
qunlock(&epslck);
return id;
}
static Ep*
newdev(Hci *hp, int ishub, int isroot)
{
Ep *ep;
Udev *d;
ep = epalloc(hp);
d = ep->dev = smalloc(sizeof(Udev));
d->nb = newusbid(hp);
d->eps[0] = ep;
ep->nb = 0;
ep->toggle[0] = ep->toggle[1] = 0;
d->ishub = ishub;
d->isroot = isroot;
if(hp->highspeed != 0)
d->speed = Highspeed;
else
d->speed = Fullspeed;
d->state = Dconfig;
ep->dev = d;
ep->ep0 = ep;
ep->ttype = Tctl;
ep->tmout = Xfertmout;
ep->mode = ORDWR;
dprint("newdev %#p ep%d.%d %#p\n", d, d->nb, ep->nb, ep);
return ep;
}
static Ep*
newdevep(Ep *ep, int i, int tt, int mode)
{
Ep *nep;
Udev *d;
d = ep->dev;
if(d->eps[i] != nil)
error("endpoint already in use");
nep = epalloc(ep->hp);
incref(ep);
d->eps[i] = nep;
nep->nb = i;
nep->toggle[0] = nep->toggle[1] = 0;
nep->ep0 = ep;
nep->dev = ep->dev;
nep->mode = mode;
nep->ttype = tt;
nep->debug = ep->debug;
switch(tt){
case Tctl:
nep->tmout = Xfertmout;
break;
case Tintr:
nep->pollival = 10;
break;
case Tiso:
nep->tmout = Xfertmout;
nep->pollival = 10;
nep->samplesz = 4;
nep->hz = 44100;
break;
}
deprint("newdevep ep%d.%d %#p\n", d->nb, nep->nb, nep);
return ep;
}
static int
epdataperm(int mode)
{
switch(mode){
case OREAD:
return 0440|DMEXCL;
break;
case OWRITE:
return 0220|DMEXCL;
break;
default:
return 0660|DMEXCL;
}
}
static int
usbgen(Chan *c, char *, Dirtab*, int, int s, Dir *dp)
{
Qid q;
Dirtab *dir;
int perm;
char *se;
Ep *ep;
int nb;
int mode;
if(0)ddprint("usbgen q %#x s %d...", QID(c->qid), s);
if(s == DEVDOTDOT){
if(QID(c->qid) <= Qusbdir){
mkqid(&q, Qdir, 0, QTDIR);
devdir(c, q, "#u", 0, eve, 0555, dp);
}else{
mkqid(&q, Qusbdir, 0, QTDIR);
devdir(c, q, "usb", 0, eve, 0555, dp);
}
if(0)ddprint("ok\n");
return 1;
}
switch(QID(c->qid)){
case Qdir:
if(s == 0){
mkqid(&q, Qusbdir, 0, QTDIR);
devdir(c, q, "usb", 0, eve, 0555, dp);
if(0)ddprint("ok\n");
return 1;
}
s--;
if(s < 0 || s >= epmax)
goto Fail;
ep = getep(s);
if(ep == nil || ep->name == nil){
if(ep != nil)
putep(ep);
if(0)ddprint("skip\n");
return 0;
}
if(waserror()){
putep(ep);
nexterror();
}
mkqid(&q, Qep0io+s*4, 0, QTFILE);
devdir(c, q, ep->name, 0, eve, epdataperm(ep->mode), dp);
putep(ep);
poperror();
if(0)ddprint("ok\n");
return 1;
case Qusbdir:
Usbdir:
if(s < nelem(usbdir)){
dir = &usbdir[s];
mkqid(&q, dir->qid.path, 0, QTFILE);
devdir(c, q, dir->name, dir->length, eve, dir->perm, dp);
if(0)ddprint("ok\n");
return 1;
}
s -= nelem(usbdir);
if(s < 0 || s >= epmax)
goto Fail;
ep = getep(s);
if(ep == nil){
if(0)ddprint("skip\n");
return 0;
}
if(waserror()){
putep(ep);
nexterror();
}
se = up->genbuf+sizeof(up->genbuf);
seprint(up->genbuf, se, "ep%d.%d", ep->dev->nb, ep->nb);
mkqid(&q, Qep0dir+4*s, 0, QTDIR);
putep(ep);
poperror();
devdir(c, q, up->genbuf, 0, eve, 0755, dp);
if(0)ddprint("ok\n");
return 1;
case Qctl:
s = 0;
goto Usbdir;
default:
nb = qid2epidx(QID(c->qid));
ep = getep(nb);
if(ep == nil)
goto Fail;
mode = ep->mode;
putep(ep);
if(isqtype(QID(c->qid), Qepdir)){
Epdir:
switch(s){
case 0:
mkqid(&q, Qep0io+nb*4, 0, QTFILE);
perm = epdataperm(mode);
devdir(c, q, "data", 0, eve, perm, dp);
break;
case 1:
mkqid(&q, Qep0ctl+nb*4, 0, QTFILE);
devdir(c, q, "ctl", 0, eve, 0664, dp);
break;
default:
goto Fail;
}
}else if(isqtype(QID(c->qid), Qepctl)){
s = 1;
goto Epdir;
}else{
s = 0;
goto Epdir;
}
if(0)ddprint("ok\n");
return 1;
}
Fail:
if(0)ddprint("fail\n");
return -1;
}
static Hci*
hciprobe(int cardno, int ctlrno)
{
Hci *hp;
char *type;
char name[64];
static int epnb = 1;
ddprint("hciprobe %d %d\n", cardno, ctlrno);
hp = smalloc(sizeof(Hci));
hp->ctlrno = ctlrno;
hp->tbdf = BUSUNKNOWN;
if(cardno < 0)
for(cardno = 0; cardno < Nhcis; cardno++){
if(hcitypes[cardno].type == nil)
break;
type = hp->type;
if(type==nil || *type==0)
type = "uhci";
if(cistrcmp(hcitypes[cardno].type, type) == 0)
break;
}
if(cardno >= Nhcis || hcitypes[cardno].type == nil){
free(hp);
return nil;
}
dprint("%s...", hcitypes[cardno].type);
if(hcitypes[cardno].reset(hp) < 0){
free(hp);
return nil;
}
snprint(name, sizeof(name), "usb%s", hcitypes[cardno].type);
intrenable(Irqlo, hp->irq, hp->interrupt, hp, name);
print("#u/usb/ep%d.0: %s: port %#luX irq %d\n",
epnb, hcitypes[cardno].type, hp->port, hp->irq);
epnb++;
return hp;
}
static void
usbreset(void)
{
int cardno, ctlrno;
Hci *hp;
dprint("usbreset\n");
for(ctlrno = 0; ctlrno < Nhcis; ctlrno++)
if((hp = hciprobe(-1, ctlrno)) != nil)
hcis[ctlrno] = hp;
cardno = ctlrno = 0;
while(cardno < Nhcis && ctlrno < Nhcis && hcitypes[cardno].type != nil)
if(hcis[ctlrno] != nil)
ctlrno++;
else{
hp = hciprobe(cardno, ctlrno);
if(hp == nil)
cardno++;
hcis[ctlrno++] = hp;
}
if(hcis[Nhcis-1] != nil)
print("usbreset: bug: Nhcis too small\n");
}
static void
usbinit(void)
{
Hci *hp;
int ctlrno;
Ep *d;
char info[40];
dprint("usbinit\n");
for(ctlrno = 0; ctlrno < Nhcis; ctlrno++){
hp = hcis[ctlrno];
if(hp != nil){
if(hp->init != nil)
hp->init(hp);
d = newdev(hp, 1, 1);
d->dev->state = Denabled;
d->maxpkt = 64;
snprint(info, sizeof(info), "ports %d", hp->nports);
kstrdup(&d->info, info);
}
}
}
static Chan*
usbattach(char *spec)
{
return devattach(L'u', spec);
}
static Walkqid*
usbwalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, nil, 0, usbgen);
}
static int
usbstat(Chan *c, uchar *db, int n)
{
return devstat(c, db, n, nil, 0, usbgen);
}
static ulong
usbload(int speed, int maxpkt)
{
enum{ Hostns = 1000, Hubns = 333 };
ulong l;
ulong bs;
l = 0;
bs = 10UL * maxpkt;
switch(speed){
case Highspeed:
l = 55*8*2 + 2 * (3 + bs) + Hostns;
break;
case Fullspeed:
l = 9107 + 84 * (4 + bs) + Hostns;
break;
case Lowspeed:
l = 64107 + 2 * Hubns + 667 * (3 + bs) + Hostns;
break;
default:
print("usbload: bad speed %d\n", speed);
}
return l / 1000UL;
}
static Chan*
usbopen(Chan *c, int omode)
{
int q;
Ep *ep;
int mode;
mode = openmode(omode);
q = QID(c->qid);
if(q >= Qep0dir && qid2epidx(q) < 0)
error(Eio);
if(q < Qep0dir || isqtype(q, Qepctl) || isqtype(q, Qepdir))
return devopen(c, omode, nil, 0, usbgen);
ep = getep(qid2epidx(q));
if(ep == nil)
error(Eio);
deprint("usbopen q %#x fid %d omode %d\n", q, c->fid, mode);
if(waserror()){
putep(ep);
nexterror();
}
qlock(ep);
if(ep->inuse){
qunlock(ep);
error(Einuse);
}
ep->inuse = 1;
qunlock(ep);
if(waserror()){
ep->inuse = 0;
nexterror();
}
if(mode != OREAD && ep->mode == OREAD)
error(Eperm);
if(mode != OWRITE && ep->mode == OWRITE)
error(Eperm);
if(ep->ttype == Tnone)
error(Enotconf);
ep->clrhalt = 0;
ep->rhrepl = -1;
if(ep->load == 0)
ep->load = usbload(ep->dev->speed, ep->maxpkt);
ep->hp->epopen(ep);
poperror();
poperror();
c->mode = mode;
c->flag |= COPEN;
c->offset = 0;
c->aux = nil;
return c;
}
static void
epclose(Ep *ep)
{
qlock(ep);
if(waserror()){
qunlock(ep);
nexterror();
}
if(ep->inuse){
ep->hp->epclose(ep);
ep->inuse = 0;
}
qunlock(ep);
poperror();
}
static void
usbclose(Chan *c)
{
int q;
Ep *ep;
q = QID(c->qid);
if(q < Qep0dir || isqtype(q, Qepctl) || isqtype(q, Qepdir))
return;
ep = getep(qid2epidx(q));
if(ep == nil)
return;
deprint("usbclose q %#x fid %d ref %ld\n", q, c->fid, ep->ref);
if(waserror()){
putep(ep);
nexterror();
}
if(c->flag & COPEN){
free(c->aux);
c->aux = nil;
epclose(ep);
putep(ep);
c->flag &= ~COPEN;
}
poperror();
putep(ep);
}
static long
ctlread(Chan *c, void *a, long n, vlong offset)
{
int q;
char *s;
char *us;
char *se;
Ep *ep;
int i;
q = QID(c->qid);
us = s = smalloc(READSTR);
se = s + READSTR;
if(waserror()){
free(us);
nexterror();
}
if(q == Qctl)
for(i = 0; i < epmax; i++){
ep = getep(i);
if(ep != nil){
if(waserror()){
putep(ep);
nexterror();
}
s = seprint(s, se, "ep%d.%d ", ep->dev->nb, ep->nb);
s = seprintep(s, se, ep, 0);
poperror();
}
putep(ep);
}
else{
ep = getep(qid2epidx(q));
if(ep == nil)
error(Eio);
if(waserror()){
putep(ep);
nexterror();
}
if(c->aux != nil){
strecpy(s, se, c->aux);
free(c->aux);
c->aux = nil;
}else
seprintep(s, se, ep, 0);
poperror();
putep(ep);
}
n = readstr(offset, a, n, us);
poperror();
free(us);
return n;
}
static long
rhubread(Ep *ep, void *a, long n)
{
char *b;
if(ep->dev->isroot == 0 || ep->nb != 0 || n < 2)
return -1;
if(ep->rhrepl < 0)
return -1;
b = a;
memset(b, 0, n);
PUT2(b, ep->rhrepl);
ep->rhrepl = -1;
return n;
}
static long
rhubwrite(Ep *ep, void *a, long n)
{
uchar *s;
int cmd;
int feature;
int port;
Hci *hp;
if(ep->dev == nil || ep->dev->isroot == 0 || ep->nb != 0)
return -1;
if(n != Rsetuplen)
error("root hub is a toy hub");
ep->rhrepl = -1;
s = a;
if(s[Rtype] != (Rh2d|Rclass|Rother) && s[Rtype] != (Rd2h|Rclass|Rother))
error("root hub is a toy hub");
hp = ep->hp;
cmd = s[Rreq];
feature = GET2(s+Rvalue);
port = GET2(s+Rindex);
if(port < 1 || port > hp->nports)
error("bad hub port number");
switch(feature){
case Rportenable:
ep->rhrepl = hp->portenable(hp, port, cmd == Rsetfeature);
break;
case Rportreset:
ep->rhrepl = hp->portreset(hp, port, cmd == Rsetfeature);
break;
case Rgetstatus:
ep->rhrepl = hp->portstatus(hp, port);
break;
default:
ep->rhrepl = 0;
}
return n;
}
static long
usbread(Chan *c, void *a, long n, vlong offset)
{
int q;
Ep *ep;
int nr;
q = QID(c->qid);
if(c->qid.type == QTDIR)
return devdirread(c, a, n, nil, 0, usbgen);
if(q == Qctl || isqtype(q, Qepctl))
return ctlread(c, a, n, offset);
ep = getep(qid2epidx(q));
if(ep == nil)
error(Eio);
if(waserror()){
putep(ep);
nexterror();
}
if(ep->dev->state == Ddetach)
error(Edetach);
if(ep->mode == OWRITE || ep->inuse == 0)
error(Ebadusefd);
switch(ep->ttype){
case Tnone:
error("endpoint not configured");
case Tctl:
nr = rhubread(ep, a, n);
if(nr >= 0){
n = nr;
break;
}
default:
ddeprint("\nusbread q %#x fid %d cnt %ld off %lld\n",q,c->fid,n,offset);
n = ep->hp->epread(ep, a, n);
break;
}
poperror();
putep(ep);
return n;
}
static long
pow2(int n)
{
return 1 << n;
}
static void
setmaxpkt(Ep *ep, char* s)
{
long spp;
if(ep->dev->speed == Highspeed)
spp = (ep->hz * ep->pollival * ep->ntds + 7999) / 8000;
else
spp = (ep->hz * ep->pollival + 999) / 1000;
ep->maxpkt = spp * ep->samplesz;
deprint("usb: %s: setmaxpkt: hz %ld poll %ld"
" ntds %d %s speed -> spp %ld maxpkt %ld\n", s,
ep->hz, ep->pollival, ep->ntds, spname[ep->dev->speed],
spp, ep->maxpkt);
if(ep->maxpkt > 1024){
print("usb: %s: maxpkt %ld > 1024. truncating\n", s, ep->maxpkt);
ep->maxpkt = 1024;
}
}
static long
epctl(Ep *ep, Chan *c, void *a, long n)
{
int i, l, mode, nb, tt;
char *b, *s;
Cmdbuf *cb;
Cmdtab *ct;
Ep *nep;
Udev *d;
static char *Info = "info ";
d = ep->dev;
cb = parsecmd(a, n);
if(waserror()){
free(cb);
nexterror();
}
ct = lookupcmd(cb, epctls, nelem(epctls));
if(ct == nil)
error(Ebadctl);
i = ct->index;
if(i == CMnew || i == CMspeed || i == CMhub || i == CMpreset)
if(ep != ep->ep0)
error("allowed only on a setup endpoint");
if(i != CMclrhalt && i != CMdetach && i != CMdebugep && i != CMname)
if(ep != ep->ep0 && ep->inuse != 0)
error("must configure before using");
switch(i){
case CMnew:
deprint("usb epctl %s\n", cb->f[0]);
nb = strtol(cb->f[1], nil, 0);
if(nb < 0 || nb >= Ndeveps)
error("bad endpoint number");
tt = name2ttype(cb->f[2]);
if(tt == Tnone)
error("unknown endpoint type");
mode = name2mode(cb->f[3]);
if(mode < 0)
error("unknown i/o mode");
newdevep(ep, nb, tt, mode);
break;
case CMnewdev:
deprint("usb epctl %s\n", cb->f[0]);
if(ep != ep->ep0 || d->ishub == 0)
error("not a hub setup endpoint");
l = name2speed(cb->f[1]);
if(l == Nospeed)
error("speed must be full|low|high");
nep = newdev(ep->hp, 0, 0);
nep->dev->speed = l;
if(nep->dev->speed  != Lowspeed)
nep->maxpkt = 64;
nep->dev->hub = d->nb;
nep->dev->port = atoi(cb->f[2]);
l = sizeof(up->genbuf);
snprint(up->genbuf, l, "ep%d.%d", nep->dev->nb, nep->nb);
kstrdup(&c->aux, up->genbuf);
break;
case CMhub:
deprint("usb epctl %s\n", cb->f[0]);
d->ishub = 1;
break;
case CMspeed:
l = name2speed(cb->f[1]);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(l == Nospeed)
error("speed must be full|low|high");
qlock(ep->ep0);
d->speed = l;
qunlock(ep->ep0);
break;
case CMmaxpkt:
l = strtoul(cb->f[1], nil, 0);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(l < 1 || l > 1024)
error("maxpkt not in [1:1024]");
qlock(ep);
ep->maxpkt = l;
qunlock(ep);
break;
case CMntds:
l = strtoul(cb->f[1], nil, 0);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(l < 1 || l > 3)
error("ntds not in [1:3]");
qlock(ep);
ep->ntds = l;
qunlock(ep);
break;
case CMpollival:
if(ep->ttype != Tintr && ep->ttype != Tiso)
error("not an intr or iso endpoint");
l = strtoul(cb->f[1], nil, 0);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(ep->ttype == Tiso ||
(ep->ttype == Tintr && ep->dev->speed == Highspeed)){
if(l < 1 || l > 16)
error("pollival power not in [1:16]");
l = pow2(l-1);
}else
if(l < 1 || l > 255)
error("pollival not in [1:255]");
qlock(ep);
ep->pollival = l;
if(ep->ttype == Tiso)
setmaxpkt(ep, "pollival");
qunlock(ep);
break;
case CMsamplesz:
if(ep->ttype != Tiso)
error("not an iso endpoint");
l = strtoul(cb->f[1], nil, 0);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(l <= 0 || l > 8)
error("samplesz not in [1:8]");
qlock(ep);
ep->samplesz = l;
setmaxpkt(ep, "samplesz");
qunlock(ep);
break;
case CMhz:
if(ep->ttype != Tiso)
error("not an iso endpoint");
l = strtoul(cb->f[1], nil, 0);
deprint("usb epctl %s %d\n", cb->f[0], l);
if(l <= 0 || l > 100000)
error("hz not in [1:100000]");
qlock(ep);
ep->hz = l;
setmaxpkt(ep, "hz");
qunlock(ep);
break;
case CMclrhalt:
qlock(ep);
deprint("usb epctl %s\n", cb->f[0]);
ep->clrhalt = 1;
qunlock(ep);
break;
case CMinfo:
deprint("usb epctl %s\n", cb->f[0]);
l = strlen(Info);
s = a;
if(n < l+2 || strncmp(Info, s, l) != 0)
error(Ebadctl);
if(n > 1024)
n = 1024;
b = smalloc(n);
memmove(b, s+l, n-l);
b[n-l] = 0;
if(b[n-l-1] == '\n')
b[n-l-1] = 0;
qlock(ep);
free(ep->info);
ep->info = b;
qunlock(ep);
break;
case CMaddress:
deprint("usb epctl %s\n", cb->f[0]);
ep->dev->state = Denabled;
break;
case CMdetach:
if(ep->dev->isroot != 0)
error("can't detach a root hub");
deprint("usb epctl %s ep%d.%d\n",
cb->f[0], ep->dev->nb, ep->nb);
ep->dev->state = Ddetach;
for(i = 0; i < nelem(ep->dev->eps); i++)
putep(ep->dev->eps[i]);
break;
case CMdebugep:
if(strcmp(cb->f[1], "on") == 0)
ep->debug = 1;
else if(strcmp(cb->f[1], "off") == 0)
ep->debug = 0;
else
ep->debug = strtoul(cb->f[1], nil, 0);
print("usb: ep%d.%d debug %d\n",
ep->dev->nb, ep->nb, ep->debug);
break;
case CMname:
deprint("usb epctl %s %s\n", cb->f[0], cb->f[1]);
validname(cb->f[1], 0);
kstrdup(&ep->name, cb->f[1]);
break;
case CMtmout:
deprint("usb epctl %s\n", cb->f[0]);
if(ep->ttype == Tiso || ep->ttype == Tctl)
error("ctl ignored for this endpoint type");
ep->tmout = strtoul(cb->f[1], nil, 0);
if(ep->tmout != 0 && ep->tmout < Xfertmout)
ep->tmout = Xfertmout;
break;
case CMpreset:
deprint("usb epctl %s\n", cb->f[0]);
if(ep->ttype != Tctl)
error("not a control endpoint");
if(ep->dev->state != Denabled)
error("forbidden on devices not enabled");
ep->dev->state = Dreset;
break;
default:
panic("usb: unknown epctl %d", ct->index);
}
free(cb);
poperror();
return n;
}
static long
usbctl(void *a, long n)
{
Cmdtab *ct;
Cmdbuf *cb;
Ep *ep;
int i;
cb = parsecmd(a, n);
if(waserror()){
free(cb);
nexterror();
}
ct = lookupcmd(cb, usbctls, nelem(usbctls));
dprint("usb ctl %s\n", cb->f[0]);
switch(ct->index){
case CMdebug:
if(strcmp(cb->f[1], "on") == 0)
debug = 1;
else if(strcmp(cb->f[1], "off") == 0)
debug = 0;
else
debug = strtol(cb->f[1], nil, 0);
print("usb: debug %d\n", debug);
for(i = 0; i < epmax; i++)
if((ep = getep(i)) != nil){
ep->hp->debug(ep->hp, debug);
putep(ep);
}
break;
case CMdump:
dumpeps();
break;
}
free(cb);
poperror();
return n;
}
static long
ctlwrite(Chan *c, void *a, long n)
{
int q;
Ep *ep;
q = QID(c->qid);
if(q == Qctl)
return usbctl(a, n);
ep = getep(qid2epidx(q));
if(ep == nil)
error(Eio);
if(waserror()){
putep(ep);
nexterror();
}
if(ep->dev->state == Ddetach)
error(Edetach);
if(isqtype(q, Qepctl) && c->aux != nil){
free(c->aux);
c->aux = nil;
error("read, not write, expected");
}
n = epctl(ep, c, a, n);
putep(ep);
poperror();
return n;
}
static long
usbwrite(Chan *c, void *a, long n, vlong off)
{
int nr, q;
Ep *ep;
if(c->qid.type == QTDIR)
error(Eisdir);
q = QID(c->qid);
if(q == Qctl || isqtype(q, Qepctl))
return ctlwrite(c, a, n);
ep = getep(qid2epidx(q));
if(ep == nil)
error(Eio);
if(waserror()){
putep(ep);
nexterror();
}
if(ep->dev->state == Ddetach)
error(Edetach);
if(ep->mode == OREAD || ep->inuse == 0)
error(Ebadusefd);
switch(ep->ttype){
case Tnone:
error("endpoint not configured");
case Tctl:
nr = rhubwrite(ep, a, n);
if(nr >= 0){
n = nr;
break;
}
default:
ddeprint("\nusbwrite q %#x fid %d cnt %ld off %lld\n",q, c->fid, n, off);
ep->hp->epwrite(ep, a, n);
}
putep(ep);
poperror();
return n;
}
void
usbshutdown(void)
{
Hci *hp;
int i;
for(i = 0; i < Nhcis; i++){
hp = hcis[i];
if(hp == nil)
continue;
if(hp->shutdown == nil)
print("#u: no shutdown function for %s\n", hp->type);
else
hp->shutdown(hp);
}
}
Dev usbdevtab = {
L'u',
"usb",
usbreset,
usbinit,
usbshutdown,
usbattach,
usbwalk,
usbstat,
usbopen,
devcreate,
usbclose,
usbread,
devbread,
usbwrite,
devbwrite,
devremove,
devwstat,
};