#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
enum
{
Data= 0,
Iena= 1,
Ircv= (1<<0),
Ixmt= (1<<1),
Irstat=(1<<2),
Imstat=(1<<3),
Istat= 2,
Ipend= 1,
Fenabd=(3<<6),
Fifoctl=2,
Fena= (1<<0),
Fdma= (1<<3),
Ftrig= (1<<6),
Fclear=(3<<1),
Format= 3,
Bits8= (3<<0),
Stop2= (1<<2),
Pena= (1<<3),
Peven= (1<<4),
Pforce=(1<<5),
Break= (1<<6),
Dra= (1<<7),
Mctl= 4,
Dtr= (1<<0),
Rts= (1<<1),
Ri= (1<<2),
Inton= (1<<3),
Loop= (1<<4),
Lstat= 5,
Inready=(1<<0),
Oerror=(1<<1),
Perror=(1<<2),
Ferror=(1<<3),
Berror=(1<<4),
Outready=(1<<5),
Mstat= 6,
Ctsc= (1<<0),
Dsrc= (1<<1),
Rire= (1<<2),
Dcdc= (1<<3),
Cts= (1<<4),
Dsr= (1<<5),
Ringl= (1<<6),
Dcd= (1<<7),
Scratch=7,
Dlsb= 0,
Dmsb= 1,
CTLS= 023,
CTLQ= 021,
Stagesize= 1024,
Nuart= 2,
};
typedef struct Uart Uart;
struct Uart
{
QLock;
int opens;
int enabled;
Uart *elist;
char name[KNAMELEN];
uchar sticky[8];
void* regs;
ulong port;
ulong freq;
uchar mask;
int dev;
int baud;
uchar istat;
int frame;
int overrun;
int (*putc)(Queue*, int);
Queue *iq;
Queue *oq;
Lock flock;
uchar fifoon;
uchar nofifo;
Lock rlock;
uchar istage[Stagesize];
uchar *ip;
uchar *ie;
int haveinput;
Lock tlock;
uchar ostage[Stagesize];
uchar *op;
uchar *oe;
int modem;
int xonoff;
int blocked;
int cts, dsr, dcd;
int ctsbackoff;
int hup_dsr, hup_dcd;
int dohup;
Rendez r;
};
static Uart *uart[Nuart];
static int nuart;
struct Uartalloc {
Lock;
Uart *elist;
} uartalloc;
static void uartintr(Uart*);
#include "uart.h"
static void
uartsetbaud(Uart *p, int rate)
{
ulong brconst;
if(rate <= 0)
return;
p->freq = archuartclock(p->port, rate);
if(p->freq == 0)
return;
brconst = (p->freq+8*rate-1)/(16*rate);
uartwrreg(p, Format, Dra);
uartwr(p, Dmsb, (brconst>>8) & 0xff);
uartwr(p, Dlsb, brconst & 0xff);
uartwrreg(p, Format, 0);
p->baud = rate;
}
static void
uartdsrhup(Uart *p, int n)
{
p->hup_dsr = n;
}
static void
uartdcdhup(Uart *p, int n)
{
p->hup_dcd = n;
}
static void
uartparity(Uart *p, char type)
{
switch(type){
case 'e':
p->sticky[Format] |= Pena|Peven;
break;
case 'o':
p->sticky[Format] &= ~Peven;
p->sticky[Format] |= Pena;
break;
default:
p->sticky[Format] &= ~(Pena|Peven);
break;
}
uartwrreg(p, Format, 0);
}
void
uartbits(Uart *p, int bits)
{
if(bits < 5 || bits > 8)
error(Ebadarg);
p->sticky[Format] &= ~3;
p->sticky[Format] |= bits-5;
uartwrreg(p, Format, 0);
}
void
uartdtr(Uart *p, int n)
{
if(n)
p->sticky[Mctl] |= Dtr;
else
p->sticky[Mctl] &= ~Dtr;
uartwrreg(p, Mctl, 0);
}
void
uartrts(Uart *p, int n)
{
if(n)
p->sticky[Mctl] |= Rts;
else
p->sticky[Mctl] &= ~Rts;
uartwrreg(p, Mctl, 0);
}
static void
uartbreak(Uart *p, int ms)
{
if(ms == 0)
ms = 200;
uartwrreg(p, Format, Break);
tsleep(&up->sleep, return0, 0, ms);
uartwrreg(p, Format, 0);
}
static void
uartfifoon(Uart *p)
{
ulong i, x;
if(p->nofifo || uartrdreg(p, Istat) & Fenabd)
return;
x = splhi();
p->sticky[Fifoctl] = 0;
uartwrreg(p, Fifoctl, Fclear);
for(i = 0; i < 16; i++){
if(uartrdreg(p, Istat)){
}
if(uartrdreg(p, Data)){
}
}
p->fifoon = 1;
p->sticky[Fifoctl] = Fena|Ftrig;
uartwrreg(p, Fifoctl, 0);
p->istat = uartrdreg(p, Istat);
if((p->istat & Fenabd) == 0) {
p->nofifo = 1;
}
splx(x);
}
static void
uartmflow(Uart *p, int n)
{
ilock(&p->tlock);
if(n){
p->sticky[Iena] |= Imstat;
uartwrreg(p, Iena, 0);
p->modem = 1;
p->cts = uartrdreg(p, Mstat) & Cts;
} else {
p->sticky[Iena] &= ~Imstat;
uartwrreg(p, Iena, 0);
p->modem = 0;
p->cts = 1;
}
iunlock(&p->tlock);
}
static void
uartenable(Uart *p)
{
Uart **l;
if(p->enabled)
return;
uartportpower(p, 1);
p->hup_dsr = p->hup_dcd = 0;
p->cts = p->dsr = p->dcd = 0;
p->sticky[Iena] = Ircv | Ixmt | Irstat;
uartwrreg(p, Iena, 0);
uartdtr(p, 1);
uartrts(p, 1);
uartfifoon(p);
ilock(&p->tlock);
p->cts = 1;
p->blocked = 0;
iunlock(&p->tlock);
uartsetbaud(p, p->baud);
lock(&uartalloc);
for(l = &uartalloc.elist; *l; l = &(*l)->elist){
if(*l == p)
break;
}
if(*l == 0){
p->elist = uartalloc.elist;
uartalloc.elist = p;
}
p->enabled = 1;
unlock(&uartalloc);
}
static void
uartdisable(Uart *p)
{
Uart **l;
p->sticky[Iena] = 0;
uartwrreg(p, Iena, 0);
p->sticky[Format] = Bits8;
uartwrreg(p, Format, 0);
uartdtr(p, 0);
uartrts(p, 0);
uartmflow(p, 0);
ilock(&p->tlock);
p->xonoff = p->blocked = 0;
iunlock(&p->tlock);
uartportpower(p, 0);
lock(&uartalloc);
for(l = &uartalloc.elist; *l; l = &(*l)->elist){
if(*l == p){
*l = p->elist;
break;
}
}
p->enabled = 0;
unlock(&uartalloc);
}
static int
stageoutput(Uart *p)
{
int n;
n = qconsume(p->oq, p->ostage, Stagesize);
if(n <= 0)
return 0;
p->op = p->ostage;
p->oe = p->ostage + n;
return n;
}
static void
uartkick0(Uart *p)
{
int i;
if((p->modem && (p->cts == 0)) || p->blocked)
return;
for(i = 0; i < 128; i++){
if(!(uartrdreg(p, Lstat) & Outready))
break;
if(p->op >= p->oe && stageoutput(p) == 0)
break;
uartwr(p, Data, *p->op++);
}
}
static void
uartkick(void *v)
{
Uart *p;
p = v;
ilock(&p->tlock);
uartkick0(p);
iunlock(&p->tlock);
}
static void
uartflow(void *v)
{
Uart *p;
p = v;
if(p->modem)
uartrts(p, 1);
ilock(&p->rlock);
p->haveinput = 1;
iunlock(&p->rlock);
}
static void
uartsetup0(Uart *p)
{
memset(p->sticky, 0, sizeof(p->sticky));
p->sticky[Format] = Bits8;
uartwrreg(p, Format, 0);
p->sticky[Mctl] |= Inton;
uartwrreg(p, Mctl, 0x0);
uartsetbaud(p, 9600);
p->iq = qopen(4*1024, 0, uartflow, p);
p->oq = qopen(4*1024, 0, uartkick, p);
if(p->iq == nil || p->oq == nil)
panic("uartsetup0");
p->ip = p->istage;
p->ie = &p->istage[Stagesize];
p->op = p->ostage;
p->oe = p->ostage;
}
void
uartsetup(ulong port, void *regs, ulong freq, char *name)
{
Uart *p;
if(nuart >= Nuart)
return;
p = xalloc(sizeof(Uart));
uart[nuart] = p;
strcpy(p->name, name);
p->dev = nuart;
nuart++;
p->port = port;
p->regs = regs;
p->freq = freq;
uartsetup0(p);
}
void
uartspecial(int port, int baud, Queue **in, Queue **out, int (*putc)(Queue*, int))
{
Uart *p = uart[port];
uartenable(p);
if(baud)
uartsetbaud(p, baud);
p->putc = putc;
if(in)
*in = p->iq;
if(out)
*out = p->oq;
p->opens++;
}
static void
uartintr(Uart *p)
{
uchar ch;
int s, l;
for (s = uartrdreg(p, Istat); !(s&Ipend); s = uartrdreg(p, Istat)) {
switch(s&0x3f){
case 4:
case 6:
case 12:
while ((l = uartrdreg(p, Lstat)) & Inready) {
if(l & Ferror)
p->frame++;
if(l & Oerror)
p->overrun++;
ch = uartrdreg(p, Data) & 0xff;
if (l & (Berror|Perror|Ferror)) {
continue;
}
if (ch == CTLS || ch == CTLQ) {
ilock(&p->tlock);
if(p->xonoff){
if(ch == CTLS)
p->blocked = 1;
else
p->blocked = 0;
}
iunlock(&p->tlock);
}
if(p->putc)
p->putc(p->iq, ch);
else {
ilock(&p->rlock);
if(p->ip < p->ie)
*p->ip++ = ch;
else
p->overrun++;
p->haveinput = 1;
iunlock(&p->rlock);
}
}
break;
case 2:
uartkick(p);
break;
case 0:
ch = uartrdreg(p, Mstat);
if(ch & Ctsc){
ilock(&p->tlock);
l = p->cts;
p->cts = ch & Cts;
if(l == 0 && p->cts)
p->ctsbackoff = 2;
iunlock(&p->tlock);
}
if (ch & Dsrc) {
l = ch & Dsr;
if(p->hup_dsr && p->dsr && !l){
ilock(&p->rlock);
p->dohup = 1;
iunlock(&p->rlock);
}
p->dsr = l;
}
if (ch & Dcdc) {
l = ch & Dcd;
if(p->hup_dcd && p->dcd && !l){
ilock(&p->rlock);
p->dohup = 1;
iunlock(&p->rlock);
}
p->dcd = l;
}
break;
default:
iprint("weird uart interrupt #%2.2ux\n", s);
break;
}
}
p->istat = s;
}
void
uartclock(void)
{
int n;
Uart *p;
for(p = uartalloc.elist; p; p = p->elist){
if(p->haveinput){
ilock(&p->rlock);
if(p->haveinput){
n = p->ip - p->istage;
if(n > 0 && p->iq){
if(n > Stagesize)
panic("uartclock");
if(qproduce(p->iq, p->istage, n) < 0)
uartrts(p, 0);
else
p->ip = p->istage;
}
p->haveinput = 0;
}
iunlock(&p->rlock);
}
if(p->dohup){
ilock(&p->rlock);
if(p->dohup){
qhangup(p->iq, 0);
qhangup(p->oq, 0);
}
p->dohup = 0;
iunlock(&p->rlock);
}
if(p->ctsbackoff){
ilock(&p->tlock);
if(p->ctsbackoff){
if(--(p->ctsbackoff) == 0)
uartkick0(p);
}
iunlock(&p->tlock);
}
}
}
Dirtab *uartdir;
int ndir;
static void
setlength(int i)
{
Uart *p;
if(i >= 0){
p = uart[i];
if(p && p->opens && p->iq)
uartdir[1+3*i].length = qlen(p->iq);
} else for(i = 0; i < nuart; i++){
p = uart[i];
if(p && p->opens && p->iq)
uartdir[1+3*i].length = qlen(p->iq);
}
}
static void
uartreset(void)
{
int i;
Dirtab *dp;
uartinstall();
ndir = 1+3*nuart;
uartdir = xalloc(ndir * sizeof(Dirtab));
dp = uartdir;
strcpy(dp->name, ".");
mkqid(&dp->qid, 0, 0, QTDIR);
dp->length = 0;
dp->perm = DMDIR|0555;
dp++;
for(i = 0; i < nuart; i++){
strcpy(dp->name, uart[i]->name);
dp->qid.path = NETQID(i, Ndataqid);
dp->perm = 0666;
dp++;
sprint(dp->name, "%sctl", uart[i]->name);
dp->qid.path = NETQID(i, Nctlqid);
dp->perm = 0666;
dp++;
sprint(dp->name, "%sstatus", uart[i]->name);
dp->qid.path = NETQID(i, Nstatqid);
dp->perm = 0444;
dp++;
}
}
static Chan*
uartattach(char *spec)
{
return devattach('t', spec);
}
static Walkqid*
uartwalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, uartdir, ndir, devgen);
}
static int
uartstat(Chan *c, uchar *dp, int n)
{
if(NETTYPE(c->qid.path) == Ndataqid)
setlength(NETID(c->qid.path));
return devstat(c, dp, n, uartdir, ndir, devgen);
}
static Chan*
uartopen(Chan *c, int omode)
{
Uart *p;
c = devopen(c, omode, uartdir, ndir, devgen);
switch(NETTYPE(c->qid.path)){
case Nctlqid:
case Ndataqid:
p = uart[NETID(c->qid.path)];
qlock(p);
if(p->opens++ == 0){
uartenable(p);
qreopen(p->iq);
qreopen(p->oq);
}
qunlock(p);
break;
}
return c;
}
static void
uartclose(Chan *c)
{
Uart *p;
if(c->qid.type & QTDIR)
return;
if((c->flag & COPEN) == 0)
return;
switch(NETTYPE(c->qid.path)){
case Ndataqid:
case Nctlqid:
p = uart[NETID(c->qid.path)];
qlock(p);
if(--(p->opens) == 0){
uartdisable(p);
qclose(p->iq);
qclose(p->oq);
p->ip = p->istage;
p->dcd = p->dsr = p->dohup = 0;
}
qunlock(p);
break;
}
}
static long
uartstatus(Chan*, Uart *p, void *buf, long n, long offset)
{
uchar mstat, fstat, istat, tstat;
char str[256];
str[0] = 0;
tstat = p->sticky[Mctl];
mstat = uartrdreg(p, Mstat);
istat = p->sticky[Iena];
fstat = p->sticky[Format];
snprint(str, sizeof str,
"b%d c%d d%d e%d l%d m%d p%c r%d s%d\n"
"%d %d %d%s%s%s%s%s\n",
p->baud,
p->hup_dcd,
(tstat & Dtr) != 0,
p->hup_dsr,
(fstat & Bits8) + 5,
(istat & Imstat) != 0,
(fstat & Pena) ? ((fstat & Peven) ? 'e' : 'o') : 'n',
(tstat & Rts) != 0,
(fstat & Stop2) ? 2 : 1,
p->dev,
p->frame,
p->overrun,
uartrdreg(p, Istat) & Fenabd ? " fifo" : "",
(mstat & Cts) ? " cts" : "",
(mstat & Dsr) ? " dsr" : "",
(mstat & Dcd) ? " dcd" : "",
(mstat & Ringl) ? " ring" : ""
);
return readstr(offset, buf, n, str);
}
static long
uartread(Chan *c, void *buf, long n, vlong off)
{
Uart *p;
ulong offset = off;
if(c->qid.type & QTDIR){
setlength(-1);
return devdirread(c, buf, n, uartdir, ndir, devgen);
}
p = uart[NETID(c->qid.path)];
switch(NETTYPE(c->qid.path)){
case Ndataqid:
return qread(p->iq, buf, n);
case Nctlqid:
return readnum(offset, buf, n, NETID(c->qid.path), NUMSIZE);
case Nstatqid:
return uartstatus(c, p, buf, n, offset);
}
return 0;
}
static void
uartctl(Uart *p, char *cmd)
{
int i, n;
for(i = 0; i < 16 && qlen(p->oq); i++)
tsleep(&p->r, (int(*)(void*))qlen, p->oq, 125);
if(strncmp(cmd, "break", 5) == 0){
uartbreak(p, 0);
return;
}
n = atoi(cmd+1);
switch(*cmd){
case 'B':
case 'b':
uartsetbaud(p, n);
break;
case 'C':
case 'c':
uartdcdhup(p, n);
break;
case 'D':
case 'd':
uartdtr(p, n);
break;
case 'E':
case 'e':
uartdsrhup(p, n);
break;
case 'f':
case 'F':
qflush(p->oq);
break;
case 'H':
case 'h':
qhangup(p->iq, 0);
qhangup(p->oq, 0);
break;
case 'L':
case 'l':
uartbits(p, n);
break;
case 'm':
case 'M':
uartmflow(p, n);
break;
case 'n':
case 'N':
qnoblock(p->oq, n);
break;
case 'P':
case 'p':
uartparity(p, *(cmd+1));
break;
case 'K':
case 'k':
uartbreak(p, n);
break;
case 'R':
case 'r':
uartrts(p, n);
break;
case 'Q':
case 'q':
qsetlimit(p->iq, n);
qsetlimit(p->oq, n);
break;
case 'W':
case 'w':
break;
case 'X':
case 'x':
ilock(&p->tlock);
p->xonoff = n;
iunlock(&p->tlock);
break;
}
}
static long
uartwrite(Chan *c, void *buf, long n, vlong)
{
Uart *p;
char cmd[32];
if(c->qid.type & QTDIR)
error(Eperm);
p = uart[NETID(c->qid.path)];
lock(&p->flock);
if((p->istat & Fenabd) == 0 && p->fifoon && p->nofifo == 0)
uartfifoon(p);
unlock(&p->flock);
switch(NETTYPE(c->qid.path)){
case Ndataqid:
return qwrite(p->oq, buf, n);
case Nctlqid:
if(n >= sizeof(cmd))
n = sizeof(cmd)-1;
memmove(cmd, buf, n);
cmd[n] = 0;
uartctl(p, cmd);
return n;
}
}
static int
uartwstat(Chan *c, uchar *dp, int n)
{
Dir d;
Dirtab *dt;
if(!iseve())
error(Eperm);
if(c->qid.type & QTDIR)
error(Eperm);
if(NETTYPE(c->qid.path) == Nstatqid)
error(Eperm);
dt = &uartdir[1+3 * NETID(c->qid.path)];
n = convM2D(dp, n, &d, nil);
if(n == 0)
error(Eshortstat);
if(d.mode != ~0UL){
d.mode &= 0666;
dt[0].perm = dt[1].perm = d.mode;
}
return n;
}
Dev uartdevtab = {
't',
"uart",
uartreset,
devinit,
devshutdown,
uartattach,
uartwalk,
uartstat,
uartopen,
devcreate,
uartclose,
uartread,
devbread,
uartwrite,
devbwrite,
devremove,
uartwstat,
};