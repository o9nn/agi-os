#include	"dat.h"
#include	"fns.h"
#include	"error.h"
#define QDEBUG if(0)
struct Queue
{
Lock	l;
Block*	bfirst;
Block*	blast;
int	len;
int	dlen;
int	limit;
int	inilim;
int	state;
int	noblock;
int	eof;
void	(*kick)(void*);
void	(*bypass)(void*, Block*);
void*	arg;
QLock	rlock;
Rendez	rr;
QLock	wlock;
Rendez	wr;
char	err[ERRMAX];
int	want;
};
enum
{
Maxatomic	= 32*1024
};
uint	qiomaxatomic = Maxatomic;
void
checkb(Block *b, char *msg)
{
if(b->base > b->lim)
panic("checkb 0 %s %lux %lux", msg, b->base, b->lim);
if(b->rp < b->base)
panic("checkb 1 %s %lux %lux", msg, b->base, b->rp);
if(b->wp < b->base)
panic("checkb 2 %s %lux %lux", msg, b->base, b->wp);
if(b->rp > b->lim)
panic("checkb 3 %s %lux %lux", msg, b->rp, b->lim);
if(b->wp > b->lim)
panic("checkb 4 %s %lux %lux", msg, b->wp, b->lim);
}
void
freeb(Block *b)
{
if(b == nil)
return;
if(b->free) {
b->free(b);
return;
}
b->next = (void*)0xdeadbabe;
b->rp = (void*)0xdeadbabe;
b->wp = (void*)0xdeadbabe;
b->lim = (void*)0xdeadbabe;
b->base = (void*)0xdeadbabe;
free(b);
}
void
freeblist(Block *b)
{
Block *next;
for(; b != 0; b = next){
next = b->next;
b->next = 0;
freeb(b);
}
}
ulong padblockoverhead;
Block*
padblock(Block *bp, int size)
{
int n;
Block *nbp;
if(size >= 0){
if(bp->rp - bp->base >= size){
bp->rp -= size;
return bp;
}
if(bp->next)
panic("padblock 0x%luX", getcallerpc(&bp));
n = BLEN(bp);
padblockoverhead += n;
nbp = allocb(size+n);
nbp->rp += size;
nbp->wp = nbp->rp;
memmove(nbp->wp, bp->rp, n);
nbp->wp += n;
freeb(bp);
nbp->rp -= size;
} else {
size = -size;
if(bp->next)
panic("padblock 0x%luX", getcallerpc(&bp));
if(bp->lim - bp->wp >= size)
return bp;
n = BLEN(bp);
padblockoverhead += n;
nbp = allocb(size+n);
memmove(nbp->wp, bp->rp, n);
nbp->wp += n;
freeb(bp);
}
return nbp;
}
int
blocklen(Block *bp)
{
int len;
len = 0;
while(bp) {
len += BLEN(bp);
bp = bp->next;
}
return len;
}
int
blockalloclen(Block *bp)
{
int len;
len = 0;
while(bp) {
len += BALLOC(bp);
bp = bp->next;
}
return len;
}
Block*
concatblock(Block *bp)
{
int len;
Block *nb, *f;
if(bp->next == 0)
return bp;
nb = allocb(blocklen(bp));
for(f = bp; f; f = f->next) {
len = BLEN(f);
memmove(nb->wp, f->rp, len);
nb->wp += len;
}
freeblist(bp);
return nb;
}
Block*
pullupblock(Block *bp, int n)
{
int i;
Block *nbp;
if(BLEN(bp) >= n)
return bp;
if(bp->lim - bp->rp < n){
nbp = allocb(n);
nbp->next = bp;
bp = nbp;
}
n -= BLEN(bp);
while(nbp = bp->next){
i = BLEN(nbp);
if(i > n) {
memmove(bp->wp, nbp->rp, n);
bp->wp += n;
nbp->rp += n;
return bp;
}
else {
memmove(bp->wp, nbp->rp, i);
bp->wp += i;
bp->next = nbp->next;
nbp->next = 0;
freeb(nbp);
n -= i;
if(n == 0)
return bp;
}
}
freeblist(bp);
return 0;
}
Block*
pullupqueue(Queue *q, int n)
{
Block *b;
if(BLEN(q->bfirst) >= n)
return q->bfirst;
q->bfirst = pullupblock(q->bfirst, n);
for(b = q->bfirst; b != nil && b->next != nil; b = b->next)
;
q->blast = b;
return q->bfirst;
}
Block *
trimblock(Block *bp, int offset, int len)
{
ulong l;
Block *nb, *startb;
if(blocklen(bp) < offset+len) {
freeblist(bp);
return nil;
}
while((l = BLEN(bp)) < offset) {
offset -= l;
nb = bp->next;
bp->next = nil;
freeb(bp);
bp = nb;
}
startb = bp;
bp->rp += offset;
while((l = BLEN(bp)) < len) {
len -= l;
bp = bp->next;
}
bp->wp -= (BLEN(bp) - len);
if(bp->next) {
freeblist(bp->next);
bp->next = nil;
}
return startb;
}
Block*
copyblock(Block *bp, int count)
{
int l;
Block *nbp;
nbp = allocb(count);
for(; count > 0 && bp != 0; bp = bp->next){
l = BLEN(bp);
if(l > count)
l = count;
memmove(nbp->wp, bp->rp, l);
nbp->wp += l;
count -= l;
}
if(count > 0){
memset(nbp->wp, 0, count);
nbp->wp += count;
}
return nbp;
}
Block*
adjustblock(Block* bp, int len)
{
int n;
Block *nbp;
if(len < 0){
freeb(bp);
return nil;
}
if(bp->rp+len > bp->lim){
nbp = copyblock(bp, len);
freeblist(bp);
QDEBUG checkb(nbp, "adjustblock 1");
return nbp;
}
n = BLEN(bp);
if(len > n)
memset(bp->wp, 0, len-n);
bp->wp = bp->rp+len;
QDEBUG checkb(bp, "adjustblock 2");
return bp;
}
int
pullblock(Block **bph, int count)
{
Block *bp;
int n, bytes;
bytes = 0;
if(bph == nil)
return 0;
while(*bph != nil && count != 0) {
bp = *bph;
n = BLEN(bp);
if(count < n)
n = count;
bytes += n;
count -= n;
bp->rp += n;
if(BLEN(bp) == 0) {
*bph = bp->next;
bp->next = nil;
freeb(bp);
}
}
return bytes;
}
Block*
iallocb(int size)
{
Block *b;
ulong addr;
b = kmalloc(sizeof(Block)+size);
if(b == 0)
return 0;
memset(b, 0, sizeof(Block));
addr = (ulong)b + sizeof(Block);
b->base = (uchar*)addr;
b->lim = b->base + size;
b->rp = b->base;
b->wp = b->rp;
return b;
}
Block*
allocb(int size)
{
Block *b;
b = iallocb(size);
if(b == 0)
exhausted("allocb");
return b;
}
Block*
qget(Queue *q)
{
int dowakeup;
Block *b;
lock(&q->l);
b = q->bfirst;
if(b == 0){
q->state |= Qstarve;
unlock(&q->l);
return 0;
}
q->bfirst = b->next;
b->next = 0;
q->len -= BALLOC(b);
q->dlen -= BLEN(b);
QDEBUG checkb(b, "qget");
if((q->state & Qflow) && q->len < q->limit/2){
q->state &= ~Qflow;
dowakeup = 1;
} else
dowakeup = 0;
unlock(&q->l);
if(dowakeup)
Wakeup(&q->wr);
return b;
}
int
qdiscard(Queue *q, int len)
{
Block *b;
int dowakeup, n, sofar;
lock(&q->l);
for(sofar = 0; sofar < len; sofar += n){
b = q->bfirst;
if(b == nil)
break;
QDEBUG checkb(b, "qdiscard");
n = BLEN(b);
if(n <= len - sofar){
q->bfirst = b->next;
b->next = 0;
q->len -= BALLOC(b);
q->dlen -= BLEN(b);
freeb(b);
} else {
n = len - sofar;
b->rp += n;
q->dlen -= n;
}
}
if((q->state & Qflow) && q->len < q->limit){
q->state &= ~Qflow;
dowakeup = 1;
} else
dowakeup = 0;
unlock(&q->l);
if(dowakeup)
Wakeup(&q->wr);
return sofar;
}
int
qconsume(Queue *q, void *vp, int len)
{
Block *b;
int n, dowakeup;
uchar *p = vp;
Block *tofree = nil;
lock(&q->l);
for(;;) {
b = q->bfirst;
if(b == 0){
q->state |= Qstarve;
unlock(&q->l);
return -1;
}
QDEBUG checkb(b, "qconsume 1");
n = BLEN(b);
if(n > 0)
break;
q->bfirst = b->next;
q->len -= BALLOC(b);
b->next = tofree;
tofree = b;
}
if(n < len)
len = n;
memmove(p, b->rp, len);
if((q->state & Qmsg) || len == n)
q->bfirst = b->next;
b->rp += len;
q->dlen -= len;
if((q->state & Qmsg) || len == n){
b->next = 0;
q->len -= BALLOC(b);
q->dlen -= BLEN(b);
b->next = tofree;
tofree = b;
}
if((q->state & Qflow) && q->len < q->limit/2){
q->state &= ~Qflow;
dowakeup = 1;
} else
dowakeup = 0;
unlock(&q->l);
if(dowakeup)
Wakeup(&q->wr);
if(tofree != nil)
freeblist(tofree);
return len;
}
int
qpass(Queue *q, Block *b)
{
int dlen, len, dowakeup;
dowakeup = 0;
lock(&q->l);
if(q->len >= q->limit){
unlock(&q->l);
freeblist(b);
return -1;
}
if(q->state & Qclosed){
unlock(&q->l);
len = blocklen(b);
freeblist(b);
return len;
}
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
len = BALLOC(b);
dlen = BLEN(b);
QDEBUG checkb(b, "qpass");
while(b->next){
b = b->next;
QDEBUG checkb(b, "qpass");
len += BALLOC(b);
dlen += BLEN(b);
}
q->blast = b;
q->len += len;
q->dlen += dlen;
if(q->len >= q->limit/2)
q->state |= Qflow;
if(q->state & Qstarve){
q->state &= ~Qstarve;
dowakeup = 1;
}
unlock(&q->l);
if(dowakeup)
Wakeup(&q->rr);
return len;
}
int
qpassnolim(Queue *q, Block *b)
{
int dlen, len, dowakeup;
dowakeup = 0;
lock(&q->l);
len = BALLOC(b);
if(q->state & Qclosed){
unlock(&q->l);
freeblist(b);
return len;
}
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
dlen = BLEN(b);
QDEBUG checkb(b, "qpass");
while(b->next){
b = b->next;
QDEBUG checkb(b, "qpass");
len += BALLOC(b);
dlen += BLEN(b);
}
q->blast = b;
q->len += len;
q->dlen += dlen;
if(q->len >= q->limit/2)
q->state |= Qflow;
if(q->state & Qstarve){
q->state &= ~Qstarve;
dowakeup = 1;
}
unlock(&q->l);
if(dowakeup)
Wakeup(&q->rr);
return len;
}
Block*
packblock(Block *bp)
{
Block **l, *nbp;
int n;
for(l = &bp; *l; l = &(*l)->next){
nbp = *l;
n = BLEN(nbp);
if((n<<2) < BALLOC(nbp)){
*l = allocb(n);
memmove((*l)->wp, nbp->rp, n);
(*l)->wp += n;
(*l)->next = nbp->next;
freeb(nbp);
}
}
return bp;
}
int
qproduce(Queue *q, void *vp, int len)
{
Block *b;
int dowakeup;
uchar *p = vp;
dowakeup = 0;
lock(&q->l);
if(q->state & Qclosed){
unlock(&q->l);
return -1;
}
if(q->len >= q->limit){
q->state |= Qflow;
unlock(&q->l);
return -1;
}
b = iallocb(len);
if(b == 0){
unlock(&q->l);
print("qproduce: iallocb failed\n");
return -1;
}
memmove(b->wp, p, len);
b->wp += len;
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
q->blast = b;
q->len += BALLOC(b);
q->dlen += BLEN(b);
QDEBUG checkb(b, "qproduce");
if(q->state & Qstarve){
q->state &= ~Qstarve;
dowakeup = 1;
}
if(q->len >= q->limit)
q->state |= Qflow;
unlock(&q->l);
if(dowakeup)
Wakeup(&q->rr);
return len;
}
Block*
qcopy(Queue *q, int len, ulong offset)
{
int sofar;
int n;
Block *b, *nb;
uchar *p;
nb = allocb(len);
lock(&q->l);
b = q->bfirst;
for(sofar = 0; ; sofar += n){
if(b == nil){
unlock(&q->l);
return nb;
}
n = BLEN(b);
if(sofar + n > offset){
p = b->rp + offset - sofar;
n -= offset - sofar;
break;
}
b = b->next;
}
for(sofar = 0; sofar < len;){
if(n > len - sofar)
n = len - sofar;
memmove(nb->wp, p, n);
sofar += n;
nb->wp += n;
b = b->next;
if(b == nil)
break;
n = BLEN(b);
p = b->rp;
}
unlock(&q->l);
return nb;
}
Queue*
qopen(int limit, int msg, void (*kick)(void*), void *arg)
{
Queue *q;
q = kmalloc(sizeof(Queue));
if(q == 0)
return 0;
q->limit = q->inilim = limit;
q->kick = kick;
q->arg = arg;
q->state = msg;
q->state |= Qstarve;
q->eof = 0;
q->noblock = 0;
return q;
}
Queue*
qbypass(void (*bypass)(void*, Block*), void *arg)
{
Queue *q;
q = malloc(sizeof(Queue));
if(q == 0)
return 0;
q->limit = 0;
q->arg = arg;
q->bypass = bypass;
q->state = 0;
return q;
}
static int
notempty(void *a)
{
Queue *q = a;
return (q->state & Qclosed) || q->bfirst != 0;
}
static int
qwait(Queue *q)
{
for(;;){
if(q->bfirst != nil)
break;
if(q->state & Qclosed){
if(++q->eof > 3)
return -1;
if(*q->err && strcmp(q->err, Ehungup) != 0)
return -1;
return 0;
}
q->state |= Qstarve;
unlock(&q->l);
Sleep(&q->rr, notempty, q);
lock(&q->l);
}
return 1;
}
void
qaddlist(Queue *q, Block *b)
{
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
q->len += blockalloclen(b);
q->dlen += blocklen(b);
while(b->next)
b = b->next;
q->blast = b;
}
Block*
qremove(Queue *q)
{
Block *b;
b = q->bfirst;
if(b == nil)
return nil;
q->bfirst = b->next;
b->next = nil;
q->dlen -= BLEN(b);
q->len -= BALLOC(b);
QDEBUG checkb(b, "qremove");
return b;
}
Block*
bl2mem(uchar *p, Block *b, int n)
{
int i;
Block *next;
for(; b != nil; b = next){
i = BLEN(b);
if(i > n){
memmove(p, b->rp, n);
b->rp += n;
return b;
}
memmove(p, b->rp, i);
n -= i;
p += i;
b->rp += i;
next = b->next;
freeb(b);
}
return nil;
}
Block*
mem2bl(uchar *p, int len)
{
int n;
Block *b, *first, **l;
first = nil;
l = &first;
if(waserror()){
freeblist(first);
nexterror();
}
do {
n = len;
if(n > Maxatomic)
n = Maxatomic;
*l = b = allocb(n);
setmalloctag(b, (up->text[0]<<24)|(up->text[1]<<16)|(up->text[2]<<8)|up->text[3]);
memmove(b->wp, p, n);
b->wp += n;
p += n;
len -= n;
l = &b->next;
} while(len > 0);
poperror();
return first;
}
void
qputback(Queue *q, Block *b)
{
b->next = q->bfirst;
if(q->bfirst == nil)
q->blast = b;
q->bfirst = b;
q->len += BALLOC(b);
q->dlen += BLEN(b);
}
static void
qwakeup_unlock(Queue *q)
{
int dowakeup = 0;
if((q->state & Qflow) && q->len < q->limit/2){
q->state &= ~Qflow;
dowakeup = 1;
}
unlock(&q->l);
if(dowakeup){
if(q->kick)
q->kick(q->arg);
Wakeup(&q->wr);
}
}
Block*
qbread(Queue *q, int len)
{
Block *b, *nb;
int n;
qlock(&q->rlock);
if(waserror()){
qunlock(&q->rlock);
nexterror();
}
lock(&q->l);
switch(qwait(q)){
case 0:
unlock(&q->l);
poperror();
qunlock(&q->rlock);
return nil;
case -1:
unlock(&q->l);
error(q->err);
}
b = qremove(q);
n = BLEN(b);
nb = b;
if(n > len){
if((q->state&Qmsg) == 0){
n -= len;
b = allocb(n);
memmove(b->wp, nb->rp+len, n);
b->wp += n;
qputback(q, b);
}
nb->wp = nb->rp + len;
}
qwakeup_unlock(q);
poperror();
qunlock(&q->rlock);
return nb;
}
long
qread(Queue *q, void *vp, int len)
{
Block *b, *first, **l;
int m, n;
qlock(&q->rlock);
if(waserror()){
qunlock(&q->rlock);
nexterror();
}
lock(&q->l);
again:
switch(qwait(q)){
case 0:
unlock(&q->l);
poperror();
qunlock(&q->rlock);
return 0;
case -1:
unlock(&q->l);
error(q->err);
}
if(q->state & Qcoalesce){
b = q->bfirst;
if(BLEN(b) <= 0){
freeb(qremove(q));
goto again;
}
n = 0;
l = &first;
m = BLEN(b);
for(;;) {
*l = qremove(q);
l = &b->next;
n += m;
b = q->bfirst;
if(b == nil)
break;
m = BLEN(b);
if(n+m > len)
break;
}
} else {
first = qremove(q);
n = BLEN(first);
}
unlock(&q->l);
b = bl2mem(vp, first, len);
lock(&q->l);
if(b != nil){
n -= BLEN(b);
if(q->state & Qmsg)
freeb(b);
else
qputback(q, b);
}
qwakeup_unlock(q);
poperror();
qunlock(&q->rlock);
return n;
}
static int
qnotfull(void *a)
{
Queue *q = a;
return q->len < q->limit || (q->state & Qclosed);
}
long
qbwrite(Queue *q, Block *b)
{
int n, dowakeup;
volatile struct {Block *b;} cb;
dowakeup = 0;
n = BLEN(b);
if(q->bypass){
(*q->bypass)(q->arg, b);
return n;
}
cb.b = b;
qlock(&q->wlock);
if(waserror()){
if(cb.b != nil)
freeb(cb.b);
qunlock(&q->wlock);
nexterror();
}
lock(&q->l);
if(q->state & Qclosed){
unlock(&q->l);
error(q->err);
}
if(q->len >= q->limit){
if(q->noblock){
unlock(&q->l);
freeb(b);
poperror();
qunlock(&q->wlock);
return n;
}
}
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
q->blast = b;
b->next = 0;
q->len += BALLOC(b);
q->dlen += n;
QDEBUG checkb(b, "qbwrite");
cb.b = nil;
if(q->state & Qstarve){
q->state &= ~Qstarve;
dowakeup = 1;
}
unlock(&q->l);
if(q->kick && (dowakeup || (q->state&Qkick)))
q->kick(q->arg);
if(dowakeup)
Wakeup(&q->rr);
for(;;){
if(q->noblock || qnotfull(q))
break;
lock(&q->l);
q->state |= Qflow;
unlock(&q->l);
Sleep(&q->wr, qnotfull, q);
}
qunlock(&q->wlock);
poperror();
return n;
}
int
qwrite(Queue *q, void *vp, int len)
{
int n, sofar;
Block *b;
uchar *p = vp;
sofar = 0;
do {
n = len-sofar;
if(n > Maxatomic)
n = Maxatomic;
b = allocb(n);
setmalloctag(b, getcallerpc(&q));
if(waserror()){
freeb(b);
nexterror();
}
memmove(b->wp, p+sofar, n);
poperror();
b->wp += n;
qbwrite(q, b);
sofar += n;
} while(sofar < len && (q->state & Qmsg) == 0);
return len;
}
int
qiwrite(Queue *q, void *vp, int len)
{
int n, sofar, dowakeup;
Block *b;
uchar *p = vp;
dowakeup = 0;
sofar = 0;
do {
n = len-sofar;
if(n > Maxatomic)
n = Maxatomic;
b = iallocb(n);
if (b == 0) {
print("qiwrite: iallocb failed\n");
break;
}
memmove(b->wp, p+sofar, n);
b->wp += n;
lock(&q->l);
QDEBUG checkb(b, "qiwrite");
if(q->bfirst)
q->blast->next = b;
else
q->bfirst = b;
q->blast = b;
q->len += BALLOC(b);
q->dlen += n;
if(q->state & Qstarve){
q->state &= ~Qstarve;
dowakeup = 1;
}
unlock(&q->l);
if(dowakeup){
if(q->kick)
q->kick(q->arg);
Wakeup(&q->rr);
}
sofar += n;
} while(sofar < len && (q->state & Qmsg) == 0);
return sofar;
}
void
qfree(Queue *q)
{
qclose(q);
free(q);
}
void
qclose(Queue *q)
{
Block *bfirst;
if(q == nil)
return;
lock(&q->l);
q->state |= Qclosed;
q->state &= ~(Qflow|Qstarve);
strcpy(q->err, Ehungup);
bfirst = q->bfirst;
q->bfirst = 0;
q->len = 0;
q->dlen = 0;
q->noblock = 0;
unlock(&q->l);
freeblist(bfirst);
Wakeup(&q->rr);
Wakeup(&q->wr);
}
void
qhangup(Queue *q, char *msg)
{
lock(&q->l);
q->state |= Qclosed;
if(msg == 0 || *msg == 0)
strcpy(q->err, Ehungup);
else
kstrcpy(q->err, msg, sizeof q->err);
unlock(&q->l);
Wakeup(&q->rr);
Wakeup(&q->wr);
}
int
qisclosed(Queue *q)
{
return q->state & Qclosed;
}
void
qreopen(Queue *q)
{
lock(&q->l);
q->state &= ~Qclosed;
q->state |= Qstarve;
q->eof = 0;
q->limit = q->inilim;
unlock(&q->l);
}
int
qlen(Queue *q)
{
return q->dlen;
}
int
qwindow(Queue *q)
{
int l;
l = q->limit - q->len;
if(l < 0)
l = 0;
return l;
}
int
qcanread(Queue *q)
{
return q->bfirst!=0;
}
void
qsetlimit(Queue *q, int limit)
{
q->limit = limit;
}
void
qnoblock(Queue *q, int onoff)
{
q->noblock = onoff;
}
void
qflush(Queue *q)
{
Block *bfirst;
lock(&q->l);
bfirst = q->bfirst;
q->bfirst = 0;
q->len = 0;
q->dlen = 0;
unlock(&q->l);
freeblist(bfirst);
Wakeup(&q->wr);
}
int
qfull(Queue *q)
{
return q->state & Qflow;
}
int
qstate(Queue *q)
{
return q->state;
}
int
qclosed(Queue *q)
{
return q->state & Qclosed;
}