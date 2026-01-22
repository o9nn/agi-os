#include "sam.h"
typedef struct Undo Undo;
typedef struct Merge Merge;
struct Undo
{
short	type;
short	mod;
uint	seq;
uint	p0;
uint	n;
};
struct Merge
{
File	*f;
uint	seq;
uint	p0;
uint	n;
uint	nbuf;
Rune	buf[RBUFSIZE];
};
enum
{
Maxmerge = 50,
Undosize = sizeof(Undo)/sizeof(Rune),
};
static Merge	merge;
File*
fileopen(void)
{
File *f;
f = emalloc(sizeof(File));
f->dot.f = f;
f->ndot.f = f;
f->seq = 0;
f->mod = FALSE;
f->unread = TRUE;
Strinit0(&f->name);
return f;
}
int
fileisdirty(File *f)
{
return f->seq != f->cleanseq;
}
static void
wrinsert(Buffer *delta, int seq, int mod, uint p0, Rune *s, uint ns)
{
Undo u;
u.type = Insert;
u.mod = mod;
u.seq = seq;
u.p0 = p0;
u.n = ns;
bufinsert(delta, delta->nc, s, ns);
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
static void
wrdelete(Buffer *delta, int seq, int mod, uint p0, uint p1)
{
Undo u;
u.type = Delete;
u.mod = mod;
u.seq = seq;
u.p0 = p0;
u.n = p1 - p0;
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
void
flushmerge(void)
{
File *f;
f = merge.f;
if(f == nil)
return;
if(merge.seq != f->seq)
panic("flushmerge seq mismatch");
if(merge.n != 0)
wrdelete(&f->epsilon, f->seq, TRUE, merge.p0, merge.p0+merge.n);
if(merge.nbuf != 0)
wrinsert(&f->epsilon, f->seq, TRUE, merge.p0+merge.n, merge.buf, merge.nbuf);
merge.f = nil;
merge.n = 0;
merge.nbuf = 0;
}
void
mergeextend(File *f, uint p0)
{
uint mp0n;
mp0n = merge.p0+merge.n;
if(mp0n != p0){
bufread(f, mp0n, merge.buf+merge.nbuf, p0-mp0n);
merge.nbuf += p0-mp0n;
merge.n = p0-merge.p0;
}
}
void
loginsert(File *f, uint p0, Rune *s, uint ns)
{
if(f->rescuing)
return;
if(ns == 0)
return;
if(ns>STRSIZE)
panic("loginsert");
if(f->seq < seq)
filemark(f);
if(p0 < f->hiposn)
error(Esequence);
if(merge.f != f
|| p0-(merge.p0+merge.n)>Maxmerge
|| merge.nbuf+((p0+ns)-(merge.p0+merge.n))>=RBUFSIZE)
flushmerge();
if(ns>=RBUFSIZE){
if(!(merge.n == 0 && merge.nbuf == 0 && merge.f == nil))
panic("loginsert bad merge state");
wrinsert(&f->epsilon, f->seq, TRUE, p0, s, ns);
}else{
if(merge.f != f){
merge.f = f;
merge.p0 = p0;
merge.seq = f->seq;
}
mergeextend(f, p0);
runemove(merge.buf+merge.nbuf, s, ns);
merge.nbuf += ns;
}
f->hiposn = p0;
if(!f->unread && !f->mod)
state(f, Dirty);
}
void
logdelete(File *f, uint p0, uint p1)
{
if(f->rescuing)
return;
if(p0 == p1)
return;
if(f->seq < seq)
filemark(f);
if(p0 < f->hiposn)
error(Esequence);
if(merge.f != f
|| p0-(merge.p0+merge.n)>Maxmerge
|| merge.nbuf+(p0-(merge.p0+merge.n))>=RBUFSIZE){
flushmerge();
merge.f = f;
merge.p0 = p0;
merge.seq = f->seq;
}
mergeextend(f, p0);
merge.n = p1-merge.p0;
f->hiposn = p1;
if(!f->unread && !f->mod)
state(f, Dirty);
}
void
logsetname(File *f, String *s)
{
Undo u;
Buffer *delta;
if(f->rescuing)
return;
if(f->unread){
filesetname(f, s);
return;
}
if(f->seq < seq)
filemark(f);
delta = &f->epsilon;
u.type = Filename;
u.mod = TRUE;
u.seq = f->seq;
u.p0 = 0;
u.n = s->n;
if(s->n)
bufinsert(delta, delta->nc, s->s, s->n);
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
if(!f->unread && !f->mod)
state(f, Dirty);
}
#ifdef NOTEXT
File*
fileaddtext(File *f, Text *t)
{
if(f == nil){
f = emalloc(sizeof(File));
f->unread = TRUE;
}
f->text = realloc(f->text, (f->ntext+1)*sizeof(Text*));
f->text[f->ntext++] = t;
f->curtext = t;
return f;
}
void
filedeltext(File *f, Text *t)
{
int i;
for(i=0; i<f->ntext; i++)
if(f->text[i] == t)
goto Found;
panic("can't find text in filedeltext");
Found:
f->ntext--;
if(f->ntext == 0){
fileclose(f);
return;
}
memmove(f->text+i, f->text+i+1, (f->ntext-i)*sizeof(Text*));
if(f->curtext == t)
f->curtext = f->text[0];
}
#endif
void
fileuninsert(File *f, Buffer *delta, uint p0, uint ns)
{
Undo u;
u.type = Delete;
u.mod = f->mod;
u.seq = f->seq;
u.p0 = p0;
u.n = ns;
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
void
fileundelete(File *f, Buffer *delta, uint p0, uint p1)
{
Undo u;
Rune *buf;
uint i, n;
u.type = Insert;
u.mod = f->mod;
u.seq = f->seq;
u.p0 = p0;
u.n = p1-p0;
buf = fbufalloc();
for(i=p0; i<p1; i+=n){
n = p1 - i;
if(n > RBUFSIZE)
n = RBUFSIZE;
bufread(f, i, buf, n);
bufinsert(delta, delta->nc, buf, n);
}
fbuffree(buf);
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
int
filereadc(File *f, uint q)
{
Rune r;
if(q >= f->nc)
return -1;
bufread(f, q, &r, 1);
return r;
}
void
filesetname(File *f, String *s)
{
if(!f->unread)
fileunsetname(f, &f->delta);
Strduplstr(&f->name, s);
sortname(f);
f->unread = TRUE;
}
void
fileunsetname(File *f, Buffer *delta)
{
String s;
Undo u;
u.type = Filename;
u.mod = f->mod;
u.seq = f->seq;
u.p0 = 0;
Strinit(&s);
Strduplstr(&s, &f->name);
fullname(&s);
u.n = s.n;
if(s.n)
bufinsert(delta, delta->nc, s.s, s.n);
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
Strclose(&s);
}
void
fileunsetdot(File *f, Buffer *delta, Range dot)
{
Undo u;
u.type = Dot;
u.mod = f->mod;
u.seq = f->seq;
u.p0 = dot.p1;
u.n = dot.p2 - dot.p1;
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
void
fileunsetmark(File *f, Buffer *delta, Range mark)
{
Undo u;
u.type = Mark;
u.mod = f->mod;
u.seq = f->seq;
u.p0 = mark.p1;
u.n = mark.p2 - mark.p1;
bufinsert(delta, delta->nc, (Rune*)&u, Undosize);
}
uint
fileload(File *f, uint p0, int fd, int *nulls)
{
if(f->seq > 0)
panic("undo in file.load unimplemented");
return bufload(f, p0, fd, nulls);
}
int
fileupdate(File *f, int notrans, int toterm)
{
uint p1, p2;
int mod;
if(f->rescuing)
return FALSE;
flushmerge();
mod = f->mod;
f->mod = f->prevmod;
if(f == cmd)
notrans = TRUE;
else{
fileunsetdot(f, &f->delta, f->prevdot);
fileunsetmark(f, &f->delta, f->prevmark);
}
f->dot = f->ndot;
fileundo(f, FALSE, !notrans, &p1, &p2, toterm);
f->mod = mod;
if(f->delta.nc == 0)
f->seq = 0;
if(f == cmd)
return FALSE;
if(f->mod){
f->closeok = 0;
quitok = 0;
}else
f->closeok = 1;
return TRUE;
}
long
prevseq(Buffer *b)
{
Undo u;
uint up;
up = b->nc;
if(up == 0)
return 0;
up -= Undosize;
bufread(b, up, (Rune*)&u, Undosize);
return u.seq;
}
long
undoseq(File *f, int isundo)
{
if(isundo)
return f->seq;
return prevseq(&f->epsilon);
}
void
fileundo(File *f, int isundo, int canredo, uint *q0p, uint *q1p, int flag)
{
Undo u;
Rune *buf;
uint i, n, up;
uint stop;
Buffer *delta, *epsilon;
if(isundo){
delta = &f->delta;
epsilon = &f->epsilon;
stop = f->seq;
}else{
delta = &f->epsilon;
epsilon = &f->delta;
stop = 0;
}
raspstart(f);
while(delta->nc > 0){
if(needoutflush())
raspflush(f);
up = delta->nc-Undosize;
bufread(delta, up, (Rune*)&u, Undosize);
if(isundo){
if(u.seq < stop){
f->seq = u.seq;
raspdone(f, flag);
return;
}
}else{
if(stop == 0)
stop = u.seq;
if(u.seq > stop){
raspdone(f, flag);
return;
}
}
switch(u.type){
default:
panic("undo unknown u.type");
break;
case Delete:
f->seq = u.seq;
if(canredo)
fileundelete(f, epsilon, u.p0, u.p0+u.n);
f->mod = u.mod;
bufdelete(f, u.p0, u.p0+u.n);
raspdelete(f, u.p0, u.p0+u.n, flag);
*q0p = u.p0;
*q1p = u.p0;
break;
case Insert:
f->seq = u.seq;
if(canredo)
fileuninsert(f, epsilon, u.p0, u.n);
f->mod = u.mod;
up -= u.n;
buf = fbufalloc();
for(i=0; i<u.n; i+=n){
n = u.n - i;
if(n > RBUFSIZE)
n = RBUFSIZE;
bufread(delta, up+i, buf, n);
bufinsert(f, u.p0+i, buf, n);
raspinsert(f, u.p0+i, buf, n, flag);
}
fbuffree(buf);
*q0p = u.p0;
*q1p = u.p0+u.n;
break;
case Filename:
f->seq = u.seq;
if(canredo)
fileunsetname(f, epsilon);
f->mod = u.mod;
up -= u.n;
Strinsure(&f->name, u.n+1);
bufread(delta, up, f->name.s, u.n);
f->name.s[u.n] = 0;
f->name.n = u.n;
fixname(&f->name);
sortname(f);
break;
case Dot:
f->seq = u.seq;
if(canredo)
fileunsetdot(f, epsilon, f->dot.r);
f->mod = u.mod;
f->dot.r.p1 = u.p0;
f->dot.r.p2 = u.p0 + u.n;
break;
case Mark:
f->seq = u.seq;
if(canredo)
fileunsetmark(f, epsilon, f->mark);
f->mod = u.mod;
f->mark.p1 = u.p0;
f->mark.p2 = u.p0 + u.n;
break;
}
bufdelete(delta, up, delta->nc);
}
if(isundo)
f->seq = 0;
raspdone(f, flag);
}
void
filereset(File *f)
{
bufreset(&f->delta);
bufreset(&f->epsilon);
f->seq = 0;
}
void
fileclose(File *f)
{
Strclose(&f->name);
bufclose(f);
bufclose(&f->delta);
bufclose(&f->epsilon);
if(f->rasp)
listfree(f->rasp);
free(f);
}
void
filemark(File *f)
{
if(f->unread)
return;
if(f->epsilon.nc)
bufdelete(&f->epsilon, 0, f->epsilon.nc);
if(f != cmd){
f->prevdot = f->dot.r;
f->prevmark = f->mark;
f->prevseq = f->seq;
f->prevmod = f->mod;
}
f->ndot = f->dot;
f->seq = seq;
f->hiposn = 0;
}