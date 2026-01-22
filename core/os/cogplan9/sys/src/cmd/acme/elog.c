#include <u.h>
#include <libc.h>
#include <draw.h>
#include <thread.h>
#include <cursor.h>
#include <mouse.h>
#include <keyboard.h>
#include <frame.h>
#include <fcall.h>
#include <plumb.h>
#include "dat.h"
#include "fns.h"
#include "edit.h"
static char Wsequence[] = "warning: changes out of sequence\n";
static int	warned = FALSE;
typedef struct Buflog Buflog;
struct Buflog
{
short	type;
uint		q0;
uint		nd;
uint		nr;
};
enum
{
Buflogsize = sizeof(Buflog)/sizeof(Rune),
};
enum
{
Minstring = 16,
Maxstring = RBUFSIZE,
};
void
eloginit(File *f)
{
if(f->elog.type != Empty)
return;
f->elog.type = Null;
if(f->elogbuf == nil)
f->elogbuf = emalloc(sizeof(Buffer));
if(f->elog.r == nil)
f->elog.r = fbufalloc();
bufreset(f->elogbuf);
}
void
elogclose(File *f)
{
if(f->elogbuf){
bufclose(f->elogbuf);
free(f->elogbuf);
f->elogbuf = nil;
}
}
void
elogreset(File *f)
{
f->elog.type = Null;
f->elog.nd = 0;
f->elog.nr = 0;
}
void
elogterm(File *f)
{
elogreset(f);
if(f->elogbuf)
bufreset(f->elogbuf);
f->elog.type = Empty;
fbuffree(f->elog.r);
f->elog.r = nil;
warned = FALSE;
}
void
elogflush(File *f)
{
Buflog b;
b.type = f->elog.type;
b.q0 = f->elog.q0;
b.nd = f->elog.nd;
b.nr = f->elog.nr;
switch(f->elog.type){
default:
warning(nil, "unknown elog type 0x%ux\n", f->elog.type);
break;
case Null:
break;
case Insert:
case Replace:
if(f->elog.nr > 0)
bufinsert(f->elogbuf, f->elogbuf->nc, f->elog.r, f->elog.nr);
case Delete:
bufinsert(f->elogbuf, f->elogbuf->nc, (Rune*)&b, Buflogsize);
break;
}
elogreset(f);
}
void
elogreplace(File *f, int q0, int q1, Rune *r, int nr)
{
uint gap;
if(q0==q1 && nr==0)
return;
eloginit(f);
if(f->elog.type!=Null && q0<f->elog.q0){
if(warned++ == 0)
warning(nil, Wsequence);
elogflush(f);
}
gap = q0 - (f->elog.q0+f->elog.nd);
if(f->elog.type==Replace && f->elog.nr+gap+nr<Maxstring){
if(gap < Minstring){
if(gap > 0){
bufread(f, f->elog.q0+f->elog.nd, f->elog.r+f->elog.nr, gap);
f->elog.nr += gap;
}
f->elog.nd += gap + q1-q0;
runemove(f->elog.r+f->elog.nr, r, nr);
f->elog.nr += nr;
return;
}
}
elogflush(f);
f->elog.type = Replace;
f->elog.q0 = q0;
f->elog.nd = q1-q0;
f->elog.nr = nr;
if(nr > RBUFSIZE)
editerror("internal error: replacement string too large(%d)", nr);
runemove(f->elog.r, r, nr);
}
void
eloginsert(File *f, int q0, Rune *r, int nr)
{
int n;
if(nr == 0)
return;
eloginit(f);
if(f->elog.type!=Null && q0<f->elog.q0){
if(warned++ == 0)
warning(nil, Wsequence);
elogflush(f);
}
if(f->elog.type==Insert && q0==f->elog.q0 && f->elog.nr+nr<Maxstring){
runemove(f->elog.r+f->elog.nr, r, nr);
f->elog.nr += nr;
return;
}
while(nr > 0){
elogflush(f);
f->elog.type = Insert;
f->elog.q0 = q0;
n = nr;
if(n > RBUFSIZE)
n = RBUFSIZE;
f->elog.nr = n;
runemove(f->elog.r, r, n);
r += n;
nr -= n;
}
}
void
elogdelete(File *f, int q0, int q1)
{
if(q0 == q1)
return;
eloginit(f);
if(f->elog.type!=Null && q0<f->elog.q0+f->elog.nd){
if(warned++ == 0)
warning(nil, Wsequence);
elogflush(f);
}
if(f->elog.type==Delete && f->elog.q0+f->elog.nd==q0){
f->elog.nd += q1-q0;
return;
}
elogflush(f);
f->elog.type = Delete;
f->elog.q0 = q0;
f->elog.nd = q1-q0;
}
#define tracelog 0
void
elogapply(File *f)
{
Buflog b;
Rune *buf;
uint i, n, up, mod;
uint tq0, tq1;
Buffer *log;
Text *t;
int owner;
elogflush(f);
log = f->elogbuf;
t = f->curtext;
buf = fbufalloc();
mod = FALSE;
owner = 0;
if(t->w){
owner = t->w->owner;
if(owner == 0)
t->w->owner = 'E';
}
while(log->nc > 0){
up = log->nc-Buflogsize;
bufread(log, up, (Rune*)&b, Buflogsize);
switch(b.type){
default:
fprint(2, "elogapply: 0x%ux\n", b.type);
abort();
break;
case Replace:
if(tracelog)
warning(nil, "elog replace %d %d (%d %d)\n",
b.q0, b.q0+b.nd, t->q0, t->q1);
if(!mod){
mod = TRUE;
filemark(f);
}
textconstrain(t, b.q0, b.q0+b.nd, &tq0, &tq1);
textdelete(t, tq0, tq1, TRUE);
up -= b.nr;
for(i=0; i<b.nr; i+=n){
n = b.nr - i;
if(n > RBUFSIZE)
n = RBUFSIZE;
bufread(log, up+i, buf, n);
textinsert(t, tq0+i, buf, n, TRUE);
}
if(t->q0 == b.q0 && t->q1 == b.q0)
t->q1 += b.nr;
break;
case Delete:
if(tracelog)
warning(nil, "elog delete %d %d (%d %d)\n",
b.q0, b.q0+b.nd, t->q0, t->q1);
if(!mod){
mod = TRUE;
filemark(f);
}
textconstrain(t, b.q0, b.q0+b.nd, &tq0, &tq1);
textdelete(t, tq0, tq1, TRUE);
break;
case Insert:
if(tracelog)
warning(nil, "elog insert %d %d (%d %d)\n",
b.q0, b.q0+b.nr, t->q0, t->q1);
if(!mod){
mod = TRUE;
filemark(f);
}
textconstrain(t, b.q0, b.q0, &tq0, &tq1);
up -= b.nr;
for(i=0; i<b.nr; i+=n){
n = b.nr - i;
if(n > RBUFSIZE)
n = RBUFSIZE;
bufread(log, up+i, buf, n);
textinsert(t, tq0+i, buf, n, TRUE);
}
if(t->q0 == b.q0 && t->q1 == b.q0)
t->q1 += b.nr;
break;
}
bufdelete(log, up, log->nc);
}
fbuffree(buf);
elogterm(f);
if(t->q0 > f->nc || t->q1 > f->nc || t->q0 > t->q1){
if(!warned)
warning(nil, "elogapply: can't happen %d %d %d\n", t->q0, t->q1, f->nc);
t->q1 = min(t->q1, f->nc);
t->q0 = min(t->q0, t->q1);
}
if(t->w)
t->w->owner = owner;
}