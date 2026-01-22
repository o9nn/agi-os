#include "lib9.h"
#include "draw.h"
#include "memdraw.h"
#include "memlayer.h"
int
memlorigin(Memimage *i, Point log, Point scr)
{
Memlayer *l;
Memscreen *s;
Memimage *t, *shad, *nsave;
Rectangle x, newr, oldr;
Point delta;
int overlap, eqlog, eqscr, wasclear;
l = i->layer;
s = l->screen;
oldr = l->screenr;
newr = Rect(scr.x, scr.y, scr.x+Dx(oldr), scr.y+Dy(oldr));
eqscr = eqpt(scr, oldr.min);
eqlog = eqpt(log, i->r.min);
if(eqscr && eqlog)
return 0;
nsave = nil;
if(eqlog==0 && l->save!=nil){
nsave = allocmemimage(Rect(log.x, log.y, log.x+Dx(oldr), log.y+Dy(oldr)), i->chan);
if(nsave == nil)
return -1;
}
memltofront(i);
wasclear = l->clear;
if(nsave){
if(!wasclear)
memimagedraw(nsave, nsave->r, l->save, l->save->r.min, nil, Pt(0,0), S);
freememimage(l->save);
l->save = nsave;
}
delta = subpt(log, i->r.min);
i->r = rectaddpt(i->r, delta);
i->clipr = rectaddpt(i->clipr, delta);
l->delta = subpt(l->screenr.min, i->r.min);
if(eqscr)
return 0;
shad = memlalloc(s, oldr, memlnorefresh, nil, DNofill);
if(shad == nil)
return -1;
s->frontmost = i;
if(s->rearmost == i)
s->rearmost = shad;
else
l->rear->layer->front = shad;
shad->layer->front = i;
shad->layer->rear = l->rear;
l->rear = shad;
l->front = nil;
shad->layer->clear = 0;
for(t=l->rear->layer->rear; t!=nil; t=t->layer->rear){
x = newr;
overlap = rectclip(&x, t->layer->screenr);
if(overlap){
memlhide(t, x);
t->layer->clear = 0;
}
}
l->screenr = newr;
l->delta = subpt(scr, i->r.min);
l->clear = rectinrect(newr, l->screen->image->clipr);
if(wasclear)
memdraw(s->image, newr, s->image, oldr.min, nil, Pt(0,0), S);
else
memlexpose(i, newr);
memldelete(shad);
return 1;
}
void
memlnorefresh(Memimage *l, Rectangle r, void *v)
{
USED(l);
USED(r.min.x);
USED(v);
}