#include "lib9.h"
#include "draw.h"
#include "memdraw.h"
#include "memlayer.h"
#define	RECUR(a,b,c,d)	_layerop(fn, i, Rect(a.x, b.y, c.x, d.y), clipr, etc, front->layer->rear);
static void
_layerop(
void (*fn)(Memimage*, Rectangle, Rectangle, void*, int),
Memimage *i,
Rectangle r,
Rectangle clipr,
void *etc,
Memimage *front)
{
Rectangle fr;
Top:
if(front == i){
fn(i->layer->screen->image, r, clipr, etc, 0);
return;
}
fr = front->layer->screenr;
if(rectXrect(r, fr) == 0){
front = front->layer->rear;
goto Top;
}
if(fr.max.y < r.max.y){
RECUR(r.min, fr.max, r.max, r.max);
r.max.y = fr.max.y;
}
if(r.min.y < fr.min.y){
RECUR(r.min, r.min, r.max, fr.min);
r.min.y = fr.min.y;
}
if(fr.max.x < r.max.x){
RECUR(fr.max, r.min, r.max, r.max);
r.max.x = fr.max.x;
}
if(r.min.x < fr.min.x){
RECUR(r.min, r.min, fr.min, r.max);
r.min.x = fr.min.x;
}
(*fn)(i->layer->save, r, clipr, etc, 1);
}
void
_memlayerop(
void (*fn)(Memimage*, Rectangle, Rectangle, void*, int),
Memimage *i,
Rectangle screenr,
Rectangle clipr,
void *etc)
{
Memlayer *l;
Rectangle r, scr;
l = i->layer;
if(!rectclip(&screenr, l->screenr))
return;
if(l->clear){
fn(l->screen->image, screenr, clipr, etc, 0);
return;
}
r = screenr;
scr = l->screen->image->clipr;
if(rectclip(&screenr, scr))
_layerop(fn, i, screenr, clipr, etc, l->screen->frontmost);
if(rectinrect(r, scr))
return;
if(!rectXrect(r, scr)){
fn(l->save, r, clipr, etc, 1);
return;
}
if(r.min.y < scr.min.y){
fn(l->save, Rect(r.min.x, r.min.y, r.max.x, scr.min.y), clipr, etc, 1);
r.min.y = scr.min.y;
}
if(r.max.y > scr.max.y){
fn(l->save, Rect(r.min.x, scr.max.y, r.max.x, r.max.y), clipr, etc, 1);
r.max.y = scr.max.y;
}
if(r.min.x < scr.min.x){
fn(l->save, Rect(r.min.x, r.min.y, scr.min.x, r.max.y), clipr, etc, 1);
r.min.x = scr.min.x;
}
if(r.max.x > scr.max.x){
fn(l->save, Rect(scr.max.x, r.min.y, r.max.x, r.max.y), clipr, etc, 1);
}
}