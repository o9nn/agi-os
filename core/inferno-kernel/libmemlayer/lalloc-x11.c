#include "lib9.h"
#include "image.h"
#include "memimage.h"
#include "memlayer.h"
#include "../memimage/xmem.h"
static ulong colorword;
static Memdata colordata = {
nil,
&colorword
};
static Memimage paint =
{
{ 0, 0, 1, 1 },
{ -1000000, -1000000, 10000000, 1000000 },
0,
1,
&colordata,
0,
1,
0,
};
static Memimage *xpaint;
static void
setcolor(int val, int ldepth)
{
int bpp;
paint.ldepth = ldepth;
bpp = 1<<ldepth;
val &= ~(0xFF>>bpp);
for(; bpp<32; bpp<<=1)
val |= val<<bpp;
colorword = val;
}
ulong*
makememones(void)
{
Xmem *xm;
extern Memimage	screenimage;
extern void drawreset();
if(memones->X)
return;
drawreset();
xm = malloc(sizeof(Xmem));
if(xm == nil){
print("can't alloc for screen pixmap\n");
return;
}
if(screenimage.ldepth == 0)
xm->pmid0 = xscreenid;
else
xm->pmid0 = PMundef;
xm->pmid = xscreenid;
xm->wordp = &xm->word;
screenimage.X = xm;
screenimage.data->data = &xm->word;
memones = allocmemimage(paint.r, paint.ldepth);
memones->clipr = paint.clipr;
memones->repl = 1;
memfillcolor(memones, ~0);
return screenimage.data->data;
}
Memimage*
memlalloc(Memscreen *s, Rectangle screenr, Refreshfn refreshfn, void *refreshptr, int val)
{
Memimage *n;
Memlayer *l;
Xmem *xm, *sxm;
n = malloc(sizeof(Memimage));
if(n == nil)
return nil;
l = malloc(sizeof(Memlayer));
if(l == nil){
free(n);
return nil;
}
xm = malloc(sizeof(Xmem));
if(xm == nil){
free(l);
free(n);
return nil;
}
if(refreshfn) {
l->save = nil;
}
else{
l->save = allocmemimage(screenr, s->image->ldepth);
if(l->save == nil){
free(l);
free(n);
free(xm);
return nil;
}
if(val >= 0)
memfillcolor(l->save, val);
}
n->r = screenr;
n->clipr = screenr;
n->ldepth = s->image->ldepth;
n->repl = 0;
n->data = s->image->data;
n->zero = s->image->zero;
n->width = s->image->width;
n->layer = l;
n->X = xm;
sxm = s->image->X;
xm->pmid0 = sxm->pmid0;
xm->pmid = sxm->pmid;
xm->flag = 0;
xm->wordp = sxm->wordp;
l->screen = s;
l->refreshfn = refreshfn;
l->screenr = screenr;
l->delta = Pt(0,0);
l->front = s->rearmost;
l->rear = nil;
if(s->rearmost)
s->rearmost->layer->rear = n;
s->rearmost = n;
if(s->frontmost == nil)
s->frontmost = n;
l->clear = 0;
l->refreshptr = nil;
memltofront(n);
l->refreshptr = refreshptr;
if(val >= 0){
setcolor(val, s->image->ldepth);
if(xpaint == nil){
xpaint = allocmemimage(paint.r, paint.ldepth);
if(xpaint == nil) {
if(l->save != nil)
freememimage(l->save);
free(l);
free(n);
free(xm);
return nil;
}
xpaint->clipr = paint.clipr;
xpaint->repl = 1;
}
((Xmem*)(xpaint->X))->word = colorword;
((Xmem*)(xpaint->X))->flag |= XXonepixel;
memfillcolor(xpaint, val);
memdraw(n, n->r, xpaint, n->r.min, memones, n->r.min, S);
}
return n;
}