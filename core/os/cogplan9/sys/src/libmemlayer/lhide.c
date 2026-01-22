#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <memlayer.h>
static
void
lhideop(Memimage *src, Rectangle screenr, Rectangle clipr, void *etc, int insave)
{
Rectangle r;
Memlayer *l;
USED(clipr.min.x);
USED(insave);
l = etc;
if(src != l->save){
r = rectsubpt(screenr, l->delta);
memdraw(l->save, r, src, screenr.min, nil, screenr.min, S);
}
}
void
memlhide(Memimage *i, Rectangle screenr)
{
if(i->layer->save == nil)
return;
if(rectclip(&screenr, i->layer->screen->image->r) == 0)
return;
_memlayerop(lhideop, i, screenr, screenr, i->layer);
}
static
void
lexposeop(Memimage *dst, Rectangle screenr, Rectangle clipr, void *etc, int insave)
{
Memlayer *l;
Rectangle r;
USED(clipr.min.x);
if(insave)
return;
l = etc;
r = rectsubpt(screenr, l->delta);
if(l->save)
memdraw(dst, screenr, l->save, r.min, nil, r.min, S);
else
l->refreshfn(dst, r, l->refreshptr);
}
void
memlexpose(Memimage *i, Rectangle screenr)
{
if(rectclip(&screenr, i->layer->screen->image->r) == 0)
return;
_memlayerop(lexposeop, i, screenr, screenr, i->layer);
}