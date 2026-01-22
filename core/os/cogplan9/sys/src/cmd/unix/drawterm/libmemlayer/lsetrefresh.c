#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <memlayer.h>
int
memlsetrefresh(Memimage *i, Refreshfn fn, void *ptr)
{
Memlayer *l;
l = i->layer;
if(l->refreshfn!=0 && fn!=0){
l->refreshfn = fn;
l->refreshptr = ptr;
return 1;
}
if(l->refreshfn == 0){
freememimage(l->save);
l->save = nil;
l->refreshfn = fn;
l->refreshptr = ptr;
return 1;
}
l->save = allocmemimage(i->r, i->chan);
if(l->save == nil)
return 0;
l->refreshfn(i, i->r, l->refreshptr);
l->refreshfn = 0;
l->refreshptr = nil;
return 1;
}