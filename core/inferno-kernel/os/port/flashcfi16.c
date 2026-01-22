#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"../port/error.h"
#include	"../port/flashif.h"
#define	I(x)	(((x)<<24)|((x)<<16)|((x)<<8)|(x))
enum {
ReadArray = I(0xFF),
ReadQuery = I(0x98),
};
#include "flashintel"
static int
cfiget1(Flash *f, ulong a)
{
ulong v;
v = flashget(f, a);
if(f->width == 2 && v == 0xFFFF)
return 0;
return v & 0xFF;
}
static int
cfiget2(Flash *f, ulong i)
{
return (cfiget1(f, i+1)<<8) | cfiget1(f, i);
}
static int
cfiquery(Flash *f)
{
Flashregion *r;
ulong addr;
int i;
flashput(f, 0x55, ReadQuery);
if(!(cfiget1(f, 0x10) == 'Q' && cfiget1(f, 0x11) == 'R' && cfiget1(f, 0x12) == 'Y'))
return 0;
f->alg = cfiget2(f, 0x13);
i = cfiget1(f, 0x27);
if(i > 0 && i < 32)
i = 1<<i;
else
i = 0;
f->devsize = i;
f->size = f->devsize;
if(f->interleave)
f->size *= 2;
i = cfiget2(f, 0x2A);
if(i > 0 && i < 32)
i = 1<<i;
else
i = 0;
f->maxwb = i;
f->nr = cfiget1(f, 0x2C);
if(f->nr != 0){
addr = 0;
for(i=0; i<f->nr; i++){
r = &f->regions[i];
r->n = cfiget2(f, 0x2D+4*i)+1;
r->erasesize = cfiget2(f, 0x2D+2+4*i)*256;
if(r->erasesize == 0)
r->erasesize = 128;
if(f->interleave)
r->erasesize *= 2;
r->start = addr;
r->end = r->start + r->n*r->erasesize;
}
if(1){
iprint("cfi: devsize=%lud maxwb=%d\n", f->devsize, f->maxwb);
for(i=0; i<f->nr; i++){
r = &f->regions[i];
iprint("flash %d: %d %lud %8.8lux %8.8lux\n", i, r->n, r->erasesize, r->start, r->end);
}
}
}else{
f->nr = 1;
f->regions[0] = (Flashregion){1, 0, f->devsize, f->devsize, 0};
}
return 1;
}
static int
reset(Flash *f)
{
if(f->xip)
return -1;
if(f->width == 0)
f->width = 2;
if(!cfiquery(f) || f->alg != 1 && f->alg != 3){
flashput(f, 0x55, ClearStatus);
flashput(f, 0x55, ReadArray);
return -1;
}
f->cmask = 0x00FF00FF;
flashput(f, 0x55, ClearStatus);
flashput(f, 0x55, ReadID);
f->id = cfiget1(f, 0x00);
f->devid = cfiget1(f, 0x01);
flashput(f, 0x55, ClearStatus);
flashput(f, 0x55, ReadArray);
if(f->width == 2){
f->cmask = 0x00FF;
f->write = intelwrite2;
}else{
f->cmask = 0x00FF00FF;
f->write = intelwrite4;
}
f->erasezone = intelerase;
f->suspend = nil;
f->resume = nil;
return 0;
}
void
flashcfi16link(void)
{
addflashcard("cfi16", reset);
}