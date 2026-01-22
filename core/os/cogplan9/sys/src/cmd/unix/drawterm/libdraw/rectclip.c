#include <u.h>
#include <libc.h>
#include <draw.h>
int
rectclip(Rectangle *rp, Rectangle b)
{
Rectangle *bp = &b;
if((rp->min.x<bp->max.x && bp->min.x<rp->max.x &&
rp->min.y<bp->max.y && bp->min.y<rp->max.y)==0)
return 0;
if(rp->min.x < bp->min.x)
rp->min.x = bp->min.x;
if(rp->min.y < bp->min.y)
rp->min.y = bp->min.y;
if(rp->max.x > bp->max.x)
rp->max.x = bp->max.x;
if(rp->max.y > bp->max.y)
rp->max.y = bp->max.y;
return 1;
}