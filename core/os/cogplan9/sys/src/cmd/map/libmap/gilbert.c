#include <u.h>
#include <libc.h>
#include "map.h"
int
Xgilbert(struct place *p, double *x, double *y)
{
struct place q;
q.nlat.s = tan(0.5*(p->nlat.l));
if(q.nlat.s > 1) q.nlat.s = 1;
if(q.nlat.s < -1) q.nlat.s = -1;
q.nlat.c = sqrt(1 - q.nlat.s*q.nlat.s);
q.wlon.l = p->wlon.l/2;
sincos(&q.wlon);
*y = q.nlat.s;
*x = -q.wlon.s*q.nlat.c;
return(1);
}
proj
gilbert(void)
{
return(Xgilbert);
}