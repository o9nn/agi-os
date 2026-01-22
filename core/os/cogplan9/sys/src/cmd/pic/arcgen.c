#include <stdio.h>
#include <math.h>
#include "pic.h"
#include "y.tab.h"
void arc_extreme(double, double, double, double, double, double);
int quadrant(double x, double y);
obj *arcgen(int type)
{
static double prevw = HT10;
static double prevh = HT5;
static double prevrad = HT2;
static int dtox[2][4] ={ 1, -1, -1, 1, 1, 1, -1, -1 };
static int dtoy[2][4] ={ 1, 1, -1, -1, -1, 1, 1, -1 };
static int dctrx[2][4] ={ 0, -1, 0, 1, 0, 1, 0, -1 };
static int dctry[2][4] ={ 1, 0, -1, 0, -1, 0, 1, 0 };
static int nexthv[2][4] ={ U_DIR, L_DIR, D_DIR, R_DIR, D_DIR, R_DIR, U_DIR, L_DIR };
double dx2, dy2, ht, phi, r, d;
int i, head, to, at, cw, invis, ddtype, battr;
obj *p, *ppos;
double fromx, fromy, tox, toy, fillval = 0;
Attr *ap;
prevrad = getfval("arcrad");
prevh = getfval("arrowht");
prevw = getfval("arrowwid");
fromx = curx;
fromy = cury;
head = to = at = cw = invis = ddtype = battr = 0;
for (i = 0; i < nattr; i++) {
ap = &attr[i];
switch (ap->a_type) {
case TEXTATTR:
savetext(ap->a_sub, ap->a_val.p);
break;
case HEAD:
head += ap->a_val.i;
break;
case INVIS:
invis = INVIS;
break;
case HEIGHT:
prevh = ap->a_val.f;
break;
case WIDTH:
prevw = ap->a_val.f;
break;
case RADIUS:
prevrad = ap->a_val.f;
break;
case DIAMETER:
prevrad = ap->a_val.f / 2;
break;
case CW:
cw = 1;
break;
case FROM:
ppos = ap->a_val.o;
fromx = ppos->o_x;
fromy = ppos->o_y;
break;
case TO:
ppos = ap->a_val.o;
tox = ppos->o_x;
toy = ppos->o_y;
to++;
break;
case AT:
ppos = ap->a_val.o;
curx = ppos->o_x;
cury = ppos->o_y;
at = 1;
break;
case UP:
hvmode = U_DIR;
break;
case DOWN:
hvmode = D_DIR;
break;
case RIGHT:
hvmode = R_DIR;
break;
case LEFT:
hvmode = L_DIR;
break;
case FILL:
battr |= FILLBIT;
if (ap->a_sub == DEFAULT)
fillval = getfval("fillval");
else
fillval = ap->a_val.f;
break;
}
}
if (!at && !to) {
curx = fromx + prevrad * dctrx[cw][hvmode];
cury = fromy + prevrad * dctry[cw][hvmode];
tox = fromx + prevrad * dtox[cw][hvmode];
toy = fromy + prevrad * dtoy[cw][hvmode];
hvmode = nexthv[cw][hvmode];
}
else if (!at) {
dx2 = (tox - fromx) / 2;
dy2 = (toy - fromy) / 2;
phi = atan2(dy2, dx2) + (cw ? -PI/2 : PI/2);
if (prevrad <= 0.0)
prevrad = dx2*dx2+dy2*dy2;
for (r=prevrad; (d = r*r - (dx2*dx2+dy2*dy2)) <= 0.0; r *= 2)
;
prevrad = r;
ht = sqrt(d);
curx = fromx + dx2 + ht * cos(phi);
cury = fromy + dy2 + ht * sin(phi);
dprintf("dx2,dy2=%g,%g, phi=%g, r,ht=%g,%g\n",
dx2, dy2, phi, r, ht);
}
else if (at && !to) {
tox = fromx + prevrad * dtox[cw][hvmode];
toy = fromy + prevrad * dtoy[cw][hvmode];
hvmode = nexthv[cw][hvmode];
}
if (cw) {
double temp;
temp = fromx; fromx = tox; tox = temp;
temp = fromy; fromy = toy; toy = temp;
if (head == HEAD1)
head = HEAD2;
else if (head == HEAD2)
head = HEAD1;
}
p = makenode(type, 7);
arc_extreme(fromx, fromy, tox, toy, curx, cury);
p->o_val[0] = fromx;
p->o_val[1] = fromy;
p->o_val[2] = tox;
p->o_val[3] = toy;
if (cw) {
curx = fromx;
cury = fromy;
} else {
curx = tox;
cury = toy;
}
p->o_val[4] = prevw;
p->o_val[5] = prevh;
p->o_val[6] = prevrad;
p->o_attr = head | (cw ? CW_ARC : 0) | invis | ddtype | battr;
p->o_fillval = fillval;
if (head)
p->o_nhead = getfval("arrowhead");
dprintf("arc rad %g at %g %g from %g %g to %g %g head %g %g\n",
prevrad, p->o_x, p->o_y,
p->o_val[0], p->o_val[1], p->o_val[2], p->o_val[3], p->o_val[4], p->o_val[5]);
return(p);
}
void arc_extreme(double x0, double y0, double x1, double y1, double xc, double yc)
{
double r, xmin, ymin, xmax, ymax;
int j, k;
x0 -= xc; y0 -= yc;
x1 -= xc; y1 -= yc;
xmin = (x0<x1)?x0:x1; ymin = (y0<y1)?y0:y1;
xmax = (x0>x1)?x0:x1; ymax = (y0>y1)?y0:y1;
r = sqrt(x0*x0 + y0*y0);
if (r > 0.0) {
j = quadrant(x0,y0);
k = quadrant(x1,y1);
if (j == k && y1*x0 < x1*y0) {
if( xmin > -r) xmin = -r; if( ymin > -r) ymin = -r;
if( xmax < r) xmax = r; if( ymax < r) ymax = r;
} else {
while (j != k) {
switch (j) {
case 1: if( ymax < r) ymax = r; break;
case 2: if( xmin > -r) xmin = -r; break;
case 3: if( ymin > -r) ymin = -r; break;
case 4: if( xmax < r) xmax = r; break;
}
j = j%4 + 1;
}
}
}
xmin += xc; ymin += yc;
xmax += xc; ymax += yc;
extreme(xmin, ymin);
extreme(xmax, ymax);
}
quadrant(double x, double y)
{
if ( x>=0.0 && y> 0.0) return(1);
else if( x< 0.0 && y>=0.0) return(2);
else if( x<=0.0 && y< 0.0) return(3);
else if( x> 0.0 && y<=0.0) return(4);
else return 0;
}