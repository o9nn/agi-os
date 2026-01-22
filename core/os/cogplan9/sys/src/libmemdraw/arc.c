#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <memlayer.h>
enum
{
R, T, L, B
};
static
Point corners[] = {
{1,1},
{-1,1},
{-1,-1},
{1,-1}
};
static
Point p00;
void
memarc(Memimage *dst, Point c, int a, int b, int t, Memimage *src, Point sp, int alpha, int phi, int op)
{
int i, w, beta, tmp, c1, c2, m, m1;
Rectangle rect;
Point p,	bnd[8];
Memimage *wedge, *figure, *mask;
if(a < 0)
a = -a;
if(b < 0)
b = -b;
w = t;
if(w < 0)
w = 0;
alpha = -alpha;
phi = -phi;
beta = alpha + phi;
if(phi < 0){
tmp = alpha;
alpha = beta;
beta = tmp;
phi = -phi;
}
if(phi >= 360){
memellipse(dst, c, a, b, t, src, sp, op);
return;
}
while(alpha < 0)
alpha += 360;
while(beta < 0)
beta += 360;
c1 = alpha/90 & 3;
c2 = beta/90 & 3;
rect = Rect(-a-w, -b-w, a+w+1, b+w+1);
m = rect.max.x;
if(m < rect.max.y)
m = rect.max.y;
m1 = (m+ICOSSCALE-1) >> 10;
m = m1 << 10;
i = 0;
bnd[i++] = Pt(0,0);
icossin(alpha, &p.x, &p.y);
bnd[i++] = mulpt(p, m1);
for(;;) {
bnd[i++] = mulpt(corners[c1], m);
if(c1==c2 && phi<180)
break;
c1 = (c1+1) & 3;
phi -= 90;
}
icossin(beta, &p.x, &p.y);
bnd[i++] = mulpt(p, m1);
figure = nil;
mask = nil;
wedge = allocmemimage(rect, GREY1);
if(wedge == nil)
goto Return;
memfillcolor(wedge, DTransparent);
memfillpoly(wedge, bnd, i, ~0, memopaque, p00, S);
figure = allocmemimage(rect, GREY1);
if(figure == nil)
goto Return;
memfillcolor(figure, DTransparent);
memellipse(figure, p00, a, b, t, memopaque, p00, S);
mask = allocmemimage(rect, GREY1);
if(mask == nil)
goto Return;
memfillcolor(mask, DTransparent);
memimagedraw(mask, rect, figure, rect.min, wedge, rect.min, S);
c = subpt(c, dst->r.min);
memdraw(dst, dst->r, src, subpt(sp, c), mask, subpt(p00, c), op);
Return:
freememimage(wedge);
freememimage(figure);
freememimage(mask);
}