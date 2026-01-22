#include <u.h>
#include <libc.h>
#include <bio.h>
#include <draw.h>
#include <event.h>
#include "page.h"
int ndraw = 0;
enum {
Xaxis,
Yaxis,
};
static void reverse(Image*, Image*, int);
static void shuffle(Image*, Image*, int, int, Image*, int, int);
static void writefile(char *name, Image *im, int gran);
static void halvemaskdim(Image*);
static void swapranges(Image*, Image*, int, int, int, int);
void
rot180(Image *img)
{
Image *tmp;
tmp = xallocimage(display, img->r, img->chan, 0, DNofill);
if(tmp == nil)
return;
reverse(img, tmp, Xaxis);
reverse(img, tmp, Yaxis);
freeimage(tmp);
}
Image *mtmp;
static void
reverse(Image *img, Image *tmp, int axis)
{
Image *mask;
Rectangle r;
int i, d;
d = axis==Xaxis ? Dx(img) : Dy(img);
for(i = 1; i*2 <= d; i *= 2)
;
r = axis==Xaxis ? Rect(0,0, i,100) : Rect(0,0, 100,i);
mask = xallocimage(display, r, GREY1, 1, DTransparent);
mtmp = xallocimage(display, r, GREY1, 1, DTransparent);
if(axis==Xaxis)
r.max.x /= 2;
else
r.max.y /= 2;
draw(mask, r, display->opaque, nil, ZP);
writefile("mask", mask, i);
shuffle(img, tmp, axis, d, mask, i, 0);
freeimage(mask);
}
static void
shuffle(Image *img, Image *tmp, int axis, int imgdim, Image *mask, int maskdim)
{
int slop;
if(maskdim == 0)
return;
slop = imgdim % maskdim;
swapadjacent(img, tmp, axis, imgdim - slop, mask, maskdim);
halvemaskdim(mask, maskdim, axis);
writefile("mask", mask, maskdim/2);
shuffle(img, tmp, axis, imgdim, mask, maskdim/2);
swapranges(img, tmp, 0, imgdim-slop, imgdim, axis);
moveup(im, tmp, lastnn, nn, n, axis);
}
static void
halvemaskdim(Image *m, int maskdim, int axis)
{
Point δ;
δ = axis==Xaxis ? Pt(maskdim,0) : Pt(0,maskdim);
draw(mtmp, mtmp->r, mask, nil, mask->r.min);
gendraw(mask, mask->r, mtmp, δ, mtmp, divpt(δ,2));
writefile("mask", mask, maskdim/2);
}
static void
swapranges(Image *img, Image *tmp, int a, int b, int c, int axis)
{
Rectangle r;
Point δ;
if(a == b || b == c)
return;
writefile("swap", img, 0);
draw(tmp, tmp->r, im, nil, im->r.min);
r = img->r;
if(axis==Xaxis){
δ = Pt(1,0);
r.min.x = img->r.min.x + a;
r.max.x = img->r.min.x + a + (c-b);
}else{
δ = Pt(0,1);
r.min.y = img->r.min.y + a;
r.max.y = img->r.min.y + a + (c-b);
}
draw(img, r, tmp, nil, addpt(tmp->r.min, mulpt(δ, b)));
r = img->r;
if(axis==Xaxis){
r.min.x = img->r.min.x + a + (c-b);
r.max.x = img->r.min.x + c;
}else{
r.min.y = img->r.min.y + a + (c-b);
r.max.y = img->r.min.y + c;
}
draw(img, r, tmp, nil, addpt(tmp->r.min, mulpt(δ, a)));
writefile("swap", img, 1);
}
static void
swapadjacent(Image *img, Image *tmp, int axis, int imgdim, Image *mask, int maskdim)
{
Point δ;
Rectangle r0, r1;
δ = axis==Xaxis ? Pt(1,0) : Pt(0,1);
r0 = img->r;
r1 = img->r;
switch(axis){
case Xaxis:
r0.max.x = imgdim;
r1.min.x = imgdim;
break;
case Yaxis:
r0.max.y = imgdim;
r1.min.y = imgdim;
}
draw(tmp, tmp->r, img, nil,
}
void
interlace(Image *im, Image *tmp, int axis, int n, Image *mask, int gran)
{
Point p0, p1;
Rectangle r0, r1;
r0 = im->r;
r1 = im->r;
switch(axis) {
case Xaxis:
r0.max.x = n;
r1.min.x = n;
p0 = (Point){gran, 0};
p1 = (Point){-gran, 0};
break;
case Yaxis:
r0.max.y = n;
r1.min.y = n;
p0 = (Point){0, gran};
p1 = (Point){0, -gran};
break;
}
draw(tmp, im->r, im, display->black, im->r.min);
gendraw(im, r0, tmp, p0, mask, mask->r.min);
gendraw(im, r0, tmp, p1, mask, p1);
}
static void
writefile(char *name, Image *im, int gran)
{
static int c = 100;
int fd;
char buf[200];
snprint(buf, sizeof buf, "%d%s%d", c++, name, gran);
fd = create(buf, OWRITE, 0666);
if(fd < 0)
return;
writeimage(fd, im, 0);
close(fd);
}