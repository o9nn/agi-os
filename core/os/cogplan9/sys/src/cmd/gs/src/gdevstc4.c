#include "gdevstc.h"
private byte *
escp2c_pick_best(byte *col)
{
static byte colour[8][3] = {
{ 0, 0, 0},{255, 0, 0},{ 0,255, 0},{255,255, 0},
{ 0, 0,255},{255, 0,255},{ 0,255,255},{255,255,255}};
register int x, y, z, dx, dy, dz, dz2, dx2, dx3, dx4;
register byte *p;
register long md, d;
md = 16777216;
x = col[0];
y = col[1];
z = col[2];
dx = x*(x-(y>>1));
dy = y*(y-(z>>1));
dz = z*(z-(x>>1));
md = dx + dy + dz;
p = colour[0];
x -= 255;
dx2 = x*(x-(y>>1));
dz2 = z*(z-(x>>1));
if ((d = dx2 + dy + dz2) < md) {md = d; p = colour[1];}
y -= 255;
dx3 = x*(x-(y>>1));
dy = y*(y-(z>>1));
if ((d = dx3 + dy + dz2) < md) {md = d; p = colour[3];}
x += 255;
dx4 = x*(x-(y>>1));
if ((d = dx4 + dy + dz) < md) {md = d; p = colour[2];}
z -= 255;
dy = y*(y-(z>>1));
dz = z*(z-(x>>1));
if ((d = dx4 + dy + dz) < md) {md = d; p = colour[6];}
x -= 255;
dz2 = z*(z-(x>>1));
if ((d = dx3 + dy + dz2) < md) {md = d; p = colour[7];}
y += 255;
dy = y*(y-(z>>1));
if ((d = dx2 + dy + dz2) < md) {md = d; p = colour[5];}
if ((d = dx + dy + dz) < md) {p = colour[4];}
return(p);
}
private void
escp2c_conv_stc(byte *p, byte *q, int i)
{
for(; i; p+=3, i-=3)
*q++ = (*p & RED) | (p[1] & GREEN) | (p[2] & BLUE);
}
#define LIMIT(a) if (a > 255) a = 255; if (a < 0) a = 0
#define LIMIT2(a) if (a > 127) a = 127; if (a < -128) a = -128; \
if (a < 0) a += 256
int
stc_fs2(stcolor_device *sd,int npixel,byte *in,byte *buf,byte *out)
{
int fullcolor_line_size = npixel*3;
if(npixel > 0) {
if(in == NULL) {
memset(buf,0,fullcolor_line_size);
} else {
int i, j, k, e, l, i2, below[3][3], *fb, *b, *bb, *tb;
byte *p, *q, *cp;
static int dir = 1;
p = buf;
if (*p != 0 || memcmp((char *) p, (char *) p + 1, fullcolor_line_size - 1))
{
for(p = in, q=buf, i=fullcolor_line_size;
i--; p++, q++ )
{
j = *p + ((*q & 128) ? *q - 256 : *q);
LIMIT(j);
*p = j;
}
}
p = in;
fb = below[2];
b = below[1];
bb = below[0];
*b = b[1] = b[2] = *bb = bb[1] = bb[2] = 0;
if (dir)
{
for(p = in, q=buf-3,
i=fullcolor_line_size; i; i-=3)
{
cp = escp2c_pick_best(p);
for(i2=3; i2--; p++, q++, fb++, b++, bb++)
{
j = *p;
*p = *cp++;
j -= *p;
if (j != 0)
{
l = (e = (j>>1)) - (*fb = (j>>4));
if (i > 2)
{
k = p[3] + l;
LIMIT(k);
p[3] = k;
}
*b += e - (l = (j>>2) - *fb);
if (i < fullcolor_line_size)
{
l += *bb;
LIMIT2(l);
*q = l;
}
}
else
*fb = 0;
}
tb = bb-3;
bb = b-3;
b = fb-3;
fb = tb;
}
*q = *bb;
q[1] = bb[1];
q[2] = bb[2];
dir = 0;
}
else
{
for(p = in+fullcolor_line_size-1,
q = buf+fullcolor_line_size+2, i=fullcolor_line_size;
i; i-=3)
{
cp = escp2c_pick_best(p-2) + 2;
for(i2=3; i2--; p--, q--, fb++, b++, bb++)
{
j = *p;
*p = *cp--;
j -= *p;
if (j != 0)
{
l = (e = (j>>1)) - (*fb = (j>>4));
if (i > 2)
{
k = p[-3] + l;
LIMIT(k);
p[-3] = k;
}
*b += e - (l = (j>>2) - *fb);
if (i < fullcolor_line_size)
{
l += *bb;
LIMIT2(l);
*q = l;
}
}
else
*fb = 0;
}
tb = bb-3;
bb = b-3;
b = fb-3;
fb = tb;
}
*q = *bb;
q[1] = bb[1];
q[2] = bb[2];
dir = 1;
}
escp2c_conv_stc(in, out, fullcolor_line_size);
}
} else {
if(sd->color_info.num_components != 3) return -1;
if(( sd->stc.dither == NULL) ||
((sd->stc.dither->flags & STC_TYPE) != STC_BYTE)) return -2;
if((sd->stc.dither->flags/STC_SCAN) < 1) return -3;
memset(buf,0,-fullcolor_line_size);
}
return 0;
}