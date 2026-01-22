#include "gdevstc.h"
#include <stdlib.h>
static const byte grayvals[2]  = { 0, BLACK };
static const byte  rgbvals[8]  = {
0, RED, GREEN, RED|GREEN, BLUE, BLUE|RED, BLUE|GREEN, BLUE|RED|GREEN};
static const byte cmykvals[16] = {
0, CYAN,MAGENTA,CYAN|MAGENTA,YELLOW,YELLOW|CYAN,YELLOW|MAGENTA,BLACK,
BLACK,BLACK,  BLACK,       BLACK, BLACK,      BLACK,         BLACK,BLACK};
static const byte  *const pixelconversion[5] = {
NULL, grayvals, NULL, rgbvals, cmykvals};
int
stc_fs(stcolor_device *sdev,int npixel,byte *bin,byte *bbuf,byte *out)
{
long *in  = (long *) bin;
long *buf = (long *) bbuf;
if(npixel > 0) {
int bstep,pstart,pstop,pstep,p;
long spotsize,threshold,*errc,*errv;
const byte *pixel2stc;
if(buf[0] >= 0) {
buf[0] = -1;
bstep  = 1;
pstep  = sdev->color_info.num_components;
pstart = 0;
pstop  = npixel * pstep;
} else {
buf[0] =  1;
bstep  = -1;
pstep  = -sdev->color_info.num_components;
pstop  = pstep;
pstart = (1-npixel) * pstep;
out   += npixel-1;
}
if(in == NULL) return 0;
spotsize  = buf[1];
threshold = buf[2];
errc      = buf+3;
errv      = errc + 2*sdev->color_info.num_components;
pixel2stc = pixelconversion[sdev->color_info.num_components];
for(p = pstart; p != pstop; p += pstep) {
int c;
int pixel;
pixel = 0;
for(c = 0; c < sdev->color_info.num_components; c++) {
long cv;
cv = in[p+c] + errv[p+c] + errc[c] - ((errc[c]+4)>>3);
if(cv > threshold) {
pixel |= 1<<c;
cv    -= spotsize;
}
errv[p+c-pstep] += ((3*cv+8)>>4);
errv[p+c      ]  = ((5*cv  )>>4)
+ ((errc[c]+4)>>3);
errc[c]          = cv
- ((5*cv  )>>4)
- ((3*cv+8)>>4);
}
*out = pixel2stc[pixel];
out += bstep;
}
} else {
int i,i2do;
long rand_max;
double offset,scale;
if((sdev->color_info.num_components < 0)                         ||
(sdev->color_info.num_components >= countof(pixelconversion)) ||
(pixelconversion[sdev->color_info.num_components] == NULL)) return -1;
if(( sdev->stc.dither                    == NULL) ||
((sdev->stc.dither->flags & STC_TYPE) != STC_LONG))         return -2;
if(((sdev->stc.dither->flags/STC_SCAN) < 1) ||
( sdev->stc.dither->bufadd          <
(3 + 3*sdev->color_info.num_components)))                  return -3;
if(sdev->stc.dither->flags & (STC_DIRECT | STC_WHITE))         return -4;
buf[0] = 1;
scale  = sdev->stc.dither->minmax[1];
buf[1] = (long)(scale + (scale > 0.0 ? 0.5 : -0.5));
offset = sdev->stc.dither->minmax[0];
scale -= offset;
if((offset+0.5*scale) > 0.0) buf[2] = (long)(offset + 0.5*scale + 0.5);
else                         buf[2] = (long)(offset + 0.5*scale - 0.5);
i2do  = sdev->color_info.num_components * (3-npixel);
rand_max = 0;
if(sdev->stc.flags & STCDFLAG0) {
for(i = 0; i < i2do; ++i) buf[i+3] = 0;
} else {
for(i = 0; i < i2do; ++i) {
buf[i+3] = rand();
if(buf[i+3] > rand_max) rand_max = buf[i+3];
}
scale = (double) buf[1] / (double) rand_max;
for(i = 0; i < sdev->color_info.num_components; ++ i)
buf[i+3] = (long)(0.25000*scale*(buf[i+3]-rand_max/2));
for(     ; i < i2do; ++i)
buf[i+3] = (long)(0.28125*scale*(buf[i+3]-rand_max/2));
}
}
return 0;
}
int
stc_fscmyk(stcolor_device *sdev,int npixel,byte *bin,byte *bbuf,byte *out)
{
long *in  = (long *) bin;
long *buf = (long *) bbuf;
if(npixel > 0) {
int bstep,pstart,pstop,pstep,p;
long spotsize,threshold,*errc,*errv;
if(buf[0] >= 0) {
buf[0] = -1;
bstep  = 1;
pstep  = 4;
pstart = 0;
pstop  = npixel * pstep;
} else {
buf[0] =  1;
bstep  = -1;
pstep  = -4;
pstop  = pstep;
pstart = (1-npixel) * pstep;
out   += npixel-1;
}
spotsize  = buf[1];
threshold = buf[2];
errc      = buf+3;
errv      = errc + 2*4;
for(p = 0; p < 4; ++p) errc[p] = 0;
for(p = pstart; p != pstop; p += pstep) {
int c;
int pixel;
long cv,k;
k  = in[p+3];
cv = k + errv[p+3] + errc[3] - ((errc[3]+4)>>3);
if(cv > threshold) {
pixel  = BLACK;
cv    -= spotsize;
} else {
pixel  = 0;
}
errv[p+3-pstep] += ((3*cv+8)>>4);
errv[p+3      ]  = ((5*cv  )>>4)
+ ((errc[3]+4)>>3);
errc[3]          = cv
- ((5*cv  )>>4)
- ((3*cv+8)>>4);
if(pixel) {
for(c = 0; c < 3; ++c) {
cv  = in[p+c] > k ? in[p+c] : k;
cv += errv[p+c] + errc[c] - ((errc[c]+4)>>3)-spotsize;
if(cv <= (threshold-spotsize)) cv = threshold-spotsize+1;
errv[p+c-pstep] += ((3*cv+8)>>4);
errv[p+c      ]  = ((5*cv  )>>4)
+ ((errc[c]+4)>>3);
errc[c]          = cv
- ((5*cv  )>>4)
- ((3*cv+8)>>4);
}
} else {
for(c = 0; c < 3; ++c) {
cv  = in[p+c];
if(cv > k) {
cv += errv[p+c] + errc[c] - ((errc[c]+4)>>3);
if(cv > threshold) {
cv -= spotsize;
pixel |= CYAN>>c;
}
} else {
cv = k + errv[p+c] + errc[c] - ((errc[c]+4)>>3);
if(cv > threshold ) cv =  threshold;
}
errv[p+c-pstep] += ((3*cv+8)>>4);
errv[p+c      ]  = ((5*cv  )>>4)
+ ((errc[c]+4)>>3);
errc[c]          = cv
- ((5*cv  )>>4)
- ((3*cv+8)>>4);
}
}
*out = pixel;
out += bstep;
}
} else {
int i,i2do;
long rand_max;
double offset,scale;
if(sdev->color_info.num_components != 4)                       return -1;
if(( sdev->stc.dither                    == NULL) ||
((sdev->stc.dither->flags & STC_TYPE) != STC_LONG))         return -2;
if(((sdev->stc.dither->flags/STC_SCAN) < 1) ||
( sdev->stc.dither->bufadd          <
(3 + 3*sdev->color_info.num_components)))                  return -3;
if(sdev->stc.dither->flags & (STC_DIRECT | STC_WHITE))         return -4;
buf[0] = 1;
scale  = sdev->stc.dither->minmax[1];
buf[1] = (long)(scale + (scale > 0.0 ? 0.5 : -0.5));
offset = sdev->stc.dither->minmax[0];
scale -= offset;
if(sdev->stc.flags & STCDFLAG1) {
buf[2] = (long)((sdev->stc.extv[0][sdev->stc.sizv[0]-1] -
sdev->stc.extv[0][0]) * scale / 2.0 + offset);
} else {
if((offset+0.5*scale) > 0.0) buf[2] = (long)(offset + 0.5*scale + 0.5);
else                         buf[2] = (long)(offset + 0.5*scale - 0.5);
}
i2do  = sdev->color_info.num_components * (3-npixel);
rand_max = 0;
if(sdev->stc.flags & STCDFLAG0) {
for(i = 0; i < i2do; ++i) buf[i+3] = 0;
} else {
for(i = 0; i < i2do; ++i) {
buf[i+3] = rand();
if(buf[i+3] > rand_max) rand_max = buf[i+3];
}
scale = (double) buf[1] / (double) rand_max;
for(i = 0; i < sdev->color_info.num_components; ++ i)
buf[i+3] = (long)(0.25000*scale*(buf[i+3]-rand_max/2));
for(     ; i < i2do; ++i)
buf[i+3] = (long)(0.28125*scale*(buf[i+3]-rand_max/2));
}
}
return 0;
}