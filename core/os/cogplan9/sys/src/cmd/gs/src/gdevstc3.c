#include "gdevstc.h"
int
stc_gsrgb(stcolor_device *sdev,int npixel,byte *ip,byte *buf,byte *out)
{
int   error = 0;
if(npixel > 0) {
int p;
for(p = 0; p < npixel; ++p,++out) {
*out = 0;
if(*ip++) *out |= RED;
if(*ip++) *out |= GREEN;
if(*ip++) *out |= BLUE;
}
} else {
if(sdev->stc.dither->flags & STC_WHITE)              error = -1;
if((sdev->stc.dither->flags & STC_TYPE) != STC_BYTE) error = -2;
if(sdev->color_info.num_components != 3)             error = -3;
if(sdev->stc.dither->flags & STC_DIRECT)             error = -4;
}
return error;
}