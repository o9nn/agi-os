#include "gdevstc.h"
int
stc_gsmono(stcolor_device *sdev,int npixel,byte *in,byte *buf,byte *out)
{
if(npixel > 0) {
if(in != NULL) {
memcpy(out,in,npixel);
} else {
memset(out,0,npixel);
}
} else {
int buf_size;
buf_size =
sdev->stc.dither->bufadd
+ (-npixel)
* (sdev->stc.dither->flags/STC_SCAN)
* sdev->color_info.num_components;
if(buf_size > 0) {
memset(buf,0,buf_size * sdev->stc.alg_item);
}
if(sdev->color_info.num_components != 1) return -1;
if((sdev->stc.dither->flags & STC_TYPE) != STC_BYTE) return -2;
if((sdev->stc.dither->flags & STC_DIRECT) != 0) return -3;
}
return 0;
}