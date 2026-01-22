#include "gdevprn.h"
#include "gsparams.h"
private dev_proc_print_page(lxm5700m_print_page);
private dev_proc_get_params(lxm_get_params);
private dev_proc_put_params(lxm_put_params);
static const gx_device_procs lxm5700m_procs =
prn_params_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
lxm_get_params, lxm_put_params);
typedef struct lxm_device_s {
gx_device_common;
gx_prn_device_common;
int headSeparation;
} lxm_device;
lxm_device far_data gs_lxm5700m_device = {
prn_device_std_body(lxm_device, lxm5700m_procs, "lxm5700m",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
600, 600,
0.2, 0.0, 0.0, 0.0,
1, lxm5700m_print_page),
16
};
#define init1() \
top(), \
0xA5,0, 3, 0x40,4,5, \
0xA5,0, 3, 0x40,4,6, \
0xA5,0, 3, 0x40,4,7, \
0xA5,0, 3, 0x40,4,8, \
0xA5,0, 4, 0x40,0xe0,0x0b, 3
#define init2() \
0xA5,0, 11, 0x40,0xe0,0x41, 0,0,0,0,0,0,0, 2, \
0xA5,0, 6, 0x40, 5, 0,0,0x80,0 \
#define init3()  \
0x1b,'*', 7,0x73,0x30, \
0x1b,'*', 'm', 0, 0x14, 3, 0x84, 2, 0, 1, 0xf4, \
0x1b,'*', 7,0x63, \
0x1b,'*', 'm', 0, 0x42,  0, 0, \
0xA5,0, 5, 0x40,0xe0,0x80, 8, 7, \
0x1b,'*', 'm', 0, 0x40, 0x15, 7, 0x0f, 0x0f  \
#define top()  \
0xA5,0, 6, 0x40, 3,3,0xc0,0x0f,0x0f \
#define fin()  \
0x1b,'*', 7, 0x65 \
#define outByte(b) putc(b, prn_stream)
#define RIGHTWARD 0
#define LEFTWARD 1
#define overLap 104
#define swipeHeight 208
#define directorySize 13
private int
lxm5700m_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int lnum,minX, maxX, i, l, highestX, leastX, extent;
int direction = RIGHTWARD;
int lastY = 0;
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int in_size = line_size * (swipeHeight);
int swipeBuf_size = in_size;
byte *buf1 = (byte *)gs_malloc(pdev->memory, in_size, 1, "lxm_print_page(buf1)");
byte *swipeBuf =
(byte *)gs_malloc(pdev->memory, swipeBuf_size, 1, "lxm_print_page(swipeBuf)");
byte *in = buf1;
if ( buf1 == 0 || swipeBuf == 0 ) {
if ( buf1 )
quit_ignomiously:
gs_free(pdev->memory, (char *)buf1, in_size, 1, "lxm_print_page(buf1)");
if ( swipeBuf )
gs_free(pdev->memory, (char *)swipeBuf, swipeBuf_size, 1, "lxm_print_page(swipeBuf)");
return_error(gs_error_VMerror);
}
{
static const char init_string[] = {
init1(),
init2(),
init3()
};
fwrite(init_string, 1, sizeof(init_string), prn_stream);
}
for (lnum=0; lnum < pdev->height-swipeHeight ; ) {
byte *in_data;
register byte *outp;
int lcnt;
{
int l;
for (l=lnum; l<pdev->height; l++) {
gdev_prn_get_bits(pdev, l, in, &in_data);
if ( in_data[0] != 0 ||
memcmp((char *)in_data, (char *)in_data + 1, line_size - 1)
) {
break;
}
}
if (l >= pdev->height) {
lnum = l;
break;
}
if (l-lnum > overLap) lnum = l - overLap;
if (lnum >=pdev->height - swipeHeight) {
lnum = pdev->height - swipeHeight;
}
}
lcnt = gdev_prn_copy_scan_lines(pdev, lnum, in, in_size);
if ( lcnt < swipeHeight ) {
memset(in + lcnt * line_size, 0,
in_size - lcnt * line_size);
}
minX = line_size;
maxX = 0;
for (l=0; l<swipeHeight; l++) {
for (i=0; i<minX; i++) {
if (in[l*line_size+i] !=0) {
minX = i;
break;
}
}
for (i=line_size-1; i>=maxX; i--) {
if (in[l*line_size+i] !=0) {
maxX = i;
break;
}
}
}
minX = (minX&(-2));
maxX = (maxX+3)&-2;
highestX = maxX*8-1;
leastX = minX*8;
extent = highestX -leastX +1;
outp = swipeBuf;
#define buffer_store(x) if(outp-swipeBuf>=swipeBuf_size) {\
gs_free(pdev->memory, (char *)swipeBuf, swipeBuf_size, 1, "lxm_print_page(swipeBuf)");\
swipeBuf_size*=2;\
swipeBuf = (byte *)gs_malloc(pdev->memory, swipeBuf_size, 1, "lxm_print_page(swipeBuf)");\
if (swipeBuf == 0) goto quit_ignomiously;\
break;}\
else *outp++ = (x)
{
int sx, sxBy8, sxMask;
int words[directorySize];
bool f, sum;
int retval=0;
int j,c,y;
int j1,c1;
int i,b,x, directory ;
for (x=leastX; x<=highestX; x++) {
for (i=0; i<directorySize; i++) {
words[i] = 0;
}
directory = 0x2000;
switch (direction) {
case(RIGHTWARD):
sx = (x&1)==1 ? x : x-(((lxm_device*)pdev)->headSeparation);
j1 = (x&1);
break;
default:
case(LEFTWARD):
sx = (x&1)==0 ? x : x-((lxm_device*)pdev)->headSeparation;
j1 = 1-(x&1);
}
c1 = 0x8000 >> j1;
sxBy8 = sx/8;
sxMask = 0x80>>(sx%8);
for (i = 0, b=1, y= sxBy8+j1*line_size; i < directorySize; i++,b<<=1) {
sum = false;
for (j=j1,c=c1 ; j<16; j+=2, y+=2*line_size, c>>=2) {
f = (in[y]&sxMask);
if (f) {
words[i] |= c;
sum |= f;
}
}
if (!sum) directory |=b;
}
retval+=2;
buffer_store(directory>>8); buffer_store(directory&0xff);
if (directory != 0x3fff) {
for (i=0; i<directorySize; i++) {
if (words[i] !=0) {
buffer_store(words[i]>>8) ; buffer_store(words[i]&0xff);
retval += 2;
}
}
}
}
#undef buffer_store
}
{
int leastY = lnum;
int sz = 0x1a + outp - swipeBuf;
int deltaY = 2*(leastY - lastY);
lastY = leastY;
outByte(0x1b); outByte('*'); outByte(3);
outByte(deltaY>>8); outByte(deltaY&0xff);
outByte(0x1b); outByte('*'); outByte(4); outByte(0); outByte(0);
outByte(sz>>8); outByte(sz&0xff); outByte(0); outByte(3);
outByte(1); outByte(1); outByte(0x1a);
outByte(0);
outByte(extent>>8); outByte(extent&0xff);
outByte(leastX>>8); outByte(leastX&0xff);
outByte(highestX>>8); outByte(highestX&0xff);
outByte(0); outByte(0);
outByte(0x22); outByte(0x33); outByte(0x44);
outByte(0x55); outByte(1);
fwrite(swipeBuf,1,outp-swipeBuf,prn_stream);
}
lnum += overLap;
direction ^= 1;
}
{
static const char bottom[] = {
fin()
};
fwrite(bottom, 1, sizeof(bottom), prn_stream);
}
fflush(prn_stream);
gs_free(pdev->memory, (char *)swipeBuf, swipeBuf_size, 1, "lxm_print_page(swipeBuf)");
gs_free(pdev->memory, (char *)buf1, in_size, 1, "lxm_print_page(buf1)");
return 0;
}
private int
lxm_get_params(gx_device *pdev, gs_param_list *plist)
{
lxm_device* const ldev = (lxm_device*)pdev;
int code = gdev_prn_get_params(pdev, plist);
if ( code < 0 ) return code;
code = param_write_int(plist,
"HeadSeparation",
(int *)&(ldev->headSeparation));
return code;
}
private int
lxm_put_params(gx_device *pdev, gs_param_list *plist)
{
int ecode;
lxm_device* const ldev = (lxm_device*)pdev;
int trialHeadSeparation=ldev->headSeparation;
int code = param_read_int(plist, "HeadSeparation", &trialHeadSeparation);
if ( trialHeadSeparation < 1 || trialHeadSeparation > 32 )
param_signal_error(plist, "HeadSeparation", gs_error_rangecheck);
ecode = gdev_prn_put_params(pdev, plist);
if ( code < 0 ) return code;
if (ecode < 0) return ecode;
ldev->headSeparation = trialHeadSeparation;
if ( code == 1) return ecode;
return 0;
}