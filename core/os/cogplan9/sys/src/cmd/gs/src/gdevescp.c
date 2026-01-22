#include "gdevprn.h"
#ifndef X_DPI
#  define X_DPI 360
#endif
#ifndef Y_DPI
#  define Y_DPI 360
#endif
#define STYLUS_L_MARGIN 0.13
#define STYLUS_B_MARGIN 0.56
#define STYLUS_T_MARGIN 0.34
#ifdef A4
#   define STYLUS_R_MARGIN 0.18
#else
#   define STYLUS_R_MARGIN 0.38
#endif
#define AP3250_L_MARGIN 0.18
#define AP3250_B_MARGIN 0.51
#define AP3250_T_MARGIN 0.34
#define AP3250_R_MARGIN 0.28
private dev_proc_print_page(escp2_print_page);
const gx_device_printer far_data gs_st800_device =
prn_device(prn_std_procs, "st800",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
STYLUS_L_MARGIN, STYLUS_B_MARGIN, STYLUS_R_MARGIN, STYLUS_T_MARGIN,
1, escp2_print_page);
const gx_device_printer far_data gs_ap3250_device =
prn_device(prn_std_procs, "ap3250",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
AP3250_L_MARGIN, AP3250_B_MARGIN, AP3250_R_MARGIN, AP3250_T_MARGIN,
1, escp2_print_page);
private int
escp2_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int line_size = gdev_prn_raster((gx_device_printer *)pdev);
int band_size = 24;
int in_size = line_size * band_size;
byte *buf1 = (byte *)gs_malloc(pdev->memory, in_size, 1, "escp2_print_page(buf1)");
byte *buf2 = (byte *)gs_malloc(pdev->memory, in_size, 1, "escp2_print_page(buf2)");
byte *in = buf1;
byte *out = buf2;
int skip, lnum, top, bottom, left, width;
int auto_feed = 1;
int count, i;
if( !( (pdev->x_pixels_per_inch == 180 &&
pdev->y_pixels_per_inch == 180) ||
(pdev->x_pixels_per_inch == 360 &&
(pdev->y_pixels_per_inch == 360 ||
pdev->y_pixels_per_inch == 180) )) )
return_error(gs_error_rangecheck);
if ( buf1 == 0 || buf2 == 0 )
{	if ( buf1 )
gs_free(pdev->memory, (char *)buf1, in_size, 1, "escp2_print_page(buf1)");
if ( buf2 )
gs_free(pdev->memory, (char *)buf2, in_size, 1, "escp2_print_page(buf2)");
return_error(gs_error_VMerror);
}
fwrite("\033@\033(G\001\000\001", 1, 8, prn_stream);
#ifdef A4
fwrite("\033(U\001\0\n\033(C\002\0t\020\033(c\004\0\0\0t\020",
1, 22, prn_stream);
#endif
if( pdev->y_pixels_per_inch == 360 )
fwrite("\033(U\001\0\012\033+\030", 1, 9, prn_stream);
else
fwrite("\033(U\001\0\024\033+\060", 1, 9, prn_stream);
if( auto_feed ) {
top = (int)(dev_t_margin(pdev) * pdev->y_pixels_per_inch);
bottom = (int)(pdev->height -
dev_b_margin(pdev) * pdev->y_pixels_per_inch);
} else {
top = 0;
bottom = pdev->height;
}
left  = ( (int) (dev_l_margin(pdev) * pdev->x_pixels_per_inch) ) >> 3;
width = ((pdev->width - (int)(dev_r_margin(pdev) * pdev->x_pixels_per_inch)) >> 3) - left;
for ( lnum = top, skip = 0 ; lnum < bottom ; )
{
byte *in_data;
byte *inp;
byte *in_end;
byte *outp;
register byte *p, *q;
int lcnt;
gdev_prn_get_bits(pdev, lnum, in, &in_data);
while ( in_data[0] == 0 &&
!memcmp((char *)in_data, (char *)in_data + 1, line_size - 1) &&
lnum < bottom )
{
lnum++;
skip++;
gdev_prn_get_bits(pdev, lnum, in, &in_data);
}
if(lnum == bottom ) break;
if( skip ) {
fwrite("\033(v\002\000", 1, 5, prn_stream);
fputc(skip & 0xff, prn_stream);
fputc(skip >> 8,   prn_stream);
skip = 0;
}
lcnt = gdev_prn_copy_scan_lines(pdev, lnum, in, in_size);
if( lcnt < band_size )
memset(in + lcnt * line_size, 0, in_size - lcnt * line_size);
for( outp = out, i = 0 ; i < band_size ; i++ ) {
inp = in + i * line_size + left;
in_end = inp + width;
for( p = inp, q = inp + 1 ; q < in_end ; ) {
if( *p != *q ) {
p += 2;
q += 2;
} else {
if( p > inp && *p == *(p-1) )
p--;
for( q++ ; *q == *p && q < in_end ; q++ ) {
if( (q-p) >= 128 ) {
if( p > inp ) {
count = p - inp;
while( count > 128 ) {
*outp++ = '\177';
memcpy(outp, inp, 128);
inp += 128;
outp += 128;
count -= 128;
}
*outp++ = (char) (count - 1);
memcpy(outp, inp, count);
outp += count;
}
*outp++ = '\201';
*outp++ = *p;
p += 128;
inp = p;
}
}
if( (q - p) > 2 ) {
if( p > inp ) {
count = p - inp;
while( count > 128 ) {
*outp++ = '\177';
memcpy(outp, inp, 128);
inp += 128;
outp += 128;
count -= 128;
}
*outp++ = (char) (count - 1);
memcpy(outp, inp, count);
outp += count;
}
count = q - p;
*outp++ = (char) (256 - count + 1);
*outp++ = *p;
p += count;
inp = p;
} else
p = q;
if( q < in_end )
q++;
}
}
if( inp < in_end ) {
count = in_end - inp;
while( count > 128 ) {
*outp++ = '\177';
memcpy(outp, inp, 128);
inp += 128;
outp += 128;
count -= 128;
}
*outp++ = (char) (count - 1);
memcpy(outp, inp, count);
outp += count;
}
}
fwrite("\033.\001", 1, 3, prn_stream);
if(pdev->y_pixels_per_inch == 360)
fputc('\012', prn_stream);
else
fputc('\024', prn_stream);
if(pdev->x_pixels_per_inch == 360)
fputc('\012', prn_stream);
else
fputc('\024', prn_stream);
fputc(band_size, prn_stream);
fputc((width << 3) & 0xff, prn_stream);
fputc( width >> 5,         prn_stream);
fwrite(out, 1, (outp - out), prn_stream);
fwrite("\r\n", 1, 2, prn_stream);
lnum += band_size;
}
fputs("\f\033@", prn_stream);
fflush(prn_stream);
gs_free(pdev->memory, (char *)buf2, in_size, 1, "escp2_print_page(buf2)");
gs_free(pdev->memory, (char *)buf1, in_size, 1, "escp2_print_page(buf1)");
return 0;
}