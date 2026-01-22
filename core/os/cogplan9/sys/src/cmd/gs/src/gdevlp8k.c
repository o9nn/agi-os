#include "gdevprn.h"
#ifndef X_DPI
#define X_DPI 300
#endif
#ifndef Y_DPI
#define Y_DPI 300
#endif
#define L_MARGIN 0.25
#define B_MARGIN 0.25
#define R_MARGIN 0.25
#define T_MARGIN 0.25
private dev_proc_print_page(lp8000_print_page);
gx_device_printer far_data gs_lp8000_device =
prn_device(prn_std_procs, "lp8000",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
L_MARGIN, B_MARGIN, R_MARGIN, T_MARGIN,
1, lp8000_print_page);
private int
lp8000_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int in_size = line_size;
byte *buf1 = (byte *)gs_malloc(pdev->memory, in_size, 1, "lp8000_print_page(buf1)");
byte *buf2 = (byte *)gs_malloc(pdev->memory, in_size, 1, "lp8000_print_page(buf2)");
byte *in = buf1;
byte *out = buf2;
int lnum, top, bottom, left, width;
int count, i, left1, left2, left0;
if ( buf1 == 0 || buf2 == 0 )
{ if ( buf1 )
gs_free(pdev->memory, (char *)buf1, in_size, 1, "lp8000_print_page(buf1)");
if ( buf2 )
gs_free(pdev->memory, (char *)buf2, in_size, 1, "lp8000_print_page(buf2)");
return_error(gs_error_VMerror);
}
fwrite("\033\001@EJL \n",1,8,prn_stream);
fwrite("@EJL EN LA=ESC/PAGE\n",1,20,prn_stream);
fwrite("\035rhE\033\001@EJL \n",1,12,prn_stream);
fwrite("@EJL SE LA=ESC/PAGE\n",1,20,prn_stream);
fwrite("@EJL SET PU=1 PS=A4 ZO=OFF\n",1,27,prn_stream);
fwrite("@EJL EN LA=ESC/PAGE\n",1,20,prn_stream);
fwrite("\0350;0.24muE\0352;300;300drE",1,23,prn_stream);
fwrite("\0350;300;300drE\0351tsE\0351mmE",1,23,prn_stream);
fwrite("\0357isE\0355iaF\0355ipP\03514psE\0350poE",1,26,prn_stream);
fwrite("\03560;60loE\0350X\0350Y",1,15,prn_stream);
fwrite("\0350;0;2360;3388caE",1,17,prn_stream);
fwrite("\0351cmE\0350alfP",1,11,prn_stream);
fwrite("\0350affP\0350boP\0350abP",1,16,prn_stream);
fwrite("\0354ilG\0350bcI\0350sarG",1,16,prn_stream);
fwrite("\0351;0;100spE\0352owE",1,16,prn_stream);
left1 = (int) (L_MARGIN * pdev->x_pixels_per_inch) - 60;
left1 = (left1 >> 3) << 3;
left0 = left1;
fwrite("\035",1,1,prn_stream);
fprintf(prn_stream,"%d",left1);
fwrite("X",1,1,prn_stream);
fwrite("\0353bcI",1,5,prn_stream);
top = T_MARGIN * pdev->y_pixels_per_inch;
bottom = pdev->height - B_MARGIN * pdev->y_pixels_per_inch;
left = ( (int) (L_MARGIN * pdev->x_pixels_per_inch) ) >> 3 ;
width = ((pdev->width - (int)(R_MARGIN * pdev->x_pixels_per_inch)) >> 3) - left;
for ( lnum = top; lnum < bottom ; )
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
gdev_prn_get_bits(pdev, lnum, in, &in_data);
}
if(lnum == bottom ) break;
lcnt = gdev_prn_copy_scan_lines(pdev, lnum, in, in_size);
inp = in + left;
in_end = inp + width;
while (in_end > inp && in_end[-1] == 0)
{
in_end--;
}
for(left2 = 0; inp < in_end && inp[0] == 0; inp++,left2++);
left2 = left1 + (left2 << 3);
outp = out;
for( p = inp, q = inp + 1 ; q < in_end ; )
{
if( *p != *q++ )
{
*outp++ = *p++;
}
else
{
for (count = 2; ( *p == *q ) && (q < in_end); q++, count++);
while (count > 257)
{
*outp++ = *p;
*outp++ = *p;
*outp++ = 255;
p += 257;
count -=257;
}
*outp++ = *p;
*outp++ = *p;
*outp++ = count - 2;
p += count;
q = p+1;
}
}
if (p == (in_end - 1)) *outp++ = *p;
if (left2 != left0)
{
left0 = left2;
fwrite("\035",1,1,prn_stream);
fprintf(prn_stream,"%d",left2);
fwrite("X",1,1,prn_stream);
}
fwrite("\035",1,1,prn_stream);
fprintf(prn_stream,"%d",lnum-60);
fwrite("Y\035",1,2,prn_stream);
fprintf(prn_stream,"%d;",(outp - out));
fprintf(prn_stream,"%d;",(in_end - inp) << 3);
fwrite("1;0bi{I",1,7,prn_stream);
fwrite(out,1,(outp - out),prn_stream);
lnum++;
}
fwrite("\0350bcI",1,5,prn_stream);
fwrite("\0351coO",1,5,prn_stream);
fwrite("\035rhE",1,4,prn_stream);
fwrite("\033\001@EJL \n",1,8,prn_stream);
fwrite("@EJL SE LA=ESC/PAGE\n",1,20,prn_stream);
fwrite("@EJL SET PU=1 PS=A4 ZO=OFF\n",1,27,prn_stream);
fwrite("@EJL EN LA=ESC/PAGE\n",1,20,prn_stream);
fwrite("\0350;0.24muE\0352;300;300drE",1,23,prn_stream);
fwrite("\0350;300;300drE\0351tsE\0351mmE",1,23,prn_stream);
fwrite("\0357isE\0355iaF\0355ipP\03514psE\0350poE",1,26,prn_stream);
fwrite("\03560;60loE\0350X\0350Y",1,15,prn_stream);
fwrite("\0350;0;2360;3388caE",1,17,prn_stream);
fwrite("\0351cmE\0350alfP",1,11,prn_stream);
fwrite("\0350affP\0350boP\0350abP",1,16,prn_stream);
fwrite("\0354ilG\0350bcI\0350sarG",1,16,prn_stream);
fwrite("\035rhE",1,4,prn_stream);
fwrite("\033\001@EJL \n",1,8,prn_stream);
fwrite("\033\001@EJL \n",1,8,prn_stream);
fflush(prn_stream);
gs_free(pdev->memory, (char *)buf2, in_size, 1, "lp8000_print_page(buf2)");
gs_free(pdev->memory, (char *)buf1, in_size, 1, "lp8000_print_page(buf1)");
return 0;
}