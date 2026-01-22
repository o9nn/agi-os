#include "gdevprn.h"
private dev_proc_print_page(sj48_print_page);
gx_device_printer far_data gs_sj48_device =
prn_device(prn_std_procs, "sj48",
80,
105,
360,
360,
0,0,0,0,
1, sj48_print_page);
private int
sj48_print_page(gx_device_printer *pdev, FILE *prn_stream)
{ int line_size = gx_device_raster((gx_device *)pdev, 0);
int xres = pdev->x_pixels_per_inch;
int yres = pdev->y_pixels_per_inch;
int mode = (yres == 180 ?
(xres == 180 ? 39 : 40) :
(xres == 180 ? 71 : 72));
int bytes_per_column = (yres == 180) ? 3 : 6;
int bits_per_column = bytes_per_column * 8;
int skip_unit = bytes_per_column * (xres == 180 ? 1 : 2);
byte *in = (byte *)gs_malloc(pdev->memory, 8, line_size, "sj48_print_page(in)");
byte *out = (byte *)gs_malloc(pdev->memory, bits_per_column, line_size, "sj48_print_page(out)");
int lnum = 0;
int skip = 0;
int skips;
int code = 0;
int last_row = dev_print_scan_lines(pdev);
int limit = last_row - bits_per_column;
if ( in == 0 || out == 0 )
{ code = gs_error_VMerror;
gs_note_error(code);
goto fin;
}
if ((xres !=180 && xres != 360) || (yres !=180 && yres != 360))
{ code = gs_error_rangecheck;
gs_note_error(code);
goto fin;
}
fwrite("\033@\000\000", 1, 4, prn_stream);
while ( lnum < last_row )
{
byte *in_data;
byte *in_end = in + line_size;
byte *out_beg = out;
byte *out_end = out + bytes_per_column * pdev->width;
byte *outl = out;
int count, bnum;
code = gdev_prn_get_bits(pdev, lnum, in, &in_data);
if ( code < 0 ) goto xit;
{ register const long *zip = (const long *)in_data;
register int zcnt = line_size;
register const byte *zipb;
for ( ; zcnt >= 4 * sizeof(long); zip += 4, zcnt -= 4 * sizeof(long) )
{ if ( zip[0] | zip[1] | zip[2] | zip[3] )
goto notz;
}
zipb = (const byte *)zip;
while ( --zcnt >= 0 )
{
if ( *zipb++ )
goto notz;
}
lnum++;
skip++;
continue;
notz: ;
}
if ( lnum > limit )
{ skip -= (limit - lnum);
lnum = limit;
}
if (yres == 180) {
skips = skip;
} else {
if (skip & 1) {
skip--;
lnum--;
}
skips = skip/2;
}
while ( skips > 255 )
{ fputs("\033J\377", prn_stream);
skips -= 255;
}
if ( skips )
fprintf(prn_stream, "\033J%c", skips);
if ( lnum == limit )
limit = last_row;
skip = 0;
for ( bnum = 0; bnum < bits_per_column; bnum += 8 )
{ int lcnt = min(8, limit - lnum);
byte *inp = in;
byte *outp = outl;
lcnt = gdev_prn_copy_scan_lines(pdev,
lnum, in, lcnt * line_size);
if ( lcnt < 0 )
{ code = lcnt;
goto xit;
}
if ( lcnt < 8 )
memset(in + lcnt * line_size, 0,
(8 - lcnt) * line_size);
for ( ; inp < in_end; inp++, outp += bits_per_column )
{ gdev_prn_transpose_8x8(inp, line_size,
outp, bytes_per_column);
}
outl++;
lnum += lcnt;
skip += lcnt;
}
outl = out;
do
{ int count;
int n;
byte *out_ptr;
while(outl < out_end)
{ n = count = min(out_end - outl, skip_unit);
out_ptr = outl;
while ( --count >= 0 )
{ if ( *out_ptr++ )
break;
}
if ( count >= 0 )
break;
else
outl = out_ptr;
}
if (outl >= out_end)
break;
if (outl > out_beg)
{ count = (outl - out_beg) / skip_unit;
fprintf(prn_stream, "\033\\%c%c",
count & 0xff, count >> 8);
}
out_beg = outl;
outl += n;
while(outl < out_end)
{ n = count = min(out_end - outl, skip_unit);
out_ptr = outl;
while ( --count >= 0 )
{ if ( *out_ptr++ )
break;
}
if ( count < 0 )
break;
else
outl += n;
}
count = outl - out_beg;
{
int count1 = count/bytes_per_column;
fprintf(prn_stream, "\033*%c%c%c",
mode, count1 & 0xff, count1 >> 8);
}
fwrite(out_beg, 1, count, prn_stream);
out_beg = outl;
outl += n;
}
while ( out_beg < out_end );
fputc('\r', prn_stream);
skip = bits_per_column;
}
xit: fputc(014, prn_stream);
fflush(prn_stream);
fin: if ( out != 0 )
gs_free(pdev->memory, (char *)out, bits_per_column, line_size,
"sj48_print_page(out)");
if ( in != 0 )
gs_free(pdev->memory, (char *)in, 8, line_size, "sj48_print_page(in)");
return code;
}