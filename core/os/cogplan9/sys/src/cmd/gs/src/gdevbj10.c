#include "gdevprn.h"
#define BJ200_TOP_MARGIN		0.12
#define BJ200_BOTTOM_MARGIN		0.29
#define BJ200_LETTER_SIDE_MARGIN	0.25
#define BJ200_A4_SIDE_MARGIN		0.13
private dev_proc_open_device(bj200_open);
private dev_proc_print_page(bj10e_print_page);
private gx_device_procs prn_bj200_procs =
prn_procs(bj200_open, gdev_prn_output_page, gdev_prn_close);
const gx_device_printer far_data gs_bj200_device =
prn_device(prn_bj200_procs, "bj200",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
360,
360,
0, 0, 0, 0,
1, bj10e_print_page);
#define BJ10E_TOP_MARGIN		0.33
#define BJ10E_BOTTOM_MARGIN		(0.50 + 0.04)
private dev_proc_open_device(bj10e_open);
private gx_device_procs prn_bj10e_procs =
prn_procs(bj10e_open, gdev_prn_output_page, gdev_prn_close);
const gx_device_printer far_data gs_bj10e_device =
prn_device(prn_bj10e_procs, "bj10e",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
360,
360,
0,0,0,0,
1, bj10e_print_page);
private int
bj200_open(gx_device *pdev)
{
static const float a4_margins[4] =
{	(float)BJ200_A4_SIDE_MARGIN, (float)BJ200_BOTTOM_MARGIN,
(float)BJ200_A4_SIDE_MARGIN, (float)BJ200_TOP_MARGIN
};
static const float letter_margins[4] =
{	(float)BJ200_LETTER_SIDE_MARGIN, (float)BJ200_BOTTOM_MARGIN,
(float)BJ200_LETTER_SIDE_MARGIN, (float)BJ200_TOP_MARGIN
};
gx_device_set_margins(pdev,
(pdev->width / pdev->x_pixels_per_inch <= 8.4 ?
a4_margins : letter_margins),
true);
return gdev_prn_open(pdev);
}
private int
bj10e_open(gx_device *pdev)
{
static const float a4_margins[4] =
{	(float)BJ200_A4_SIDE_MARGIN, (float)BJ10E_BOTTOM_MARGIN,
(float)BJ200_A4_SIDE_MARGIN, (float)BJ10E_TOP_MARGIN
};
static const float letter_margins[4] =
{	(float)BJ200_LETTER_SIDE_MARGIN, (float)BJ10E_BOTTOM_MARGIN,
(float)BJ200_LETTER_SIDE_MARGIN, (float)BJ10E_TOP_MARGIN
};
gx_device_set_margins(pdev,
(pdev->width / pdev->x_pixels_per_inch <= 8.4 ?
a4_margins : letter_margins),
true);
return gdev_prn_open(pdev);
}
private int
bj10e_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	int line_size = gx_device_raster((gx_device *)pdev, 0);
int xres = (int)pdev->x_pixels_per_inch;
int yres = (int)pdev->y_pixels_per_inch;
int mode = (yres == 180 ?
(xres == 180 ? 11 : 12) :
(xres == 180 ? 14 : 16));
int bytes_per_column = (yres == 180) ? 3 : 6;
int bits_per_column = bytes_per_column * 8;
int skip_unit = bytes_per_column * 3;
byte *in = (byte *)gs_malloc(pdev->memory, 8, line_size, "bj10e_print_page(in)");
byte *out = (byte *)gs_malloc(pdev->memory, bits_per_column, line_size, "bj10e_print_page(out)");
int lnum = 0;
int skip = 0;
int code = 0;
int last_row = dev_print_scan_lines(pdev);
int limit = last_row - bits_per_column;
if ( in == 0 || out == 0 )
{	code = gs_note_error(gs_error_VMerror);
goto fin;
}
#ifdef USE_FACTORY_DEFAULTS
fwrite(( pdev->width / pdev->x_pixels_per_inch <= 8.4 ?
"\033[K\002\000\000\044"	 :
"\033[K\002\000\004\044"	 ),
1, 7, prn_stream);
#else
fwrite("\033[K\002\000\000\044", 1, 7, prn_stream);
#endif
fwrite("\0335\000", 1, 3, prn_stream);
fwrite("\033[\\\004\000\000\000", 1, 7, prn_stream);
fputc(yres & 0xff, prn_stream);
fputc(yres >> 8, prn_stream);
fwrite("\033C\000", 1, 3, prn_stream);
fputc((last_row + yres - 1)/yres, prn_stream);
while ( lnum < last_row )
{
byte *in_data;
byte *in_end = in + line_size;
byte *out_beg = out;
byte *out_end = out + bytes_per_column * pdev->width;
byte *outl = out;
int bnum;
code = gdev_prn_get_bits(pdev, lnum, in, &in_data);
if ( code < 0 ) goto xit;
{	register const long *zip = (const long *)in_data;
register int zcnt = line_size;
register const byte *zipb;
for ( ; zcnt >= 4 * sizeof(long); zip += 4, zcnt -= 4 * sizeof(long) )
{	if ( zip[0] | zip[1] | zip[2] | zip[3] )
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
notz:			;
}
if ( lnum > limit )
{	skip -= (lnum - limit);
lnum = limit;
}
while ( skip > 255 )
{	fputs("\033J\377", prn_stream);
skip -= 255;
}
if ( skip )
fprintf(prn_stream, "\033J%c", skip);
if ( lnum == limit )
limit = last_row;
skip = 0;
for ( bnum = 0; bnum < bits_per_column; bnum += 8 )
{	int lcnt = min(8, limit - lnum);
byte *inp = in;
byte *outp = outl;
lcnt = gdev_prn_copy_scan_lines(pdev,
lnum, in, lcnt * line_size);
if ( lcnt < 0 )
{	code = lcnt;
goto xit;
}
if ( lcnt < 8 )
memset(in + lcnt * line_size, 0,
(8 - lcnt) * line_size);
for ( ; inp < in_end; inp++, outp += bits_per_column )
{	gdev_prn_transpose_8x8(inp, line_size,
outp, bytes_per_column);
}
outl++;
lnum += lcnt;
skip += lcnt;
}
outl = out;
do
{	int count;
int n;
byte *out_ptr;
while(outl < out_end)
{	n = count = min(out_end - outl, skip_unit);
out_ptr = outl;
while ( --count >= 0 )
{	if ( *out_ptr++ )
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
{	count = (outl - out_beg) / skip_unit;
if ( xres == 180 ) count <<= 1;
fprintf(prn_stream, "\033d%c%c",
count & 0xff, count >> 8);
}
out_beg = outl;
outl += n;
while(outl < out_end)
{	n = count = min(out_end - outl, skip_unit);
out_ptr = outl;
while ( --count >= 0 )
{	if ( *out_ptr++ )
break;
}
if ( count < 0 )
break;
else
outl += n;
}
count = outl - out_beg + 1;
fprintf(prn_stream, "\033[g%c%c%c",
count & 0xff, count >> 8, mode);
fwrite(out_beg, 1, count - 1, prn_stream);
out_beg = outl;
outl += n;
}
while ( out_beg < out_end );
fputc('\r', prn_stream);
}
xit:	fputc(014, prn_stream);
fflush(prn_stream);
fin:	if ( out != 0 )
gs_free(pdev->memory, (char *)out, bits_per_column, line_size,
"bj10e_print_page(out)");
if ( in != 0 )
gs_free(pdev->memory, (char *)in, 8, line_size, "bj10e_print_page(in)");
return code;
}