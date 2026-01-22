#include "gdevprn.h"
#ifndef X_DPI
# define X_DPI 180
#endif
#ifndef Y_DPI
# define Y_DPI 180
#endif
#define BLACK 0
#define MAGENTA 1
#define CYAN 2
#define VIOLET 3
#define YELLOW 4
#define RED 5
#define GREEN 6
#define WHITE 7
static char rgb_color[2][2][2] = {
{{BLACK, VIOLET}, {GREEN, CYAN}},
{{RED, MAGENTA}, {YELLOW, WHITE}}
};
#define cv_shift (sizeof(gx_color_value) * 8 - 1)
private gx_color_index
epson_map_rgb_color(gx_device *dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
if (gx_device_has_color(dev))
return (gx_color_index)rgb_color[r >> cv_shift][g >> cv_shift][b >> cv_shift] ^ 7;
else
return gx_default_map_rgb_color(dev, cv);
}
private int
epson_map_color_rgb(gx_device *dev, gx_color_index color,
gx_color_value prgb[3])
{
#define c1 gx_max_color_value
if (gx_device_has_color(dev))
switch ((ushort)color ^ 7)
{
case BLACK:
prgb[0] = 0; prgb[1] = 0; prgb[2] = 0; break;
case VIOLET:
prgb[0] = 0; prgb[1] = 0; prgb[2] = c1; break;
case GREEN:
prgb[0] = 0; prgb[1] = c1; prgb[2] = 0; break;
case CYAN:
prgb[0] = 0; prgb[1] = c1; prgb[2] = c1; break;
case RED:
prgb[0] = c1; prgb[1] = 0; prgb[2] = 0; break;
case MAGENTA:
prgb[0] = c1; prgb[1] = 0; prgb[2] = c1; break;
case YELLOW:
prgb[0] = c1; prgb[1] = c1; prgb[2] = 0; break;
case WHITE:
prgb[0] = c1; prgb[1] = c1; prgb[2] = c1; break;
}
else
return gx_default_map_color_rgb(dev, color, prgb);
return 0;
}
private dev_proc_print_page(epsc_print_page);
private gx_device_procs epson_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
epson_map_rgb_color, epson_map_color_rgb);
const gx_device_printer far_data gs_epsonc_device =
prn_device(epson_procs, "epsonc",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0.25, 0,
3, epsc_print_page);
private void epsc_output_run(byte *, int, int, char, FILE *, int);
#define DD 0x80
private int
epsc_print_page(gx_device_printer *pdev, FILE *prn_stream)
{ static int graphics_modes_9[5] =
{ -1, 0 , 1 , -1, DD+3
};
static int graphics_modes_24[7] =
{ -1, 32 , 33 , 39 ,
-1, -1, DD+40
};
int y_24pin = pdev->y_pixels_per_inch > 72;
int y_mult = (y_24pin ? 3 : 1);
int line_size = (pdev->width + 7) >> 3;
int in_size = line_size * (8 * y_mult);
byte *in = (byte *)gs_malloc(pdev->memory, in_size+1, 1, "epsc_print_page(in)");
int out_size = ((pdev->width + 7) & -8) * y_mult;
byte *out = (byte *)gs_malloc(pdev->memory, out_size+1, 1, "epsc_print_page(out)");
int x_dpi = (int)pdev->x_pixels_per_inch;
char start_graphics = (char)
((y_24pin ? graphics_modes_24 : graphics_modes_9)[x_dpi / 60]);
int first_pass = (start_graphics & DD ? 1 : 0);
int last_pass = first_pass * 2;
int dots_per_space = x_dpi / 10;
int bytes_per_space = dots_per_space * y_mult;
int skip = 0, lnum = 0, pass;
byte *color_in;
int color_line_size, color_in_size;
int spare_bits = (pdev->width % 8);
int whole_bits = pdev->width - spare_bits;
if ( in == 0 || out == 0 )
{ if ( in ) gs_free(pdev->memory, (char *)in, in_size+1, 1, "epsc_print_page(in)");
if ( out ) gs_free(pdev->memory, (char *)out, out_size+1, 1, "epsc_print_page(out)");
return -1;
}
fwrite("\033@\033P\033l\000\033Q\377\033U\001\r", 1, 14, prn_stream);
if (gx_device_has_color(pdev))
{
color_line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
color_in_size = color_line_size * (8 * y_mult);
if((color_in = (byte *)gs_malloc(pdev->memory, color_in_size+1, 1,
"epsc_print_page(color)")) == 0)
{
gs_free(pdev->memory, (char *)in, in_size+1, 1, "epsc_print_page(in)");
gs_free(pdev->memory, (char *)out, out_size+1, 1, "epsc_print_page(out)");
return(-1);
}
}
else
{
color_in = in;
color_in_size = in_size;
color_line_size = line_size;
}
while ( lnum < pdev->height )
{
int lcnt;
byte *nextcolor = NULL;
byte *nextmono = NULL;
gdev_prn_copy_scan_lines(pdev, lnum, color_in, color_line_size);
if ( color_in[0] == 0 &&
!memcmp((char *)color_in, (char *)color_in + 1, color_line_size - 1)
)
{ lnum++;
skip += 3 / y_mult;
continue;
}
while ( skip > 255 )
{ fputs("\033J\377", prn_stream);
skip -= 255;
}
if ( skip )
fprintf(prn_stream, "\033J%c", skip);
lcnt = 1 + gdev_prn_copy_scan_lines(pdev, lnum + 1,
color_in + color_line_size, color_in_size - color_line_size);
if ( lcnt < 8 * y_mult )
{
memset((char *)(color_in + lcnt * color_line_size), 0,
color_in_size - lcnt * color_line_size);
if (gx_device_has_color(pdev))
memset((char *)(in + lcnt * line_size), 0,
in_size - lcnt * line_size);
}
do
{
byte *inp = in;
byte *in_end = in + line_size;
byte *out_end = out;
byte *out_blk;
register byte *outp;
if (gx_device_has_color(pdev))
{
register int i,j;
register byte *outbuf, *realbuf;
byte current_color;
int end_next_bits = whole_bits;
int lastbits;
if (nextcolor)
{
realbuf = nextcolor;
outbuf = nextmono;
memset((char *)in, 0, (nextmono - in));
i = nextcolor - color_in;
nextcolor = NULL;
end_next_bits = (i / color_line_size) * color_line_size
+ whole_bits;
}
else
{
i = 0;
realbuf = color_in;
outbuf = in;
nextcolor = NULL;
}
for (current_color = 0; i <= color_in_size && outbuf < in + in_size; outbuf++)
{
if (spare_bits && i == end_next_bits)
{
end_next_bits = whole_bits + i + spare_bits;
lastbits = 8 - spare_bits;
}
else
lastbits = 0;
for (*outbuf = 0, j = 8; --j >= lastbits && i <= color_in_size;
realbuf++,i++)
{
if (*realbuf)
{
if (current_color > 0)
{
if (*realbuf == current_color)
{
*outbuf |= 1 << j;
*realbuf = 0;
}
else if (nextcolor == NULL)
{
nextcolor = realbuf - (7 - j);
nextmono = outbuf;
}
}
else
{
*outbuf |= 1 << j;
current_color = *realbuf;
*realbuf = 0;
}
}
}
}
*outbuf = 0;
if (current_color)
fprintf(prn_stream,"\033r%d",current_color ^ 7);
}
if ( y_24pin )
{ for ( ; inp < in_end; inp++, out_end += 24 )
{ gdev_prn_transpose_8x8(inp, line_size, out_end, 3);
gdev_prn_transpose_8x8(inp + line_size * 8, line_size, out_end + 1, 3);
gdev_prn_transpose_8x8(inp + line_size * 16, line_size, out_end + 2, 3);
}
while ( out_end > out && out_end[-1] == 0 &&
out_end[-2] == 0 && out_end[-3] == 0
)
out_end -= 3;
}
else
{ for ( ; inp < in_end; inp++, out_end += 8 )
{ gdev_prn_transpose_8x8(inp, line_size, out_end, 1);
}
while ( out_end > out && out_end[-1] == 0 )
out_end--;
}
for ( pass = first_pass; pass <= last_pass; pass++ )
{
for ( out_blk = outp = out; outp < out_end; )
{
if ( *outp == 0 && outp + 12 <= out_end &&
outp[1] == 0 && outp[2] == 0 &&
(outp[3] | outp[4] | outp[5]) == 0 &&
(outp[6] | outp[7] | outp[8]) == 0 &&
(outp[9] | outp[10] | outp[11]) == 0
)
{ byte *zp = outp;
int tpos;
byte *newp;
outp += 12;
while ( outp + 3 <= out_end && *outp == 0 &&
outp[1] == 0 && outp[2] == 0
)
outp += 3;
tpos = (outp - out) / bytes_per_space;
newp = out + tpos * bytes_per_space;
if ( newp > zp + 10 )
{
if ( zp > out_blk )
epsc_output_run(out_blk, (int)(zp - out_blk),
y_mult, start_graphics,
prn_stream, pass);
fprintf(prn_stream, "\033D%c%c\t", tpos, 0);
out_blk = outp = newp;
}
}
else
outp += y_mult;
}
if ( outp > out_blk )
epsc_output_run(out_blk, (int)(outp - out_blk),
y_mult, start_graphics,
prn_stream, pass);
fputc('\r', prn_stream);
}
} while (nextcolor);
skip = 24;
lnum += 8 * y_mult;
}
fputs("\f\033@", prn_stream);
gs_free(pdev->memory, (char *)out, out_size+1, 1, "epsc_print_page(out)");
gs_free(pdev->memory, (char *)in, in_size+1, 1, "epsc_print_page(in)");
if (gx_device_has_color(pdev))
gs_free(pdev->memory, (char *)color_in, color_in_size+1, 1, "epsc_print_page(rin)");
return 0;
}
private void
epsc_output_run(byte *data, int count, int y_mult,
char start_graphics, FILE *prn_stream, int pass)
{ int xcount = count / y_mult;
fputc(033, prn_stream);
if ( !(start_graphics & ~3) )
{ fputc("KLYZ"[(int)start_graphics], prn_stream);
}
else
{ fputc('*', prn_stream);
fputc(start_graphics & ~DD, prn_stream);
}
fputc(xcount & 0xff, prn_stream);
fputc(xcount >> 8, prn_stream);
if ( !pass )
fwrite((char *)data, 1, count, prn_stream);
else
{
int which = pass;
byte *dp = data;
register int i, j;
for ( i = 0; i < xcount; i++, which++ )
for ( j = 0; j < y_mult; j++, dp++ )
{ putc(((which & 1) ? *dp : 0), prn_stream);
}
}
}