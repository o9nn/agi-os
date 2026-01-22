#include "gdevprn.h"
#include "gdevpcl.h"
#define X_DPI 180
#define Y_DPI 180
#define LINE_SIZE ((X_DPI * 85 / 10 + 63) / 64 * 8)
private dev_proc_print_page(lj250_print_page);
private dev_proc_print_page(paintjet_print_page);
private dev_proc_print_page(pjetxl_print_page);
private int pj_common_print_page(gx_device_printer *, FILE *, int, const char *);
private gx_device_procs paintjet_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);
const gx_device_printer far_data gs_lj250_device =
prn_device(paintjet_procs, "lj250",
85,
110,
X_DPI, Y_DPI,
0.25, 0, 0.25, 0,
3, lj250_print_page);
const gx_device_printer far_data gs_paintjet_device =
prn_device(paintjet_procs, "paintjet",
85,
110,
X_DPI, Y_DPI,
0.25, 0, 0.25, 0,
3, paintjet_print_page);
private gx_device_procs pjetxl_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);
const gx_device_printer far_data gs_pjetxl_device =
prn_device(pjetxl_procs, "pjetxl",
85,
110,
X_DPI, Y_DPI,
0.25, 0, 0, 0,
3, pjetxl_print_page);
private int compress1_row(const byte *, const byte *, byte *);
private int
lj250_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	fputs("\033%8", prn_stream);
fputs("\033*rB", prn_stream);
return pj_common_print_page(pdev, prn_stream, 0, "\033*r0B\014\033%@");
}
private int
paintjet_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
fputs("\033*rB", prn_stream);
return pj_common_print_page(pdev, prn_stream, 0, "\033*r0B\014");
}
private int
pjetxl_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
fputs("\033E", prn_stream);
return pj_common_print_page(pdev, prn_stream, -360, "\033*rC");
}
private int
pj_common_print_page(gx_device_printer *pdev, FILE *prn_stream, int y_origin,
const char *end_page)
{
#define DATA_SIZE (LINE_SIZE * 8)
byte *data =
(byte *)gs_malloc(pdev->memory, DATA_SIZE, 1,
"paintjet_print_page(data)");
byte *plane_data =
(byte *)gs_malloc(pdev->memory, LINE_SIZE * 3, 1,
"paintjet_print_page(plane_data)");
if ( data == 0 || plane_data == 0 )
{	if ( data )
gs_free(pdev->memory, (char *)data, DATA_SIZE, 1,
"paintjet_print_page(data)");
if ( plane_data )
gs_free(pdev->memory, (char *)plane_data, LINE_SIZE * 3, 1,
"paintjet_print_page(plane_data)");
return_error(gs_error_VMerror);
}
fprintf(prn_stream, "\033*t%dR", X_DPI);
fprintf(prn_stream, "\033*r%dS", DATA_SIZE);
fprintf(prn_stream, "\033*r%dU", 3);
fprintf(prn_stream, "\033&a0H\033&a%dV", y_origin);
fputs("\033*b1M", prn_stream);
fputs("\033*r1A", prn_stream);
{	int lnum;
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int num_blank_lines = 0;
for ( lnum = 0; lnum < pdev->height; lnum++ )
{	byte *end_data = data + line_size;
gdev_prn_copy_scan_lines(pdev, lnum,
(byte *)data, line_size);
while ( end_data > data && end_data[-1] == 0 )
end_data--;
if ( end_data == data )
{
num_blank_lines++;
}
else
{	int i;
byte *odp;
byte *row;
memset(end_data, 0, 7);
for ( i = 0, odp = plane_data; i < DATA_SIZE;
i += 8, odp++
)
{
#define spread3(c)\
{ 0, c, c*0x100, c*0x101, c*0x10000L, c*0x10001L, c*0x10100L, c*0x10101L }
static ulong spr40[8] = spread3(0x40);
static ulong spr8[8] = spread3(8);
static ulong spr2[8] = spread3(2);
register byte *dp = data + i;
register ulong pword =
(spr40[dp[0]] << 1) +
(spr40[dp[1]]) +
(spr40[dp[2]] >> 1) +
(spr8[dp[3]] << 1) +
(spr8[dp[4]]) +
(spr8[dp[5]] >> 1) +
(spr2[dp[6]]) +
(spr2[dp[7]] >> 1);
odp[0] = (byte)(pword >> 16);
odp[LINE_SIZE] = (byte)(pword >> 8);
odp[LINE_SIZE*2] = (byte)(pword);
}
if ( num_blank_lines > 0 )
{
fprintf(prn_stream, "\033&a+%dV",
num_blank_lines * (720 / Y_DPI));
num_blank_lines = 0;
}
for ( row = plane_data + LINE_SIZE * 2, i = 0;
i < 3; row -= LINE_SIZE, i++
)
{	byte temp[LINE_SIZE * 2];
int count = compress1_row(row, row + LINE_SIZE, temp);
fprintf(prn_stream, "\033*b%d%c",
count, "VVW"[i]);
fwrite(temp, sizeof(byte),
count, prn_stream);
}
}
}
}
fputs(end_page, prn_stream);
gs_free(pdev->memory, (char *)data, DATA_SIZE, 1, "paintjet_print_page(data)");
gs_free(pdev->memory, (char *)plane_data, LINE_SIZE * 3, 1, "paintjet_print_page(plane_data)");
return 0;
}
private int
compress1_row(const byte *row, const byte *end_row,
byte *compressed)
{	register const byte *in = row;
register byte *out = compressed;
while ( in < end_row )
{	byte test = *in++;
const byte *run = in;
while ( in < end_row && *in == test ) in++;
while ( in - run > 255 )
{	*out++ = 255;
*out++ = ~test;
run += 256;
}
*out++ = in - run;
*out++ = ~test;
}
return out - compressed;
}