#include "gdevprn.h"
#include "gdevpccm.h"
#include "gxlum.h"
#define X_DPI 72
#define Y_DPI 72
private dev_proc_print_page(pcxmono_print_page);
private const gx_device_procs pcxmono_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_map_rgb_color, gx_default_map_color_rgb);
const gx_device_printer gs_pcxmono_device =
prn_device(pcxmono_procs, "pcxmono",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, pcxmono_print_page);
private dev_proc_print_page(pcx256_print_page);
private const gx_device_procs pcxgray_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_gray_map_rgb_color, gx_default_gray_map_color_rgb);
const gx_device_printer gs_pcxgray_device =
{prn_device_body(gx_device_printer, pcxgray_procs, "pcxgray",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, 8, 255, 255, 256, 256, pcx256_print_page)
};
private dev_proc_print_page(pcx16_print_page);
private const gx_device_procs pcx16_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
pc_4bit_map_rgb_color, pc_4bit_map_color_rgb);
const gx_device_printer gs_pcx16_device =
{prn_device_body(gx_device_printer, pcx16_procs, "pcx16",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
3, 4, 1, 1, 2, 2, pcx16_print_page)
};
private const gx_device_procs pcx256_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
pc_8bit_map_rgb_color, pc_8bit_map_color_rgb);
const gx_device_printer gs_pcx256_device =
{prn_device_body(gx_device_printer, pcx256_procs, "pcx256",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
3, 8, 5, 5, 6, 6, pcx256_print_page)
};
private dev_proc_print_page(pcx24b_print_page);
private const gx_device_procs pcx24b_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb);
const gx_device_printer gs_pcx24b_device =
prn_device(pcx24b_procs, "pcx24b",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
24, pcx24b_print_page);
private dev_proc_print_page(pcxcmyk_print_page);
private const gx_device_procs pcxcmyk_procs =
{
gdev_prn_open,
NULL,
NULL,
gdev_prn_output_page,
gdev_prn_close,
NULL,
cmyk_1bit_map_color_rgb,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
gdev_prn_get_params,
gdev_prn_put_params,
cmyk_1bit_map_cmyk_color,
NULL,
NULL,
NULL,
gx_page_device_get_page_device
};
const gx_device_printer gs_pcxcmyk_device =
{prn_device_body(gx_device_printer, pcxcmyk_procs, "pcxcmyk",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
4, 4, 1, 1, 2, 2, pcxcmyk_print_page)
};
#if arch_is_big_endian
# define assign_ushort(a,v) a = ((v) >> 8) + ((v) << 8)
#else
# define assign_ushort(a,v) a = (v)
#endif
typedef struct pcx_header_s {
byte manuf;
byte version;
#define version_2_5 0
#define version_2_8_with_palette 2
#define version_2_8_without_palette 3
#define version_3_0 5
byte encoding;
byte bpp;
ushort x1;
ushort y1;
ushort x2;
ushort y2;
ushort hres;
ushort vres;
byte palette[16 * 3];
byte reserved;
byte nplanes;
ushort bpl;
ushort palinfo;
#define palinfo_color 1
#define palinfo_gray 2
byte xtra[58];
} pcx_header;
private const pcx_header pcx_header_prototype =
{
10,
0,
1,
0,
00, 00,
00, 00,
00, 00,
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
0,
0,
00,
00,
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
};
#define dcx_magic 987654321
#define dcx_max_pages 1023
private void pcx_write_rle(const byte *, const byte *, int, FILE *);
private int pcx_write_page(gx_device_printer *, FILE *, pcx_header *, bool);
private int
pcxmono_print_page(gx_device_printer * pdev, FILE * file)
{
pcx_header header;
header = pcx_header_prototype;
header.version = version_2_8_with_palette;
header.bpp = 1;
header.nplanes = 1;
assign_ushort(header.palinfo, palinfo_gray);
memcpy((byte *) header.palette, "\000\000\000\377\377\377", 6);
return pcx_write_page(pdev, file, &header, false);
}
static const byte pcx_ega_palette[16 * 3] =
{
0x00, 0x00, 0x00, 0x00, 0x00, 0xaa, 0x00, 0xaa, 0x00, 0x00, 0xaa, 0xaa,
0xaa, 0x00, 0x00, 0xaa, 0x00, 0xaa, 0xaa, 0xaa, 0x00, 0xaa, 0xaa, 0xaa,
0x55, 0x55, 0x55, 0x55, 0x55, 0xff, 0x55, 0xff, 0x55, 0x55, 0xff, 0xff,
0xff, 0x55, 0x55, 0xff, 0x55, 0xff, 0xff, 0xff, 0x55, 0xff, 0xff, 0xff
};
private int
pcx16_print_page(gx_device_printer * pdev, FILE * file)
{
pcx_header header;
header = pcx_header_prototype;
header.version = version_2_8_with_palette;
header.bpp = 1;
header.nplanes = 4;
memcpy((byte *) header.palette, pcx_ega_palette,
sizeof(pcx_ega_palette));
return pcx_write_page(pdev, file, &header, true);
}
private int
pcx256_print_page(gx_device_printer * pdev, FILE * file)
{
pcx_header header;
int code;
header = pcx_header_prototype;
header.version = version_3_0;
header.bpp = 8;
header.nplanes = 1;
assign_ushort(header.palinfo,
(pdev->color_info.num_components > 1 ?
palinfo_color : palinfo_gray));
code = pcx_write_page(pdev, file, &header, false);
if (code >= 0) {
fputc(0x0c, file);
code = pc_write_palette((gx_device *) pdev, 256, file);
}
return code;
}
private int
pcx24b_print_page(gx_device_printer * pdev, FILE * file)
{
pcx_header header;
header = pcx_header_prototype;
header.version = version_3_0;
header.bpp = 8;
header.nplanes = 3;
assign_ushort(header.palinfo, palinfo_color);
return pcx_write_page(pdev, file, &header, true);
}
static const byte pcx_cmyk_palette[16 * 3] =
{
0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x0f, 0x0f, 0x00,
0xff, 0x00, 0xff, 0x0f, 0x00, 0x0f, 0xff, 0x00, 0x00, 0x0f, 0x00, 0x00,
0x00, 0xff, 0xff, 0x00, 0x0f, 0x0f, 0x00, 0xff, 0x00, 0x00, 0x0f, 0x00,
0x00, 0x00, 0xff, 0x00, 0x00, 0x0f, 0x1f, 0x1f, 0x1f, 0x0f, 0x0f, 0x0f,
};
private int
pcxcmyk_print_page(gx_device_printer * pdev, FILE * file)
{
pcx_header header;
header = pcx_header_prototype;
header.version = 2;
header.bpp = 4;
header.nplanes = 1;
memcpy((byte *) header.palette, pcx_cmyk_palette,
sizeof(pcx_cmyk_palette));
return pcx_write_page(pdev, file, &header, false);
}
private int
pcx_write_page(gx_device_printer * pdev, FILE * file, pcx_header * phdr,
bool planar)
{
int raster = gdev_prn_raster(pdev);
uint rsize = ROUND_UP((pdev->width * phdr->bpp + 7) >> 3, 2);
int height = pdev->height;
int depth = pdev->color_info.depth;
uint lsize = raster + rsize;
byte *line = gs_alloc_bytes(pdev->memory, lsize, "pcx file buffer");
byte *plane = line + raster;
int y;
int code = 0;
if (line == 0)
return_error(gs_error_VMerror);
assign_ushort(phdr->x2, pdev->width - 1);
assign_ushort(phdr->y2, height - 1);
assign_ushort(phdr->hres, (int)pdev->x_pixels_per_inch);
assign_ushort(phdr->vres, (int)pdev->y_pixels_per_inch);
assign_ushort(phdr->bpl, (planar || depth == 1 ? rsize :
raster + (raster & 1)));
if (fwrite((const char *)phdr, 1, 128, file) < 128) {
code = gs_error_ioerror;
goto pcx_done;
}
for (y = 0; y < height; y++) {
byte *row;
byte *end;
code = gdev_prn_get_bits(pdev, y, line, &row);
if (code < 0)
break;
end = row + raster;
if (!planar) {
if (raster & 1) {
*end = end[-1];
++end;
}
pcx_write_rle(row, end, 1, file);
} else
switch (depth) {
case 4:
{
byte *pend = plane + rsize;
int shift;
for (shift = 0; shift < 4; shift++) {
register byte *from, *to;
register int bright = 1 << shift;
register int bleft = bright << 4;
for (from = row, to = plane;
from < end; from += 4
) {
*to++ =
(from[0] & bleft ? 0x80 : 0) |
(from[0] & bright ? 0x40 : 0) |
(from[1] & bleft ? 0x20 : 0) |
(from[1] & bright ? 0x10 : 0) |
(from[2] & bleft ? 0x08 : 0) |
(from[2] & bright ? 0x04 : 0) |
(from[3] & bleft ? 0x02 : 0) |
(from[3] & bright ? 0x01 : 0);
}
if (to < pend)
*to = to[-1];
pcx_write_rle(plane, pend, 1, file);
}
}
break;
case 24:
{
int pnum;
for (pnum = 0; pnum < 3; ++pnum) {
pcx_write_rle(row + pnum, row + raster, 3, file);
if (pdev->width & 1)
fputc(0, file);
}
}
break;
default:
code = gs_note_error(gs_error_rangecheck);
goto pcx_done;
}
}
pcx_done:
gs_free_object(pdev->memory, line, "pcx file buffer");
return code;
}
private void
pcx_write_rle(const byte * from, const byte * end, int step, FILE * file)
{
#define MAX_RUN_COUNT 15
int max_run = step * MAX_RUN_COUNT;
while (from < end) {
byte data = *from;
from += step;
if (data != *from || from == end) {
if (data >= 0xc0)
putc(0xc1, file);
} else {
const byte *start = from;
while ((from < end) && (*from == data))
from += step;
while (from - start >= max_run) {
putc(0xc0 + MAX_RUN_COUNT, file);
putc(data, file);
start += max_run;
}
if (from > start || data >= 0xc0)
putc((from - start) / step + 0xc1, file);
}
putc(data, file);
}
#undef MAX_RUN_COUNT
}