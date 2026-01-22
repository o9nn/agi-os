#include "gdevprn.h"
#include "gdevbmp.h"
typedef ushort word;
#if arch_sizeof_int == 4
typedef uint dword;
#else
#  if arch_sizeof_long == 4
typedef ulong dword;
#  endif
#endif
#if arch_is_big_endian
#  define BMP_ASSIGN_WORD(a,v) a = ((v) >> 8) + ((v) << 8)
#  define BMP_ASSIGN_DWORD(a,v)\
a = ((v) >> 24) + (((v) >> 8) & 0xff00L) +\
(((dword)(v) << 8) & 0xff0000L) + ((dword)(v) << 24)
#else
#  define BMP_ASSIGN_WORD(a,v) a = (v)
#  define BMP_ASSIGN_DWORD(a,v) a = (v)
#endif
typedef struct bmp_file_header_s {
dword size;
word reserved1;
word reserved2;
dword offBits;
} bmp_file_header;
#define sizeof_bmp_file_header (2 + sizeof(bmp_file_header))
typedef struct bmp_info_header_s {
dword size;
dword width;
dword height;
word planes;
word bitCount;
dword compression;
dword sizeImage;
dword xPelsPerMeter;
dword yPelsPerMeter;
dword clrUsed;
dword clrImportant;
} bmp_info_header;
typedef struct bmp_quad_s {
byte blue, green, red, reserved;
} bmp_quad;
private int
write_bmp_depth_header(gx_device_printer *pdev, FILE *file, int depth,
const byte *palette ,
int raster)
{
ulong bmp_raster = raster + (-raster & 3);
int height = pdev->height;
int quads = (depth <= 8 ? sizeof(bmp_quad) << depth : 0);
fputc('B', file);
fputc('M', file);
{
bmp_file_header fhdr;
BMP_ASSIGN_DWORD(fhdr.size,
sizeof_bmp_file_header +
sizeof(bmp_info_header) + quads +
bmp_raster * height);
BMP_ASSIGN_WORD(fhdr.reserved1, 0);
BMP_ASSIGN_WORD(fhdr.reserved2, 0);
BMP_ASSIGN_DWORD(fhdr.offBits,
sizeof_bmp_file_header +
sizeof(bmp_info_header) + quads);
if (fwrite((const char *)&fhdr, 1, sizeof(fhdr), file) != sizeof(fhdr))
return_error(gs_error_ioerror);
}
{
bmp_info_header ihdr;
BMP_ASSIGN_DWORD(ihdr.size, sizeof(ihdr));
BMP_ASSIGN_DWORD(ihdr.width, pdev->width);
BMP_ASSIGN_DWORD(ihdr.height, height);
BMP_ASSIGN_WORD(ihdr.planes, 1);
BMP_ASSIGN_WORD(ihdr.bitCount, depth);
BMP_ASSIGN_DWORD(ihdr.compression, 0);
BMP_ASSIGN_DWORD(ihdr.sizeImage, bmp_raster * height);
#define INCHES_PER_METER (100  / 2.54 )
BMP_ASSIGN_DWORD(ihdr.xPelsPerMeter,
(dword)(pdev->x_pixels_per_inch * INCHES_PER_METER + 0.5));
BMP_ASSIGN_DWORD(ihdr.yPelsPerMeter,
(dword)(pdev->y_pixels_per_inch * INCHES_PER_METER + 0.5));
#undef INCHES_PER_METER
BMP_ASSIGN_DWORD(ihdr.clrUsed, 0);
BMP_ASSIGN_DWORD(ihdr.clrImportant, 0);
if (fwrite((const char *)&ihdr, 1, sizeof(ihdr), file) != sizeof(ihdr))
return_error(gs_error_ioerror);
}
if (depth <= 8)
fwrite(palette, sizeof(bmp_quad), 1 << depth, file);
return 0;
}
int
write_bmp_header(gx_device_printer *pdev, FILE *file)
{
int depth = pdev->color_info.depth;
bmp_quad palette[256];
if (depth <= 8) {
int i;
gx_color_value rgb[3];
bmp_quad q;
q.reserved = 0;
for (i = 0; i != 1 << depth; i++) {
(*dev_proc(pdev, map_color_rgb))((gx_device *)pdev,
(gx_color_index)i, rgb);
q.red = gx_color_value_to_byte(rgb[0]);
q.green = gx_color_value_to_byte(rgb[1]);
q.blue = gx_color_value_to_byte(rgb[2]);
palette[i] = q;
}
}
return write_bmp_depth_header(pdev, file, depth, (const byte *)palette,
gdev_prn_raster(pdev));
}
int
write_bmp_separated_header(gx_device_printer *pdev, FILE *file)
{
int depth = pdev->color_info.depth;
int plane_depth = depth / 4;
bmp_quad palette[256];
bmp_quad q;
int i;
q.reserved = 0;
for (i = 0; i < 1 << plane_depth; i++) {
q.red = q.green = q.blue =
255 - i * 255 / ((1 << plane_depth) - 1);
palette[i] = q;
}
return write_bmp_depth_header(pdev, file, plane_depth,
(const byte *)palette,
(pdev->width*plane_depth + 7) >> 3);
}
gx_color_index
bmp_map_16m_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_value r, g, b;
r = cv[0]; g = cv[1]; b = cv[2];
return gx_color_value_to_byte(r) +
((uint) gx_color_value_to_byte(g) << 8) +
((ulong) gx_color_value_to_byte(b) << 16);
}
int
bmp_map_16m_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
prgb[2] = gx_color_value_from_byte(color >> 16);
prgb[1] = gx_color_value_from_byte((color >> 8) & 0xff);
prgb[0] = gx_color_value_from_byte(color & 0xff);
return 0;
}