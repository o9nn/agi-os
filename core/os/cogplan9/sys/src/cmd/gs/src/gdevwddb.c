#include "gdevmswn.h"
typedef struct gx_device_win_ddb_s gx_device_win_ddb;
#undef wdev
#define wdev ((gx_device_win_ddb *)dev)
private void near win_addtool(gx_device_win_ddb *, int);
private void near win_maketools(gx_device_win_ddb *, HDC);
private void near win_destroytools(gx_device_win_ddb *);
private dev_proc_open_device(win_ddb_open);
private dev_proc_close_device(win_ddb_close);
private dev_proc_map_rgb_color(win_ddb_map_rgb_color);
private dev_proc_fill_rectangle(win_ddb_fill_rectangle);
private dev_proc_tile_rectangle(win_ddb_tile_rectangle);
private dev_proc_copy_mono(win_ddb_copy_mono);
private dev_proc_copy_color(win_ddb_copy_color);
private win_proc_copy_to_clipboard(win_ddb_copy_to_clipboard);
private win_proc_repaint(win_ddb_repaint);
private win_proc_alloc_bitmap(win_ddb_alloc_bitmap);
private win_proc_free_bitmap(win_ddb_free_bitmap);
struct gx_device_win_ddb_s {
gx_device_common;
gx_device_win_common;
HBITMAP FAR hbitmap;
HDC FAR hdcbit;
HPEN hpen, *hpens;
uint hpensize;
HBRUSH hbrush, *hbrushs;
uint hbrushsize;
#define select_brush(color)\
if (wdev->hbrush != wdev->hbrushs[color])\
{	wdev->hbrush = wdev->hbrushs[color];\
SelectObject(wdev->hdcbit,wdev->hbrush);\
}
HPALETTE hpalette;
LPLOGPALETTE lpalette;
#define bmWidthBytes 4
#define bmWidthBits (bmWidthBytes * 8)
#define bmHeight 32
HBITMAP FAR hbmmono;
HDC FAR hdcmono;
gx_bitmap_id bm_id;
};
private const gx_device_procs win_ddb_procs =
{
win_ddb_open,
NULL,
win_sync_output,
win_output_page,
win_ddb_close,
win_ddb_map_rgb_color,
win_map_color_rgb,
win_ddb_fill_rectangle,
win_ddb_tile_rectangle,
win_ddb_copy_mono,
win_ddb_copy_color,
NULL,
NULL,
win_get_params,
win_put_params,
NULL,
win_get_xfont_procs
};
gx_device_win_ddb far_data gs_mswin_device =
{
std_device_std_body(gx_device_win_ddb, &win_ddb_procs, "mswin",
INITIAL_WIDTH, INITIAL_HEIGHT,
INITIAL_RESOLUTION, INITIAL_RESOLUTION
),
{0},
0,
5000,
"\0",
0,
2,
0,
win_ddb_copy_to_clipboard,
win_ddb_repaint,
win_ddb_alloc_bitmap,
win_ddb_free_bitmap
};
private int
win_ddb_open(gx_device * dev)
{
int code = win_open(dev);
HDC hdc;
if (code < 0)
return code;
if (wdev->BitsPerPixel > 8)
return gs_error_limitcheck;
code = win_ddb_alloc_bitmap((gx_device_win *) dev, dev);
if (code < 0)
return code;
hdc = GetDC(wdev->hwndimg);
wdev->hbmmono = CreateBitmap(bmWidthBits, bmHeight, 1, 1, NULL);
wdev->hdcmono = CreateCompatibleDC(hdc);
if (wdev->hbmmono == NULL || wdev->hdcmono == NULL) {
win_ddb_free_bitmap((gx_device_win *) dev);
ReleaseDC(wdev->hwndimg, hdc);
return win_nomemory();
}
SetMapMode(wdev->hdcmono, GetMapMode(hdc));
SelectObject(wdev->hdcmono, wdev->hbmmono);
wdev->bm_id = gx_no_bitmap_id;
ReleaseDC(wdev->hwndimg, hdc);
if ((wdev->lpalette = win_makepalette((gx_device_win *) dev))
== (LPLOGPALETTE) NULL)
return win_nomemory();
wdev->hpalette = CreatePalette(wdev->lpalette);
(void)SelectPalette(wdev->hdcbit, wdev->hpalette, NULL);
RealizePalette(wdev->hdcbit);
win_maketools(wdev, wdev->hdcbit);
wdev->hdctext = wdev->hdcbit;
return 0;
}
private int
win_ddb_close(gx_device * dev)
{
win_destroytools(wdev);
DeleteDC(wdev->hdcmono);
win_ddb_free_bitmap((gx_device_win *) dev);
DeleteObject(wdev->hpalette);
DeleteObject(wdev->hbmmono);
gs_free((char *)(wdev->lpalette), 1, sizeof(LOGPALETTE) +
(1 << (wdev->color_info.depth)) * sizeof(PALETTEENTRY),
"win_ddb_close");
return win_close(dev);
}
private gx_color_index
win_ddb_map_rgb_color(gx_device * dev, gx_color_value r, gx_color_value g,
gx_color_value b)
{
int i = wdev->nColors;
gx_color_index color = win_map_rgb_color(dev, r, g, b);
LPLOGPALETTE lipal = wdev->limgpalette;
LPLOGPALETTE lpal = wdev->lpalette;
if (color != i)
return color;
DeleteObject(wdev->hpalette);
lpal->palPalEntry[i].peFlags = NULL;
lpal->palPalEntry[i].peRed = lipal->palPalEntry[i].peRed;
lpal->palPalEntry[i].peGreen = lipal->palPalEntry[i].peGreen;
lpal->palPalEntry[i].peBlue = lipal->palPalEntry[i].peBlue;
lpal->palNumEntries = i + 1;
wdev->hpalette = CreatePalette(lpal);
(void)SelectPalette(wdev->hdcbit, wdev->hpalette, NULL);
RealizePalette(wdev->hdcbit);
win_addtool(wdev, i);
return color;
}
#define fill_rect(x, y, w, h, color)\
RECT rect;\
rect.left = x, rect.top = y;\
rect.right = x + w, rect.bottom = y + h;\
FillRect(wdev->hdcbit, &rect, wdev->hbrushs[(int)color])
private int
win_ddb_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
fit_fill(dev, x, y, w, h);
if (color == 0)
PatBlt(wdev->hdcbit, x, y, w, h, rop_write_0s);
else {
select_brush((int)color);
PatBlt(wdev->hdcbit, x, y, w, h, rop_write_pattern);
}
win_update((gx_device_win *) dev);
return 0;
}
private int
win_ddb_tile_rectangle(gx_device * dev, const gx_tile_bitmap * tile,
int x, int y, int w, int h, gx_color_index czero, gx_color_index cone,
int px, int py)
{
fit_fill(dev, x, y, w, h);
if (czero != gx_no_color_index && cone != gx_no_color_index) {
fill_rect(x, y, w, h, czero);
czero = gx_no_color_index;
}
if (tile->raster == bmWidthBytes && tile->size.y <= bmHeight &&
(px | py) == 0 && cone != gx_no_color_index
) {
int width = tile->size.x;
int height = tile->size.y;
int rwidth = tile->rep_width;
int irx = ((rwidth & (rwidth - 1)) == 0 ?
x & (rwidth - 1) :
x % rwidth);
int ry = y % tile->rep_height;
int icw = width - irx;
int ch = height - ry;
int ex = x + w, ey = y + h;
int fex = ex - width, fey = ey - height;
int cx, cy;
select_brush((int)cone);
if (tile->id != wdev->bm_id || tile->id == gx_no_bitmap_id) {
wdev->bm_id = tile->id;
SetBitmapBits(wdev->hbmmono,
(DWORD) (bmWidthBytes * tile->size.y),
(BYTE *) tile->data);
}
#define copy_tile(srcx, srcy, tx, ty, tw, th)\
BitBlt(wdev->hdcbit, tx, ty, tw, th, wdev->hdcmono, srcx, srcy, rop_write_at_1s)
if (ch > h)
ch = h;
for (cy = y;;) {
if (w <= icw)
copy_tile(irx, ry, x, cy, w, ch);
else {
copy_tile(irx, ry, x, cy, icw, ch);
cx = x + icw;
while (cx <= fex) {
copy_tile(0, ry, cx, cy, width, ch);
cx += width;
}
if (cx < ex) {
copy_tile(0, ry, cx, cy, ex - cx, ch);
}
}
if ((cy += ch) >= ey)
break;
ch = (cy > fey ? ey - cy : height);
ry = 0;
}
win_update((gx_device_win *) dev);
return 0;
}
return gx_default_tile_rectangle(dev, tile, x, y, w, h, czero, cone, px, py);
}
private int
win_ddb_copy_mono(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one)
{
int endx;
const byte *ptr_line;
int width_bytes, height;
DWORD rop = rop_write_at_1s;
int color;
BYTE aBit[bmWidthBytes * bmHeight];
BYTE *aptr = aBit;
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
if (sourcex & ~7) {
base += sourcex >> 3;
sourcex &= 7;
}
while ((endx = sourcex + w) > bmWidthBits) {
int lastx = (endx - 1) & -bmWidthBits;
int subw = endx - lastx;
int code = win_ddb_copy_mono(dev, base, lastx,
raster, gx_no_bitmap_id,
x + lastx - sourcex, y,
subw, h, zero, one);
if (code < 0)
return code;
w -= subw;
}
while (h > bmHeight) {
int code;
h -= bmHeight;
code = win_ddb_copy_mono(dev, base + h * raster, sourcex,
raster, gx_no_bitmap_id,
x, y + h, w, bmHeight, zero, one);
if (code < 0)
return code;
}
width_bytes = (sourcex + w + 7) >> 3;
ptr_line = base;
if (zero == gx_no_color_index) {
if (one == gx_no_color_index)
return 0;
color = (int)one;
if (color == 0)
rop = rop_write_0_at_1s;
else
select_brush(color);
} else {
if (one == gx_no_color_index) {
color = (int)zero;
rop = rop_write_at_0s;
} else {
fill_rect(x, y, w, h, zero);
color = (int)one;
}
select_brush(color);
}
if (id != wdev->bm_id || id == gx_no_bitmap_id) {
wdev->bm_id = id;
if (raster == bmWidthBytes) {
SetBitmapBits(wdev->hbmmono,
(DWORD) (bmWidthBytes * h),
(BYTE *) base);
} else {
for (height = h; height--;
ptr_line += raster, aptr += bmWidthBytes
) {
switch (width_bytes) {
default:
memcpy(aptr, ptr_line, width_bytes);
break;
case 4:
aptr[3] = ptr_line[3];
case 3:
aptr[2] = ptr_line[2];
case 2:
aptr[1] = ptr_line[1];
case 1:
aptr[0] = ptr_line[0];
}
}
SetBitmapBits(wdev->hbmmono,
(DWORD) (bmWidthBytes * h),
&aBit[0]);
}
}
BitBlt(wdev->hdcbit, x, y, w, h, wdev->hdcmono, sourcex, 0, rop);
win_update((gx_device_win *) dev);
return 0;
}
private int
win_ddb_copy_color(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
if (gx_device_has_color(dev)) {
switch (dev->color_info.depth) {
case 8:
{
int xi, yi;
int skip = raster - w;
const byte *sptr = base + sourcex;
if (w <= 0)
return 0;
if (x < 0 || x + w > dev->width)
return_error(gs_error_rangecheck);
for (yi = y; yi - y < h; yi++) {
for (xi = x; xi - x < w; xi++) {
int color = *sptr++;
SetPixel(wdev->hdcbit, xi, yi, PALETTEINDEX(color));
}
sptr += skip;
}
}
break;
case 4:
{
const byte *line = base + (sourcex >> 1);
int dest_y = y, end_x = x + w;
if (w <= 0)
return 0;
while (h--) {
const byte *source = line;
register int dest_x = x;
if (sourcex & 1) {
int color = *source++ & 0xf;
SetPixel(wdev->hdcbit, dest_x, dest_y, PALETTEINDEX(color));
dest_x++;
}
while (dest_x < end_x) {
int color = *source >> 4;
SetPixel(wdev->hdcbit, dest_x, dest_y, PALETTEINDEX(color));
dest_x++;
if (dest_x < end_x) {
color = *source++ & 0xf;
SetPixel(wdev->hdcbit, dest_x, dest_y, PALETTEINDEX(color));
dest_x++;
}
}
dest_y++;
line += raster;
}
}
break;
default:
return (-1);
}
} else
{
win_ddb_copy_mono(dev, base, sourcex, raster, id, x, y, w, h,
(gx_color_index) 0,
(gx_color_index) (dev->color_info.depth == 8 ? 63 : dev->color_info.max_gray));
}
win_update((gx_device_win *) dev);
return 0;
}
private void
win_ddb_copy_to_clipboard(gx_device_win * dev)
{
HDC hdcbit = wdev->hdcbit;
HBITMAP bitmap = CreateCompatibleBitmap(hdcbit, dev->width,
dev->height);
if (bitmap) {
HDC mem = CreateCompatibleDC(hdcbit);
SelectObject(mem, bitmap);
BitBlt(mem, 0, 0, dev->width, dev->height,
hdcbit, 0, 0, SRCCOPY);
DeleteDC(mem);
OpenClipboard(wdev->hwndimg);
EmptyClipboard();
SetClipboardData(CF_BITMAP, bitmap);
SetClipboardData(CF_PALETTE, CreatePalette(wdev->limgpalette));
CloseClipboard();
}
}
private void
win_ddb_repaint(gx_device_win * dev, HDC hdc, int dx, int dy, int wx, int wy,
int sx, int sy)
{
BitBlt(hdc, dx, dy, wx, wy, wdev->hdcbit, sx, sy, SRCCOPY);
}
private int
win_ddb_alloc_bitmap(gx_device_win * dev, gx_device * param_dev)
{
HDC hdc;
int i;
hdc = GetDC(wdev->hwndimg);
for (i = 0;; i++) {
wdev->hbitmap = CreateCompatibleBitmap(hdc,
param_dev->width, param_dev->height);
if (wdev->hbitmap != (HBITMAP) NULL)
break;
if (i >= 4) {
ReleaseDC(wdev->hwndimg, hdc);
return win_nomemory();
}
errprintf("\nNot enough memory for bitmap.  Halving resolution... ");
param_dev->x_pixels_per_inch /= 2;
param_dev->y_pixels_per_inch /= 2;
param_dev->width /= 2;
param_dev->height /= 2;
}
wdev->hdcbit = CreateCompatibleDC(hdc);
SelectObject(wdev->hdcbit, wdev->hbitmap);
ReleaseDC(wdev->hwndimg, hdc);
return 0;
}
private void
win_ddb_free_bitmap(gx_device_win * dev)
{
DeleteDC(wdev->hdcbit);
DeleteObject(wdev->hbitmap);
}
#undef wdev
private void near
win_addtool(gx_device_win_ddb * wdev, int i)
{
wdev->hpens[i] = CreatePen(PS_SOLID, 1, PALETTEINDEX(i));
wdev->hbrushs[i] = CreateSolidBrush(PALETTEINDEX(i));
}
private void near
win_maketools(gx_device_win_ddb * wdev, HDC hdc)
{
int i;
wdev->hpensize = (1 << (wdev->color_info.depth)) * sizeof(HPEN);
wdev->hpens = (HPEN *) gs_malloc(wdev->memory, 1, wdev->hpensize,
"win_maketools(pens)");
wdev->hbrushsize = (1 << (wdev->color_info.depth)) * sizeof(HBRUSH);
wdev->hbrushs = (HBRUSH *) gs_malloc(wdev->memory, 1, wdev->hbrushsize,
"win_maketools(brushes)");
if (wdev->hpens && wdev->hbrushs) {
for (i = 0; i < wdev->nColors; i++)
win_addtool(wdev, i);
wdev->hpen = wdev->hpens[0];
SelectObject(hdc, wdev->hpen);
wdev->hbrush = wdev->hbrushs[0];
SelectObject(hdc, wdev->hbrush);
}
}
private void near
win_destroytools(gx_device_win_ddb * wdev)
{
int i;
for (i = 0; i < wdev->nColors; i++) {
DeleteObject(wdev->hpens[i]);
DeleteObject(wdev->hbrushs[i]);
}
gs_free(wdev->memory, (char *)wdev->hbrushs, 1, wdev->hbrushsize,
"win_destroytools(brushes)");
gs_free(wdev->memory, (char *)wdev->hpens, 1, wdev->hpensize,
"win_destroytools(pens)");
}