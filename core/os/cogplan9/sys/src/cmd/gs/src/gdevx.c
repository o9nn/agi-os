#include "gx.h"
#include "math_.h"
#include "memory_.h"
#include "x_.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gscoord.h"
#include "gsdevice.h"
#include "gsparam.h"
#include "gxdevice.h"
#include "gxpath.h"
#include "gxgetbit.h"
#include "gxiparam.h"
#include "gsiparm2.h"
#include "gxdevmem.h"
#include "gdevx.h"
#define GET_IMAGE_EXPOSURES 0
private_st_device_X();
private int x_copy_image(gx_device_X * xdev, const byte * base, int sourcex,
int raster, int x, int y, int w, int h);
private int set_tile(gx_device *, const gx_strip_bitmap *);
private void free_cp(gx_device *);
private void update_init(gx_device_X *);
private void update_do_flush(gx_device_X *);
#define flush_text(xdev)\
if (IN_TEXT(xdev)) do_flush_text(xdev)
private void do_flush_text(gx_device_X *);
private dev_proc_open_device(x_open);
private dev_proc_get_initial_matrix(x_get_initial_matrix);
private dev_proc_sync_output(x_sync);
private dev_proc_output_page(x_output_page);
private dev_proc_close_device(x_close);
private dev_proc_fill_rectangle(x_fill_rectangle);
private dev_proc_copy_mono(x_copy_mono);
private dev_proc_copy_color(x_copy_color);
private dev_proc_get_page_device(x_get_page_device);
private dev_proc_strip_tile_rectangle(x_strip_tile_rectangle);
private dev_proc_begin_typed_image(x_begin_typed_image);
private dev_proc_get_bits_rectangle(x_get_bits_rectangle);
#define x_device(this_device, dev_body, max_bitmap) \
const gx_device_X this_device = { \
dev_body, \
{				 \
x_open, \
x_get_initial_matrix, \
x_sync, \
x_output_page, \
x_close, \
gdev_x_map_rgb_color, \
gdev_x_map_color_rgb, \
x_fill_rectangle, \
NULL,			 \
x_copy_mono, \
x_copy_color, \
NULL,			 \
NULL,			 \
gdev_x_get_params, \
gdev_x_put_params, \
NULL,			 \
gdev_x_get_xfont_procs, \
NULL,			 \
NULL,			 \
x_get_page_device, \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
x_strip_tile_rectangle, \
NULL,			 \
NULL,			 \
x_begin_typed_image, \
x_get_bits_rectangle, \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
gdev_x_finish_copydevice \
}, \
gx_device_bbox_common_initial(0 , 1 , 1 ), \
0 ,		 \
1 ,			 \
max_bitmap,			 \
NULL,			 \
0,				 \
{				 \
0, 0,			 \
0, XYBitmap, NULL,	 \
MSBFirst, 8,		 \
MSBFirst, 8, 1,		 \
0, 1,			 \
0, 0, 0,		 \
NULL,			 \
{NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL,			 \
NULL			 \
}, \
}, \
NULL, NULL,			 \
\
NULL,			 \
(Colormap) None,		 \
(Window) None,		 \
NULL,			 \
(Window) None,		 \
(Pixmap) 0,			 \
0,				 \
(Window) None,		 \
{identity_matrix_body},	 \
(Atom) 0, (Atom) 0, (Atom) 0,	 \
{				 \
{			 \
{max_int_in_fixed, max_int_in_fixed}, \
{min_int_in_fixed, min_int_in_fixed} \
}, \
0,			 \
0,			 \
0			 \
}, \
(Pixmap) 0,			 \
0L, (ulong)~0L,		 \
{				 \
(Pixmap) 0,		 \
NULL,			 \
-1, -1			 \
}, \
{				 \
(Pixmap) None,		 \
(Pixmap) None,		 \
gx_no_bitmap_id,	 \
0, 0, 0,		 \
0, 0			 \
}, \
GXcopy,			 \
FillSolid,			 \
0,				 \
0, 0,			 \
0, 0,			 \
{ 0 },			 \
0, 0,			 \
NULL,			 \
128, 5,			 \
NULL,			 \
NULL, NULL, NULL,		 \
NULL, NULL, NULL,		 \
1, 1,			 \
1, 0,			 \
0.0, 0.0,			 \
1,				 \
1, 1,			 \
\
0 ,		 \
20000,			 \
5000,			 \
100000,			 \
100000,			 \
max_int,			 \
\
{				 \
0,			 \
0,			 \
{0, 0},			 \
0,			 \
{ \
{0}},		 \
{0}			 \
} \
};
x_device(gs_x11_device,
std_device_color_stype_body(gx_device_X, 0, "x11", &st_device_X,
FAKE_RES * DEFAULT_WIDTH_10THS / 10,
FAKE_RES * DEFAULT_HEIGHT_10THS / 10,
FAKE_RES, FAKE_RES,
24, 255, 256 ),
0)
x_device(gs_x11alpha_device,
std_device_dci_alpha_type_body(gx_device_X, 0, "x11alpha", &st_device_X,
FAKE_RES * DEFAULT_WIDTH_10THS / 10,
FAKE_RES * DEFAULT_HEIGHT_10THS / 10,
FAKE_RES, FAKE_RES,
3, 24, 255, 255, 256, 256, 4, 4 ),
50000000)
private int alt_put_image(gx_device * dev, Display * dpy, Drawable win,
GC gc, XImage * pi, int sx, int sy, int dx, int dy, unsigned w, unsigned h);
#define put_image(dpy,win,gc,im,sx,sy,x,y,w,h)\
BEGIN\
if ( xdev->useXPutImage ) {\
if (XInitImage(im) == 0)\
return_error(gs_error_unknownerror);\
XPutImage(dpy,win,gc,im,sx,sy,x,y,w,h);\
} else {\
int code_ = alt_put_image(dev,dpy,win,gc,im,sx,sy,x,y,w,h);\
if ( code_ < 0 ) return code_;\
}\
END
private int
x_open(gx_device * dev)
{
gx_device_X *xdev = (gx_device_X *) dev;
int code = gdev_x_open(xdev);
if (code < 0)
return code;
update_init(xdev);
return 0;
}
private int
x_close(gx_device * dev)
{
gx_device_X *xdev = (gx_device_X *) dev;
return gdev_x_close(xdev);
}
private void
x_get_initial_matrix(gx_device * dev, gs_matrix * pmat)
{
gx_device_X *xdev = (gx_device_X *) dev;
if (!xdev->ghostview) {
gx_default_get_initial_matrix(dev, pmat);
return;
}
pmat->xx = xdev->initial_matrix.xx;
pmat->xy = xdev->initial_matrix.xy;
pmat->yx = xdev->initial_matrix.yx;
pmat->yy = xdev->initial_matrix.yy;
pmat->tx = xdev->initial_matrix.tx;
pmat->ty = xdev->initial_matrix.ty;
}
private int
x_sync(gx_device * dev)
{
gx_device_X *xdev = (gx_device_X *) dev;
update_do_flush(xdev);
XSync(xdev->dpy, False);
return 0;
}
void
gdev_x_send_event(gx_device_X *xdev, Atom msg)
{
XEvent event;
event.xclient.type = ClientMessage;
event.xclient.display = xdev->dpy;
event.xclient.window = xdev->win;
event.xclient.message_type = msg;
event.xclient.format = 32;
event.xclient.data.l[0] = xdev->mwin;
event.xclient.data.l[1] = xdev->dest;
XSendEvent(xdev->dpy, xdev->win, False, 0, &event);
}
private int
x_output_page(gx_device * dev, int num_copies, int flush)
{
gx_device_X *xdev = (gx_device_X *) dev;
x_sync(dev);
if (xdev->ghostview) {
XEvent event;
gdev_x_send_event(xdev, xdev->PAGE);
XNextEvent(xdev->dpy, &event);
while (event.type != ClientMessage ||
event.xclient.message_type != xdev->NEXT) {
XNextEvent(xdev->dpy, &event);
}
}
return gx_finish_output_page(dev, num_copies, flush);
}
private int
x_fill_rectangle(gx_device * dev,
int x, int y, int w, int h, gx_color_index gscolor)
{
gx_device_X *xdev = (gx_device_X *) dev;
unsigned long color = (unsigned long) gscolor;
fit_fill(dev, x, y, w, h);
flush_text(xdev);
X_SET_FILL_STYLE(xdev, FillSolid);
X_SET_FORE_COLOR(xdev, color);
X_SET_FUNCTION(xdev, GXcopy);
XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);
if (x == 0 && y == 0 && w == xdev->width && h == xdev->height) {
if (color == xdev->foreground || color == xdev->background)
gdev_x_free_dynamic_colors(xdev);
xdev->colors_or = xdev->colors_and = color;
}
if (xdev->bpixmap != (Pixmap) 0) {
x_update_add(xdev, x, y, w, h);
}
if_debug5('F', "[F] fill (%d,%d):(%d,%d) %ld\n",
x, y, w, h, (long)color);
return 0;
}
private int
x_copy_mono(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one)
{
gx_device_X *xdev = (gx_device_X *) dev;
int function = GXcopy;
unsigned long
lzero = zero,
lone = one;
x_pixel
bc = lzero,
fc = lone;
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
flush_text(xdev);
xdev->image.width = sourcex + w;
xdev->image.height = h;
xdev->image.data = (char *)base;
xdev->image.bytes_per_line = raster;
X_SET_FILL_STYLE(xdev, FillSolid);
if (zero != gx_no_color_index) {
if (one != gx_no_color_index) {
} else if (!(~xdev->colors_and & bc)) {
function = GXand;
fc = ~(x_pixel) 0;
} else if (!(~bc & xdev->colors_or)) {
function = GXor;
fc = 0;
} else {
goto hard;
}
} else {
if (one == gx_no_color_index) {
return 0;
} else if (!(~xdev->colors_and & fc)) {
function = GXand;
bc = ~(x_pixel) 0;
} else if (!(~fc & xdev->colors_or)) {
function = GXor;
bc = 0;
} else {
goto hard;
}
}
xdev->image.format = XYBitmap;
X_SET_FUNCTION(xdev, function);
if (bc != xdev->back_color) {
XSetBackground(xdev->dpy, xdev->gc, (xdev->back_color = bc));
}
if (fc != xdev->fore_color) {
XSetForeground(xdev->dpy, xdev->gc, (xdev->fore_color = fc));
}
if (zero != gx_no_color_index)
NOTE_COLOR(xdev, lzero);
if (one != gx_no_color_index)
NOTE_COLOR(xdev, lone);
put_image(xdev->dpy, xdev->dest, xdev->gc, &xdev->image,
sourcex, 0, x, y, w, h);
goto out;
hard:
if (raster > xdev->cp.raster || h > xdev->cp.height) {
free_cp(dev);
xdev->cp.pixmap =
XCreatePixmap(xdev->dpy, xdev->win, raster << 3, h, 1);
if (xdev->cp.pixmap == (Pixmap) 0) {
lprintf("x_copy_mono: can't allocate pixmap\n");
return_error(gs_error_VMerror);
}
xdev->cp.gc = XCreateGC(xdev->dpy, xdev->cp.pixmap, 0, 0);
if (xdev->cp.gc == (GC) 0) {
lprintf("x_copy_mono: can't allocate GC\n");
return_error(gs_error_VMerror);
}
xdev->cp.raster = raster;
xdev->cp.height = h;
}
xdev->image.format = XYBitmap;
X_SET_FUNCTION(xdev, GXcopy);
if (one == gx_no_color_index) {
XSetBackground(xdev->dpy, xdev->cp.gc, (x_pixel) 1);
XSetForeground(xdev->dpy, xdev->cp.gc, (x_pixel) 0);
X_SET_FORE_COLOR(xdev, lzero);
} else {
XSetBackground(xdev->dpy, xdev->cp.gc, (x_pixel) 0);
XSetForeground(xdev->dpy, xdev->cp.gc, (x_pixel) 1);
X_SET_FORE_COLOR(xdev, lone);
}
put_image(xdev->dpy, xdev->cp.pixmap, xdev->cp.gc,
&xdev->image, sourcex, 0, 0, 0, w, h);
XSetClipMask(xdev->dpy, xdev->gc, xdev->cp.pixmap);
XSetClipOrigin(xdev->dpy, xdev->gc, x, y);
XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);
XSetClipMask(xdev->dpy, xdev->gc, None);
if (raster * h > xdev->MaxTempPixmap)
free_cp(dev);
out:if (xdev->bpixmap != (Pixmap) 0) {
x_update_add(xdev, x, y, w, h);
}
return 0;
}
private void
free_cp(gx_device * dev)
{
gx_device_X *xdev = (gx_device_X *) dev;
if (xdev->cp.gc != NULL) {
XFreeGC(xdev->dpy, xdev->cp.gc);
xdev->cp.gc = NULL;
}
if (xdev->cp.pixmap != (Pixmap) 0) {
XFreePixmap(xdev->dpy, xdev->cp.pixmap);
xdev->cp.pixmap = (Pixmap) 0;
}
xdev->cp.raster = -1;
}
private int
x_copy_image(gx_device_X * xdev, const byte * base, int sourcex, int raster,
int x, int y, int w, int h)
{
int depth = xdev->color_info.depth;
X_SET_FILL_STYLE(xdev, FillSolid);
X_SET_FUNCTION(xdev, GXcopy);
if (h == 1 && w == 1) {
uint sbit = sourcex * depth;
const byte *ptr = base + (sbit >> 3);
x_pixel pixel;
if (depth < 8)
pixel = (byte) (*ptr << (sbit & 7)) >> (8 - depth);
else {
pixel = *ptr++;
while ((depth -= 8) > 0)
pixel = (pixel << 8) + *ptr++;
}
X_SET_FORE_COLOR(xdev, pixel);
XDrawPoint(xdev->dpy, xdev->dest, xdev->gc, x, y);
} else {
xdev->image.width = sourcex + w;
xdev->image.height = h;
xdev->image.format = ZPixmap;
xdev->image.data = (char *)base;
xdev->image.depth = xdev->vinfo->depth;
xdev->image.bytes_per_line = raster;
xdev->image.bits_per_pixel = depth;
if (XInitImage(&xdev->image) == 0)
return_error(gs_error_unknownerror);
XPutImage(xdev->dpy, xdev->dest, xdev->gc, &xdev->image,
sourcex, 0, x, y, w, h);
xdev->image.depth = xdev->image.bits_per_pixel = 1;
xdev->colors_or = (x_pixel)(-1);
xdev->colors_and = 0;
}
return 0;
}
private int
x_copy_color(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
gx_device_X *xdev = (gx_device_X *) dev;
int code;
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
flush_text(xdev);
code = x_copy_image(xdev, base, sourcex, raster, x, y, w, h);
if (xdev->bpixmap != (Pixmap) 0)
x_update_add(xdev, x, y, w, h);
if_debug4('F', "[F] copy_color (%d,%d):(%d,%d)\n",
x, y, w, h);
return code;
}
private gx_device *
x_get_page_device(gx_device * dev)
{
return (((gx_device_X *) dev)->IsPageDevice ? dev : (gx_device *) 0);
}
private int
x_strip_tile_rectangle(gx_device * dev, const gx_strip_bitmap * tiles,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one,
int px, int py)
{
gx_device_X *xdev = (gx_device_X *) dev;
unsigned long lzero = (unsigned long) zero;
unsigned long lone = (unsigned long) one;
if (one == gx_no_color_index || zero == gx_no_color_index)
return gx_default_strip_tile_rectangle(dev, tiles, x, y, w, h,
zero, one, px, py);
if (tiles->shift | px | py)
return gx_default_strip_tile_rectangle(dev, tiles, x, y, w, h,
zero, one, px, py);
fit_fill(dev, x, y, w, h);
flush_text(xdev);
if (h <= 2 && w <= 2) {
int j;
X_SET_FILL_STYLE(xdev, FillSolid);
X_SET_FUNCTION(xdev, GXcopy);
for (j = y + h; --j >= y;) {
const byte *ptr =
tiles->data + (j % tiles->rep_height) * tiles->raster;
int i;
for (i = x + w; --i >= x;) {
uint tx = i % tiles->rep_width;
byte mask = 0x80 >> (tx & 7);
x_pixel pixel = (ptr[tx >> 3] & mask ? lone : lzero);
X_SET_FORE_COLOR(xdev, pixel);
XDrawPoint(xdev->dpy, xdev->dest, xdev->gc, i, j);
}
}
if (xdev->bpixmap != (Pixmap) 0) {
x_update_add(xdev, x, y, w, h);
}
return 0;
}
if ((lzero != xdev->ht.back_c) || (lone != xdev->ht.fore_c))
xdev->ht.id = ~tiles->id;
X_SET_BACK_COLOR(xdev, lzero);
X_SET_FORE_COLOR(xdev, lone);
if (!set_tile(dev, tiles)) {
return gx_default_strip_tile_rectangle(dev, tiles, x, y, w, h,
zero, one, px, py);
}
X_SET_FILL_STYLE(xdev, FillTiled);
X_SET_FUNCTION(xdev, GXcopy);
XFillRectangle(xdev->dpy, xdev->dest, xdev->gc, x, y, w, h);
if (xdev->bpixmap != (Pixmap) 0) {
x_update_add(xdev, x, y, w, h);
}
if_debug6('F', "[F] tile (%d,%d):(%d,%d) %ld,%ld\n",
x, y, w, h, lzero, lone);
return 0;
}
private int
x_begin_typed_image(gx_device * dev,
const gs_imager_state * pis, const gs_matrix * pmat,
const gs_image_common_t * pic, const gs_int_rect * prect,
const gx_drawing_color * pdcolor, const gx_clip_path * pcpath,
gs_memory_t * mem, gx_image_enum_common_t ** pinfo)
{
gx_device_X *xdev = (gx_device_X *) dev;
const gs_image2_t *pim;
gs_state *pgs;
gx_device *sdev;
gs_matrix smat, dmat;
if (pic->type->index != 2)
goto punt;
pim = (const gs_image2_t *)pic;
if (!pim->PixelCopy)
goto punt;
pgs = pim->DataSource;
sdev = gs_currentdevice(pgs);
if (dev->dname != sdev->dname ||
memcmp(&dev->color_info, &sdev->color_info,
sizeof(dev->color_info))
)
goto punt;
flush_text(xdev);
gs_currentmatrix(pgs, &smat);
gs_matrix_multiply(&pim->ImageMatrix, &smat, &smat);
if (pis == 0)
dmat = *pmat;
else
gs_currentmatrix((const gs_state *)pis, &dmat);
if (!((is_xxyy(&dmat) || is_xyyx(&dmat)) &&
#define eqe(e) smat.e == dmat.e
eqe(xx) && eqe(xy) && eqe(yx) && eqe(yy))
#undef eqe
)
goto punt;
{
gs_rect rect, src, dest;
gs_int_point size;
int srcx, srcy, destx, desty;
rect.p.x = rect.p.y = 0;
rect.q.x = pim->Width, rect.q.y = pim->Height;
gs_bbox_transform(&rect, &dmat, &dest);
if (pcpath != NULL &&
!gx_cpath_includes_rectangle(pcpath,
float2fixed(dest.p.x), float2fixed(dest.p.y),
float2fixed(dest.q.x), float2fixed(dest.q.y))
)
goto punt;
rect.q.x += (rect.p.x = pim->XOrigin);
rect.q.y += (rect.p.y = pim->YOrigin);
gs_bbox_transform(&rect, &smat, &src);
(*pic->type->source_size) (pis, pic, &size);
X_SET_FILL_STYLE(xdev, FillSolid);
X_SET_FUNCTION(xdev, GXcopy);
srcx = (int)(src.p.x + 0.5);
srcy = (int)(src.p.y + 0.5);
destx = (int)(dest.p.x + 0.5);
desty = (int)(dest.p.y + 0.5);
XCopyArea(xdev->dpy, xdev->bpixmap, xdev->bpixmap, xdev->gc,
srcx, srcy, size.x, size.y, destx, desty);
x_update_add(xdev, destx, desty, size.x, size.y);
}
return 0;
punt:return gx_default_begin_typed_image(dev, pis, pmat, pic, prect,
pdcolor, pcpath, mem, pinfo);
}
private int
x_get_bits_rectangle(gx_device * dev, const gs_int_rect * prect,
gs_get_bits_params_t * params, gs_int_rect ** unread)
{
gx_device_X *xdev = (gx_device_X *) dev;
int depth = dev->color_info.depth;
int x0 = prect->p.x, y0 = prect->p.y, x1 = prect->q.x, y1 = prect->q.y;
uint width_bytes = ((x1 - x0) * depth + 7) >> 3;
uint band = xdev->MaxTempImage / width_bytes;
uint default_raster = bitmap_raster((x1 - x0) * depth);
gs_get_bits_options_t options = params->options;
uint raster =
(options & GB_RASTER_SPECIFIED ? params->raster :
(params->raster = default_raster));
long plane_mask = (1L << depth) - 1;
int y, h;
XImage *image;
int code = 0;
#if GET_IMAGE_EXPOSURES
XWindowAttributes attributes;
#endif
if (x0 < 0 || y0 < 0 || x1 > dev->width || y1 > dev->height)
return_error(gs_error_rangecheck);
if ((options & GB_OFFSET_SPECIFIED) && params->x_offset == 0)
options = (options & ~GB_OFFSET_SPECIFIED) | GB_OFFSET_0;
if (~options &
(GB_RETURN_COPY | GB_OFFSET_0 | GB_PACKING_CHUNKY |
GB_COLORS_NATIVE) ||
!(options & GB_ALIGN_ALL) ||
!(options & GB_RASTER_ALL)
)
return
gx_default_get_bits_rectangle(dev, prect, params, unread);
params->options =
GB_COLORS_NATIVE | GB_ALPHA_NONE | GB_PACKING_CHUNKY |
GB_RETURN_COPY | GB_OFFSET_0 |
(options & GB_ALIGN_ALL) |
(options & GB_RASTER_SPECIFIED ? GB_RASTER_SPECIFIED :
GB_RASTER_STANDARD);
if (x0 >= x1 || y0 >= y1)
return 0;
if (x1 <= xdev->update.box.p.x || x0 >= xdev->update.box.q.x ||
y1 <= xdev->update.box.p.y || y0 >= xdev->update.box.q.y
) {
flush_text(xdev);
} else
update_do_flush(xdev);
#if GET_IMAGE_EXPOSURES
if (unread) {
XSetGraphicsExposures(xdev->dpy, xdev->gc, True);
XGetWindowAttributes(xdev->dpy, xdev->win, &attributes);
XSelectInput(xdev->dpy, xdev->win,
attributes.your_event_mask | ExposureMask);
}
#endif
if (band == 0)
band = 1;
for (y = y0; y < y1; y += h) {
int cy;
h = min(band, y1 - y);
image = XGetImage(xdev->dpy, xdev->dest, x0, y, x1 - x0, h,
plane_mask, ZPixmap);
for (cy = y; cy < y + h; ++cy) {
const byte *source =
(const byte *)image->data + (cy - y) * image->bytes_per_line;
byte *dest = params->data[0] + (cy - y0) * raster;
if (image->bits_per_pixel == image->depth &&
(image->depth > 1 || image->bitmap_bit_order == MSBFirst) &&
(image->byte_order == MSBFirst || image->depth <= 8)
) {
memcpy(dest, source, width_bytes);
} else {
if (image->depth == 24) {
int cx;
const byte *p = source;
byte *q = dest;
int step = image->bits_per_pixel >> 3;
if (image->byte_order == MSBFirst) {
p += step - 3;
for (cx = x0; cx < x1; p += step, q += 3, ++cx)
q[0] = p[0], q[1] = p[1], q[2] = p[2];
} else {
for (cx = x0; cx < x1; p += step, q += 3, ++cx)
q[0] = p[2], q[1] = p[1], q[2] = p[0];
}
} else if (image->depth == 16) {
int cx;
const byte *p = source;
byte *q = dest;
int step = image->bits_per_pixel >> 3;
if (image->byte_order == MSBFirst) {
p += step - 2;
for (cx = x0; cx < x1; p += step, q += 2, ++cx)
q[0] = p[0], q[1] = p[1];
} else {
for (cx = x0; cx < x1; p += step, q += 2, ++cx)
q[0] = p[1], q[1] = p[0];
}
} else
code = gs_note_error(gs_error_rangecheck);
}
}
XDestroyImage(image);
}
if (unread) {
#if GET_IMAGE_EXPOSURES
XEvent event;
#endif
*unread = 0;
#if GET_IMAGE_EXPOSURES
XWindowEvent(xdev->dpy, xdev->win, ExposureMask, &event);
if (event.type == GraphicsExpose) {
gs_int_rect *rects = (gs_int_rect *)
gs_alloc_bytes(dev->memory, sizeof(gs_int_rect),
"x_get_bits_rectangle");
int num_rects = 0;
for (;;) {
if (rects == 0) {
code = gs_note_error(gs_error_VMerror);
break;
}
#define xevent (*(XGraphicsExposeEvent *)&event)
rects[num_rects].q.x = xevent.width +
(rects[num_rects].p.x = xevent.x);
rects[num_rects].q.y = xevent.height +
(rects[num_rects].p.y = xevent.y);
++num_rects;
if (!xevent.count)
break;
#undef xevent
rects = gs_resize_object(dev->memory, rects,
(num_rects + 1) * sizeof(gs_int_rect),
"x_get_bits_rectangle");
}
if (code >= 0) {
*unread = rects;
code = num_rects;
}
}
XSetGraphicsExposures(xdev->dpy, xdev->gc, False);
XSelectInput(xdev->dpy, xdev->win, attributes.your_event_mask);
#endif
}
return code;
}
private int
set_tile(gx_device * dev, const gx_strip_bitmap * tile)
{
gx_device_X *xdev = (gx_device_X *) dev;
#ifdef DEBUG
if (gs_debug['T'])
return 0;
#endif
if (tile->id == xdev->ht.id && tile->id != gx_no_bitmap_id)
return xdev->useXSetTile;
if (tile->size.x != xdev->ht.width ||
tile->size.y != xdev->ht.height ||
xdev->ht.pixmap == (Pixmap) 0) {
if (xdev->ht.pixmap != (Pixmap) 0)
XFreePixmap(xdev->dpy, xdev->ht.pixmap);
xdev->ht.pixmap = XCreatePixmap(xdev->dpy, xdev->win,
tile->size.x, tile->size.y,
xdev->vinfo->depth);
if (xdev->ht.pixmap == (Pixmap) 0)
return 0;
xdev->ht.width = tile->size.x, xdev->ht.height = tile->size.y;
xdev->ht.raster = tile->raster;
}
xdev->ht.fore_c = xdev->fore_color;
xdev->ht.back_c = xdev->back_color;
xdev->image.data = (char *)tile->data;
xdev->image.width = tile->size.x;
xdev->image.height = tile->size.y;
xdev->image.bytes_per_line = tile->raster;
xdev->image.format = XYBitmap;
X_SET_FILL_STYLE(xdev, FillSolid);
#ifdef DEBUG
if (gs_debug['H']) {
int i;
dlprintf4("[H] 0x%lx: width=%d height=%d raster=%d\n",
(ulong) tile->data, tile->size.x, tile->size.y, tile->raster);
dlputs("");
for (i = 0; i < tile->raster * tile->size.y; i++)
dprintf1(" %02x", tile->data[i]);
dputc('\n');
}
#endif
XSetTile(xdev->dpy, xdev->gc, xdev->ht.no_pixmap);
X_SET_FUNCTION(xdev, GXcopy);
put_image(xdev->dpy, xdev->ht.pixmap, xdev->gc, &xdev->image,
0, 0, 0, 0, tile->size.x, tile->size.y);
XSetTile(xdev->dpy, xdev->gc, xdev->ht.pixmap);
xdev->ht.id = tile->id;
return xdev->useXSetTile;
}
private void
update_init(gx_device_X *xdev)
{
xdev->update.box.p.x = xdev->update.box.p.y = max_int_in_fixed;
xdev->update.box.q.x = xdev->update.box.q.y = min_int_in_fixed;
xdev->update.area = xdev->update.total = xdev->update.count = 0;
}
private void
update_do_flush(gx_device_X * xdev)
{
flush_text(xdev);
if (xdev->update.count != 0) {
int x = xdev->update.box.p.x, y = xdev->update.box.p.y;
int w = xdev->update.box.q.x - x, h = xdev->update.box.q.y - y;
fit_fill_xywh(xdev, x, y, w, h);
if (w > 0 && h > 0) {
if (xdev->is_buffered) {
const gx_device_memory *mdev =
(const gx_device_memory *)xdev->target;
if (mdev == NULL)
return;
x_copy_image(xdev, mdev->line_ptrs[y], x, mdev->raster,
x, y, w, h);
}
if (xdev->bpixmap) {
X_SET_FUNCTION(xdev, GXcopy);
XCopyArea(xdev->dpy, xdev->bpixmap, xdev->win, xdev->gc,
x, y, w, h, x, y);
}
}
update_init(xdev);
}
}
void
x_update_add(gx_device_X * xdev, int xo, int yo, int w, int h)
{
int xe = xo + w, ye = yo + h;
long added = (long)w * h;
long old_area = xdev->update.area;
gs_int_rect u;
int nw, nh;
long new_up_area;
u.p.x = min(xo, xdev->update.box.p.x);
u.p.y = min(yo, xdev->update.box.p.y);
u.q.x = max(xe, xdev->update.box.q.x);
u.q.y = max(ye, xdev->update.box.q.y);
nw = u.q.x - u.p.x;
nh = u.q.y - u.p.y;
new_up_area = (long)nw * nh;
xdev->update.count++;
xdev->update.area = new_up_area;
xdev->update.total += added;
if (!xdev->AlwaysUpdate &&
xdev->update.count < xdev->MaxBufferedCount &&
xdev->update.area < xdev->MaxBufferedArea &&
xdev->update.total < xdev->MaxBufferedTotal
) {
if (nw + nh >= 70 && (nw | nh) >= 16 &&
old_area + added < new_up_area - (new_up_area >> 2)
)
DO_NOTHING;
else {
xdev->update.box = u;
return;
}
}
if (xdev->is_buffered && (xdev->target == NULL))
xdev->update.box = u;
else {
update_do_flush(xdev);
xdev->update.box.p.x = xo, xdev->update.box.p.y = yo;
xdev->update.box.q.x = xe, xdev->update.box.q.y = ye;
xdev->update.count = 1;
xdev->update.area = xdev->update.total = added;
}
}
private void
do_flush_text(gx_device_X * xdev)
{
if (!IN_TEXT(xdev))
return;
DRAW_TEXT(xdev);
xdev->text.item_count = xdev->text.char_count = 0;
}
private bool
x_bbox_init_box(void *pdata)
{
gx_device_X *const xdev = pdata;
update_init(xdev);
return true;
}
private void
x_bbox_get_box(const void *pdata, gs_fixed_rect *pbox)
{
const gx_device_X *const xdev = pdata;
pbox->p.x = int2fixed(xdev->update.box.p.x);
pbox->p.y = int2fixed(xdev->update.box.p.y);
pbox->q.x = int2fixed(xdev->update.box.q.x);
pbox->q.y = int2fixed(xdev->update.box.q.y);
}
private void
x_bbox_add_rect(void *pdata, fixed x0, fixed y0, fixed x1, fixed y1)
{
gx_device_X *const xdev = pdata;
int x = fixed2int(x0), y = fixed2int(y0);
x_update_add(xdev, x, y, fixed2int_ceiling(x1) - x,
fixed2int_ceiling(y1) - y);
}
private bool
x_bbox_in_rect(const void *pdata, const gs_fixed_rect *pbox)
{
gs_fixed_rect box;
x_bbox_get_box(pdata, &box);
return rect_within(*pbox, box);
}
const gx_device_bbox_procs_t gdev_x_box_procs = {
x_bbox_init_box, x_bbox_get_box, x_bbox_add_rect, x_bbox_in_rect
};
private int
alt_put_image(gx_device *dev, Display *dpy, Drawable win, GC gc, XImage *pi,
int sx, int sy, int dx, int dy, unsigned w, unsigned h)
{
int raster = pi->bytes_per_line;
byte *data = (byte *) pi->data + sy * raster + (sx >> 3);
int init_mask = 0x80 >> (sx & 7);
int invert = 0;
int yi;
#define NUM_RECTS 40
XRectangle rects[NUM_RECTS];
XRectangle *rp = rects;
XGCValues gcv;
#ifdef DEBUG
if (pi->format != XYBitmap || pi->byte_order != MSBFirst ||
pi->bitmap_bit_order != MSBFirst || pi->depth != 1
) {
lprintf("alt_put_image: unimplemented parameter values!\n");
return_error(gs_error_rangecheck);
}
#endif
XGetGCValues(dpy, gc, (GCFunction | GCForeground | GCBackground), &gcv);
if (gcv.function == GXcopy) {
XSetForeground(dpy, gc, gcv.background);
XFillRectangle(dpy, win, gc, dx, dy, w, h);
XSetForeground(dpy, gc, gcv.foreground);
} else if (gcv.function == GXand) {
#ifdef DEBUG
if (gcv.foreground != ~(x_pixel)0 && gcv.background != ~(x_pixel)0) {
lprintf("alt_put_image: unimplemented GXand case!\n");
return_error(gs_error_rangecheck);
}
#endif
if (gcv.background != ~(x_pixel) 0) {
XSetForeground(dpy, gc, gcv.background);
invert = 0xff;
}
} else if (gcv.function == GXor) {
#ifdef DEBUG
if (gcv.foreground != 0 && gcv.background != 0) {
lprintf("alt_put_image: unimplemented GXor case!\n");
return_error(gs_error_rangecheck);
}
#endif
if (gcv.background != 0) {
XSetForeground(dpy, gc, gcv.background);
invert = 0xff;
}
} else {
lprintf("alt_put_image: unimplemented function.\n");
return_error(gs_error_rangecheck);
}
for (yi = 0; yi < h; yi++, data += raster) {
int mask = init_mask;
byte *dp = data;
int xi = 0;
while (xi < w) {
if ((*dp ^ invert) & mask) {
int xleft = xi;
if (rp == &rects[NUM_RECTS]) {
XFillRectangles(dpy, win, gc, rects, NUM_RECTS);
rp = rects;
}
rp->x = dx + xi, rp->y = dy + yi;
do {
if (!(mask >>= 1))
mask = 0x80, dp++;
xi++;
} while (xi < w && ((*dp ^ invert) & mask));
rp->width = xi - xleft, rp->height = 1;
rp++;
} else {
if (!(mask >>= 1))
mask = 0x80, dp++;
xi++;
}
}
}
XFillRectangles(dpy, win, gc, rects, rp - rects);
if (invert)
XSetForeground(dpy, gc, gcv.foreground);
return 0;
#undef NUM_RECTS
}