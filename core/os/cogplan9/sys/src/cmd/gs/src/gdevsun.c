#include "gx.h"
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sunwindow/cms_mono.h>
#include <stdio.h>
#include "gscdefs.h"
#include "gsmatrix.h"
#include "gxdevice.h"
#include "malloc_.h"
#ifndef DEFAULT_DPI
#  define DEFAULT_DPI 75
#endif
#ifdef A4
#  define PAPER_X 8.27
#  define PAPER_Y 11.69
#endif
#ifndef PAPER_X
#  define PAPER_X 8.5
#  define PAPER_Y 11
#endif
dev_proc_open_device(sun_open);
dev_proc_sync_output(sun_sync);
dev_proc_close_device(sun_close);
dev_proc_map_rgb_color(sun_map_rgb_color);
dev_proc_map_color_rgb(sun_map_color_rgb);
dev_proc_fill_rectangle(sun_fill_rectangle);
dev_proc_copy_mono(sun_copy_mono);
dev_proc_copy_color(sun_copy_color);
dev_proc_draw_line(sun_draw_line);
private gx_device_procs sun_procs = {
sun_open,
NULL,
sun_sync,
NULL,
sun_close,
sun_map_rgb_color,
sun_map_color_rgb,
sun_fill_rectangle,
NULL,
sun_copy_mono,
sun_copy_color,
sun_draw_line
};
#define CMSNAME	"GHOSTVIEW"
typedef struct gx_device_sun {
gx_device_common;
Frame frame;
Canvas canvas;
Pixwin *pw;
struct mpr_data mpr;
Pixrect	pr;
int truecolor;
int freecols;
byte *red, *green, *blue;
char cmsname[sizeof(CMSNAME)+9];
#if !arch_is_big_endian
#  define BUF_WIDTH_BYTES (((int)(8.5*DEFAULT_DPI)+15)/16*2)
byte swap_buf[BUF_WIDTH_BYTES];
#endif
} gx_device_sun;
#if !arch_is_big_endian
static byte reverse_bits[256] = {
0, 128, 64, 192, 32, 160, 96, 224, 16, 144, 80, 208, 48, 176, 112, 240,
8, 136, 72, 200, 40, 168, 104, 232, 24, 152, 88, 216, 56, 184, 120, 248,
4, 132, 68, 196, 36, 164, 100, 228, 20, 148, 84, 212, 52, 180, 116, 244,
12, 140, 76, 204, 44, 172, 108, 236, 28, 156, 92, 220, 60, 188, 124, 252,
2, 130, 66, 194, 34, 162, 98, 226, 18, 146, 82, 210, 50, 178, 114, 242,
10, 138, 74, 202, 42, 170, 106, 234, 26, 154, 90, 218, 58, 186, 122, 250,
6, 134, 70, 198, 38, 166, 102, 230, 22, 150, 86, 214, 54, 182, 118, 246,
14, 142, 78, 206, 46, 174, 110, 238, 30, 158, 94, 222, 62, 190, 126, 254,
1, 129, 65, 193, 33, 161, 97, 225, 17, 145, 81, 209, 49, 177, 113, 241,
9, 137, 73, 201, 41, 169, 105, 233, 25, 153, 89, 217, 57, 185, 121, 249,
5, 133, 69, 197, 37, 165, 101, 229, 21, 149, 85, 213, 53, 181, 117, 245,
13, 141, 77, 205, 45, 173, 109, 237, 29, 157, 93, 221, 61, 189, 125, 253,
3, 131, 67, 195, 35, 163, 99, 227, 19, 147, 83, 211, 51, 179, 115, 243,
11, 139, 75, 203, 43, 171, 107, 235, 27, 155, 91, 219, 59, 187, 123, 251,
7, 135, 71, 199, 39, 167, 103, 231, 23, 151, 87, 215, 55, 183, 119, 247,
15, 143, 79, 207, 47, 175, 111, 239, 31, 159, 95, 223, 63, 191, 127, 255
};
#endif
gx_device_sun far_data gs_sunview_device = {
std_device_std_body(gx_device_sun, &sun_procs, "sunview",
(int)(PAPER_X*DEFAULT_DPI), (int)(PAPER_Y*DEFAULT_DPI),
DEFAULT_DPI, DEFAULT_DPI
),
{ 0 },
0,
};
#define xdev ((gx_device_sun *)dev)
#define RED_BITS	2
#define GREEN_BITS	RED_BITS
#define BLUE_BITS	RED_BITS
#define DEPTH		8
#define RGB_BITS	(RED_BITS + GREEN_BITS + BLUE_BITS)
#define MAX_BITS	RED_BITS
#if (GREEN_BITS > MAX_BITS)
#undef MAX_BITS
#define MAX_BITS	GREEN_BITS
#endif
#if (BLUE_BITS > MAX_BITS)
#undef MAX_BITS
#define MAX_BITS	BLUE_BITS
#endif
#define BLUE_MASK	((1 << BLUE_BITS) - 1)
#define GREEN_MASK	((1 << (BLUE_BITS + GREEN_BITS)) - 1 - BLUE_MASK)
#define RED_MASK	((1 << (BLUE_BITS + GREEN_BITS + RED_BITS)) - 1 \
- BLUE_MASK - GREEN_MASK)
#define RED_COLS	(1 << RED_BITS)
#define GREEN_COLS	(1 << GREEN_BITS)
#define BLUE_COLS	(1 << BLUE_BITS)
#define RGB_COLS	(RED_COLS * GREEN_COLS * BLUE_COLS)
#define MAX_COLS	(1 << MAX_BITS)
#define ALL_COLS	(1 << DEPTH)
#define CMS_SIZE	ALL_COLS
#if (RGB_COLS > CMS_SIZE)
CMS_SIZE_too_small_for_color_cube
#endif
#if (RGB_BITS < 0) || (RGB_BITS > DEPTH)
Display_does_not_support_this_many_colors
#endif
#ifdef FAKE_TRUE_COLOR
# define TRUE_RED_BITS	3
# define TRUE_GREEN_BITS 2
# define TRUE_BLUE_BITS	(DEPTH - TRUE_RED_BITS - TRUE_GREEN_BITS)
#else
# define TRUE_RED_BITS	8
# define TRUE_GREEN_BITS TRUE_RED_BITS
# define TRUE_BLUE_BITS	TRUE_RED_BITS
#endif .
#define TRUE_DEPTH	(TRUE_RED_BITS + TRUE_GREEN_BITS + TRUE_BLUE_BITS)
#define TRUE_RED_MASK	((1 << TRUE_RED_BITS) - 1)
#define TRUE_GREEN_MASK	((1 << (TRUE_RED_BITS + TRUE_GREEN_BITS)) - 1 \
- TRUE_RED_MASK)
#define TRUE_BLUE_MASK	((1 << (TRUE_RED_BITS + TRUE_GREEN_BITS \
+ TRUE_BLUE_BITS)) - 1 \
- TRUE_GREEN_MASK - TRUE_RED_MASK)
#define TRUE_RED_COLS	(1 << TRUE_RED_BITS)
#define TRUE_GREEN_COLS	(1 << TRUE_GREEN_BITS)
#define TRUE_BLUE_COLS	(1 << TRUE_BLUE_BITS)
private Notify_value destroy_func();
int
sun_open(register gx_device *dev)
{
#ifdef gs_DEBUG
if ( gs_debug['X'] )
{ extern int _Xdebug;
_Xdebug = 1;
}
#endif
if (xdev->frame == (Frame)0)
xdev->frame =
window_create(NULL, FRAME, FRAME_LABEL, gs_product,
WIN_WIDTH, min(xdev->width + 24, 900),
WIN_HEIGHT, min(xdev->height + 36, 900),
WIN_Y, 0,
WIN_X, 200,
0);
if (xdev->frame == (Frame)0)
return -1;
xdev->canvas = window_create(xdev->frame, CANVAS,
CANVAS_AUTO_EXPAND,		FALSE,
CANVAS_AUTO_SHRINK,		FALSE,
CANVAS_WIDTH,			xdev->width,
CANVAS_HEIGHT,			xdev->height,
#ifndef PRE_IBIS
CANVAS_COLOR24,			TRUE,
#endif
CANVAS_RETAINED,		FALSE,
0);
xdev->pw = canvas_pixwin(xdev->canvas);
switch (xdev->pw->pw_pixrect->pr_depth) {
static gx_device_color_info mono_ci =
dci_black_and_white;
static gx_device_color_info color_ci =
#if (RGB_COLS < CMS_SIZE)
dci_color(DEPTH, 31, MAX_COLS);
#else
dci_color(DEPTH, MAX_COLS - 1, MAX_COLS);
#endif
static gx_device_color_info truecolor_ci =
dci_color(TRUE_DEPTH,31,4);
case 1:
xdev->color_info = mono_ci;
break;
#ifndef FAKE_TRUE_COLOR
case DEPTH:
xdev->color_info = color_ci;
xdev->truecolor = 0;
break;
#endif
case TRUE_DEPTH:
case TRUE_DEPTH+8:
xdev->color_info = truecolor_ci;
xdev->truecolor = 1;
break;
default:
eprintf1("gs: Cannot handle display of depth %d.\n",
xdev->pw->pw_pixrect->pr_depth);
return -1;
}
if ( gx_device_has_color(xdev)
#ifndef FAKE_TRUE_COLOR
&& !xdev->truecolor
#endif
)
{
int j;
int color;
xdev->red = (byte *)malloc(CMS_SIZE);
xdev->green = (byte *)malloc(CMS_SIZE);
xdev->blue = (byte *)malloc(CMS_SIZE);
if (!xdev->red || !xdev->green || !xdev->blue) {
eprintf("gs: no memory for colormap\n");
return -1;
}
#ifdef FAKE_TRUE_COLOR
for ( j = 0; j < ALL_COLS; j++ ) {
xdev->blue[j] =
(double)((j & TRUE_BLUE_MASK)
>> (TRUE_GREEN_BITS + TRUE_RED_BITS))
/ (TRUE_BLUE_COLS - 1)
* (ALL_COLS - 1);
xdev->green[j] =
(double)((j & TRUE_GREEN_MASK) >> TRUE_RED_BITS)
/ (TRUE_GREEN_COLS - 1)
* (ALL_COLS - 1);
xdev->red[j] =
(double)((j & TRUE_RED_MASK))
/ (TRUE_RED_COLS - 1)
* (ALL_COLS - 1);
}
xdev->freecols = 0;
#else
j = CMS_SIZE - 2;
cms_monochromeload(xdev->red + j,
xdev->green + j,
xdev->blue + j);
for ( color = 1; color < RGB_COLS - 1; color++ ) {
j--;
xdev->red[j] =
(double)((color & RED_MASK) >> (GREEN_BITS + BLUE_BITS))
/ (RED_COLS - 1)
* (ALL_COLS - 1);
xdev->green[j] =
(double)((color & GREEN_MASK) >> BLUE_BITS)
/ (GREEN_COLS - 1)
* (ALL_COLS - 1);
xdev->blue[j] =
(double)((color & BLUE_MASK))
/ (BLUE_COLS - 1)
* (ALL_COLS - 1);
}
xdev->freecols = j;
for (j-- ; j >= 0 ; j--) {
xdev->red[j] = xdev->green[j] = xdev->blue[j] =
~xdev->red[CMS_SIZE - 1];
}
#endif
sprintf(xdev->cmsname, "%s-%d", CMSNAME, getpid());
pw_setcmsname(xdev->pw, xdev->cmsname);
pw_putcolormap(xdev->pw, 0, CMS_SIZE,
xdev->red, xdev->green, xdev->blue);
}
else {
xdev->freecols = 0;
xdev->red = (byte *)0;
xdev->green = (byte *)0;
xdev->blue = (byte *)0;
}
window_set(xdev->canvas,
CANVAS_RETAINED, 		TRUE,
WIN_VERTICAL_SCROLLBAR,		scrollbar_create(0),
WIN_HORIZONTAL_SCROLLBAR,	scrollbar_create(0),
0);
window_set(xdev->frame, WIN_SHOW, TRUE, 0);
notify_interpose_destroy_func(xdev->frame, destroy_func);
(void) notify_do_dispatch();
(void) notify_dispatch();
return 0;
}
private Notify_value
destroy_func(Frame frame, Destroy_status status)
{	if ( status == DESTROY_CHECKING )
{	notify_veto_destroy(frame);
return (NOTIFY_DONE);
}
return (notify_next_destroy_func(frame, status));
}
int
sun_close(gx_device *dev)
{	window_destroy(xdev->frame);
xdev->frame = (Frame)0;
xdev->canvas = (Canvas)0;
xdev->pw = (Pixwin *)0;
xdev->freecols = 0;
if (xdev->red)
free(xdev->red);
if (xdev->green)
free(xdev->green);
if (xdev->blue)
free(xdev->blue);
return 0;
}
int
sun_sync(register gx_device *dev)
{	(void) notify_dispatch();
return 0;
}
gx_color_index
sun_map_rgb_color(gx_device *dev, unsigned short red,
unsigned short green, unsigned short blue)
{	if ( !xdev->frame || !gx_device_has_color(dev) )
return !gx_default_map_rgb_color(dev, red, green, blue);
else if ( !xdev->truecolor ) {
byte red_val, green_val, blue_val;
int i;
static int warn = 1;
red_val = (double)red/gx_max_color_value * (ALL_COLS - 1);
green_val = (double)green/gx_max_color_value * (ALL_COLS - 1);
blue_val = (double)blue/gx_max_color_value * (ALL_COLS - 1);
for (i = CMS_SIZE - 1; i >= xdev->freecols; i--) {
if (xdev->red[i] == red_val &&
xdev->green[i] == green_val &&
xdev->blue[i] == blue_val) {
return i;
}
}
if (xdev->freecols <= 1) {
if (warn) {
eprintf("gs: last spare color map entry allocated\n");
warn = 0;
}
return gx_no_color_index;
}
xdev->red[i] = red_val;
xdev->green[i] = green_val;
xdev->blue[i] = blue_val;
pw_setcmsname(xdev->pw, xdev->cmsname);
pw_putcolormap(xdev->pw, i, 1,
&xdev->red[i], &xdev->green[i], &xdev->blue[i]);
xdev->freecols = i;
return i;
}
else {
return ((blue >> (gx_color_value_bits - TRUE_BLUE_BITS))
<< (TRUE_GREEN_BITS + TRUE_RED_BITS)) |
((green >> (gx_color_value_bits - TRUE_GREEN_BITS))
<< TRUE_RED_BITS) |
(red >> (gx_color_value_bits - TRUE_RED_BITS));
}
}
int
sun_map_color_rgb(gx_device *dev, gx_color_index color,
unsigned short rgb[3])
{	if ( !xdev->frame || !gx_device_has_color(dev) )
return gx_default_map_color_rgb(dev, !color, rgb);
else if ( !xdev->truecolor ) {
if (color < xdev->freecols || color >= CMS_SIZE) {
eprintf1("gs: attempt to get RGB values for unallocated color index %d\n", (int)color);
return -1;
}
rgb[0] = (double)xdev->red[color] / (ALL_COLS - 1)
* gx_max_color_value;
rgb[1] = (double)xdev->green[color] / (ALL_COLS - 1)
* gx_max_color_value;
rgb[2] = (double)xdev->blue[color] / (ALL_COLS - 1)
* gx_max_color_value;
return 0;
}
else {
rgb[0] = (double)((unsigned short)(color & TRUE_RED_MASK))
/ (TRUE_RED_COLS - 1)
* gx_max_color_value;
rgb[1] = (double)((unsigned short)(color & TRUE_GREEN_MASK)
>> TRUE_RED_BITS)
/ (TRUE_GREEN_COLS - 1)
* gx_max_color_value;
rgb[2] = (double)((unsigned short)(color & TRUE_BLUE_MASK)
>> (TRUE_GREEN_BITS + TRUE_RED_BITS))
/ (TRUE_BLUE_COLS - 1)
* gx_max_color_value;
return 0;
}
}
int
sun_fill_rectangle(register gx_device *dev,
int x, int y, int w, int h, gx_color_index color)
{	fit_fill(dev, x, y, w, h);
pw_write(xdev->pw, x, y, w, h, PIX_SRC | PIX_COLOR((int)(color)),
(Pixrect *)0, 0, 0);
(void) notify_dispatch();
return 0;
}
int
sun_copy_mono(register gx_device *dev,
const byte *base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{
#define non_const_base ((byte *)base)
register int i;
int nbytes;
extern struct pixrectops mem_ops;
#if !arch_is_big_endian
#  define BUF_WIDTH_BYTES (((int)(8.5*DEFAULT_DPI)+15)/16*2)
byte swap_buf[BUF_WIDTH_BYTES];
#endif
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
nbytes = h * raster;
xdev->pr.pr_ops = &mem_ops;
xdev->pr.pr_width = w + sourcex + 8;
xdev->pr.pr_height = h;
xdev->pr.pr_depth = 1;
xdev->pr.pr_data = (caddr_t)&(xdev->mpr);
xdev->mpr.md_linebytes = raster;
xdev->mpr.md_image = (short *)((ulong)base & ~1);
#if !arch_is_big_endian
for ( i = 0; i < nbytes; i++ )
non_const_base[i] = reverse_bits[base[i]];
#endif
pw_batch_on(xdev->pw);
if (one != gx_no_color_index)
{	pw_stencil(xdev->pw, x, y, w, h,
PIX_SRC | PIX_COLOR(one), &(xdev->pr),
((int)base & 1) ? sourcex + 8 : sourcex, 0,
(Pixrect *)0, 0, 0);
}
if (zero != gx_no_color_index)
{	for (i = 0; i < nbytes; i++)
non_const_base[i] = ~base[i];
pw_stencil(xdev->pw, x, y, w, h,
PIX_SRC | PIX_COLOR(zero), &(xdev->pr),
((int)base & 1) ? sourcex + 8 : sourcex, 0,
(Pixrect *)0, 0, 0);
for (i = 0; i < nbytes; i++)
non_const_base[i] = ~base[i];
}
pw_batch_off(xdev->pw);
#if !arch_is_big_endian
for ( i = 0; i < nbytes; i++ )
non_const_base[i] = reverse_bits[base[i]];
#endif
(void) notify_dispatch();
return 0;
}
int
sun_copy_color(register gx_device *dev,
const byte *base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
extern struct pixrectops mem_ops;
if ( !gx_device_has_color(dev) )
return sun_copy_mono(dev, base, sourcex, raster, id,
x, y, w, h,
(gx_color_index)0, (gx_color_index)1);
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
xdev->pr.pr_ops = &mem_ops;
xdev->pr.pr_width = w + sourcex + 8;
xdev->pr.pr_height = h;
xdev->pr.pr_depth = 8;
xdev->pr.pr_data = (caddr_t)&(xdev->mpr);
xdev->mpr.md_linebytes = raster;
xdev->mpr.md_image = (short *)((ulong)base & ~1);
pw_write(xdev->pw, x, y, w, h,
PIX_SRC, &(xdev->pr),
(((int)base & 1) ? sourcex + 8 : sourcex), 0);
(void) notify_dispatch();
return 0;
}
int
sun_draw_line(register gx_device *dev,
int x0, int y0, int x1, int y1, gx_color_index color)
{	pw_vector(xdev->pw, x0, y0, x1, y1, PIX_SRC, color);
(void) notify_dispatch();
return 0;
}