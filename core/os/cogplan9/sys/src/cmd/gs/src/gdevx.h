#ifndef gdevx_INCLUDED
# define gdevx_INCLUDED
typedef unsigned long x_pixel;
#include "gdevbbox.h"
#include "gdevxcmp.h"
extern XtResource gdev_x_resources[];
extern const int gdev_x_resource_count;
extern String gdev_x_fallback_resources[];
typedef struct x11fontlist_s {
char **names;
int count;
} x11fontlist;
typedef struct x11fontmap_s x11fontmap;
struct x11fontmap_s {
char *ps_name;
char *x11_name;
x11fontlist std, iso;
x11fontmap *next;
};
#define private_st_x11fontmap() \
gs_private_st_ptrs3(st_x11fontmap, x11fontmap, "x11fontmap",\
x11fontmap_enum_ptrs, x11fontmap_reloc_ptrs, ps_name, x11_name, next)
typedef struct gx_device_X_s {
gx_device_bbox_common;
bool is_buffered;
bool IsPageDevice;
long MaxBitmap;
byte *buffer;
long buffer_size;
XImage image;
Display *dpy;
Screen *scr;
XVisualInfo *vinfo;
Colormap cmap;
Window win;
GC gc;
Window pwin;
Pixmap bpixmap;
int ghostview;
Window mwin;
gs_matrix initial_matrix;
Atom NEXT, PAGE, DONE;
struct {
gs_int_rect box;
long area;
long total;
int count;
} update;
Pixmap dest;
x_pixel colors_or;
x_pixel colors_and;
struct {
Pixmap pixmap;
GC gc;
int raster, height;
} cp;
struct {
Pixmap pixmap;
Pixmap no_pixmap;
gx_bitmap_id id;
int width, height, raster;
x_pixel fore_c, back_c;
} ht;
int function;
int fill_style;
Font fid;
#define X_SET_FILL_STYLE(xdev, style)\
BEGIN\
if (xdev->fill_style != (style))\
XSetFillStyle(xdev->dpy, xdev->gc, (xdev->fill_style = (style)));\
END
#define X_SET_FUNCTION(xdev, func)\
BEGIN\
if (xdev->function != (func))\
XSetFunction(xdev->dpy, xdev->gc, (xdev->function = (func)));\
END
#define X_SET_FONT(xdev, font)\
BEGIN\
if (xdev->fid != (font))\
XSetFont(xdev->dpy, xdev->gc, (xdev->fid = (font)));\
END
x_pixel back_color, fore_color;
Pixel background, foreground;
x11_cman_t cman;
#define NOTE_COLOR(xdev, pixel)\
(xdev->colors_or |= (pixel),\
xdev->colors_and &= (pixel))
#define X_SET_BACK_COLOR(xdev, pixel)\
BEGIN\
if (xdev->back_color != (pixel)) {\
xdev->back_color = (pixel);\
NOTE_COLOR(xdev, pixel);\
XSetBackground(xdev->dpy, xdev->gc, (pixel));\
}\
END
#define X_SET_FORE_COLOR(xdev, pixel)\
BEGIN\
if (xdev->fore_color != (pixel)) {\
xdev->fore_color = (pixel);\
NOTE_COLOR(xdev, pixel);\
XSetForeground(xdev->dpy, xdev->gc, (pixel));\
}\
END
Pixel borderColor;
Dimension borderWidth;
String geometry;
int maxGrayRamp, maxRGBRamp;
String palette;
String regularFonts;
String symbolFonts;
String dingbatFonts;
x11fontmap *regular_fonts;
x11fontmap *symbol_fonts;
x11fontmap *dingbat_fonts;
Boolean useXFonts, useFontExtensions, useScalableFonts, logXFonts;
float xResolution, yResolution;
Boolean useBackingPixmap;
Boolean useXPutImage;
Boolean useXSetTile;
bool AlwaysUpdate;
int MaxTempPixmap;
int MaxTempImage;
int MaxBufferedTotal;
int MaxBufferedArea;
int MaxBufferedCount;
struct {
int item_count;
#define IN_TEXT(xdev) ((xdev)->text.item_count != 0)
int char_count;
gs_int_point origin;
int x;
#define MAX_TEXT_ITEMS 12
XTextItem items[MAX_TEXT_ITEMS];
#define MAX_TEXT_CHARS 25
char chars[MAX_TEXT_CHARS];
} text;
#define DRAW_TEXT(xdev)\
XDrawText(xdev->dpy, xdev->dest, xdev->gc, xdev->text.origin.x,\
xdev->text.origin.y, xdev->text.items, xdev->text.item_count)
} gx_device_X;
#define private_st_device_X() \
gs_public_st_suffix_add4_final(st_device_X, gx_device_X,\
"gx_device_X", device_x_enum_ptrs, device_x_reloc_ptrs,\
gx_device_finalize, st_device_bbox, buffer, regular_fonts,\
symbol_fonts, dingbat_fonts)
void gdev_x_send_event(gx_device_X *xdev, Atom msg);
void x_update_add(gx_device_X *, int, int, int, int);
void gdev_x_clear_window(gx_device_X *);
int x_catch_free_colors(Display *, XErrorEvent *);
#define FAKE_RES (16*72)
int gdev_x_setup_colors(gx_device_X *);
void gdev_x_free_colors(gx_device_X *);
void gdev_x_free_dynamic_colors(gx_device_X *);
int gdev_x_open(gx_device_X *);
int gdev_x_close(gx_device_X *);
dev_proc_map_rgb_color(gdev_x_map_rgb_color);
dev_proc_map_color_rgb(gdev_x_map_color_rgb);
dev_proc_get_params(gdev_x_get_params);
dev_proc_put_params(gdev_x_put_params);
dev_proc_get_xfont_procs(gdev_x_get_xfont_procs);
dev_proc_finish_copydevice(gdev_x_finish_copydevice);
#endif