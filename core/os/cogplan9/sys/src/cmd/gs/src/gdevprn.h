#ifndef gdevprn_INCLUDED
# define gdevprn_INCLUDED
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gsutil.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxclist.h"
#include "gxrplane.h"
#include "gsparam.h"
#define PRN_MAX_BITMAP_SMALL 32000
#define PRN_BUFFER_SPACE_SMALL 25000
#define PRN_MIN_MEMORY_LEFT_SMALL 32000
#define PRN_MAX_BITMAP_LARGE 10000000L
#define PRN_BUFFER_SPACE_LARGE 4000000L
#define PRN_MIN_MEMORY_LEFT_LARGE 500000L
#define PRN_MIN_BUFFER_SPACE 10000
#if arch_small_memory
# define PRN_MAX_BITMAP PRN_MAX_BITMAP_SMALL
# define PRN_BUFFER_SPACE PRN_BUFFER_SPACE_SMALL
# define PRN_MIN_MEMORY_LEFT PRN_MIN_MEMORY_LEFT_SMALL
#else
# if 0
# define PRN_MAX_BITMAP\
(gs_debug_c('.') ? PRN_MAX_BITMAP_SMALL : PRN_MAX_BITMAP_LARGE)
# define PRN_BUFFER_SPACE\
(gs_debug_c('.') ? PRN_BUFFER_SPACE_SMALL : PRN_BUFFER_SPACE_LARGE)
# define PRN_MIN_MEMORY_LEFT\
(gs_debug_c('.') ? PRN_MIN_MEMORY_LEFT_SMALL : PRN_MIN_MEMORY_LEFT_LARGE)
# else
# define PRN_MAX_BITMAP PRN_MAX_BITMAP_LARGE
# define PRN_BUFFER_SPACE PRN_BUFFER_SPACE_LARGE
# define PRN_MIN_MEMORY_LEFT PRN_MIN_MEMORY_LEFT_LARGE
# endif
#endif
#ifndef gx_device_printer_DEFINED
# define gx_device_printer_DEFINED
typedef struct gx_device_printer_s gx_device_printer;
#endif
typedef struct gdev_prn_start_render_params_s gdev_prn_start_render_params;
#ifndef gx_page_queue_DEFINED
# define gx_page_queue_DEFINED
typedef struct gx_page_queue_s gx_page_queue_t;
#endif
#ifndef gdev_prn_space_params_DEFINED
# define gdev_prn_space_params_DEFINED
typedef struct gdev_prn_space_params_s gdev_prn_space_params;
#endif
typedef struct gx_printer_device_procs_s {
#define prn_dev_proc_print_page(proc)\
int proc(gx_device_printer *, FILE *)
prn_dev_proc_print_page((*print_page));
#define dev_proc_print_page(proc) prn_dev_proc_print_page(proc)
#define prn_dev_proc_print_page_copies(proc)\
int proc(gx_device_printer *, FILE *, int)
prn_dev_proc_print_page_copies((*print_page_copies));
#define dev_proc_print_page_copies(proc) prn_dev_proc_print_page_copies(proc)
gx_device_buf_procs_t buf_procs;
#define prn_dev_proc_get_space_params(proc)\
void proc(const gx_device_printer *, gdev_prn_space_params *)
prn_dev_proc_get_space_params((*get_space_params));
#define prn_dev_proc_start_render_thread(proc)\
int proc(gdev_prn_start_render_params *)
prn_dev_proc_start_render_thread((*start_render_thread));
#define prn_dev_proc_open_render_device(proc)\
int proc(gx_device_printer *)
prn_dev_proc_open_render_device((*open_render_device));
#define prn_dev_proc_close_render_device(proc)\
int proc(gx_device_printer *)
prn_dev_proc_close_render_device((*close_render_device));
#define prn_dev_proc_buffer_page(proc)\
int proc(gx_device_printer *, FILE *, int)
prn_dev_proc_buffer_page((*buffer_page));
} gx_printer_device_procs;
#define prn_fname_sizeof gp_file_name_sizeof
typedef enum {
BandingAuto = 0,
BandingAlways,
BandingNever
} gdev_prn_banding_type;
struct gdev_prn_space_params_s {
long MaxBitmap;
long BufferSpace;
gx_band_params_t band;
bool params_are_read_only;
gdev_prn_banding_type banding_type;
};
#define gx_prn_device_common\
byte skip[max(sizeof(gx_device_memory), sizeof(gx_device_clist)) -\
sizeof(gx_device) + sizeof(double) ];\
gx_printer_device_procs printer_procs;\
\
\
gdev_prn_space_params space_params;\
char fname[prn_fname_sizeof]; \
\
bool OpenOutputFile;\
bool ReopenPerPage;\
bool page_uses_transparency; \
bool Duplex;\
int Duplex_set; \
\
bool file_is_new; \
FILE *file; \
long buffer_space; \
\
byte *buf; \
\
gs_memory_t *buffer_memory; \
gs_memory_t *bandlist_memory; \
proc_free_up_bandlist_memory((*free_up_bandlist_memory)); \
gx_page_queue_t *page_queue; \
bool is_async_renderer; \
gx_device_printer *async_renderer; \
uint clist_disable_mask; \
\
gx_device_procs orig_procs
struct gx_device_printer_s {
gx_device_common;
gx_prn_device_common;
};
extern_st(st_device_printer);
#define public_st_device_printer() \
gs_public_st_complex_only(st_device_printer, gx_device_printer,\
"gx_device_printer", 0, device_printer_enum_ptrs,\
device_printer_reloc_ptrs, gx_device_finalize)
typedef dev_proc_print_page((*dev_proc_print_page_t));
dev_proc_open_device(gdev_prn_open);
dev_proc_output_page(gdev_prn_output_page);
dev_proc_close_device(gdev_prn_close);
#define gdev_prn_map_rgb_color gx_default_b_w_map_rgb_color
#define gdev_prn_map_color_rgb gx_default_b_w_map_color_rgb
dev_proc_get_params(gdev_prn_get_params);
dev_proc_put_params(gdev_prn_put_params);
prn_dev_proc_get_space_params(gx_default_get_space_params);
#define gdev_prn_default_get_space_params gx_default_get_space_params
prn_dev_proc_start_render_thread(gx_default_start_render_thread);
prn_dev_proc_open_render_device(gx_default_open_render_device);
prn_dev_proc_close_render_device(gx_default_close_render_device);
prn_dev_proc_buffer_page(gx_default_buffer_page);
#define prn_procs(p_open, p_output_page, p_close)\
prn_color_procs(p_open, p_output_page, p_close, gdev_prn_map_rgb_color, gdev_prn_map_color_rgb)
#define prn_params_procs(p_open, p_output_page, p_close, p_get_params, p_put_params)\
prn_color_params_procs(p_open, p_output_page, p_close, gdev_prn_map_rgb_color, gdev_prn_map_color_rgb, p_get_params, p_put_params)
#define prn_color_procs(p_open, p_output_page, p_close, p_map_rgb_color, p_map_color_rgb)\
prn_color_params_procs(p_open, p_output_page, p_close, p_map_rgb_color, p_map_color_rgb, gdev_prn_get_params, gdev_prn_put_params)
#define prn_color_params_procs(p_open, p_output_page, p_close, p_map_rgb_color, p_map_color_rgb, p_get_params, p_put_params) {\
p_open,\
NULL, \
NULL, \
p_output_page,\
p_close,\
p_map_rgb_color,\
p_map_color_rgb,\
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
p_get_params,\
p_put_params,\
NULL, \
NULL, \
NULL, \
NULL, \
gx_page_device_get_page_device,\
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL \
}
extern const gx_device_procs prn_std_procs;
#define prn_device_body_rest2_(print_page, print_page_copies)\
{ 0 }, \
{ 0 }, \
{ print_page,\
print_page_copies,\
{ gx_default_create_buf_device,\
gx_default_size_buf_device,\
gx_default_setup_buf_device,\
gx_default_destroy_buf_device\
},\
gdev_prn_default_get_space_params,\
gx_default_start_render_thread,\
gx_default_open_render_device,\
gx_default_close_render_device,\
gx_default_buffer_page\
},\
{ PRN_MAX_BITMAP, PRN_BUFFER_SPACE,\
{ BAND_PARAMS_INITIAL_VALUES },\
0, \
BandingAuto \
},\
{ 0 }, \
0, \
0, \
0, \
0, -1, \
0, 0, 0, 0, \
0, 0, 0, 0, 0, 0, 0, \
{ 0 }
#define prn_device_body_rest_(print_page)\
prn_device_body_rest2_(print_page, gx_default_print_page_copies)
#define prn_device_body_copies_rest_(print_page_copies)\
prn_device_body_rest2_(gx_print_page_single_copy, print_page_copies)
#define prn_device_margins_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page)\
std_device_full_body_type(dtype, &procs, dname, &st_device_printer,\
(int)((float)(w10) * (xdpi) / 10 + 0.5),\
(int)((float)(h10) * (ydpi) / 10 + 0.5),\
xdpi, ydpi,\
ncomp, depth, mg, mc, dg, dc,\
(float)(-(lo) * (xdpi)), (float)(-(to) * (ydpi)),\
(float)((lm) * 72.0), (float)((bm) * 72.0),\
(float)((rm) * 72.0), (float)((tm) * 72.0)\
),\
prn_device_body_rest_(print_page)
#define prn_device_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page)\
prn_device_margins_body(dtype, procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, ncomp, depth, mg, mc, dg, dc, print_page)
#define prn_device_margins_body_extended(dtype, procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, mcomp, ncomp, pol, depth, gi, mg, mc, dg, dc, ef, cn, print_page)\
std_device_full_body_type_extended(dtype, &procs, dname, &st_device_printer,\
(int)((long)(w10) * (xdpi) / 10),\
(int)((long)(h10) * (ydpi) / 10),\
xdpi, ydpi,\
mcomp, ncomp, pol, depth, gi, mg, mc, dg, dc, ef, cn,\
-(lo) * (xdpi), -(to) * (ydpi),\
(lm) * 72.0, (bm) * 72.0,\
(rm) * 72.0, (tm) * 72.0\
),\
prn_device_body_rest_(print_page)
#define prn_device_body_extended(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, mcomp, ncomp, pol, depth, gi, mg, mc, dg, dc, ef, cn, print_page)\
prn_device_margins_body_extended(dtype, procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, mcomp, ncomp, pol, depth, gi, mg, mc, dg, dc, ef, cn, print_page)
#define prn_device_std_margins_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page)\
std_device_std_color_full_body_type(dtype, &procs, dname, &st_device_printer,\
(int)((float)(w10) * (xdpi) / 10 + 0.5),\
(int)((float)(h10) * (ydpi) / 10 + 0.5),\
xdpi, ydpi, color_bits,\
(float)(-(lo) * (xdpi)), (float)(-(to) * (ydpi)),\
(float)((lm) * 72.0), (float)((bm) * 72.0),\
(float)((rm) * 72.0), (float)((tm) * 72.0)\
),\
prn_device_body_rest_(print_page)
#define prn_device_std_body(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page)\
prn_device_std_margins_body(dtype, procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, color_bits, print_page)
#define prn_device_std_margins_body_copies(dtype, procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page_copies)\
std_device_std_color_full_body_type(dtype, &procs, dname, &st_device_printer,\
(int)((float)(w10) * (xdpi) / 10 + 0.5),\
(int)((float)(h10) * (ydpi) / 10 + 0.5),\
xdpi, ydpi, color_bits,\
(float)(-(lo) * (xdpi)), (float)(-(to) * (ydpi)),\
(float)((lm) * 72.0), (float)((bm) * 72.0),\
(float)((rm) * 72.0), (float)((tm) * 72.0)\
),\
prn_device_body_copies_rest_(print_page_copies)
#define prn_device_std_body_copies(dtype, procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page_copies)\
prn_device_std_margins_body_copies(dtype, procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, color_bits, print_page_copies)
#define prn_device_margins(procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page)\
{ prn_device_std_margins_body(gx_device_printer, procs, dname,\
w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page)\
}
#define prn_device(procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page)\
prn_device_margins(procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, color_bits, print_page)
#define prn_device_margins_copies(procs, dname, w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page_copies)\
{ prn_device_std_margins_body_copies(gx_device_printer, procs, dname,\
w10, h10, xdpi, ydpi, lo, to, lm, bm, rm, tm, color_bits, print_page_copies)\
}
#define prn_device_copies(procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page_copies)\
prn_device_margins_copies(procs, dname, w10, h10, xdpi, ydpi,\
lm, tm, lm, bm, rm, tm, color_bits, print_page_copies)
int gdev_prn_open_printer_seekable(gx_device *dev, bool binary_mode,
bool seekable);
#define gdev_prn_open_printer_positionable gdev_prn_open_printer_seekable
int gdev_prn_open_printer(gx_device * dev, bool binary_mode);
bool gdev_prn_file_is_new(const gx_device_printer *pdev);
#define gdev_prn_raster(pdev) gx_device_raster((gx_device *)(pdev), 0)
int gdev_prn_colors_used(gx_device *dev, int y, int height,
gx_colors_used_t *colors_used,
int *range_start);
int gx_page_info_colors_used(const gx_device *dev,
const gx_band_page_info_t *page_info,
int y, int height,
gx_colors_used_t *colors_used,
int *range_start);
int gdev_prn_render_rectangle(gx_device_printer *pdev,
const gs_int_rect *prect,
gx_device *target,
const gx_render_plane_t *render_plane,
bool clear);
int gdev_prn_get_lines(gx_device_printer *pdev, int y, int height,
byte *buffer, uint bytes_per_line,
byte **actual_buffer, uint *actual_bytes_per_line,
const gx_render_plane_t *render_plane);
int gdev_prn_get_bits(gx_device_printer *pdev, int y, byte *buffer,
byte **actual_buffer);
int gdev_prn_copy_scan_lines(gx_device_printer *, int, byte *, uint);
void gdev_prn_clear_trailing_bits(byte *data, uint raster, int height,
const gx_device *dev);
int gdev_prn_close_printer(gx_device *);
prn_dev_proc_print_page(gx_print_page_single_copy);
prn_dev_proc_print_page_copies(gx_default_print_page_copies);
int gdev_prn_print_scan_lines(gx_device *);
int gdev_prn_allocate_memory(gx_device *pdev,
gdev_prn_space_params *space,
int new_width, int new_height);
int gdev_prn_reallocate_memory(gx_device *pdev,
gdev_prn_space_params *space,
int new_width, int new_height);
int gdev_prn_free_memory(gx_device *pdev);
typedef dev_proc_create_buf_device((*create_buf_device_proc_t));
int gdev_create_buf_device(create_buf_device_proc_t cbd_proc,
gx_device **pbdev, gx_device *target,
const gx_render_plane_t *render_plane,
gs_memory_t *mem, bool for_band);
#define dev_print_scan_lines(dev)\
gdev_prn_print_scan_lines((gx_device *)(dev))
#define gdev_mem_bytes_per_scan_line(dev)\
gdev_prn_raster((gx_device_printer *)(dev))
#define gdev_prn_transpose_8x8(inp,ils,outp,ols)\
memflip8x8(inp,ils,outp,ols)
#if 0
#endif
int gdev_prn_initialize(gx_device *, const char *, dev_proc_print_page((*)));
void gdev_prn_init_color(gx_device *, int, dev_proc_map_rgb_color((*)), dev_proc_map_color_rgb((*)));
#define prn_device_type(dtname, initproc, pageproc)\
private dev_proc_print_page(pageproc);\
device_type(dtname, st_prn_device, initproc)
#define prn_device_type_mono(dtname, dname, initproc, pageproc)\
private dev_proc_print_page(pageproc);\
private int \
initproc(gx_device *dev)\
{ return gdev_prn_initialize(dev, dname, pageproc);\
}\
device_type(dtname, st_prn_device, initproc)
#define prn_device_type_color(dtname, dname, depth, initproc, pageproc, rcproc, crproc)\
private dev_proc_print_page(pageproc);\
private int \
initproc(gx_device *dev)\
{ int code = gdev_prn_initialize(dev, dname, pageproc);\
gdev_prn_init_color(dev, depth, rcproc, crproc);\
return code;\
}\
device_type(dtname, st_prn_device, initproc)
#endif