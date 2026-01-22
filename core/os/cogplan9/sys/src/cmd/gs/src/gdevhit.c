#include "std.h"
#include "gserror.h"
#include "gserrors.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxdevice.h"
const int gs_hit_detected = gs_error_hit_detected;
private dev_proc_fill_rectangle(hit_fill_rectangle);
const gx_device gs_hit_device = {
std_device_std_body(gx_device, 0, "hit detector",
0, 0, 1, 1),
{NULL,
NULL,
NULL,
NULL,
NULL,
gx_default_map_rgb_color,
gx_default_map_color_rgb,
hit_fill_rectangle,
NULL,
NULL,
NULL,
gx_default_draw_line,
NULL,
NULL,
NULL,
gx_default_map_cmyk_color,
NULL,
NULL,
gx_default_map_rgb_alpha_color,
gx_default_get_page_device,
gx_default_get_alpha_bits,
NULL,
gx_default_get_band,
NULL,
gx_default_fill_path,
NULL,
NULL,
gx_default_fill_trapezoid,
gx_default_fill_parallelogram,
gx_default_fill_triangle,
gx_default_draw_thin_line,
gx_default_begin_image,
gx_default_image_data,
gx_default_end_image,
gx_default_strip_tile_rectangle,
gx_default_strip_copy_rop,
gx_get_largest_clipping_box,
gx_default_begin_typed_image,
NULL,
gx_default_map_color_rgb_alpha,
gx_non_imaging_create_compositor,
NULL
}
};
private int
hit_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
if (w > 0 && h > 0)
return_error(gs_error_hit_detected);
return 0;
}