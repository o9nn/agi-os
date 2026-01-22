#ifndef gdevprna_INCLUDED
# define gdevprna_INCLUDED
# include "gdevprn.h"
# include "gxsync.h"
struct gdev_prn_start_render_params_s {
gx_device_printer *writer_device;
gx_semaphore_t *open_semaphore;
int open_code;
};
#define init_async_render_procs(xpdev, xstart_render_thread,\
xbuffer_page, xprint_page_copies)\
BEGIN\
(xpdev)->printer_procs.start_render_thread = (xstart_render_thread);\
(xpdev)->printer_procs.buffer_page = (xbuffer_page);\
(xpdev)->printer_procs.print_page_copies = (xprint_page_copies);\
END
int gdev_prn_async_write_open(gx_device_printer *pdev, int max_raster,
int min_band_height, int max_src_image_row);
int gdev_prn_async_render_open(gx_device_printer *prdev);
int
gdev_prn_async_render_thread(gdev_prn_start_render_params *);
#endif