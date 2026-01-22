#ifndef gxclpage_INCLUDED
#  define gxclpage_INCLUDED
#include "gxclio.h"
int gdev_prn_save_page(gx_device_printer * pdev, gx_saved_page * page,
int num_copies);
int gdev_prn_render_pages(gx_device_printer * pdev,
const gx_placed_page * ppages, int count);
#endif