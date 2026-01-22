#ifndef gdevmswn_INCLUDED
# define gdevmswn_INCLUDED
#include "string_.h"
#include <stdlib.h>
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "memory_.h"
#include "windows_.h"
#include <shellapi.h>
#include "gp_mswin.h"
typedef struct gx_device_win_s gx_device_win;
LPLOGPALETTE win_makepalette(gx_device_win *);
int win_nomemory(void);
void win_update(gx_device_win *);
dev_proc_open_device(win_open);
dev_proc_sync_output(win_sync_output);
dev_proc_output_page(win_output_page);
dev_proc_close_device(win_close);
dev_proc_map_rgb_color(win_map_rgb_color);
dev_proc_map_color_rgb(win_map_color_rgb);
dev_proc_get_params(win_get_params);
dev_proc_put_params(win_put_params);
dev_proc_get_xfont_procs(win_get_xfont_procs);
dev_proc_get_alpha_bits(win_get_alpha_bits);
#define win_proc_copy_to_clipboard(proc)\
void proc(gx_device_win *)
#define win_proc_repaint(proc)\
void proc(gx_device_win *, HDC, int, int, int, int, int, int)
#define win_proc_alloc_bitmap(proc)\
int proc(gx_device_win *, gx_device *)
#define win_proc_free_bitmap(proc)\
void proc(gx_device_win *)
#define win_gsview_sizeof 80
#define gx_device_win_common\
int BitsPerPixel;\
int nColors;\
byte *mapped_color_flags;\
\
win_proc_alloc_bitmap((*alloc_bitmap));\
win_proc_free_bitmap((*free_bitmap));\
\
HPALETTE himgpalette;\
LPLOGPALETTE limgpalette
struct gx_device_win_s {
gx_device_common;
gx_device_win_common;
};
#define INITIAL_RESOLUTION 96.0
#define INITIAL_WIDTH (int)(INITIAL_RESOLUTION * 85 / 10 + 0.5)
#define INITIAL_HEIGHT (int)(INITIAL_RESOLUTION * 11 + 0.5)
#define wdev ((gx_device_win *)dev)
#define rop_write_at_1s 0xE20746L
#define rop_write_at_0s 0xB8074AL
#define rop_write_0_at_1s 0x220326L
#define rop_write_0_at_0s 0x8800C6L
#define rop_write_1s 0xFF0062L
#define rop_write_0s 0x000042L
#define rop_write_pattern 0xF00021L
#define win_color_value(z)\
((((z) >> (gx_color_value_bits - 5)) << 3) +\
((z) >> (gx_color_value_bits - 3)))
#endif