#ifndef gxclist_INCLUDED
#  define gxclist_INCLUDED
#include "gscspace.h"
#include "gxband.h"
#include "gxbcache.h"
#include "gxclio.h"
#include "gxdevbuf.h"
#include "gxistate.h"
#include "gxrplane.h"
typedef struct gx_saved_page_s {
gx_device device;
char dname[8 + 1];
gx_band_page_info_t info;
int num_copies;
} gx_saved_page;
typedef struct gx_placed_page_s {
gx_saved_page *page;
gs_int_point offset;
} gx_placed_page;
#define proc_free_up_bandlist_memory(proc)\
int proc(gx_device *dev, bool flush_current)
typedef struct {
ulong offset;
} tile_hash;
typedef struct {
gx_cached_bits_common;
byte x_reps, y_reps;
ushort rep_shift;
ushort index;
ushort num_bands;
#define ts_mask(pts) (byte *)((pts) + 1)
#define ts_bits(cldev,pts) (ts_mask(pts) + (cldev)->tile_band_mask_size)
} tile_slot;
typedef struct cmd_prefix_s cmd_prefix;
struct cmd_prefix_s {
cmd_prefix *next;
uint size;
};
typedef struct cmd_list_s {
cmd_prefix *head, *tail;
} cmd_list;
extern const gs_imager_state clist_imager_state_initial;
typedef struct gx_clist_state_s gx_clist_state;
#define gx_device_clist_common_members\
gx_device_forward_common;	\
\
\
\
\
gx_device_buf_procs_t buf_procs;\
gs_memory_t *bandlist_memory;	\
byte *data;			\
uint data_size;			\
gx_band_params_t band_params;	\
bool do_not_open_or_close_bandfiles;	\
bool page_uses_transparency;	\
\
gx_bits_cache_chunk chunk;	\
gx_bits_cache bits;\
uint tile_hash_mask;		\
uint tile_band_mask_size;	\
\
tile_hash *tile_table;		\
\
\
int ymin, ymax;			\
\
gx_band_page_info_t page_info;	\
int nbands
typedef struct gx_device_clist_common_s {
gx_device_clist_common_members;
} gx_device_clist_common;
#define clist_band_height(cldev) ((cldev)->page_info.band_height)
#define clist_cfname(cldev) ((cldev)->page_info.cfname)
#define clist_cfile(cldev) ((cldev)->page_info.cfile)
#define clist_bfname(cldev) ((cldev)->page_info.bfname)
#define clist_bfile(cldev) ((cldev)->page_info.bfile)
#define cmd_max_dash 11
typedef struct clist_color_space_s {
byte byte1;
gs_id id;
const gs_color_space *space;
} clist_color_space_t;
typedef struct gx_device_clist_writer_s {
gx_device_clist_common_members;
int error_code;
gx_clist_state *states;
byte *cbuf;
byte *cnext;
byte *cend;
cmd_list *ccl;
cmd_list band_range_list;
int band_range_min, band_range_max;
uint tile_max_size;
uint tile_max_count;
gx_strip_bitmap tile_params;
int tile_depth;
int tile_known_min, tile_known_max;
gs_imager_state imager_state;
float dash_pattern[cmd_max_dash];
const gx_clip_path *clip_path;
gs_id clip_path_id;
clist_color_space_t color_space;
gs_id transfer_ids[4];
gs_id black_generation_id;
gs_id undercolor_removal_id;
gs_id device_halftone_id;
gs_id image_enum_id;
int error_is_retryable;
int permanent_error;
int driver_call_nesting;
int ignore_lo_mem_warnings;
proc_free_up_bandlist_memory((*free_up_bandlist_memory));
int disable_mask;
} gx_device_clist_writer;
#define clist_disable_fill_path	(1 << 0)
#define clist_disable_stroke_path (1 << 1)
#define clist_disable_hl_image (1 << 2)
#define clist_disable_complex_clip (1 << 3)
#define clist_disable_nonrect_hl_image (1 << 4)
#define clist_disable_pass_thru_params (1 << 5)
#define clist_disable_copy_alpha (1 << 6)
typedef struct gx_device_clist_reader_s {
gx_device_clist_common_members;
gx_render_plane_t yplane;
const gx_placed_page *pages;
int num_pages;
} gx_device_clist_reader;
typedef union gx_device_clist_s {
gx_device_clist_common common;
gx_device_clist_reader reader;
gx_device_clist_writer writer;
} gx_device_clist;
extern_st(st_device_clist);
#define public_st_device_clist()	\
gs_public_st_complex_only(st_device_clist, gx_device_clist,\
"gx_device_clist", 0, device_clist_enum_ptrs, device_clist_reloc_ptrs,\
gx_device_finalize)
#define st_device_clist_max_ptrs\
(st_device_forward_max_ptrs + st_imager_state_num_ptrs + 1)
#define clist_init_params(xclist, xdata, xdata_size, xtarget, xbuf_procs, xband_params, xexternal, xmemory, xfree_bandlist, xdisable, pageusestransparency)\
BEGIN\
(xclist)->common.data = (xdata);\
(xclist)->common.data_size = (xdata_size);\
(xclist)->common.target = (xtarget);\
(xclist)->common.buf_procs = (xbuf_procs);\
(xclist)->common.band_params = (xband_params);\
(xclist)->common.do_not_open_or_close_bandfiles = (xexternal);\
(xclist)->common.bandlist_memory = (xmemory);\
(xclist)->writer.free_up_bandlist_memory = (xfree_bandlist);\
(xclist)->writer.disable_mask = (xdisable);\
(xclist)->writer.page_uses_transparency = (pageusestransparency);\
END
#define clist_test_VMerror_recoverable(cldev)\
((cldev)->free_up_bandlist_memory != 0)
extern const gx_device_procs gs_clist_device_procs;
int clist_finish_page(gx_device * dev, bool flush);
int clist_close_output_file(gx_device *dev);
int clist_close_page_info(gx_band_page_info_t *ppi);
void clist_compute_colors_used(gx_device_clist_writer *cldev);
#ifndef gx_device_printer_DEFINED
#  define gx_device_printer_DEFINED
typedef struct gx_device_printer_s gx_device_printer;
#endif
int clist_setup_params(gx_device *dev);
int clist_render_rectangle(gx_device_clist *cdev,
const gs_int_rect *prect, gx_device *bdev,
const gx_render_plane_t *render_plane,
bool clear);
#endif