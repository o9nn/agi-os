#ifndef gxdcolor_INCLUDED
# define gxdcolor_INCLUDED
#include "gscsel.h"
#include "gsdcolor.h"
#include "gsropt.h"
#include "gsstruct.h"
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
typedef struct gx_rop_source_s {
const byte *sdata;
int sourcex;
uint sraster;
gx_bitmap_id id;
gx_color_index scolors[2];
bool use_scolors;
} gx_rop_source_t;
#define gx_rop_no_source_body(black_pixel)\
NULL, 0, 0, gx_no_bitmap_id, {black_pixel, black_pixel}, true
#define gx_rop_source_set_color(prs, pixel)\
((prs)->scolors[0] = (prs)->scolors[1] = (pixel))
void gx_set_rop_no_source(const gx_rop_source_t **psource,
gx_rop_source_t *pno_source, gx_device *dev);
#define set_rop_no_source(source, no_source, dev)\
gx_set_rop_no_source(&(source), &(no_source), dev)
struct gx_device_color_type_s {
gs_memory_type_ptr_t stype;
#define dev_color_proc_save_dc(proc)\
void proc(const gx_device_color * pdevc, gx_device_color_saved * psdc)
dev_color_proc_save_dc((*save_dc));
#define dev_color_proc_get_dev_halftone(proc)\
const gx_device_halftone * proc(const gx_device_color * pdevc)
dev_color_proc_get_dev_halftone((*get_dev_halftone));
#define dev_color_proc_get_phase(proc)\
bool proc(const gx_device_color * pdevc, gs_int_point * pphase)
dev_color_proc_get_phase((*get_phase));
#define dev_color_proc_load(proc)\
int proc(gx_device_color *pdevc, const gs_imager_state *pis,\
gx_device *dev, gs_color_select_t select)
dev_color_proc_load((*load));
#define dev_color_proc_fill_rectangle(proc)\
int proc(const gx_device_color *pdevc, int x, int y, int w, int h,\
gx_device *dev, gs_logical_operation_t lop, const gx_rop_source_t *source)
dev_color_proc_fill_rectangle((*fill_rectangle));
#define dev_color_proc_fill_masked(proc)\
int proc(const gx_device_color *pdevc, const byte *data, int data_x,\
int raster, gx_bitmap_id id, int x, int y, int w, int h,\
gx_device *dev, gs_logical_operation_t lop, bool invert)
dev_color_proc_fill_masked((*fill_masked));
#define dev_color_proc_equal(proc)\
bool proc(const gx_device_color *pdevc1, const gx_device_color *pdevc2)
dev_color_proc_equal((*equal));
#define dev_color_proc_write(proc)\
int proc(const gx_device_color *pdevc, const gx_device_color_saved *psdc,\
const gx_device * dev, byte *data, uint *psize)
dev_color_proc_write((*write));
#define dev_color_proc_read(proc)\
int proc(gx_device_color *pdevc, const gs_imager_state * pis,\
const gx_device_color *prior_devc, const gx_device * dev,\
const byte *data, uint size, gs_memory_t *mem)
dev_color_proc_read((*read));
#define dev_color_proc_get_nonzero_comps(proc)\
int proc(const gx_device_color * pdevc, const gx_device * dev,\
gx_color_index * pcomp_bits)
dev_color_proc_get_nonzero_comps((*get_nonzero_comps));
};
dev_color_proc_fill_masked(gx_dc_default_fill_masked);
extern_st(st_device_color);
extern const gx_device_color_type_t
#define gx_dc_type_none (&gx_dc_type_data_none)
gx_dc_type_data_none,
#define gx_dc_type_null (&gx_dc_type_data_null)
gx_dc_type_data_null,
#define gx_dc_type_pure (&gx_dc_type_data_pure)
gx_dc_type_data_pure,
#define gx_dc_type_ht_binary (&gx_dc_type_data_ht_binary)
gx_dc_type_data_ht_binary,
#define gx_dc_type_ht_colored (&gx_dc_type_data_ht_colored)
gx_dc_type_data_ht_colored,
#define gx_dc_type_wts (&gx_dc_type_data_wts)
gx_dc_type_data_wts;
extern dev_color_proc_get_nonzero_comps(gx_dc_pure_get_nonzero_comps);
extern dev_color_proc_get_nonzero_comps(gx_dc_ht_binary_get_nonzero_comps);
extern dev_color_proc_get_nonzero_comps(gx_dc_ht_colored_get_nonzero_comps);
extern int gx_get_dc_type_index(const gx_device_color *);
extern const gx_device_color_type_t * gx_get_dc_type_from_index(int);
extern dev_color_proc_get_phase(gx_dc_no_get_phase);
extern dev_color_proc_get_phase(gx_dc_ht_get_phase);
#define gs_color_writes_pure(pgs)\
color_writes_pure((pgs)->dev_color, (pgs)->log_op)
void gx_set_device_color_1(gs_state * pgs);
int gx_remap_color(gs_state *);
#define gx_set_dev_color(pgs)\
if ( !color_is_set((pgs)->dev_color) )\
{ int code_dc = gx_remap_color(pgs);\
if ( code_dc != 0 ) return code_dc;\
}
#define gx_unset_dev_color(pgs)\
color_unset((pgs)->dev_color)
#define gx_color_load_select(pdevc, pis, dev, select)\
(*(pdevc)->type->load)(pdevc, pis, dev, select)
#define gx_color_load(pdevc, pis, dev)\
gx_color_load_select(pdevc, pis, dev, gs_color_select_texture)
#define gs_state_color_load(pgs)\
gx_color_load((pgs)->dev_color, (const gs_imager_state *)(pgs),\
(pgs)->device)
#define gx_device_color_fill_rectangle(pdevc, x, y, w, h, dev, lop, source)\
(*(pdevc)->type->fill_rectangle)(pdevc, x, y, w, h, dev, lop, source)
#define gx_fill_rectangle_device_rop(x, y, w, h, pdevc, dev, lop)\
gx_device_color_fill_rectangle(pdevc, x, y, w, h, dev, lop, NULL)
#define gx_fill_rectangle_rop(x, y, w, h, pdevc, lop, pgs)\
gx_fill_rectangle_device_rop(x, y, w, h, pdevc, (pgs)->device, lop)
#define gx_fill_rectangle(x, y, w, h, pdevc, pgs)\
gx_fill_rectangle_rop(x, y, w, h, pdevc, (pgs)->log_op, pgs)
extern int gx_dc_write_color( gx_color_index color,
const gx_device * dev,
byte * pdata,
uint * psize );
extern int gx_dc_read_color( gx_color_index * pcolor,
const gx_device * dev,
const byte * pdata,
int size );
#endif