#ifndef gxpcolor_INCLUDED
# define gxpcolor_INCLUDED
#include "gspcolor.h"
#include "gxcspace.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxpcache.h"
#ifndef gs_pattern_type_DEFINED
# define gs_pattern_type_DEFINED
typedef struct gs_pattern_type_s gs_pattern_type_t;
#endif
struct gs_pattern_type_s {
int PatternType;
struct pp_ {
#define pattern_proc_uses_base_space(proc)\
bool proc(const gs_pattern_template_t *)
pattern_proc_uses_base_space((*uses_base_space));
#define pattern_proc_make_pattern(proc)\
int proc(gs_client_color *, const gs_pattern_template_t *,\
const gs_matrix *, gs_state *, gs_memory_t *)
pattern_proc_make_pattern((*make_pattern));
#define pattern_proc_get_pattern(proc)\
const gs_pattern_template_t *proc(const gs_pattern_instance_t *)
pattern_proc_get_pattern((*get_pattern));
#define pattern_proc_remap_color(proc)\
cs_proc_remap_color(proc)
pattern_proc_remap_color((*remap_color));
#define pattern_proc_set_color(proc)\
int proc(const gs_client_color *, gs_state *)
pattern_proc_set_color((*set_color));
} procs;
};
void gs_pattern_common_init(gs_pattern_template_t *,
const gs_pattern_type_t *);
int gs_make_pattern_common(gs_client_color *, const gs_pattern_template_t *,
const gs_matrix *, gs_state *, gs_memory_t *,
gs_memory_type_ptr_t);
extern rc_free_proc(rc_free_pattern_instance);
extern const gs_color_space_type gs_color_space_type_Pattern;
extern const gx_device_color_type_t
gx_dc_pattern,
gx_dc_pure_masked, gx_dc_binary_masked, gx_dc_colored_masked;
#define gx_dc_type_pattern (&gx_dc_pattern)
extern dev_color_proc_save_dc(gx_dc_pattern_save_dc);
extern dev_color_proc_write(gx_dc_pattern_write);
extern dev_color_proc_read(gx_dc_pattern_read);
extern dev_color_proc_get_nonzero_comps(gx_dc_pattern_get_nonzero_comps);
struct gx_color_tile_s {
gx_bitmap_id id;
int depth;
gs_uid uid;
int tiling_type;
gs_matrix step_matrix;
gs_rect bbox;
gx_strip_bitmap tbits;
gx_strip_bitmap tmask;
bool is_simple;
bool is_dummy;
uint index;
};
#define private_st_color_tile() \
gs_private_st_ptrs2(st_color_tile, gx_color_tile, "gx_color_tile",\
color_tile_enum_ptrs, color_tile_reloc_ptrs, tbits.data, tmask.data)
#define private_st_color_tile_element() \
gs_private_st_element(st_color_tile_element, gx_color_tile,\
"gx_color_tile[]", color_tile_elt_enum_ptrs, color_tile_elt_reloc_ptrs,\
st_color_tile)
uint gx_pat_cache_default_tiles(void);
ulong gx_pat_cache_default_bits(void);
gx_pattern_cache *gx_pattern_alloc_cache(gs_memory_t *, uint, ulong);
gx_pattern_cache *gstate_pattern_cache(gs_state *);
void gstate_set_pattern_cache(gs_state *, gx_pattern_cache *);
typedef struct gx_device_pattern_accum_s {
gx_device_forward_common;
gs_memory_t *bitmap_memory;
const gs_pattern1_instance_t *instance;
gx_device_memory *bits;
gx_device_memory *mask;
} gx_device_pattern_accum;
#define private_st_device_pattern_accum() \
gs_private_st_suffix_add3_final(st_device_pattern_accum,\
gx_device_pattern_accum, "pattern accumulator", pattern_accum_enum,\
pattern_accum_reloc, gx_device_finalize, st_device_forward,\
instance, bits, mask)
gx_device_pattern_accum *gx_pattern_accum_alloc(gs_memory_t * memory, client_name_t);
int gx_pattern_cache_add_entry(gs_imager_state *, gx_device_pattern_accum *,
gx_color_tile **);
int gx_pattern_cache_add_dummy_entry(gs_imager_state *pis, gs_pattern1_instance_t *pinst,
int depth);
bool gx_pattern_cache_lookup(gx_device_color *, const gs_imager_state *,
gx_device *, gs_color_select_t);
void gx_pattern_cache_winnow(gx_pattern_cache *,
bool (*)(gx_color_tile *, void *),
void *);
#endif