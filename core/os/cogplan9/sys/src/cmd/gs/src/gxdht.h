#ifndef gxdht_INCLUDED
#  define gxdht_INCLUDED
#include "gsrefct.h"
#include "gsmatrix.h"
#include "gxarith.h"
#include "gxhttype.h"
#include "gscspace.h"
#include "gxcindex.h"
#include "gxfrac.h"
typedef struct gx_ht_cell_params_s {
short M, N, R;
short M1, N1, R1;
ulong C;
short D, D1;
uint W, W1;
int S;
} gx_ht_cell_params_t;
void gx_compute_cell_values(gx_ht_cell_params_t *);
typedef uint ht_mask_t;
#define ht_mask_bits (sizeof(ht_mask_t) * 8)
typedef struct gx_ht_bit_s {
uint offset;
ht_mask_t mask;
} gx_ht_bit;
typedef ht_mask_t ht_sample_t;
#define max_ht_sample (ht_sample_t)(((1 << (ht_mask_bits - 2)) - 1) * 2 + 1)
#ifndef wts_screen_t_DEFINED
#  define wts_screen_t_DEFINED
typedef struct wts_screen_s wts_screen_t;
#endif
#ifndef gs_wts_screen_enum_t_DEFINED
#  define gs_wts_screen_enum_t_DEFINED
typedef struct gs_wts_screen_enum_s gs_wts_screen_enum_t;
#endif
typedef struct gx_ht_cache_s gx_ht_cache;
#ifndef gx_ht_order_DEFINED
#  define gx_ht_order_DEFINED
typedef struct gx_ht_order_s gx_ht_order;
#endif
#ifndef gx_ht_tile_DEFINED
#  define gx_ht_tile_DEFINED
typedef struct gx_ht_tile_s gx_ht_tile;
#endif
typedef struct gx_ht_order_procs_s {
uint bit_data_elt_size;
int (*construct_order)(gx_ht_order *order, const byte *thresholds);
int (*bit_index)(const gx_ht_order *order, uint index,
gs_int_point *ppt);
int (*render)(gx_ht_tile *tile, int new_bit_level,
const gx_ht_order *order);
int (*draw)(gx_ht_order *order, frac shade,
byte *data, int data_raster,
int x, int y, int w, int h);
} gx_ht_order_procs_t;
extern const gx_ht_order_procs_t ht_order_procs_table[2];
#define ht_order_procs_default ht_order_procs_table[0]
#define ht_order_procs_short ht_order_procs_table[1]
typedef struct gx_ht_order_screen_params_s {
gs_matrix matrix;
ulong max_size;
} gx_ht_order_screen_params_t;
struct gx_ht_order_s {
gx_ht_cell_params_t params;
gs_wts_screen_enum_t *wse;
wts_screen_t *wts;
ushort width;
ushort height;
ushort raster;
ushort shift;
ushort orig_height;
ushort orig_shift;
uint full_height;
uint num_levels;
uint num_bits;
const gx_ht_order_procs_t *procs;
gs_memory_t *data_memory;
uint *levels;
void *bit_data;
gx_ht_cache *cache;
gx_transfer_map *transfer;
gx_ht_order_screen_params_t screen_params;
};
#define ht_order_is_complete(porder)\
((porder)->shift == 0)
#define ht_order_full_height(porder)\
((porder)->shift == 0 ? (porder)->height :\
(porder)->width / igcd((porder)->width, (porder)->shift) *\
(porder)->height)
extern_st(st_ht_order);
#define public_st_ht_order()	\
gs_public_st_composite(st_ht_order, gx_ht_order, "gx_ht_order",\
ht_order_enum_ptrs, ht_order_reloc_ptrs)
#define st_ht_order_max_ptrs 4
typedef struct gx_ht_order_component_s {
gx_ht_order corder;
int comp_number;
int cname;
} gx_ht_order_component;
#define private_st_ht_order_component()	\
gs_private_st_ptrs_add0(st_ht_order_component, gx_ht_order_component,\
"gx_ht_order_component", ht_order_component_enum_ptrs,\
ht_order_component_reloc_ptrs, st_ht_order, corder)
#define st_ht_order_component_max_ptrs st_ht_order_max_ptrs
extern_st(st_ht_order_component_element);
#define public_st_ht_order_comp_element() \
gs_public_st_element(st_ht_order_component_element, gx_ht_order_component,\
"gx_ht_order_component[]", ht_order_element_enum_ptrs,\
ht_order_element_reloc_ptrs, st_ht_order_component)
#ifndef gx_device_halftone_DEFINED
#  define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
struct gx_device_halftone_s {
gx_ht_order order;
rc_header rc;
gs_id id;
gs_halftone_type type;
gx_ht_order_component *components;
uint num_comp;
uint num_dev_comp;
int lcm_width, lcm_height;
};
extern_st(st_device_halftone);
#define public_st_device_halftone() \
gs_public_st_ptrs_add1(st_device_halftone, gx_device_halftone,\
"gx_device_halftone", device_halftone_enum_ptrs,\
device_halftone_reloc_ptrs, st_ht_order, order, components)
#define st_device_halftone_max_ptrs (st_ht_order_max_ptrs + 1)
void gx_ht_complete_threshold_order(gx_ht_order *porder);
void gx_device_halftone_release(gx_device_halftone * pdht, gs_memory_t * mem);
#endif