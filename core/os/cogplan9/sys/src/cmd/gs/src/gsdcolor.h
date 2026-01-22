#ifndef gsdcolor_INCLUDED
# define gsdcolor_INCLUDED
#include "gsccolor.h"
#include "gxarith.h"
#include "gxbitmap.h"
#include "gxhttile.h"
#include "gxcindex.h"
#include "gxwts.h"
#ifndef gx_device_color_DEFINED
# define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#ifndef gx_device_saved_color_DEFINED
# define gx_device_saved_color_DEFINED
typedef struct gx_device_color_saved_s gx_device_color_saved;
#endif
#ifndef gx_device_halftone_DEFINED
# define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
#define gx_dc_is_pure(pdc)\
((pdc)->type == gx_dc_type_pure)
#define gx_dc_writes_pure(pdc, lop)\
(gx_dc_is_pure(pdc) && lop_no_S_is_T(lop))
#define gx_dc_pure_color(pdc)\
((pdc)->colors.pure)
#define gx_dc_phase(pdc)\
((pdc)->phase)
#define gx_dc_is_binary_halftone(pdc)\
((pdc)->type == gx_dc_type_ht_binary)
#define gx_dc_binary_tile(pdc)\
(&(pdc)->colors.binary.b_tile->tiles)
#define gx_dc_binary_color0(pdc)\
((pdc)->colors.binary.color[0])
#define gx_dc_binary_color1(pdc)\
((pdc)->colors.binary.color[1])
#define gx_dc_is_colored_halftone(pdc)\
((pdc)->type == gx_dc_type_ht_colored)
bool gx_device_color_equal(const gx_device_color *pdevc1,
const gx_device_color *pdevc2);
#define color_is_set(pdc)\
((pdc)->type != gx_dc_type_none)
#define color_unset(pdc)\
((pdc)->type = gx_dc_type_none)
#define gx_dc_is_null(pdc)\
((pdc)->type == gx_dc_type_null)
#define color_is_null(pdc) gx_dc_is_null(pdc)
#define color_set_null(pdc)\
((pdc)->type = gx_dc_type_null)
#define color_is_pure(pdc) gx_dc_is_pure(pdc)
#define color_writes_pure(pdc, lop) gx_dc_writes_pure(pdc, lop)
#define color_set_pure(pdc, color)\
((pdc)->colors.pure = (color),\
(pdc)->type = gx_dc_type_pure)
#define set_nonclient_dev_color(pdc, color)\
color_set_pure(pdc, color);\
(pdc)->ccolor_valid = false
#define color_set_phase(pdc, px, py)\
((pdc)->phase.x = (px),\
(pdc)->phase.y = (py))
#define color_set_phase_mod(pdc, px, py, tw, th)\
color_set_phase(pdc, imod(-(px), tw), imod(-(py), th))
#define color_is_binary_halftone(pdc) gx_dc_is_binary_halftone(pdc)
#define color_set_binary_halftone_component(pdc, ht, index, color0, color1, level)\
((pdc)->colors.binary.b_ht = (ht),\
(pdc)->colors.binary.b_index = (index),\
(pdc)->colors.binary.color[0] = (color0),\
(pdc)->colors.binary.color[1] = (color1),\
(pdc)->colors.binary.b_level = (level),\
(pdc)->type = gx_dc_type_ht_binary)
#define color_set_binary_halftone(pdc, ht, color0, color1, level)\
color_set_binary_halftone_component(pdc, ht, -1, color0, color1, level)
#define color_set_binary_tile(pdc, color0, color1, tile)\
((pdc)->colors.binary.b_ht = 0,\
(pdc)->colors.binary.color[0] = (color0),\
(pdc)->colors.binary.color[1] = (color1),\
(pdc)->colors.binary.b_index = -1, \
(pdc)->colors.binary.b_tile = (tile),\
(pdc)->type = gx_dc_type_ht_binary)
#define color_is_colored_halftone(pdc) gx_dc_is_colored_halftone(pdc)
#define _color_set_c(pdc, i, b, l)\
((pdc)->colors.colored.c_base[i] = (b),\
(pdc)->colors.colored.c_level[i] = (l))
void gx_complete_halftone(gx_device_color *pdevc, int num_comps,
gx_device_halftone *pdht);
#define color_set_null_pattern(pdc)\
((pdc)->mask.id = gx_no_bitmap_id,\
(pdc)->mask.m_tile = 0,\
(pdc)->colors.pattern.p_tile = 0,\
(pdc)->type = gx_dc_type_pattern)
#ifndef gx_ht_tile_DEFINED
# define gx_ht_tile_DEFINED
typedef struct gx_ht_tile_s gx_ht_tile;
#endif
#ifndef gx_color_tile_DEFINED
# define gx_color_tile_DEFINED
typedef struct gx_color_tile_s gx_color_tile;
#endif
typedef struct gx_device_color_type_s gx_device_color_type_t;
typedef const gx_device_color_type_t *gx_device_color_type;
struct gx_device_color_s {
gx_device_color_type type;
union _c {
gx_color_index pure;
struct _bin {
const gx_device_halftone *b_ht;
gx_color_index color[2];
uint b_level;
int b_index;
gx_ht_tile *b_tile;
} binary;
struct _col {
gx_device_halftone *c_ht;
ushort num_components;
byte c_base[GX_DEVICE_COLOR_MAX_COMPONENTS];
uint c_level[GX_DEVICE_COLOR_MAX_COMPONENTS];
ushort alpha;
#if GX_DEVICE_COLOR_MAX_COMPONENTS <= ARCH_SIZEOF_SHORT * 8
ushort plane_mask;
#else
#if GX_DEVICE_COLOR_MAX_COMPONENTS <= ARCH_SIZEOF_INT * 8
uint plane_mask;
#else
gx_color_index plane_mask;
#endif
#endif
} colored;
struct _wts {
const gx_device_halftone *w_ht;
wts_screen_sample_t levels[GX_DEVICE_COLOR_MAX_COMPONENTS];
ushort num_components;
gx_color_index plane_vector[GX_DEVICE_COLOR_MAX_COMPONENTS];
} wts;
struct _pat {
gx_color_tile *p_tile;
} pattern;
} colors;
gs_int_point phase;
bool ccolor_valid;
gs_client_color ccolor;
struct _mask {
struct mp_ {
short x, y;
} m_phase;
gx_bitmap_id id;
gx_color_tile *m_tile;
} mask;
};
#define public_st_device_color() \
gs_public_st_composite(st_device_color, gx_device_color, "gx_device_color",\
device_color_enum_ptrs, device_color_reloc_ptrs)
#define st_device_color_max_ptrs (st_client_color_max_ptrs + 2)
struct gx_device_color_saved_s {
gx_device_color_type type;
union _svc {
gx_color_index pure;
struct _svbin {
gx_color_index b_color[2];
uint b_level;
int b_index;
} binary;
struct _svcol {
byte c_base[GX_DEVICE_COLOR_MAX_COMPONENTS];
uint c_level[GX_DEVICE_COLOR_MAX_COMPONENTS];
ushort alpha;
} colored;
struct _swts {
wts_screen_sample_t levels[GX_DEVICE_COLOR_MAX_COMPONENTS];
} wts;
struct _pattern {
gs_id id;
gs_int_point phase;
} pattern;
struct _pattern2 {
gs_id id;
} pattern2;
} colors;
gs_int_point phase;
};
#ifndef gx_dc_type_none
extern const gx_device_color_type_t *const gx_dc_type_none;
#endif
#ifndef gx_dc_type_null
extern const gx_device_color_type_t *const gx_dc_type_null;
#endif
#ifndef gx_dc_type_pure
extern const gx_device_color_type_t *const gx_dc_type_pure;
#endif
#ifndef gx_dc_type_pattern
#endif
#ifndef gx_dc_type_ht_binary
extern const gx_device_color_type_t *const gx_dc_type_ht_binary;
#endif
#ifndef gx_dc_type_ht_colored
extern const gx_device_color_type_t *const gx_dc_type_ht_colored;
#endif
#ifndef gx_dc_type_ht_colored
extern const gx_device_color_type_t *const gx_dc_type_wts;
#endif
#endif