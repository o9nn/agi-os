#ifndef gxpath_INCLUDED
#  define gxpath_INCLUDED
#include "gscpm.h"
#include "gslparam.h"
#include "gspenum.h"
#include "gsrect.h"
#ifndef gx_path_DEFINED
#  define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#define gx_rule_winding_number (-1)
#define gx_rule_even_odd 1
typedef enum {
sn_none = 0,
sn_not_first = 1,
sn_from_arc = 2
} segment_notes;
#ifdef DEBUG
void gx_dump_path(const gx_path *, const char *);
void gx_path_print(const gx_path *);
#endif
gx_path *gx_path_alloc_shared(const gx_path * shared, gs_memory_t * mem,
client_name_t cname);
#define gx_path_alloc(mem, cname)\
gx_path_alloc_shared(NULL, mem, cname)
int gx_path_init_contained_shared(gx_path * ppath, const gx_path * shared,
gs_memory_t * mem, client_name_t cname);
#define gx_path_alloc_contained(ppath, mem, cname)\
gx_path_init_contained_shared(ppath, NULL, mem, cname)
int gx_path_init_local_shared(gx_path * ppath, const gx_path * shared,
gs_memory_t * mem);
#define gx_path_init_local(ppath, mem)\
(void)gx_path_init_local_shared(ppath, NULL, mem)
void gx_path_init_bbox_accumulator(gx_path * ppath);
int gx_path_unshare(gx_path * ppath);
void gx_path_free(gx_path * ppath, client_name_t cname);
int gx_path_assign_preserve(gx_path * ppto, gx_path * ppfrom);
int gx_path_assign_free(gx_path * ppto, gx_path * ppfrom);
int gx_path_new(gx_path *),
gx_path_add_point(gx_path *, fixed, fixed),
gx_path_add_relative_point(gx_path *, fixed, fixed),
gx_path_add_line_notes(gx_path *, fixed, fixed, segment_notes),
gx_path_add_lines_notes(gx_path *, const gs_fixed_point *, int, segment_notes),
gx_path_add_rectangle(gx_path *, fixed, fixed, fixed, fixed),
gx_path_add_char_path(gx_path *, gx_path *, gs_char_path_mode),
gx_path_add_curve_notes(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, segment_notes),
gx_path_add_partial_arc_notes(gx_path *, fixed, fixed, fixed, fixed, floatp, segment_notes),
gx_path_add_path(gx_path *, gx_path *),
gx_path_close_subpath_notes(gx_path *, segment_notes),
gx_path_pop_close_notes(gx_path *, segment_notes);
byte gx_path_get_state_flags(gx_path *ppath);
void gx_path_set_state_flags(gx_path *ppath, byte flags);
bool gx_path_is_drawing(gx_path *ppath);
#define quarter_arc_fraction 0.55228474983079334
#define gx_path_add_line(ppath, x, y)\
gx_path_add_line_notes(ppath, x, y, sn_none)
#define gx_path_add_lines(ppath, pts, count)\
gx_path_add_lines_notes(ppath, pts, count, sn_none)
#define gx_path_add_curve(ppath, x1, y1, x2, y2, x3, y3)\
gx_path_add_curve_notes(ppath, x1, y1, x2, y2, x3, y3, sn_none)
#define gx_path_add_partial_arc(ppath, x3, y3, xt, yt, fraction)\
gx_path_add_partial_arc_notes(ppath, x3, y3, xt, yt, fraction, sn_none)
#define gx_path_close_subpath(ppath)\
gx_path_close_subpath_notes(ppath, sn_none)
#define gx_path_pop_close_subpath(ppath)\
gx_path_pop_close_notes(ppath, sn_none)
typedef enum {
pco_none = 0,
pco_monotonize = 1,
pco_accurate = 2,
pco_for_stroke = 4,
pco_small_curves = 8
} gx_path_copy_options;
gx_path *gx_current_path(const gs_state *);
int gx_path_current_point(const gx_path *, gs_fixed_point *),
gx_path_bbox(gx_path *, gs_fixed_rect *),
gx_path_bbox_set(gx_path *, gs_fixed_rect *);
int gx_path_subpath_start_point(const gx_path *, gs_fixed_point *);
bool gx_path_has_curves(const gx_path *),
gx_path_is_void(const gx_path *),
gx_path_is_null(const gx_path *),
gx_path__check_curves(const gx_path * ppath, gx_path_copy_options options, fixed fixed_flat);
typedef enum {
prt_none = 0,
prt_open = 1,
prt_fake_closed = 2,
prt_closed = 3
} gx_path_rectangular_type;
gx_path_rectangular_type
gx_path_is_rectangular(const gx_path *, gs_fixed_rect *);
#define gx_path_is_rectangle(ppath, pbox)\
(gx_path_is_rectangular(ppath, pbox) != prt_none)
#define gx_path_is_null_inline(ppath)\
(gx_path_is_void(ppath) && !path_position_valid(ppath))
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
int gx_path_copy_reducing(const gx_path * ppath_old, gx_path * ppath_new,
fixed fixed_flatness, const gs_imager_state *pis,
gx_path_copy_options options);
#define gx_path_copy(old, new)\
gx_path_copy_reducing(old, new, max_fixed, NULL, pco_none)
#define gx_path_add_flattened(old, new, flatness)\
gx_path_copy_reducing(old, new, float2fixed(flatness), NULL, pco_none)
#define gx_path_add_flattened_accurate(old, new, flatness, accurate)\
gx_path_copy_reducing(old, new, float2fixed(flatness), NULL,\
(accurate ? pco_accurate : pco_none))
#define gx_path_add_flattened_for_stroke(old, new, flatness, pis)\
gx_path_copy_reducing(old, new, float2fixed(flatness), pis,\
(pis->accurate_curves ?\
pco_accurate | pco_for_stroke : pco_for_stroke))
#define gx_path_add_monotonized(old, new)\
gx_path_copy_reducing(old, new, max_fixed, NULL, pco_monotonize)
int gx_path_add_dash_expansion(const gx_path * , gx_path * ,
const gs_imager_state *),
gx_path_copy_reversed(const gx_path * , gx_path * ),
gx_path_translate(gx_path *, fixed, fixed),
gx_path_scale_exp2_shared(gx_path *ppath, int log2_scale_x,
int log2_scale_y, bool segments_shared);
void gx_point_scale_exp2(gs_fixed_point *, int, int),
gx_rect_scale_exp2(gs_fixed_rect *, int, int);
int gx_path_enum_init(gs_path_enum *, const gx_path *);
int gx_path_enum_next(gs_path_enum *, gs_fixed_point[3]);
segment_notes
gx_path_enum_notes(const gs_path_enum *);
bool gx_path_enum_backup(gs_path_enum *);
int gs_moveto_aux(gs_imager_state *pis, gx_path *ppath, floatp x, floatp y);
int gx_setcurrentpoint_from_path(gs_imager_state *pis, gx_path *path);
int gx_path_merge_contacting_contours(gx_path *ppath);
#ifndef gx_clip_path_DEFINED
#  define gx_clip_path_DEFINED
typedef struct gx_clip_path_s gx_clip_path;
#endif
int gx_clip_to_rectangle(gs_state *, gs_fixed_rect *);
int gx_clip_to_path(gs_state *);
int gx_default_clip_box(const gs_state *, gs_fixed_rect *);
int gx_effective_clip_path(gs_state *, gx_clip_path **);
#ifndef gx_clip_list_DEFINED
#  define gx_clip_list_DEFINED
typedef struct gx_clip_list_s gx_clip_list;
#endif
typedef struct gs_cpath_enum_s gs_cpath_enum;
gx_clip_path *gx_cpath_alloc_shared(const gx_clip_path * shared,
gs_memory_t * mem, client_name_t cname);
#define gx_cpath_alloc(mem, cname)\
gx_cpath_alloc_shared(NULL, mem, cname)
int gx_cpath_init_contained_shared(gx_clip_path * pcpath,
const gx_clip_path * shared,
gs_memory_t * mem,
client_name_t cname);
#define gx_cpath_alloc_contained(pcpath, mem, cname)\
gx_cpath_init_contained_shared(pcpath, NULL, mem, cname)
int gx_cpath_init_local_shared(gx_clip_path * pcpath,
const gx_clip_path * shared,
gs_memory_t * mem);
#define gx_cpath_init_local(pcpath, mem)\
(void)gx_cpath_init_local_shared(pcpath, NULL, mem)
int gx_cpath_unshare(gx_clip_path * pcpath);
void gx_cpath_free(gx_clip_path * pcpath, client_name_t cname);
int gx_cpath_assign_preserve(gx_clip_path * pcpto, gx_clip_path * pcpfrom);
int gx_cpath_assign_free(gx_clip_path * pcpto, gx_clip_path * pcpfrom);
int
gx_cpath_reset(gx_clip_path *),
gx_cpath_from_rectangle(gx_clip_path *, gs_fixed_rect *),
gx_cpath_clip(gs_state *, gx_clip_path *,  gx_path *, int),
gx_cpath_intersect(gx_clip_path *,  gx_path *, int,
gs_imager_state *),
gx_cpath_scale_exp2_shared(gx_clip_path *pcpath, int log2_scale_x,
int log2_scale_y, bool list_shared,
bool segments_shared),
gx_cpath_to_path(gx_clip_path *, gx_path *);
bool
gx_cpath_inner_box(const gx_clip_path *, gs_fixed_rect *),
gx_cpath_outer_box(const gx_clip_path *, gs_fixed_rect *),
gx_cpath_includes_rectangle(const gx_clip_path *, fixed, fixed,
fixed, fixed);
const gs_fixed_rect *cpath_is_rectangle(const gx_clip_path * pcpath);
int gx_cpath_enum_init(gs_cpath_enum *, gx_clip_path *);
int gx_cpath_enum_next(gs_cpath_enum *, gs_fixed_point[3]);
segment_notes
gx_cpath_enum_notes(const gs_cpath_enum *);
#ifdef DEBUG
void gx_cpath_print(const gx_clip_path *);
#endif
#endif