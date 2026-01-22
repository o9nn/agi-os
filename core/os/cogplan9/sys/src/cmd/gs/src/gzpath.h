#ifndef gzpath_INCLUDED
#  define gzpath_INCLUDED
#include "gxpath.h"
#include "gsmatrix.h"
#include "gsrefct.h"
#include "gsstype.h"
typedef enum {
s_start,
s_line,
s_line_close,
s_curve
} segment_type;
#define segment_common\
segment *prev;\
segment *next;\
ushort  type;\
ushort  notes;\
gs_fixed_point pt;		\
#ifndef segment_DEFINED
#  define segment_DEFINED
typedef struct segment_s segment;
#endif
typedef struct subpath_s subpath;
struct segment_s {
segment_common
};
#define private_st_segment()	\
gs_private_st_ptrs2(st_segment, struct segment_s, "segment",\
segment_enum_ptrs, segment_reloc_ptrs, prev, next)
typedef struct {
segment_common
} line_segment;
#define private_st_line()	\
gs_private_st_suffix_add0(st_line, line_segment, "line",\
line_enum_ptrs, line_reloc_ptrs, st_segment)
typedef struct {
segment_common
subpath * sub;
} line_close_segment;
#define private_st_line_close()	\
gs_private_st_suffix_add1(st_line_close, line_close_segment, "close",\
close_enum_ptrs, close_reloc_ptrs, st_segment, sub)
#define curve_points_to_coefficients(v0, v1, v2, v3, a, b, c, t01, t12)\
(\
t01 = (v1) - (v0), c = (t01 << 1) + t01,\
t12 = (v2) - (v1), b = (t12 << 1) + t12 - c,\
a = (v3) - b - c - (v0))
#define curve_coefficients_to_points(a, b, c, d, v1, v2, v3)\
(\
v1 = (d) + ((c) / 3),\
v2 = v1 + (((b) + (c)) / 3),\
v3 = (a) + (b) + (c) + (d))
typedef struct {
segment_common
gs_fixed_point p1, p2;
} curve_segment;
#define private_st_curve()	\
gs_private_st_suffix_add0_local(st_curve, curve_segment, "curve",\
segment_enum_ptrs, segment_reloc_ptrs, st_segment)
struct subpath_s {
segment_common
segment * last;
int curve_count;
line_close_segment closer;
char  is_closed;
};
#define private_st_subpath()	\
gs_private_st_suffix_add1(st_subpath, subpath, "subpath",\
subpath_enum_ptrs, subpath_reloc_ptrs, st_segment, last)
gx_path_rectangular_type
gx_subpath_is_rectangular(const subpath * pstart, gs_fixed_rect * pbox,
const subpath ** ppnext);
#define gx_subpath_is_rectangle(pstart, pbox, ppnext)\
(gx_subpath_is_rectangular(pstart, pbox, ppnext) != prt_none)
int gx_curve_log2_samples(fixed, fixed, const curve_segment *, fixed);
int gx_curve_monotonic_points(fixed, fixed, fixed, fixed, double[2]);
int gx_curve_monotonize(gx_path * ppath, const curve_segment * pc);
int gx_subdivide_curve(gx_path *, int, curve_segment *, segment_notes);
#define k_sample_max min((size_of(int) * 8 - 1) / 3, 10)
typedef enum {
psf_position_valid = 1,
psf_subpath_open = 2,
psf_is_drawing = 4,
psf_outside_range = 8,
psf_last_newpath = 0,
psf_last_moveto = psf_position_valid | psf_subpath_open,
psf_last_draw = psf_position_valid | psf_subpath_open | psf_is_drawing,
psf_last_closepath = psf_position_valid
} gx_path_state_flags;
#define path_position_valid(ppath)\
(((ppath)->state_flags & psf_position_valid) != 0)
#define path_subpath_open(ppath)\
(((ppath)->state_flags & psf_subpath_open) != 0)
#define path_is_drawing(ppath)\
(((ppath)->state_flags & psf_is_drawing) != 0)
#define path_outside_range(ppath)\
(((ppath)->state_flags & psf_outside_range) != 0)
#define path_last_is_moveto(ppath)\
(((ppath)->state_flags & ~psf_outside_range) == psf_last_moveto)
#define path_position_in_range(ppath)\
(((ppath)->state_flags & (psf_position_valid + psf_outside_range)) ==\
psf_position_valid)
#define path_start_outside_range(ppath)\
((ppath)->state_flags != 0 &&\
((ppath)->start_flags & psf_outside_range) != 0)
#define path_update_newpath(ppath)\
((ppath)->state_flags = psf_last_newpath)
#define path_update_moveto(ppath)\
((ppath)->state_flags = (ppath)->start_flags = psf_last_moveto)
#define path_update_draw(ppath)\
((ppath)->state_flags = psf_last_draw)
#define path_update_closepath(ppath)\
((ppath)->state_flags = psf_last_closepath)
typedef struct gx_path_segments_s {
rc_header rc;
struct psc_ {
subpath *subpath_first;
subpath *subpath_current;
} contents;
} gx_path_segments;
#define private_st_path_segments()	\
gs_private_st_ptrs2(st_path_segments, gx_path_segments, "path segments",\
path_segments_enum_ptrs, path_segments_reloc_ptrs,\
contents.subpath_first, contents.subpath_current)
typedef enum {
path_allocated_on_stack,
path_allocated_contained,
path_allocated_on_heap
} gx_path_allocation_t;
typedef struct gx_path_procs_s {
int (*add_point)(gx_path *, fixed, fixed);
int (*add_line)(gx_path *, fixed, fixed, segment_notes);
int (*add_curve)(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, segment_notes);
int (*close_subpath)(gx_path *, segment_notes);
byte (*state_flags)(gx_path *, byte);
} gx_path_procs;
struct gx_path_s {
gx_path_segments local_segments;
gs_memory_t *memory;
gx_path_allocation_t allocation;
gx_path_segments *segments;
gs_fixed_rect bbox;
segment *box_last;
#define first_subpath segments->contents.subpath_first
#define current_subpath segments->contents.subpath_current
byte  start_flags;
byte  state_flags;
byte  bbox_set;
byte  bbox_accurate;
byte _pad;
int subpath_count;
int curve_count;
gs_fixed_point position;
gx_path_procs *procs;
};
extern_st(st_path);
#define public_st_path()	\
gs_public_st_ptrs2(st_path, gx_path, "path",\
path_enum_ptrs, path_reloc_ptrs, segments, box_last)
#define st_path_max_ptrs 2
struct gs_path_enum_s {
gs_memory_t *memory;
gs_matrix mat;
const segment *pseg;
const gx_path *path;
gx_path *copied_path;
bool moveto_done;
segment_notes notes;
};
extern_st(st_path_enum);
#define public_st_path_enum()	\
gs_public_st_ptrs3(st_path_enum, gs_path_enum, "gs_path_enum",\
path_enum_enum_ptrs, path_enum_reloc_ptrs, pseg, path, copied_path)
#define gx_path_has_curves_inline(ppath)\
((ppath)->curve_count != 0)
#define gx_path_has_curves(ppath)\
gx_path_has_curves_inline(ppath)
#define gx_path_is_void_inline(ppath)\
((ppath)->segments != 0 && (ppath)->first_subpath == 0)
#define gx_path_is_void(ppath)\
gx_path_is_void_inline(ppath)
#define gx_path_subpath_count(ppath)\
((ppath)->subpath_count)
#define gx_path_is_shared(ppath)\
((ppath)->segments != 0 && (ppath)->segments->rc.ref_count > 1)
#define gx_path_current_point_inline(ppath,ppt)\
( !path_position_valid(ppath) ? gs_note_error(gs_error_nocurrentpoint) :\
((ppt)->x = ppath->position.x, (ppt)->y = ppath->position.y, 0) )
typedef struct gx_flattened_iterator_s gx_flattened_iterator;
struct gx_flattened_iterator_s {
fixed x0, y0, x3, y3;
fixed cx, bx, ax, cy, by, ay;
fixed x, y;
uint i, k;
uint rmask;
fixed idx, idy, id2x, id2y, id3x, id3y;
uint rx, ry, rdx, rdy, rd2x, rd2y, rd3x, rd3y;
bool curve;
fixed lx0, ly0, lx1, ly1;
};
bool gx_flattened_iterator__init(gx_flattened_iterator *this,
fixed x0, fixed y0, const curve_segment *pc, int k);
bool gx_flattened_iterator__init_line(gx_flattened_iterator *this,
fixed x0, fixed y0, fixed x1, fixed y1);
void gx_flattened_iterator__switch_to_backscan(gx_flattened_iterator *this, bool not_first);
bool gx_flattened_iterator__next(gx_flattened_iterator *this);
bool gx_flattened_iterator__prev(gx_flattened_iterator *this);
bool curve_coeffs_ranged(fixed x0, fixed x1, fixed x2, fixed x3,
fixed y0, fixed y1, fixed y2, fixed y3,
fixed *ax, fixed *bx, fixed *cx,
fixed *ay, fixed *by, fixed *cy,
int k);
#endif