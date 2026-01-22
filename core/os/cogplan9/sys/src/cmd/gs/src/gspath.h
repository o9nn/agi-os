#ifndef gspath_INCLUDED
# define gspath_INCLUDED
#include "gspenum.h"
int gs_newpath(gs_state *),
gs_moveto(gs_state *, floatp, floatp),
gs_rmoveto(gs_state *, floatp, floatp),
gs_lineto(gs_state *, floatp, floatp),
gs_rlineto(gs_state *, floatp, floatp),
gs_arc(gs_state *, floatp, floatp, floatp, floatp, floatp),
gs_arcn(gs_state *, floatp, floatp, floatp, floatp, floatp),
gs_arc_add(gs_state *, bool, floatp, floatp, floatp, floatp, floatp, bool),
gs_arcto(gs_state *, floatp, floatp, floatp, floatp, floatp, float[4]),
gs_curveto(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp),
gs_rcurveto(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp),
gs_closepath(gs_state *);
#ifndef gs_imager_state_DEFINED
# define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef gs_matrix_fixed_DEFINED
#define gs_matrix_fixed_DEFINED
typedef struct gs_matrix_fixed_s gs_matrix_fixed;
#endif
int gs_imager_arc_add(gx_path * ppath, gs_imager_state * pis,
bool clockwise, floatp axc, floatp ayc,
floatp arad, floatp aang1, floatp aang2,
bool add_line);
void make_quadrant_arc(gs_point *p, const gs_point *c,
const gs_point *p0, const gs_point *p1, double r);
int gs_upmergepath(gs_state *);
int gs_currentpoint(gs_state *, gs_point *),
gs_upathbbox(gs_state *, gs_rect *, bool),
gs_dashpath(gs_state *),
gs_flattenpath(gs_state *),
gs_reversepath(gs_state *),
gs_strokepath(gs_state *);
#define gs_pathbbox(pgs, prect)\
gs_upathbbox(pgs, prect, false)
gs_path_enum *gs_path_enum_alloc(gs_memory_t *, client_name_t);
int gs_path_enum_copy_init(gs_path_enum *, const gs_state *, bool);
#define gs_path_enum_init(penum, pgs)\
gs_path_enum_copy_init(penum, pgs, true)
int gs_path_enum_next(gs_path_enum *, gs_point[3]);
void gs_path_enum_cleanup(gs_path_enum *);
int gs_clippath(gs_state *),
gs_initclip(gs_state *),
gs_clip(gs_state *),
gs_eoclip(gs_state *);
#endif