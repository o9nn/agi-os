#ifndef gzcpath_INCLUDED
# define gzcpath_INCLUDED
#include "gxcpath.h"
typedef struct gx_clip_rect_list_s {
rc_header rc;
gx_clip_list list;
} gx_clip_rect_list;
#define private_st_clip_rect_list() \
gs_private_st_ptrs_add0(st_clip_rect_list, gx_clip_rect_list,\
"gx_clip_rect_list", clip_rect_list_enum_ptrs, clip_rect_list_reloc_ptrs,\
st_clip_list, list)
typedef struct gx_cpath_path_list_s gx_cpath_path_list;
struct gx_cpath_path_list_s {
gx_path path;
rc_header rc;
int rule;
gx_cpath_path_list *next;
};
#define private_st_cpath_path_list() \
gs_private_st_suffix_add1(st_cpath_path_list, gx_cpath_path_list,\
"gs_cpath_list", cpath_path_list_enum_ptrs, cpath_path_list_reloc_ptrs,\
st_path, next)
struct gx_clip_path_s {
gx_path path;
gx_clip_rect_list local_list;
int rule;
gs_fixed_rect inner_box;
gs_fixed_rect outer_box;
gx_clip_rect_list *rect_list;
bool path_valid;
gx_cpath_path_list *path_list;
gs_id id;
};
extern_st(st_clip_path);
#define public_st_clip_path() \
gs_public_st_composite(st_clip_path, gx_clip_path, "clip_path",\
clip_path_enum_ptrs, clip_path_reloc_ptrs)
#define st_clip_path_max_ptrs (st_path_max_ptrs + 1)
#define gx_cpath_is_shared(pcpath)\
((pcpath)->rect_list->rc.ref_count > 1)
typedef enum {
visit_left = 1,
visit_right = 2
} cpe_visit_t;
typedef enum {
cpe_scan, cpe_left, cpe_right, cpe_close, cpe_done
} cpe_state_t;
struct gs_cpath_enum_s {
gs_path_enum path_enum;
bool using_path;
gx_clip_rect *visit;
gx_clip_rect *rp;
cpe_visit_t first_visit;
cpe_state_t state;
bool have_line;
gs_int_point line_end;
bool any_rectangles;
};
#define private_st_cpath_enum() \
gs_private_st_suffix_add2(st_cpath_enum, gs_cpath_enum, "gs_cpath_enum",\
cpath_enum_enum_ptrs, cpath_enum_reloc_ptrs, st_path_enum,\
visit, rp)
#endif