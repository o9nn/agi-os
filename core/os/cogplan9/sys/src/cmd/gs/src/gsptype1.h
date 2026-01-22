#ifndef gsptype1_INCLUDED
#  define gsptype1_INCLUDED
#include "gspcolor.h"
#include "gxbitmap.h"
typedef struct gs_pattern1_template_s {
gs_pattern_template_common;
int PaintType;
int TilingType;
gs_rect BBox;
float XStep;
float YStep;
int (*PaintProc) (const gs_client_color *, gs_state *);
} gs_pattern1_template_t;
#define private_st_pattern1_template() \
gs_private_st_suffix_add0(st_pattern1_template,\
gs_pattern1_template_t, "gs_pattern1_template_t",\
pattern1_template_enum_ptrs, pattern1_template_reloc_ptrs,\
st_pattern_template)
#define st_pattern1_template_max_ptrs st_pattern_template_max_ptrs
typedef gs_pattern1_template_t gs_client_pattern;
extern int gs_cspace_build_Pattern1(
gs_color_space ** ppcspace,
const gs_color_space * pbase_cspace,
gs_memory_t * pmem
);
void gs_pattern1_init(gs_pattern1_template_t *);
#define gs_client_pattern_init(ppat) gs_pattern1_init(ppat)
int gs_makepattern(gs_client_color *, const gs_client_pattern *,
const gs_matrix *, gs_state *, gs_memory_t *);
const gs_client_pattern *gs_getpattern(const gs_client_color *);
extern int gs_makepixmappattern(
gs_client_color * pcc,
const gs_depth_bitmap * pbitmap,
bool mask,
const gs_matrix * pmat,
long id,
const gs_color_space * pcspace,
uint white_index,
gs_state * pgs,
gs_memory_t * mem
);
extern int gs_makebitmappattern_xform(
gs_client_color * pcc,
const gx_tile_bitmap * ptile,
bool mask,
const gs_matrix * pmat,
long id,
gs_state * pgs,
gs_memory_t * mem
);
#define gs_makebitmappattern(pcc, tile, mask, pgs, mem)                 \
gs_makebitmappattern_xform(pcc, tile, mask, 0, no_UniqueID, pgs, mem)
#endif