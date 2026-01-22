#ifndef gxxfont_INCLUDED
# define gxxfont_INCLUDED
#include "gsccode.h"
#include "gsmatrix.h"
#include "gsuid.h"
#include "gsxfont.h"
typedef struct gx_xfont_common_s {
const gx_xfont_procs *procs;
} gx_xfont_common;
struct gx_xfont_s {
gx_xfont_common common;
};
struct gx_xfont_procs_s {
#define xfont_proc_lookup_font(proc)\
gx_xfont *proc(gx_device *dev, const byte *fname, uint len,\
int encoding_index, const gs_uid *puid, const gs_matrix *pmat,\
gs_memory_t *mem)
xfont_proc_lookup_font((*lookup_font));
#define xfont_proc_char_xglyph(proc)\
gx_xglyph proc(gx_xfont *xf, gs_char chr, int encoding_index,\
gs_glyph glyph, const gs_const_string *glyph_name)
xfont_proc_char_xglyph((*char_xglyph));
#define xfont_proc_char_metrics(proc)\
int proc(gx_xfont *xf, gx_xglyph xg, int wmode,\
gs_point *pwidth, gs_int_rect *pbbox)
xfont_proc_char_metrics((*char_metrics));
#define xfont_proc_render_char(proc)\
int proc(gx_xfont *xf, gx_xglyph xg, gx_device *target,\
int x, int y, gx_color_index color, int required)
xfont_proc_render_char((*render_char));
#define xfont_proc_release(proc)\
int proc(gx_xfont *xf, gs_memory_t *mem)
xfont_proc_release((*release));
};
#define gs__st_dev_ptrs1(scope_st, stname, stype, sname, penum, preloc, de)\
private ENUM_PTRS_WITH(penum, stype *xfptr) return 0;\
case 0: ENUM_RETURN(gx_device_enum_ptr((gx_device *)(xfptr->de)));\
ENUM_PTRS_END\
private RELOC_PTRS_WITH(preloc, stype *xfptr) ;\
xfptr->de = (void *)gx_device_reloc_ptr((gx_device *)(xfptr->de), gcst);\
RELOC_PTRS_END\
gs__st_composite_only(scope_st, stname, stype, sname, penum, preloc)
#define gs_private_st_dev_ptrs1(stname, stype, sname, penum, preloc, de)\
gs__st_dev_ptrs1(private_st, stname, stype, sname, penum, preloc, de)
#endif