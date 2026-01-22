#include "ghost.h"
#include "oper.h"
#include "gsccode.h"
#include "gsmatrix.h"
#include "gsutil.h"
#include "gxfont.h"
#include "bfont.h"
#include "store.h"
private gs_glyph
zfont_no_encode_char(gs_font *pfont, gs_char chr, gs_glyph_space_t ignored)
{
return gs_no_glyph;
}
private int
zbuildfont32(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
build_proc_refs build;
gs_font_base *pfont;
check_type(*op, t_dictionary);
code = build_proc_name_refs(imemory, &build, NULL, "%Type32BuildGlyph");
if (code < 0)
return code;
code = build_gs_simple_font(i_ctx_p, op, &pfont, ft_CID_bitmap,
&st_gs_font_base, &build,
bf_Encoding_optional);
if (code < 0)
return code;
pfont->BitmapWidths = true;
pfont->ExactSize = fbit_transform_bitmaps;
pfont->InBetweenSize = fbit_transform_bitmaps;
pfont->TransformedChar = fbit_transform_bitmaps;
pfont->procs.encode_char = zfont_no_encode_char;
return define_gs_font((gs_font *) pfont);
}
const op_def zfont32_op_defs[] =
{
{"2.buildfont32", zbuildfont32},
op_def_end(0)
};