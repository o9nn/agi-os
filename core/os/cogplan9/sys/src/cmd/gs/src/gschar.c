#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsmatrix.h"
#include "gscoord.h"
#include "gzstate.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxchar.h"
#include "gxfont.h"
private int show_n_begin(gs_show_enum *penum, gs_state *pgs, int code,
gs_text_enum_t *pte);
extern_st(st_gs_show_enum);
void
gs_show_enum_release(gs_show_enum * penum, gs_memory_t * emem)
{
if (penum->text.operation)
penum->procs->release((gs_text_enum_t *)penum, "gs_show_enum_release");
if (emem != 0)
gs_free_object(emem, penum, "gs_show_enum_release");
}
int
gs_show_n_init(gs_show_enum * penum, gs_state * pgs,
const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_show_begin(pgs, (const byte *)str, size, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_ashow_n_init(gs_show_enum * penum, gs_state * pgs,
floatp ax, floatp ay, const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_ashow_begin(pgs, ax, ay, (const byte *)str, size,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_widthshow_n_init(gs_show_enum * penum, gs_state * pgs,
floatp cx, floatp cy, gs_char chr,
const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_widthshow_begin(pgs, cx, cy, chr, (const byte *)str, size,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_awidthshow_n_init(gs_show_enum * penum, gs_state * pgs,
floatp cx, floatp cy, gs_char chr, floatp ax, floatp ay,
const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_awidthshow_begin(pgs, cx, cy, chr, ax, ay,
(const byte *)str, size, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_kshow_n_init(gs_show_enum * penum,
gs_state * pgs, const char *str, uint size)
{
gs_text_enum_t *pte;
int code;
switch (pgs->font->FontType) {
case ft_composite:
case ft_CID_encrypted:
case ft_CID_user_defined:
case ft_CID_TrueType:
case ft_CID_bitmap:
return_error(gs_error_invalidfont);
default:
break;
}
code = gs_kshow_begin(pgs, (const byte *)str, size, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_xyshow_n_init(gs_show_enum * penum,
gs_state * pgs, const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_xyshow_begin(pgs, (const byte *)str, size, NULL, NULL, 0,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_glyphshow_init(gs_show_enum * penum, gs_state * pgs, gs_glyph glyph)
{
gs_text_enum_t *pte;
int code = gs_glyphshow_begin(pgs, glyph, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_glyphpath_init(gs_show_enum * penum, gs_state * pgs, gs_glyph glyph,
bool stroke_path)
{
gs_text_enum_t *pte;
int code = gs_glyphpath_begin(pgs, glyph, stroke_path, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_glyphwidth_init(gs_show_enum * penum, gs_state * pgs, gs_glyph glyph)
{
gs_text_enum_t *pte;
int code = gs_glyphwidth_begin(pgs, glyph, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_cshow_n_init(gs_show_enum * penum,
gs_state * pgs, const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_cshow_begin(pgs, (const byte *)str, size, pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_stringwidth_n_init(gs_show_enum * penum, gs_state * pgs,
const char *str, uint size)
{
gs_text_enum_t *pte;
int code = gs_stringwidth_begin(pgs, (const byte *)str, size,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_charpath_n_init(gs_show_enum * penum, gs_state * pgs,
const char *str, uint size, bool stroke_path)
{
gs_text_enum_t *pte;
int code = gs_charpath_begin(pgs, (const byte *)str, size, stroke_path,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_charboxpath_n_init(gs_show_enum * penum, gs_state * pgs,
const char *str, uint size, bool use_boxes)
{
gs_text_enum_t *pte;
int code = gs_charboxpath_begin(pgs, (const byte *)str, size, use_boxes,
pgs->memory, &pte);
return show_n_begin(penum, pgs, code, pte);
}
int
gs_setcachedevice_double(gs_show_enum *penum, gs_state *pgs, const double *pw)
{
if (penum->pgs != pgs)
return_error(gs_error_rangecheck);
return gs_text_setcachedevice((gs_text_enum_t *)penum, pw);
}
int
gs_setcachedevice_float(gs_show_enum * penum, gs_state * pgs, const float *pw)
{
double w[6];
int i;
for (i = 0; i < 6; ++i)
w[i] = pw[i];
return gs_setcachedevice_double(penum, pgs, w);
}
int
gs_setcachedevice2_double(gs_show_enum * penum, gs_state * pgs,
const double *pw2)
{
if (penum->pgs != pgs)
return_error(gs_error_rangecheck);
return gs_text_setcachedevice2((gs_text_enum_t *)penum, pw2);
}
int
gs_setcachedevice2_float(gs_show_enum * penum, gs_state * pgs, const float *pw2)
{
double w2[10];
int i;
for (i = 0; i < 10; ++i)
w2[i] = pw2[i];
return gs_setcachedevice2_double(penum, pgs, w2);
}
int
gs_setcharwidth(gs_show_enum * penum, gs_state * pgs,
floatp wx, floatp wy)
{
double w[2];
if (penum->pgs != pgs)
return_error(gs_error_rangecheck);
w[0] = wx, w[1] = wy;
return gs_text_setcharwidth((gs_text_enum_t *)penum, w);
}
int
gs_show_next(gs_show_enum * penum)
{
return gs_text_process((gs_text_enum_t *)penum);
}
bool
gs_show_width_only(const gs_show_enum * penum)
{
return gs_text_is_width_only((const gs_text_enum_t *)penum);
}
gs_char
gs_show_current_char(const gs_show_enum * penum)
{
return gs_text_current_char((const gs_text_enum_t *)penum);
}
gs_glyph
gs_show_current_glyph(const gs_show_enum * penum)
{
return gs_text_current_glyph((const gs_text_enum_t *)penum);
}
int
gs_show_current_width(const gs_show_enum * penum, gs_point * ppt)
{
return gs_text_current_width((const gs_text_enum_t *)penum, ppt);
}
gs_char
gs_kshow_previous_char(const gs_show_enum * penum)
{
return gs_text_current_char((const gs_text_enum_t *)penum);
}
gs_char
gs_kshow_next_char(const gs_show_enum * penum)
{
return penum->text.data.bytes[penum->index];
}
void
gs_show_width(const gs_show_enum * penum, gs_point * ppt)
{
gs_text_total_width((const gs_text_enum_t *)penum, ppt);
}
private int
show_n_begin(gs_show_enum *penum, gs_state *pgs, int code, gs_text_enum_t *pte)
{
if (code < 0)
return code;
if (gs_object_type(pgs->memory, pte) != &st_gs_show_enum) {
gx_device *dev = pgs->device;
gs_text_params_t text;
gs_memory_t *mem = pte->memory;
dev_proc_text_begin((*text_begin)) = dev_proc(dev, text_begin);
text = pte->text;
gs_text_release(pte, "show_n_begin");
set_dev_proc(dev, text_begin, gx_default_text_begin);
code = gs_text_begin(pgs, &text, mem, &pte);
set_dev_proc(dev, text_begin, text_begin);
if (code < 0)
return code;
}
*penum = *(gs_show_enum *)pte;
gs_free_object(pgs->memory, pte, "show_n_begin");
return code;
}