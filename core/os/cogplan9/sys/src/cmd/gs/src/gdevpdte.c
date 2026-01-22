#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gxfcmap.h"
#include "gxfcopy.h"
#include "gxfont.h"
#include "gxfont0.h"
#include "gxfont0c.h"
#include "gxpath.h"
#include "gdevpsf.h"
#include "gdevpdfx.h"
#include "gdevpdfg.h"
#include "gdevpdtx.h"
#include "gdevpdtd.h"
#include "gdevpdtf.h"
#include "gdevpdts.h"
#include "gdevpdtt.h"
private int pdf_char_widths(gx_device_pdf *const pdev,
pdf_font_resource_t *pdfont, int ch,
gs_font_base *font,
pdf_glyph_widths_t *pwidths );
private int pdf_encode_string(gx_device_pdf *pdev, pdf_text_enum_t *penum,
const gs_string *pstr, const gs_glyph *gdata,
pdf_font_resource_t **ppdfont);
private int pdf_process_string(pdf_text_enum_t *penum, gs_string *pstr,
pdf_font_resource_t *pdfont,
const gs_matrix *pfmat,
pdf_text_process_state_t *ppts);
int
pdf_encode_process_string(pdf_text_enum_t *penum, gs_string *pstr,
const gs_glyph *gdata, const gs_matrix *pfmat,
pdf_text_process_state_t *ppts)
{
gx_device_pdf *const pdev = (gx_device_pdf *)penum->dev;
pdf_font_resource_t *pdfont;
gs_font_base *font;
int code = 0;
switch (penum->current_font->FontType) {
case ft_TrueType:
case ft_encrypted:
case ft_encrypted2:
case ft_user_defined:
break;
default:
return_error(gs_error_rangecheck);
}
font = (gs_font_base *)penum->current_font;
code = pdf_encode_string(pdev, penum, pstr, gdata, &pdfont);
if (code < 0)
return code;
return pdf_process_string(penum, pstr, pdfont, pfmat, ppts);
}
int
pdf_add_ToUnicode(gx_device_pdf *pdev, gs_font *font, pdf_font_resource_t *pdfont,
gs_glyph glyph, gs_char ch)
{ int code;
gs_char unicode;
if (glyph == GS_NO_GLYPH)
return 0;
unicode = font->procs.decode_glyph((gs_font *)font, glyph);
if (unicode != GS_NO_CHAR) {
if (pdfont->cmap_ToUnicode == NULL) {
uint num_codes = 256, key_size = 1;
if (font->FontType == ft_CID_encrypted) {
gs_font_cid0 *pfcid = (gs_font_cid0 *)font;
num_codes = pfcid->cidata.common.CIDCount;
key_size = 2;
} else if (font->FontType == ft_CID_TrueType) {
#if 0
gs_font_cid2 *pfcid = (gs_font_cid2 *)font;
num_codes = pfcid->cidata.common.CIDCount;
#else
num_codes = 65536;
#endif
key_size = 2;
}
code = gs_cmap_ToUnicode_alloc(pdev->pdf_memory, pdfont->rid, num_codes, key_size,
&pdfont->cmap_ToUnicode);
if (code < 0)
return code;
}
if (pdfont->cmap_ToUnicode != NULL)
gs_cmap_ToUnicode_add_pair(pdfont->cmap_ToUnicode, ch, unicode);
}
return 0;
}
int
pdf_register_charproc_resource(gx_device_pdf *pdev, gs_id id, pdf_resource_type_t type)
{
if (pdev->font3 != 0) {
pdf_font_resource_t *pdfont = (pdf_font_resource_t *)pdev->font3;
pdf_resource_ref_t *used_resources = pdfont->u.simple.s.type3.used_resources;
int i, used_resources_count = pdfont->u.simple.s.type3.used_resources_count;
int used_resources_max = pdfont->u.simple.s.type3.used_resources_max;
for (i = 0; i < used_resources_count; i++)
if (used_resources[i].id == id && used_resources[i].type == type)
return 0;
if (used_resources_count >= used_resources_max) {
used_resources_max += 10;
used_resources = (pdf_resource_ref_t *)gs_alloc_bytes(pdev->pdf_memory,
sizeof(pdf_resource_ref_t) * used_resources_max,
"pdf_register_charproc_resource");
if (!used_resources)
return_error(gs_error_VMerror);
if (used_resources_count) {
memcpy(used_resources, pdfont->u.simple.s.type3.used_resources,
sizeof(pdf_resource_ref_t) * used_resources_count);
gs_free_object(pdev->pdf_memory, pdfont->u.simple.s.type3.used_resources,
"pdf_register_charproc_resource");
}
pdfont->u.simple.s.type3.used_resources = used_resources;
pdfont->u.simple.s.type3.used_resources_max = used_resources_max;
}
used_resources[used_resources_count].id = id;
used_resources[used_resources_count].type = type;
pdfont->u.simple.s.type3.used_resources_count = used_resources_count + 1;
}
return 0;
}
int
pdf_used_charproc_resources(gx_device_pdf *pdev, pdf_font_resource_t *pdfont)
{
if (pdfont->where_used & pdev->used_mask)
return 0;
pdfont->where_used |= pdev->used_mask;
if (pdfont->FontType == ft_user_defined) {
pdf_resource_ref_t *used_resources = pdfont->u.simple.s.type3.used_resources;
int i, used_resources_count = pdfont->u.simple.s.type3.used_resources_count;
for (i = 0; i < used_resources_count; i++) {
pdf_resource_t *pres =
pdf_find_resource_by_resource_id(pdev,
used_resources[i].type, used_resources[i].id);
if (pres == NULL)
return_error(gs_error_unregistered);
pres->where_used |= pdev->used_mask;
}
}
return 0;
}
private int
pdf_encode_string(gx_device_pdf *pdev, pdf_text_enum_t *penum,
const gs_string *pstr, const gs_glyph *gdata,
pdf_font_resource_t **ppdfont)
{
gs_font *font = (gs_font *)penum->current_font;
pdf_font_resource_t *pdfont = 0;
gs_font_base *cfont, *ccfont;
int code, i;
code = pdf_obtain_font_resource(penum, pstr, &pdfont);
if (code < 0)
return code;
code = pdf_add_resource(pdev, pdev->substream_Resources, "/Font", (pdf_resource_t *)pdfont);
if (code < 0)
return code;
code = pdf_register_charproc_resource(pdev, pdf_resource_id((pdf_resource_t *)pdfont), resourceFont);
if (code < 0)
return code;
cfont = pdf_font_resource_font(pdfont, false);
ccfont = pdf_font_resource_font(pdfont, true);
for (i = 0; i < pstr->size; ++i) {
int ch = pstr->data[i];
pdf_encoding_element_t *pet = &pdfont->u.simple.Encoding[ch];
gs_glyph glyph = (gdata == NULL
? font->procs.encode_char(font, ch, GLYPH_SPACE_NAME)
: gdata[i]);
gs_glyph copied_glyph;
gs_const_string gnstr;
if (glyph == GS_NO_GLYPH || glyph == pet->glyph)
continue;
if (pet->glyph != GS_NO_GLYPH) {
return_error(gs_error_rangecheck);
}
code = font->procs.glyph_name(font, glyph, &gnstr);
if (code < 0)
return code;
if (font->FontType != ft_user_defined) {
code = (pdfont->base_font != 0 ?
pdf_base_font_copy_glyph(pdfont->base_font, glyph, (gs_font_base *)font) :
pdf_font_used_glyph(pdfont->FontDescriptor, glyph, (gs_font_base *)font));
if (code < 0 && code != gs_error_undefined)
return code;
if (code == gs_error_undefined) {
if (bytes_compare(gnstr.data, gnstr.size, (const byte *)".notdef", 7)) {
pet->glyph = glyph;
pet->str = gnstr;
pet->is_difference = true;
}
} else if (pdfont->base_font == NULL && ccfont != NULL &&
(gs_copy_glyph_options(font, glyph, (gs_font *)ccfont, COPY_GLYPH_NO_NEW) != 1 ||
gs_copied_font_add_encoding((gs_font *)ccfont, ch, glyph) < 0)) {
ccfont = NULL;
pdf_font_descriptor_drop_complete_font(pdfont->FontDescriptor);
}
copied_glyph = cfont->procs.encode_char((gs_font *)cfont, ch,
GLYPH_SPACE_NAME);
if (glyph != copied_glyph &&
gs_copied_font_add_encoding((gs_font *)cfont, ch, glyph) < 0
)
pet->is_difference = true;
pdfont->used[ch >> 3] |= 0x80 >> (ch & 7);
}
code = pdf_add_ToUnicode(pdev, font, pdfont, glyph, ch);
if (code < 0)
return code;
pet->glyph = glyph;
pet->str = gnstr;
}
*ppdfont = pdfont;
return 0;
}
private int
process_text_estimate_bbox(pdf_text_enum_t *pte, gs_font_base *font,
const gs_const_string *pstr,
const gs_matrix *pfmat,
gs_rect *text_bbox, gs_point *pdpt)
{
int i;
int space_char =
(pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH ?
pte->text.space.s_char : -1);
int WMode = font->WMode;
int code = 0;
gs_point total = {0, 0};
gs_fixed_point origin;
gs_matrix m;
int xy_index = pte->xy_index;
if (font->FontBBox.p.x == font->FontBBox.q.x ||
font->FontBBox.p.y == font->FontBBox.q.y)
return_error(gs_error_undefined);
code = gx_path_current_point(pte->path, &origin);
if (code < 0)
return code;
m = ctm_only(pte->pis);
m.tx = fixed2float(origin.x);
m.ty = fixed2float(origin.y);
gs_matrix_multiply(pfmat, &m, &m);
for (i = 0; i < pstr->size; ++i) {
byte c = pstr->data[i];
gs_rect bbox;
gs_point wanted, tpt, p0, p1, p2, p3;
gs_glyph glyph = font->procs.encode_char((gs_font *)font, c,
GLYPH_SPACE_NAME);
gs_glyph_info_t info;
int code = font->procs.glyph_info((gs_font *)font, glyph, NULL,
GLYPH_INFO_WIDTH0 << WMode,
&info);
if (code < 0)
return code;
gs_point_transform(font->FontBBox.p.x, font->FontBBox.p.y, &m, &p0);
gs_point_transform(font->FontBBox.p.x, font->FontBBox.q.y, &m, &p1);
gs_point_transform(font->FontBBox.q.x, font->FontBBox.p.y, &m, &p2);
gs_point_transform(font->FontBBox.q.x, font->FontBBox.q.y, &m, &p3);
bbox.p.x = min(min(p0.x, p1.x), min(p1.x, p2.x)) + total.x;
bbox.p.y = min(min(p0.y, p1.y), min(p1.y, p2.y)) + total.y;
bbox.q.x = max(max(p0.x, p1.x), max(p1.x, p2.x)) + total.x;
bbox.q.y = max(max(p0.y, p1.y), max(p1.y, p2.y)) + total.y;
if (i == 0)
*text_bbox = bbox;
else
rect_merge(*text_bbox, bbox);
if (pte->text.operation & TEXT_REPLACE_WIDTHS) {
gs_text_replaced_width(&pte->text, xy_index++, &tpt);
gs_distance_transform(tpt.x, tpt.y, &ctm_only(pte->pis), &wanted);
} else {
gs_distance_transform(info.width[WMode].x,
info.width[WMode].y,
&m, &wanted);
if (pte->text.operation & TEXT_ADD_TO_ALL_WIDTHS) {
gs_distance_transform(pte->text.delta_all.x,
pte->text.delta_all.y,
&ctm_only(pte->pis), &tpt);
wanted.x += tpt.x;
wanted.y += tpt.y;
}
if (pstr->data[i] == space_char && pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH) {
gs_distance_transform(pte->text.delta_space.x,
pte->text.delta_space.y,
&ctm_only(pte->pis), &tpt);
wanted.x += tpt.x;
wanted.y += tpt.y;
}
}
total.x += wanted.x;
total.y += wanted.y;
}
*pdpt = total;
return 0;
}
private void
adjust_first_last_char(pdf_font_resource_t *pdfont, byte *str, int size)
{
int i;
for (i = 0; i < size; ++i) {
int chr = str[i];
if (chr < pdfont->u.simple.FirstChar)
pdfont->u.simple.FirstChar = chr;
if (chr > pdfont->u.simple.LastChar)
pdfont->u.simple.LastChar = chr;
}
}
int
pdf_shift_text_currentpoint(pdf_text_enum_t *penum, gs_point *wpt)
{
gs_state *pgs;
extern_st(st_gs_state);
if (gs_object_type(penum->dev->memory, penum->pis) != &st_gs_state) {
return_error(gs_error_unregistered);
}
pgs = (gs_state *)penum->pis;
return gs_moveto_aux(penum->pis, gx_current_path(pgs),
fixed2float(penum->origin.x) + wpt->x,
fixed2float(penum->origin.y) + wpt->y);
}
private int process_text_return_width(const pdf_text_enum_t *pte,
gs_font_base *font,
pdf_text_process_state_t *ppts,
const gs_const_string *pstr,
gs_point *pdpt, int *accepted);
private int
pdf_process_string(pdf_text_enum_t *penum, gs_string *pstr,
pdf_font_resource_t *pdfont, const gs_matrix *pfmat,
pdf_text_process_state_t *ppts)
{
gx_device_pdf *const pdev = (gx_device_pdf *)penum->dev;
gs_font_base *font = (gs_font_base *)penum->current_font;
const gs_text_params_t *text = &penum->text;
int code = 0, mask;
gs_point width_pt;
gs_rect text_bbox;
int accepted;
if (pfmat == 0)
pfmat = &font->FontMatrix;
if (text->operation & TEXT_RETURN_WIDTH) {
code = gx_path_current_point(penum->path, &penum->origin);
if (code < 0)
return code;
}
if (text->size == 0)
return 0;
if (penum->pis->text_rendering_mode != 3 && !(text->operation & TEXT_DO_NONE)) {
code = process_text_estimate_bbox(penum, font, (gs_const_string *)pstr, pfmat,
&text_bbox, &width_pt);
if (code == 0) {
gs_fixed_rect clip_bbox;
gs_rect rect;
gx_cpath_outer_box(penum->pcpath, &clip_bbox);
rect.p.x = fixed2float(clip_bbox.p.x);
rect.p.y = fixed2float(clip_bbox.p.y);
rect.q.x = fixed2float(clip_bbox.q.x);
rect.q.y = fixed2float(clip_bbox.q.y);
rect_intersect(rect, text_bbox);
if (rect.p.x > rect.q.x || rect.p.y > rect.q.y) {
penum->index += pstr->size;
goto finish;
}
}
} else {
}
code = pdf_update_text_state(ppts, penum, pdfont, pfmat);
if (code > 0) {
if (code & TEXT_ADD_TO_SPACE_WIDTH) {
if (!memchr(pstr->data, penum->text.space.s_char, pstr->size))
code &= ~TEXT_ADD_TO_SPACE_WIDTH;
}
}
if (code < 0)
return code;
mask = code;
if (text->operation & TEXT_REPLACE_WIDTHS)
mask |= TEXT_REPLACE_WIDTHS;
if (mask == 0) {
if (!(text->operation & (TEXT_DO_DRAW | TEXT_RETURN_WIDTH)))
return 0;
code = process_text_return_width(penum, font, ppts,
(gs_const_string *)pstr,
&width_pt, &accepted);
if (code < 0)
return code;
if (code == 0) {
if (text->operation & TEXT_DO_DRAW || penum->pis->text_rendering_mode == 3) {
code = pdf_append_chars(pdev, pstr->data, accepted,
width_pt.x, width_pt.y, false);
if (code < 0)
return code;
adjust_first_last_char(pdfont, pstr->data, accepted);
penum->index += accepted;
} else if (text->operation & TEXT_DO_NONE)
penum->index += accepted;
} else {
mask = TEXT_RETURN_WIDTH;
}
}
if (mask) {
int index0 = penum->index, xy_index = penum->xy_index;
gs_text_params_t text = penum->text;
int xy_index_step = (penum->text.x_widths != NULL &&
penum->text.x_widths == penum->text.y_widths ? 2 : 1);
if (penum->text.x_widths != NULL) {
penum->text.x_widths += xy_index * xy_index_step;
}
if (penum->text.y_widths != NULL)
penum->text.y_widths += xy_index * xy_index_step;
penum->xy_index = 0;
code = process_text_modify_width(penum, (gs_font *)font, ppts,
(gs_const_string *)pstr,
&width_pt);
if (penum->text.x_widths != NULL)
penum->text.x_widths -= xy_index * xy_index_step;
if (penum->text.y_widths != NULL)
penum->text.y_widths -= xy_index * xy_index_step;
penum->xy_index += xy_index;
adjust_first_last_char(pdfont, pstr->data, penum->index);
penum->text = text;
penum->index += index0;
if (code < 0)
return code;
}
finish:
if (!(text->operation & TEXT_RETURN_WIDTH))
return 0;
if (text->operation & TEXT_DO_NONE) {
gs_point p;
gs_distance_transform_inverse(width_pt.x, width_pt.y, &ctm_only(penum->pis), &p);
penum->returned.total_width.x += p.x;
penum->returned.total_width.y += p.y;
} else
penum->returned.total_width = width_pt;
return pdf_shift_text_currentpoint(penum, &width_pt);
}
private int
pdf_char_widths(gx_device_pdf *const pdev,
pdf_font_resource_t *pdfont, int ch, gs_font_base *font,
pdf_glyph_widths_t *pwidths )
{
pdf_glyph_widths_t widths;
int code;
byte *glyph_usage;
double *real_widths;
int char_cache_size, width_cache_size;
pdf_font_resource_t *pdfont1;
code = pdf_attached_font_resource(pdev, (gs_font *)font, &pdfont1,
&glyph_usage, &real_widths, &char_cache_size, &width_cache_size);
if (code < 0)
return code;
if (pdfont1 != pdfont)
return_error(gs_error_unregistered);
if (ch < 0 || ch > 255)
return_error(gs_error_rangecheck);
if (ch >= width_cache_size)
return_error(gs_error_unregistered);
if (pwidths == 0)
pwidths = &widths;
if (font->FontType != ft_user_defined && real_widths[ch] == 0) {
gs_glyph glyph = pdfont->u.simple.Encoding[ch].glyph;
code = pdf_glyph_widths(pdfont, font->WMode, glyph, (gs_font *)font, pwidths, NULL);
if (code < 0)
return code;
if (font->WMode != 0 && code > 0 && !pwidths->replaced_v) {
code = pdf_glyph_widths(pdfont, 0, glyph, (gs_font *)font, pwidths, NULL);
}
if (pwidths->replaced_v) {
pdfont->u.simple.v[ch].x = pwidths->real_width.v.x - pwidths->Width.v.x;
pdfont->u.simple.v[ch].y = pwidths->real_width.v.y - pwidths->Width.v.y;
} else
pdfont->u.simple.v[ch].x = pdfont->u.simple.v[ch].y = 0;
if (code == 0) {
pdfont->Widths[ch] = pwidths->Width.w;
real_widths[ch] = pwidths->real_width.w;
}
} else {
if (font->FontType == ft_user_defined) {
if (!(pdfont->used[ch >> 3] & 0x80 >> (ch & 7)))
return gs_error_undefined;
if (!pdev->charproc_just_accumulated &&
!(pdfont->u.simple.s.type3.cached[ch >> 3] & 0x80 >> (ch & 7))) {
return gs_error_undefined;
}
}
pwidths->Width.w = pdfont->Widths[ch];
pwidths->Width.v = pdfont->u.simple.v[ch];
if (font->FontType == ft_user_defined) {
pwidths->real_width.w = real_widths[ch * 2];
pwidths->Width.xy.x = pwidths->Width.w;
pwidths->Width.xy.y = 0;
pwidths->real_width.xy.x = real_widths[ch * 2 + 0];
pwidths->real_width.xy.y = real_widths[ch * 2 + 1];
} else if (font->WMode) {
pwidths->real_width.w = real_widths[ch];
pwidths->Width.xy.x = 0;
pwidths->Width.xy.y = pwidths->Width.w;
pwidths->real_width.xy.x = 0;
pwidths->real_width.xy.y = pwidths->real_width.w;
} else {
pwidths->real_width.w = real_widths[ch];
pwidths->Width.xy.x = pwidths->Width.w;
pwidths->Width.xy.y = 0;
pwidths->real_width.xy.x = pwidths->real_width.w;
pwidths->real_width.xy.y = 0;
}
code = 0;
}
return code;
}
private int
process_text_return_width(const pdf_text_enum_t *pte, gs_font_base *font,
pdf_text_process_state_t *ppts,
const gs_const_string *pstr,
gs_point *pdpt, int *accepted)
{
int i;
gs_point w;
double scale;
gs_point dpt;
int num_spaces = 0;
int space_char =
(pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH ?
pte->text.space.s_char : -1);
int widths_differ = 0, code;
gx_device_pdf *pdev = (gx_device_pdf *)pte->dev;
pdf_font_resource_t *pdfont;
code = pdf_attached_font_resource(pdev, (gs_font *)font, &pdfont, NULL, NULL, NULL, NULL);
if (code < 0)
return code;
if (font->FontType == ft_user_defined) {
pdf_font3_scale(pdev, (gs_font *)font, &scale);
scale *= ppts->values.size;
} else
scale = 0.001 * ppts->values.size;
for (i = 0, w.x = w.y = 0; i < pstr->size; ++i) {
pdf_glyph_widths_t cw;
gs_char ch = pstr->data[i];
if (font->FontType == ft_user_defined &&
(i > 0 || !pdev->charproc_just_accumulated) &&
!(pdfont->u.simple.s.type3.cached[ch >> 3] & (0x80 >> (ch & 7))))
code = gs_error_undefined;
else
code = pdf_char_widths((gx_device_pdf *)pte->dev,
ppts->values.pdfont, ch, font,
&cw);
if (code < 0) {
if (i)
break;
*accepted = 0;
return code;
}
w.x += cw.real_width.xy.x;
w.y += cw.real_width.xy.y;
if (cw.real_width.xy.x != cw.Width.xy.x ||
cw.real_width.xy.y != cw.Width.xy.y
)
widths_differ = 1;
if (pstr->data[i] == space_char)
++num_spaces;
}
*accepted = i;
gs_distance_transform(w.x * scale, w.y * scale,
&ppts->values.matrix, &dpt);
if (pte->text.operation & TEXT_ADD_TO_ALL_WIDTHS) {
int num_chars = *accepted;
gs_point tpt;
gs_distance_transform(pte->text.delta_all.x, pte->text.delta_all.y,
&ctm_only(pte->pis), &tpt);
dpt.x += tpt.x * num_chars;
dpt.y += tpt.y * num_chars;
}
if (pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH) {
gs_point tpt;
gs_distance_transform(pte->text.delta_space.x, pte->text.delta_space.y,
&ctm_only(pte->pis), &tpt);
dpt.x += tpt.x * num_spaces;
dpt.y += tpt.y * num_spaces;
}
*pdpt = dpt;
return widths_differ;
}
#define RIGHT_SBW 1
#if !RIGHT_SBW
private void
pdf_glyph_origin(pdf_font_resource_t *pdfont, int ch, int WMode, gs_point *p)
{
switch (pdfont->FontType) {
case ft_encrypted:
case ft_encrypted2:
case ft_TrueType:
case ft_user_defined:
*p = pdfont->u.simple.v[ch];
break;
default:
p->x = p->y = 0;
break;
}
}
#endif
int
process_text_modify_width(pdf_text_enum_t *pte, gs_font *font,
pdf_text_process_state_t *ppts,
const gs_const_string *pstr,
gs_point *pdpt)
{
gx_device_pdf *const pdev = (gx_device_pdf *)pte->dev;
double scale;
int space_char =
(pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH ?
pte->text.space.s_char : -1);
int code = 0;
gs_point start, total;
bool composite = (ppts->values.pdfont->FontType == ft_composite);
if (font->FontType == ft_user_defined) {
gx_device_pdf *pdev = (gx_device_pdf *)pte->dev;
pdf_font3_scale(pdev, font, &scale);
scale *= ppts->values.size;
} else
scale = 0.001 * ppts->values.size;
pte->text.data.bytes = pstr->data;
pte->text.size = pstr->size;
pte->index = 0;
pte->text.operation &= ~TEXT_FROM_ANY;
pte->text.operation |= TEXT_FROM_STRING;
start.x = ppts->values.matrix.tx;
start.y = ppts->values.matrix.ty;
total.x = total.y = 0;
for (;;) {
pdf_glyph_widths_t cw;
gs_point did, wanted, tpt;
gs_point v = {0, 0};
gs_char chr;
gs_glyph glyph;
int code, index = pte->index;
gs_text_enum_t pte1 = *(gs_text_enum_t *)pte;
int FontType;
#if RIGHT_SBW
bool use_cached_v = true;
#endif
code = pte1.orig_font->procs.next_char_glyph(&pte1, &chr, &glyph);
if (code == 2) {
gs_text_enum_copy_dynamic((gs_text_enum_t *)pte, &pte1, true);
break;
}
if (code < 0)
return code;
if (composite) {
gs_font *subfont = pte1.fstack.items[pte1.fstack.depth].font;
pdf_font_resource_t *pdsubf = ppts->values.pdfont->u.type0.DescendantFont;
FontType = pdsubf->FontType;
code = pdf_glyph_widths(pdsubf, font->WMode, glyph, subfont, &cw,
pte->cdevproc_callout ? pte->cdevproc_result : NULL);
} else {
FontType = font->FontType;
if (chr == GS_NO_CHAR && glyph != GS_NO_GLYPH) {
code = pdf_glyph_widths(ppts->values.pdfont, font->WMode, glyph, font, &cw, NULL);
use_cached_v = false;
} else
code = pdf_char_widths((gx_device_pdf *)pte->dev,
ppts->values.pdfont, chr, (gs_font_base *)font,
&cw);
}
if (code < 0) {
if (index > 0)
break;
return code;
}
gs_text_enum_copy_dynamic((gs_text_enum_t *)pte, &pte1, true);
#if RIGHT_SBW
if (composite || !use_cached_v) {
if (cw.replaced_v) {
v.x = cw.real_width.v.x - cw.Width.v.x;
v.y = cw.real_width.v.y - cw.Width.v.y;
}
} else
v = ppts->values.pdfont->u.simple.v[chr];
if (font->WMode) {
v.x = - v.x;
v.y = - v.y;
} else {
}
#else
if ((pte->text.operation & TEXT_FROM_SINGLE_GLYPH) ||
(pte->text.operation & TEXT_FROM_GLYPHS)) {
v.x = v.y = 0;
} else if (composite) {
if (cw.replaced_v) {
v.x = cw.real_width.v.x - cw.Width.v.x;
v.y = cw.real_width.v.y - cw.Width.v.y;
}
} else
pdf_glyph_origin(ppts->values.pdfont, chr, font->WMode, &v);
#endif
if (v.x != 0 || v.y != 0) {
gs_point glyph_origin_shift;
double scale0;
if (FontType == ft_TrueType || FontType == ft_CID_TrueType)
scale0 = (float)0.001;
else
scale0 = 1;
#if RIGHT_SBW
glyph_origin_shift.x = v.x * scale0;
glyph_origin_shift.y = v.y * scale0;
#else
glyph_origin_shift.x = - v.x * scale0;
glyph_origin_shift.y = - v.y * scale0;
#endif
if (composite) {
gs_font *subfont = pte->fstack.items[pte->fstack.depth].font;
gs_distance_transform(glyph_origin_shift.x, glyph_origin_shift.y,
&subfont->FontMatrix, &glyph_origin_shift);
}
gs_distance_transform(glyph_origin_shift.x, glyph_origin_shift.y,
&font->FontMatrix, &glyph_origin_shift);
gs_distance_transform(glyph_origin_shift.x, glyph_origin_shift.y,
&ctm_only(pte->pis), &glyph_origin_shift);
if (glyph_origin_shift.x != 0 || glyph_origin_shift.y != 0) {
ppts->values.matrix.tx = start.x + total.x + glyph_origin_shift.x;
ppts->values.matrix.ty = start.y + total.y + glyph_origin_shift.y;
code = pdf_set_text_state_values(pdev, &ppts->values);
if (code < 0)
break;
}
}
if (pte->text.operation & TEXT_DO_DRAW) {
gs_distance_transform(cw.Width.xy.x * scale,
cw.Width.xy.y * scale,
&ppts->values.matrix, &did);
gs_distance_transform((font->WMode ? 0 : ppts->values.character_spacing),
(font->WMode ? ppts->values.character_spacing : 0),
&ppts->values.matrix, &tpt);
did.x += tpt.x;
did.y += tpt.y;
if (chr == space_char) {
gs_distance_transform((font->WMode ? 0 : ppts->values.word_spacing),
(font->WMode ? ppts->values.word_spacing : 0),
&ppts->values.matrix, &tpt);
did.x += tpt.x;
did.y += tpt.y;
}
code = pdf_append_chars(pdev, pstr->data + index, pte->index - index, did.x, did.y, composite);
if (code < 0)
break;
} else
did.x = did.y = 0;
if (pte->text.operation & TEXT_REPLACE_WIDTHS) {
gs_point dpt;
code = gs_text_replaced_width(&pte->text, pte->xy_index++, &dpt);
if (code < 0)
return_error(gs_error_unregistered);
gs_distance_transform(dpt.x, dpt.y, &ctm_only(pte->pis), &wanted);
} else {
gs_distance_transform(cw.real_width.xy.x * scale,
cw.real_width.xy.y * scale,
&ppts->values.matrix, &wanted);
if (pte->text.operation & TEXT_ADD_TO_ALL_WIDTHS) {
gs_distance_transform(pte->text.delta_all.x,
pte->text.delta_all.y,
&ctm_only(pte->pis), &tpt);
wanted.x += tpt.x;
wanted.y += tpt.y;
}
if (chr == space_char && pte->text.operation & TEXT_ADD_TO_SPACE_WIDTH) {
gs_distance_transform(pte->text.delta_space.x,
pte->text.delta_space.y,
&ctm_only(pte->pis), &tpt);
wanted.x += tpt.x;
wanted.y += tpt.y;
}
}
total.x += wanted.x;
total.y += wanted.y;
if (wanted.x != did.x || wanted.y != did.y) {
ppts->values.matrix.tx = start.x + total.x;
ppts->values.matrix.ty = start.y + total.y;
code = pdf_set_text_state_values(pdev, &ppts->values);
if (code < 0)
break;
}
pdev->charproc_just_accumulated = false;
}
*pdpt = total;
return code;
}
int
pdf_encode_glyph(gs_font_base *bfont, gs_glyph glyph0,
byte *buf, int buf_size, int *char_code_length)
{
gs_char c;
*char_code_length = 1;
if (*char_code_length > buf_size)
return_error(gs_error_rangecheck);
for (c = 0; c < 255; c++) {
gs_glyph glyph1 = bfont->procs.encode_char((gs_font *)bfont, c,
GLYPH_SPACE_NAME);
if (glyph1 == glyph0) {
buf[0] = (byte)c;
return 0;
}
}
return_error(gs_error_rangecheck);
}
int
process_plain_text(gs_text_enum_t *pte, void *vbuf, uint bsize)
{
byte *const buf = vbuf;
uint count;
uint operation = pte->text.operation;
pdf_text_enum_t *penum = (pdf_text_enum_t *)pte;
int code;
gs_string str;
pdf_text_process_state_t text_state;
const gs_glyph *gdata = NULL;
if (operation & (TEXT_FROM_STRING | TEXT_FROM_BYTES)) {
count = pte->text.size - pte->index;
if (bsize < count)
return_error(gs_error_unregistered);
memcpy(buf, (const byte *)pte->text.data.bytes + pte->index, count);
} else if (operation & (TEXT_FROM_CHARS | TEXT_FROM_SINGLE_CHAR)) {
const gs_char *cdata;
int i;
if (operation & TEXT_FROM_CHARS) {
cdata = pte->text.data.chars;
count = (pte->text.size - pte->index);
} else {
cdata = &pte->text.data.d_char;
count = 1;
}
if (bsize < count * sizeof(gs_char))
return_error(gs_error_unregistered);
for (i = 0; i < count; ++i) {
gs_char chr = cdata[pte->index + i];
if (chr & ~0xff)
return_error(gs_error_rangecheck);
buf[i] = (byte)chr;
}
} else if (operation & (TEXT_FROM_GLYPHS | TEXT_FROM_SINGLE_GLYPH)) {
gs_font *font = pte->current_font;
uint size;
int i;
if (operation & TEXT_FROM_GLYPHS) {
gdata = pte->text.data.glyphs;
size = pte->text.size - pte->index;
} else {
gdata = &pte->text.data.d_glyph;
size = 1;
}
if (!pdf_is_simple_font(font))
return_error(gs_error_unregistered);
count = 0;
for (i = 0; i < size; ++i) {
gs_glyph glyph = gdata[pte->index + i];
int char_code_length;
code = pdf_encode_glyph((gs_font_base *)font, glyph,
buf + count, size - count, &char_code_length);
if (code < 0)
break;
count += char_code_length;
if (operation & TEXT_INTERVENE)
break;
}
if (i < size) {
pdf_font_resource_t *pdfont;
str.data = buf;
str.size = size;
if (pdf_obtain_font_resource_unencoded(penum, &str, &pdfont, gdata) != 0) {
return code;
}
count = size;
}
} else
return_error(gs_error_rangecheck);
str.data = buf;
if (count > 1 && (operation & TEXT_INTERVENE)) {
str.size = 1;
code = pdf_encode_process_string(penum, &str, gdata, NULL, &text_state);
if (code >= 0) {
pte->returned.current_char = buf[0];
code = TEXT_PROCESS_INTERVENE;
}
} else {
str.size = count;
code = pdf_encode_process_string(penum, &str, gdata, NULL, &text_state);
}
return code;
}