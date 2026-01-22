#include "math_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gscencs.h"
#include "gscedata.h"
#include "gsmatrix.h"
#include "gzstate.h"
#include "gxfcache.h"
#include "gxfont.h"
#include "gxfont0.h"
#include "gxfcid.h"
#include "gxfcopy.h"
#include "gxfcmap.h"
#include "gxpath.h"
#include "gxchar.h"
#include "gxstate.h"
#include "gdevpdfx.h"
#include "gdevpdfg.h"
#include "gdevpdtx.h"
#include "gdevpdtd.h"
#include "gdevpdtf.h"
#include "gdevpdts.h"
#include "gdevpdtt.h"
#include "gdevpdti.h"
#include "gxhldevc.h"
private_st_pdf_text_enum();
private int
pdf_text_resync(gs_text_enum_t *pte, const gs_text_enum_t *pfrom)
{
pdf_text_enum_t *const penum = (pdf_text_enum_t *)pte;
if ((pte->text.operation ^ pfrom->text.operation) & ~TEXT_FROM_ANY)
return_error(gs_error_rangecheck);
if (penum->pte_default) {
int code = gs_text_resync(penum->pte_default, pfrom);
if (code < 0)
return code;
}
pte->text = pfrom->text;
gs_text_enum_copy_dynamic(pte, pfrom, false);
return 0;
}
private bool
pdf_text_is_width_only(const gs_text_enum_t *pte)
{
const pdf_text_enum_t *const penum = (const pdf_text_enum_t *)pte;
if (penum->pte_default)
return gs_text_is_width_only(penum->pte_default);
return false;
}
private int
pdf_text_current_width(const gs_text_enum_t *pte, gs_point *pwidth)
{
const pdf_text_enum_t *const penum = (const pdf_text_enum_t *)pte;
if (penum->pte_default)
return gs_text_current_width(penum->pte_default, pwidth);
return_error(gs_error_rangecheck);
}
private int
pdf_text_set_cache(gs_text_enum_t *pte, const double *pw,
gs_text_cache_control_t control)
{
pdf_text_enum_t *const penum = (pdf_text_enum_t *)pte;
gx_device_pdf *pdev = (gx_device_pdf *)pte->dev;
switch (control) {
case TEXT_SET_CHAR_WIDTH:
case TEXT_SET_CACHE_DEVICE:
gs_distance_transform(pw[0], pw[1], &ctm_only(pte->pis), &pdev->char_width);
break;
case TEXT_SET_CACHE_DEVICE2:
gs_distance_transform(pw[0], pw[1], &ctm_only(pte->pis), &pdev->char_width);
if (penum->cdevproc_callout) {
memcpy(penum->cdevproc_result, pw, sizeof(penum->cdevproc_result));
return 0;
}
break;
default:
return_error(gs_error_rangecheck);
}
if (penum->current_font->FontType == ft_user_defined &&
penum->orig_font->FontType != ft_composite &&
penum->outer_CID == GS_NO_GLYPH &&
!(penum->pte_default->text.operation & TEXT_DO_CHARWIDTH)) {
int code;
gs_font *font = penum->orig_font;
gs_char ch;
gs_glyph glyph;
gs_const_string gnstr;
if (penum->text.operation & TEXT_FROM_SINGLE_GLYPH) {
byte buf[1];
int char_code_length;
glyph = pte->text.data.d_glyph;
code = pdf_encode_glyph((gs_font_base *)font, glyph,
buf, sizeof(buf), &char_code_length);
if (code < 0) {
ch = GS_NO_CHAR;
} else if (char_code_length != 1) {
ch = GS_NO_CHAR;
} else
ch = buf[0];
} else {
ch = penum->text.data.bytes[penum->index];
glyph = font->procs.encode_char(font, ch, GLYPH_SPACE_NAME);
}
if (glyph != GS_NO_GLYPH && ch != GS_NO_CHAR) {
gs_show_enum *penum_s;
extern_st(st_gs_show_enum);
gs_fixed_rect clip_box;
double pw1[10];
int narg = (control == TEXT_SET_CHAR_WIDTH ? 2 :
control == TEXT_SET_CACHE_DEVICE ? 6 : 10), i;
if (penum->pte_default == NULL)
return_error(gs_error_unregistered);
if (gs_object_type(penum->pte_default->memory, penum->pte_default) != &st_gs_show_enum) {
return_error(gs_error_unregistered);
}
penum_s = (gs_show_enum *)penum->pte_default;
code = font->procs.glyph_name(font, glyph, &gnstr);
if (code < 0)
return_error(gs_error_unregistered);
for (i = 0; i < narg; i += 2) {
gs_point p;
gs_point_transform(pw[i], pw[i + 1], &ctm_only(penum_s->pgs), &p);
pw1[i] = p.x;
pw1[i + 1] = p.y;
}
if (control != TEXT_SET_CHAR_WIDTH) {
clip_box.p.x = float2fixed(pw1[2]);
clip_box.p.y = float2fixed(pw1[3]);
clip_box.q.x = float2fixed(pw1[4]);
clip_box.q.y = float2fixed(pw1[5]);
} else {
clip_box.p.x = clip_box.p.y = min_int / 2;
clip_box.q.x = clip_box.q.y = max_int / 2;
}
code = gx_clip_to_rectangle(penum_s->pgs, &clip_box);
if (code < 0)
return code;
code = pdf_set_charproc_attrs(pdev, pte->current_font,
pw1, narg, control, ch, &gnstr);
if (code < 0)
return code;
pdev->clip_path_id = gx_get_clip_path_id(penum_s->pgs);
penum->charproc_accum = true;
return code;
} else {
gs_matrix m;
pdf_resource_t *pres = pdev->accumulating_substream_resource;
code = pdf_exit_substream(pdev);
if (code < 0)
return code;
code = pdf_cancel_resource(pdev, pres, resourceCharProc);
if (code < 0)
return code;
pdf_forget_resource(pdev, pres, resourceCharProc);
gs_matrix_multiply((gs_matrix *)&pdev->charproc_ctm, (gs_matrix *)&penum->pis->ctm, &m);
gs_matrix_fixed_from_matrix(&penum->pis->ctm, &m);
}
}
if (penum->pte_default) {
if (penum->pte_default->text.operation & TEXT_DO_CHARWIDTH )
return gs_text_set_cache(penum->pte_default, pw, TEXT_SET_CHAR_WIDTH);
else
return gs_text_set_cache(penum->pte_default, pw, control);
}
return_error(gs_error_unregistered);
}
private int
pdf_text_retry(gs_text_enum_t *pte)
{
pdf_text_enum_t *const penum = (pdf_text_enum_t *)pte;
if (penum->pte_default)
return gs_text_retry(penum->pte_default);
return_error(gs_error_rangecheck);
}
private void
pdf_text_release(gs_text_enum_t *pte, client_name_t cname)
{
pdf_text_enum_t *const penum = (pdf_text_enum_t *)pte;
if (penum->pte_default) {
gs_text_release(penum->pte_default, cname);
penum->pte_default = 0;
}
pdf_text_release_cgp(penum);
gx_default_text_release(pte, cname);
}
void
pdf_text_release_cgp(pdf_text_enum_t *penum)
{
if (penum->cgp) {
gs_free_object(penum->memory, penum->cgp, "pdf_text_release");
penum->cgp = 0;
}
}
private text_enum_proc_process(pdf_text_process);
private const gs_text_enum_procs_t pdf_text_procs = {
pdf_text_resync, pdf_text_process,
pdf_text_is_width_only, pdf_text_current_width,
pdf_text_set_cache, pdf_text_retry,
pdf_text_release
};
private int
pdf_prepare_text_drawing(gx_device_pdf *const pdev, gs_text_enum_t *pte)
{
gs_imager_state * pis = pte->pis;
const gx_device_color * pdcolor = pte->pdcolor;
const gx_clip_path * pcpath = pte->pcpath;
const gs_text_params_t *text = &pte->text;
bool new_clip = false;
int code;
if (!(text->operation & TEXT_DO_NONE) || pis->text_rendering_mode == 3) {
new_clip = pdf_must_put_clip_path(pdev, pcpath);
if (new_clip)
code = pdf_unclip(pdev);
else if (pdev->context == PDF_IN_NONE)
code = pdf_open_page(pdev, PDF_IN_STREAM);
else
code = 0;
if (code < 0)
return code;
code = pdf_prepare_fill(pdev, pis);
if (code < 0)
return code;
}
if (text->operation & TEXT_DO_DRAW) {
if (new_clip) {
code = pdf_put_clip_path(pdev, pcpath);
if (code < 0)
return code;
}
if ((code =
pdf_set_drawing_color(pdev, pis, pdcolor, &pdev->saved_stroke_color,
&pdev->stroke_used_process_color,
&psdf_set_stroke_color_commands)) < 0 ||
(code =
pdf_set_drawing_color(pdev, pis, pdcolor, &pdev->saved_fill_color,
&pdev->fill_used_process_color,
&psdf_set_fill_color_commands)) < 0
)
return code;
}
return 0;
}
int
gdev_pdf_text_begin(gx_device * dev, gs_imager_state * pis,
const gs_text_params_t *text, gs_font * font,
gx_path * path0, const gx_device_color * pdcolor,
const gx_clip_path * pcpath,
gs_memory_t * mem, gs_text_enum_t ** ppte)
{
gx_device_pdf *const pdev = (gx_device_pdf *)dev;
gx_path *path = path0;
pdf_text_enum_t *penum;
gs_fixed_point cpt;
int code;
{
gs_matrix tmat;
int i;
gs_matrix_multiply(&font->FontMatrix, &ctm_only(pis), &tmat);
if (is_xxyy(&tmat))
i = (tmat.xx >= 0 ? 0 : 2);
else if (is_xyyx(&tmat))
i = (tmat.xy >= 0 ? 1 : 3);
else
i = 4;
pdf_current_page(pdev)->text_rotation.counts[i] += text->size;
}
if (font->FontType == ft_user_defined &&
(text->operation & TEXT_DO_NONE) && (text->operation & TEXT_RETURN_WIDTH)) {
code = gx_hld_stringwidth_begin(pis, &path);
if (code < 0)
return code;
} else if ((!(text->operation & TEXT_DO_DRAW) && pis->text_rendering_mode != 3)
|| path == 0 || gx_path_current_point(path, &cpt) < 0
)
return gx_default_text_begin(dev, pis, text, font, path, pdcolor,
pcpath, mem, ppte);
rc_alloc_struct_1(penum, pdf_text_enum_t, &st_pdf_text_enum, mem,
return_error(gs_error_VMerror), "gdev_pdf_text_begin");
penum->rc.free = rc_free_text_enum;
penum->pte_default = 0;
penum->charproc_accum = false;
penum->cdevproc_callout = false;
penum->returned.total_width.x = penum->returned.total_width.y = 0;
penum->cgp = NULL;
code = gs_text_enum_init((gs_text_enum_t *)penum, &pdf_text_procs,
dev, pis, text, font, path, pdcolor, pcpath, mem);
if (code < 0) {
gs_free_object(mem, penum, "gdev_pdf_text_begin");
return code;
}
if (pdev->font3 != 0) {
penum->device_disabled_grid_fitting = true;
}
*ppte = (gs_text_enum_t *)penum;
return 0;
}
private_st_pdf_font_cache_elem();
private ulong
pdf_font_cache_elem_id(gs_font *font)
{
#if 0
if (font->FontType == ft_composite || font->PaintType != 0 ||
!uid_is_valid(&(((gs_font_base *)font)->UID)))
return font->id;
else
return ((gs_font_base *)font)->UID.id;
#else
return font->id;
#endif
}
private pdf_font_cache_elem_t **
pdf_locate_font_cache_elem(gx_device_pdf *pdev, gs_font *font)
{
pdf_font_cache_elem_t **e = &pdev->font_cache;
long id = pdf_font_cache_elem_id(font);
for (; *e != 0; e = &(*e)->next)
if ((*e)->font_id == id) {
return e;
}
return 0;
}
private void
pdf_remove_font_cache_elem(pdf_font_cache_elem_t *e0)
{
gx_device_pdf *pdev = e0->pdev;
pdf_font_cache_elem_t **e = &pdev->font_cache;
for (; *e != 0; e = &(*e)->next)
if (*e == e0) {
*e = e0->next;
gs_free_object(pdev->pdf_memory, e0->glyph_usage,
"pdf_remove_font_cache_elem");
gs_free_object(pdev->pdf_memory, e0->real_widths,
"pdf_remove_font_cache_elem");
e0->glyph_usage = 0;
e0->real_widths = 0;
gs_free_object(pdev->pdf_memory, e0,
"pdf_remove_font_cache_elem");
return;
}
}
private void
font_cache_elem_array_sizes(gx_device_pdf *pdev, gs_font *font,
int *num_widths, int *num_chars)
{
switch (font->FontType) {
case ft_composite:
*num_widths = 0;
*num_chars = 65536;
break;
case ft_encrypted:
case ft_encrypted2:
case ft_user_defined:
case ft_disk_based:
case ft_Chameleon:
case ft_TrueType:
*num_widths = *num_chars = 256;
break;
case ft_CID_encrypted:
*num_widths = *num_chars = ((gs_font_cid0 *)font)->cidata.common.CIDCount;
break;
case ft_CID_TrueType:
*num_widths = *num_chars = ((gs_font_cid2 *)font)->cidata.common.CIDCount;
break;
default:
*num_widths = *num_chars = 65536;
}
}
private int
alloc_font_cache_elem_arrays(gx_device_pdf *pdev, pdf_font_cache_elem_t *e,
gs_font *font)
{
int num_widths, num_chars, len;
font_cache_elem_array_sizes(pdev, font, &num_widths, &num_chars);
len = (num_chars + 7) / 8;
e->glyph_usage = gs_alloc_bytes(pdev->pdf_memory,
len, "alloc_font_cache_elem_arrays");
e->real_widths = (num_widths > 0 ? (double *)gs_alloc_bytes(pdev->pdf_memory,
num_widths * sizeof(*e->real_widths) *
(font->FontType == ft_user_defined ? 2 : 1),
"alloc_font_cache_elem_arrays") : NULL);
if (e->glyph_usage == NULL || (num_widths !=0 && e->real_widths == NULL)) {
gs_free_object(pdev->pdf_memory, e->glyph_usage,
"pdf_attach_font_resource");
gs_free_object(pdev->pdf_memory, e->real_widths,
"alloc_font_cache_elem_arrays");
return_error(gs_error_VMerror);
}
e->num_chars = num_chars;
e->num_widths = num_widths;
memset(e->glyph_usage, 0, len);
memset(e->real_widths, 0, num_widths * sizeof(*e->real_widths));
return 0;
}
int
pdf_free_font_cache(gx_device_pdf *pdev)
{
pdev->font_cache = NULL;
return 0;
}
int
pdf_attached_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **pdfont, byte **glyph_usage,
double **real_widths, int *num_chars, int *num_widths)
{
pdf_font_cache_elem_t **e = pdf_locate_font_cache_elem(pdev, font);
if (e != NULL && (((*e)->glyph_usage == NULL && glyph_usage !=NULL) ||
((*e)->real_widths == NULL && real_widths !=NULL))) {
int code = alloc_font_cache_elem_arrays(pdev, *e, font);
if (code < 0)
return code;
}
*pdfont = (e == NULL ? NULL : (*e)->pdfont);
if (glyph_usage != NULL)
*glyph_usage = (e == NULL ? NULL : (*e)->glyph_usage);
if (real_widths != NULL)
*real_widths = (e == NULL ? NULL : (*e)->real_widths);
if (num_chars != NULL)
*num_chars = (e == NULL ? 0 : (*e)->num_chars);
if (num_widths != NULL)
*num_widths = (e == NULL ? 0 : (*e)->num_widths);
return 0;
}
private int
pdf_notify_remove_font(void *proc_data, void *event_data)
{
if (event_data == NULL)
pdf_remove_font_cache_elem((pdf_font_cache_elem_t *)proc_data);
return 0;
}
int
pdf_attach_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t *pdfont)
{
int num_chars, num_widths, len;
pdf_font_cache_elem_t *e, **pe = pdf_locate_font_cache_elem(pdev, font);
if (pdfont->FontType != font->FontType)
return_error(gs_error_unregistered);
font_cache_elem_array_sizes(pdev, font, &num_widths, &num_chars);
len = (num_chars + 7) / 8;
if (pe != NULL) {
e = *pe;
if (e->pdfont == pdfont)
return 0;
e->pdfont = pdfont;
memset(e->glyph_usage, 0, len);
memset(e->real_widths, 0, num_widths * sizeof(*e->real_widths));
} else {
int code;
e = (pdf_font_cache_elem_t *)gs_alloc_struct(pdev->pdf_memory,
pdf_font_cache_elem_t, &st_pdf_font_cache_elem,
"pdf_attach_font_resource");
if (e == NULL)
return_error(gs_error_VMerror);
e->pdfont = pdfont;
e->font_id = pdf_font_cache_elem_id(font);
e->num_chars = 0;
e->glyph_usage = NULL;
e->real_widths = NULL;
e->pdev = pdev;
e->next = pdev->font_cache;
pdev->font_cache = e;
code = gs_notify_register(&font->notify_list, pdf_notify_remove_font, e);
if (code < 0)
return code;
}
return 0;
}
int
pdf_font_orig_matrix(const gs_font *font, gs_matrix *pmat)
{
switch (font->FontType) {
case ft_composite:
case ft_TrueType:
case ft_CID_TrueType:
gs_make_identity(pmat);
return 0;
case ft_encrypted:
case ft_encrypted2:
case ft_CID_encrypted:
case ft_user_defined:
{
const gs_font *base_font = font;
while (base_font->base != base_font)
base_font = base_font->base;
if (font->FontType == ft_user_defined)
*pmat = base_font->FontMatrix;
else if (base_font->orig_FontMatrix.xx != 0 || base_font->orig_FontMatrix.xy != 0 ||
base_font->orig_FontMatrix.yx != 0 || base_font->orig_FontMatrix.yy != 0)
*pmat = base_font->orig_FontMatrix;
else {
if (base_font->FontMatrix.xx == 1.0/2048 &&
base_font->FontMatrix.xy == 0 &&
base_font->FontMatrix.yx == 0 &&
any_abs(base_font->FontMatrix.yy) == 1.0/2048
)
*pmat = base_font->FontMatrix;
else
gs_make_scaling(0.001, 0.001, pmat);
}
}
return 0;
default:
return_error(gs_error_rangecheck);
}
}
int
font_orig_scale(const gs_font *font, double *sx)
{
gs_matrix mat;
int code = pdf_font_orig_matrix(font, &mat);
if (code < 0)
return code;
*sx = mat.xx;
return 0;
}
bool
pdf_check_encoding_compatibility(const pdf_font_resource_t *pdfont,
const pdf_char_glyph_pair_t *pairs, int num_chars)
{
int i;
for (i = 0; i < num_chars; ++i) {
gs_char ch = pairs[i].chr;
pdf_encoding_element_t *pet = &pdfont->u.simple.Encoding[ch];
if (pairs[i].glyph == pet->glyph)
continue;
if (pet->glyph != GS_NO_GLYPH)
return false;
}
return true;
}
private bool
pdf_is_compatible_encoding(gx_device_pdf *pdev, pdf_font_resource_t *pdfont,
gs_font *font, const pdf_char_glyph_pair_t *pairs, int num_chars)
{
switch (pdfont->FontType) {
case ft_composite:
{
gs_font_type0 *pfont = (gs_font_type0 *)font;
if (pfont->data.FMapType == fmap_CMap) {
const gs_cmap_t *pcmap = pfont->data.CMap;
const gs_const_string *s0 = &pdfont->u.type0.CMapName;
const gs_const_string *s1 = &pcmap->CMapName;
return (s0->size == s1->size &&
!memcmp(s0->data, s1->data, s0->size));
}
}
return false;
case ft_user_defined:
if (pdfont->u.simple.Encoding == NULL)
return false;
case ft_encrypted:
case ft_encrypted2:
case ft_TrueType:
return pdf_check_encoding_compatibility(pdfont, pairs, num_chars);
case ft_CID_encrypted:
case ft_CID_TrueType:
{
gs_font *font1 = (gs_font *)pdf_font_resource_font(pdfont, false);
return gs_is_CIDSystemInfo_compatible(
gs_font_cid_system_info(font),
gs_font_cid_system_info(font1));
}
default:
return false;
}
}
private int
pdf_find_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_resource_type_t type,
pdf_font_resource_t **ppdfont,
pdf_char_glyph_pairs_t *cgp)
{
pdf_resource_t **pchain = pdev->resources[type].chains;
pdf_resource_t *pres;
int i;
for (i = 0; i < NUM_RESOURCE_CHAINS; i++) {
for (pres = pchain[i]; pres != 0; pres = pres->next) {
pdf_font_resource_t *pdfont = (pdf_font_resource_t *)pres;
const gs_font_base *cfont;
gs_font *ofont = font;
int code;
if (font->FontType != pdfont->FontType)
continue;
if (pdfont->FontType == ft_composite) {
gs_font_type0 *font0 = (gs_font_type0 *)font;
ofont = font0->data.FDepVector[0];
cfont = pdf_font_resource_font(pdfont->u.type0.DescendantFont, false);
if (font0->data.CMap->WMode != pdfont->u.type0.WMode)
continue;
} else
cfont = pdf_font_resource_font(pdfont, false);
if (!pdf_is_CID_font(ofont) &&
!pdf_is_compatible_encoding(pdev, pdfont, font, cgp->s, cgp->num_all_chars))
continue;
if (cfont == 0)
continue;
code = gs_copied_can_copy_glyphs((const gs_font *)cfont, ofont,
&cgp->s[cgp->unused_offset].glyph, cgp->num_unused_chars,
sizeof(pdf_char_glyph_pair_t), true);
if (code == gs_error_unregistered)
return code;
if(code > 0) {
*ppdfont = pdfont;
return 1;
}
}
}
return 0;
}
private int
pdf_find_type0_font_resource(gx_device_pdf *pdev, const pdf_font_resource_t *pdsubf,
const gs_const_string *CMapName, pdf_font_resource_t **ppdfont)
{
pdf_resource_t **pchain = pdev->resources[resourceFont].chains;
pdf_resource_t *pres;
int i;
for (i = 0; i < NUM_RESOURCE_CHAINS; i++) {
for (pres = pchain[i]; pres != 0; pres = pres->next) {
pdf_font_resource_t *pdfont = (pdf_font_resource_t *)pres;
if (pdfont->FontType != ft_composite)
continue;
if (pdfont->u.type0.DescendantFont != pdsubf)
continue;
if (pdfont->BaseFont.size != pdsubf->BaseFont.size + CMapName->size + 1)
continue;
if (memcmp(pdfont->BaseFont.data + pdsubf->BaseFont.size + 1,
CMapName->data, CMapName->size))
continue;
*ppdfont = pdfont;
return 1;
}
}
return 0;
}
private int pdf_make_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **ppdfont,
pdf_char_glyph_pairs_t *cgp);
int
pdf_obtain_cidfont_resource(gx_device_pdf *pdev, gs_font *subfont,
pdf_font_resource_t **ppdsubf,
pdf_char_glyph_pairs_t *cgp)
{
int code = 0;
pdf_attached_font_resource(pdev, subfont, ppdsubf, NULL, NULL, NULL, NULL);
if (*ppdsubf != NULL) {
const gs_font_base *cfont = pdf_font_resource_font(*ppdsubf, false);
code = gs_copied_can_copy_glyphs((const gs_font *)cfont, subfont,
&cgp->s[cgp->unused_offset].glyph, cgp->num_unused_chars,
sizeof(pdf_char_glyph_pair_t), true);
if (code > 0)
return 0;
if (code < 0)
return code;
*ppdsubf = NULL;
}
code = pdf_find_font_resource(pdev, subfont,
resourceCIDFont, ppdsubf, cgp);
if (code < 0)
return code;
if (*ppdsubf == NULL) {
code = pdf_make_font_resource(pdev, subfont, ppdsubf, cgp);
if (code < 0)
return code;
}
return pdf_attach_font_resource(pdev, subfont, *ppdsubf);
}
private int
pdf_refine_encoding_index(const gx_device_pdf *pdev, int index, bool is_standard)
{
if (pdev->ForOPDFRead) {
switch (index) {
case ENCODING_INDEX_STANDARD: return index;
case ENCODING_INDEX_ISOLATIN1: return index;
default:
return ENCODING_INDEX_STANDARD;
}
}
switch (index) {
case ENCODING_INDEX_WINANSI:
case ENCODING_INDEX_MACROMAN:
case ENCODING_INDEX_MACEXPERT:
return index;
case ENCODING_INDEX_STANDARD:
if (is_standard)
return index;
default:
return ENCODING_INDEX_WINANSI;
}
}
int
pdf_make_font3_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **ppdfont)
{
const gs_font_base *bfont = (const gs_font_base *)font;
pdf_font_resource_t *pdfont;
byte *cached;
int code;
cached = gs_alloc_bytes(pdev->pdf_memory, 256/8, "pdf_make_font3_resource");
if (cached == NULL)
return_error(gs_error_VMerror);
code = font_resource_encoded_alloc(pdev, &pdfont, bfont->id,
ft_user_defined, pdf_write_contents_bitmap);
if (code < 0) {
gs_free_object(pdev->pdf_memory, cached, "pdf_make_font3_resource");
return code;
}
memset(cached, 0, 256 / 8);
pdfont->u.simple.s.type3.bitmap_font = false;
pdfont->u.simple.BaseEncoding = pdf_refine_encoding_index(pdev,
bfont->nearest_encoding_index, true);
pdfont->u.simple.s.type3.char_procs = NULL;
pdfont->u.simple.s.type3.cached = cached;
pdfont->u.simple.s.type3.FontBBox.p.x = (int)floor(bfont->FontBBox.p.x);
pdfont->u.simple.s.type3.FontBBox.p.y = (int)floor(bfont->FontBBox.p.y);
pdfont->u.simple.s.type3.FontBBox.q.x = (int)ceil(bfont->FontBBox.q.x);
pdfont->u.simple.s.type3.FontBBox.q.y = (int)ceil(bfont->FontBBox.q.y);
pdfont->u.simple.s.type3.FontMatrix = bfont->FontMatrix;
while (any_abs(pdfont->u.simple.s.type3.FontMatrix.xx) < 0.001 &&
any_abs(pdfont->u.simple.s.type3.FontMatrix.xy) < 0.001 &&
any_abs(pdfont->u.simple.s.type3.FontMatrix.yx) < 0.001 &&
any_abs(pdfont->u.simple.s.type3.FontMatrix.yy) < 0.001) {
pdfont->u.simple.s.type3.FontMatrix.xx *= 10;
pdfont->u.simple.s.type3.FontMatrix.xy *= 10;
pdfont->u.simple.s.type3.FontMatrix.yx *= 10;
pdfont->u.simple.s.type3.FontMatrix.yy *= 10;
}
*ppdfont = pdfont;
return 0;
}
private int
pdf_make_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **ppdfont,
pdf_char_glyph_pairs_t *cgp)
{
int index = -1;
int BaseEncoding = ENCODING_INDEX_UNKNOWN;
pdf_font_embed_t embed;
pdf_font_descriptor_t *pfd = 0;
int (*font_alloc)(gx_device_pdf *, pdf_font_resource_t **,
gs_id, pdf_font_descriptor_t *);
gs_font *base_font = font;
pdf_font_resource_t *pdfont;
pdf_standard_font_t *const psfa =
pdev->text->outline_fonts->standard_fonts;
int code = 0;
if (pdev->version < psdf_version_level2_with_TT) {
switch(font->FontType) {
case ft_TrueType:
case ft_CID_TrueType:
return_error(gs_error_undefined);
default:
break;
}
}
if (pdev->ForOPDFRead && !pdev->HaveCIDSystem) {
switch(font->FontType) {
case ft_CID_encrypted:
case ft_CID_TrueType:
return_error(gs_error_undefined);
default:
break;
}
}
if (!pdev->HaveCFF) {
if (font->FontType == ft_encrypted2)
return_error(gs_error_undefined);
}
embed = pdf_font_embed_status(pdev, base_font, &index, cgp->s, cgp->num_all_chars);
if (embed == FONT_EMBED_STANDARD) {
pdf_standard_font_t *psf = &psfa[index];
if (psf->pdfont == NULL ||
!pdf_is_compatible_encoding(pdev, psf->pdfont, font,
cgp->s, cgp->num_all_chars)) {
code = pdf_font_std_alloc(pdev, ppdfont, (psf->pdfont == NULL), base_font->id,
(gs_font_base *)base_font, index);
if (code < 0)
return code;
if (psf->pdfont == NULL)
psf->pdfont = *ppdfont;
(*ppdfont)->u.simple.BaseEncoding = pdf_refine_encoding_index(pdev,
((const gs_font_base *)base_font)->nearest_encoding_index, true);
code = 1;
} else
*ppdfont = psf->pdfont;
return code;
}
switch (font->FontType) {
case ft_CID_encrypted:
case ft_CID_TrueType:
font_alloc = pdf_font_cidfont_alloc;
break;
case ft_encrypted:
case ft_encrypted2:
case ft_TrueType:
font_alloc = pdf_font_simple_alloc;
break;
case ft_user_defined:
code = pdf_make_font3_resource(pdev, font, ppdfont);
if (code < 0)
return code;
return 1;
default:
return_error(gs_error_invalidfont);
}
if (embed == FONT_EMBED_YES) {
if (font->FontType == ft_TrueType &&
pdev->CompatibilityLevel <= 1.2
) {
int i;
for (i = 0; i <= 0xff; ++i) {
gs_glyph glyph =
font->procs.encode_char(font, (gs_char)i,
GLYPH_SPACE_INDEX);
if (glyph == GS_NO_GLYPH ||
(glyph >= GS_MIN_GLYPH_INDEX &&
glyph <= GS_MIN_GLYPH_INDEX + 0xff)
)
continue;
return_error(gs_error_rangecheck);
}
}
}
if (font->FontType == ft_encrypted || font->FontType == ft_encrypted2 ||
font->FontType == ft_TrueType) {
BaseEncoding = pdf_refine_encoding_index(pdev,
((const gs_font_base *)base_font)->nearest_encoding_index, false);
}
if ((code = pdf_font_descriptor_alloc(pdev, &pfd,
(gs_font_base *)base_font,
embed == FONT_EMBED_YES)) < 0 ||
(code = font_alloc(pdev, &pdfont, base_font->id, pfd)) < 0
)
return code;
code = 1;
if (!pdf_is_CID_font(font))
pdfont->u.simple.BaseEncoding = BaseEncoding;
*ppdfont = pdfont;
return 1;
}
void
pdf_font3_scale(gx_device_pdf *pdev, gs_font *font, double *scale)
{
pdf_font_resource_t *pdfont;
pdf_attached_font_resource(pdev, font, &pdfont, NULL, NULL, NULL, NULL);
*scale = pdfont->u.simple.s.type3.FontMatrix.xx;
}
bool
pdf_is_simple_font(gs_font *font)
{
return (font->FontType == ft_encrypted ||
font->FontType == ft_encrypted2 ||
font->FontType == ft_TrueType ||
font->FontType == ft_user_defined);
}
bool
pdf_is_CID_font(gs_font *font)
{
return (font->FontType == ft_CID_encrypted ||
font->FontType == ft_CID_TrueType);
}
private int
pdf_next_char_glyph(gs_text_enum_t *penum, const gs_string *pstr,
gs_font *font, bool font_is_simple,
gs_char *char_code, gs_char *cid, gs_glyph *glyph)
{
int code = font->procs.next_char_glyph(penum, char_code, glyph);
if (code == 2)
return code;
if (code < 0)
return code;
if (font_is_simple) {
*cid = *char_code;
*glyph = font->procs.encode_char(font, *char_code, GLYPH_SPACE_NAME);
if (*glyph == GS_NO_GLYPH)
return 3;
} else {
if (*glyph < GS_MIN_CID_GLYPH)
return 3;
*cid = *glyph - GS_MIN_CID_GLYPH;
}
return 0;
}
private void
store_glyphs(pdf_char_glyph_pairs_t *cgp,
byte *glyph_usage, int char_cache_size,
gs_char char_code, gs_char cid, gs_glyph glyph)
{
int j;
for (j = 0; j < cgp->num_all_chars; j++)
if (cgp->s[j].chr == cid)
break;
if (j < cgp->num_all_chars)
return;
cgp->s[cgp->num_all_chars].glyph = glyph;
cgp->s[cgp->num_all_chars].chr = char_code;
cgp->num_all_chars++;
if (glyph_usage == 0 || !(glyph_usage[cid / 8] & (0x80 >> (cid & 7)))) {
cgp->s[cgp->unused_offset + cgp->num_unused_chars].glyph = glyph;
cgp->s[cgp->unused_offset + cgp->num_unused_chars].chr = char_code;
cgp->num_unused_chars++;
}
}
private int
pdf_alloc_text_glyphs_table(gx_device_pdf *pdev, pdf_text_enum_t *penum, const gs_string *pstr)
{
const int go = (pstr != NULL ? pstr->size : penum->text.size);
const int struct_size = sizeof(pdf_char_glyph_pairs_t) +
sizeof(pdf_char_glyph_pair_t) * (2 * go - 1);
pdf_char_glyph_pairs_t *cgp = (pdf_char_glyph_pairs_t *)gs_alloc_bytes(penum->memory,
struct_size, "pdf_alloc_text_glyphs_table");
if (cgp == NULL)
return_error(gs_error_VMerror);
penum->cgp = cgp;
cgp->unused_offset = go;
cgp->num_all_chars = 0;
cgp->num_unused_chars = 0;
return 0;
}
private int
pdf_make_text_glyphs_table(pdf_text_enum_t *penum, const gs_string *pstr,
byte *glyph_usage, int char_cache_size)
{
gs_text_enum_t scan = *(gs_text_enum_t *)penum;
gs_font *font = (gs_font *)penum->current_font;
bool font_is_simple = pdf_is_simple_font(font);
pdf_char_glyph_pairs_t *cgp = penum->cgp;
gs_char char_code, cid;
gs_glyph glyph;
int code;
cgp->num_unused_chars = 0;
cgp->num_all_chars = 0;
if (pstr != NULL) {
scan.text.data.bytes = pstr->data;
scan.text.size = pstr->size;
scan.index = 0;
if ( scan.text.operation & TEXT_FROM_CHARS )
scan.text.operation = ((scan.text.operation & ~TEXT_FROM_CHARS) | TEXT_FROM_STRING);
}
for (;;) {
code = pdf_next_char_glyph(&scan, pstr, font, font_is_simple,
&char_code, &cid, &glyph);
if (code == 2)
break;
if (code == 3)
continue;
if (code < 0)
return code;
if (cgp->num_all_chars > cgp->unused_offset)
return_error(gs_error_unregistered);
if (glyph_usage != 0 && cid > char_cache_size)
continue;
store_glyphs(cgp, glyph_usage, char_cache_size,
char_code, cid, glyph);
}
return 0;
}
private int
pdf_make_text_glyphs_table_unencoded(pdf_char_glyph_pairs_t *cgp,
gs_font *font, const gs_string *pstr, const gs_glyph *gdata,
int *ps_encoding_index)
{
int i, ei;
gs_char ch;
gs_const_string gname;
gs_glyph *gid = (gs_glyph *)pstr->data;
for (i = 0; i < pstr->size; i++) {
int code = font->procs.glyph_name(font, gdata[i], &gname);
if (code < 0)
return code;
gid[i] = gs_c_name_glyph(gname.data, gname.size);
if (gid[i] == GS_NO_GLYPH)
return_error(gs_error_rangecheck);
}
for (ei = 0; gs_c_known_encodings[ei]; ei++) {
cgp->num_unused_chars = 0;
cgp->num_all_chars = 0;
for (i = 0; i < pstr->size; i++) {
ch = gs_c_decode(gid[i], ei);
if (ch == GS_NO_CHAR)
break;
store_glyphs(cgp, NULL, 0,
ch, ch, gdata[i]);
}
*ps_encoding_index = ei;
if (i == pstr->size) {
for (i = 0; i < pstr->size; i++)
pstr->data[i] = (byte)gs_c_decode(gid[i], ei);
return 0;
}
}
return_error(gs_error_rangecheck);
}
private int
pdf_obtain_font_resource_encoded(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **ppdfont, pdf_char_glyph_pairs_t *cgp)
{
int code;
pdf_font_resource_t *pdfont_not_allowed = NULL;
if (*ppdfont != 0) {
gs_font_base *cfont = pdf_font_resource_font(*ppdfont, false);
if (font->FontType != ft_user_defined) {
code = gs_copied_can_copy_glyphs((gs_font *)cfont, font,
&cgp->s[cgp->unused_offset].glyph, cgp->num_unused_chars,
sizeof(pdf_char_glyph_pair_t), true);
if (code < 0)
return code;
} else
code = 1;
if (code == 0) {
pdfont_not_allowed = *ppdfont;
*ppdfont = 0;
} else if(!pdf_is_compatible_encoding(pdev, *ppdfont, font,
cgp->s, cgp->num_all_chars)) {
pdfont_not_allowed = *ppdfont;
*ppdfont = 0;
}
}
if (*ppdfont == 0) {
gs_font *base_font = font;
gs_font *below;
bool same_encoding = true;
while ((below = base_font->base) != base_font &&
base_font->procs.same_font(base_font, below, FONT_SAME_OUTLINES))
base_font = below;
if (base_font != font)
same_encoding = ((base_font->procs.same_font(base_font, font,
FONT_SAME_ENCODING) & FONT_SAME_ENCODING) != 0);
pdf_attached_font_resource(pdev, base_font, ppdfont, NULL, NULL, NULL, NULL);
if (*ppdfont != NULL && base_font != font) {
if (pdfont_not_allowed == *ppdfont)
*ppdfont = NULL;
else if(!pdf_is_compatible_encoding(pdev, *ppdfont,
base_font, cgp->s, cgp->num_all_chars))
*ppdfont = NULL;
}
if (*ppdfont == NULL || *ppdfont == pdfont_not_allowed) {
pdf_resource_type_t type =
(pdf_is_CID_font(base_font) ? resourceCIDFont
: resourceFont);
*ppdfont = NULL;
code = pdf_find_font_resource(pdev, base_font, type, ppdfont, cgp);
if (code < 0)
return code;
if (*ppdfont == NULL) {
code = pdf_make_font_resource(pdev, base_font, ppdfont, cgp);
if (code < 0)
return code;
}
if (base_font != font && same_encoding) {
code = pdf_attach_font_resource(pdev, base_font, *ppdfont);
if (code < 0)
return code;
}
}
code = pdf_attach_font_resource(pdev, font, *ppdfont);
if (code < 0)
return code;
}
return 0;
}
private int
pdf_mark_text_glyphs(const gs_text_enum_t *penum, const gs_string *pstr,
byte *glyph_usage, int char_cache_size)
{
gs_text_enum_t scan = *penum;
gs_font *font = (gs_font *)penum->current_font;
bool font_is_simple = pdf_is_simple_font(font);
gs_char char_code, cid;
gs_glyph glyph;
if (pstr != NULL) {
scan.text.data.bytes = pstr->data;
scan.text.size = pstr->size;
scan.index = 0;
if ( scan.text.operation & TEXT_FROM_CHARS )
scan.text.operation =
((scan.text.operation & ~TEXT_FROM_CHARS) | TEXT_FROM_STRING);
}
for (;;) {
int code = pdf_next_char_glyph(&scan, pstr, font, font_is_simple,
&char_code, &cid, &glyph);
if (code == 2)
break;
if (code == 3)
continue;
if (code < 0)
return code;
if (glyph_usage != 0 && cid >= char_cache_size)
continue;
glyph_usage[cid / 8] |= 0x80 >> (cid & 7);
}
return 0;
}
private int
pdf_mark_text_glyphs_unencoded(const gs_text_enum_t *penum, const gs_string *pstr,
byte *glyph_usage, int char_cache_size)
{
int i;
for(i = 0; i < pstr->size; i++) {
byte ch = pstr->data[i];
if (ch >= char_cache_size)
return_error(gs_error_rangecheck);
glyph_usage[ch / 8] |= 0x80 >> (ch & 7);
}
return 0;
}
int
pdf_obtain_font_resource(pdf_text_enum_t *penum,
const gs_string *pstr, pdf_font_resource_t **ppdfont)
{
gx_device_pdf *pdev = (gx_device_pdf *)penum->dev;
gs_font *font = (gs_font *)penum->current_font;
byte *glyph_usage = 0;
double *real_widths;
int char_cache_size, width_cache_size;
int code;
if (font->FontType == ft_composite) {
return_error(gs_error_unregistered);
}
code = pdf_attached_font_resource(pdev, font, ppdfont,
&glyph_usage, &real_widths, &char_cache_size, &width_cache_size);
if (code < 0)
return code;
if (penum->cgp == NULL) {
code = pdf_alloc_text_glyphs_table(pdev, penum, pstr);
if (code < 0)
return code;
code = pdf_make_text_glyphs_table(penum, pstr,
glyph_usage, char_cache_size);
if (code < 0)
return code;
}
code = pdf_obtain_font_resource_encoded(pdev, font, ppdfont, penum->cgp);
if (code < 0)
return code;
code = pdf_attached_font_resource(pdev, font, ppdfont,
&glyph_usage, &real_widths, &char_cache_size, &width_cache_size);
if (code < 0)
return code;
return pdf_mark_text_glyphs((const gs_text_enum_t *)penum, pstr, glyph_usage, char_cache_size);
}
int
pdf_obtain_font_resource_unencoded(pdf_text_enum_t *penum,
const gs_string *pstr, pdf_font_resource_t **ppdfont, const gs_glyph *gdata)
{
gx_device_pdf *pdev = (gx_device_pdf *)penum->dev;
gs_font *font = (gs_font *)penum->current_font;
byte *glyph_usage = 0;
double *real_widths = 0;
int char_cache_size = 0, width_cache_size = 0;
int code, ps_encoding_index;
if (font->FontType == ft_composite) {
return_error(gs_error_unregistered);
}
code = pdf_attached_font_resource(pdev, font, ppdfont,
&glyph_usage, &real_widths, &char_cache_size, &width_cache_size);
if (code < 0)
return code;
if (penum->cgp == NULL) {
code = pdf_alloc_text_glyphs_table(pdev, penum, pstr);
if (code < 0)
return code;
code = pdf_make_text_glyphs_table_unencoded(penum->cgp, font, pstr, gdata,
&ps_encoding_index);
if (code < 0)
return code;
}
code = pdf_obtain_font_resource_encoded(pdev, font, ppdfont, penum->cgp);
if (code < 0)
return code;
code = pdf_attached_font_resource(pdev, font, ppdfont,
&glyph_usage, &real_widths, &char_cache_size, &width_cache_size);
if (code < 0)
return code;
return pdf_mark_text_glyphs_unencoded((const gs_text_enum_t *)penum,
pstr, glyph_usage, char_cache_size);
}
private inline bool
strings_equal(const gs_const_string *s1, const gs_const_string *s2)
{
return s1->size == s2->size &&
!memcmp(s1->data, s2->data, s1->size);
}
int
pdf_obtain_parent_type0_font_resource(gx_device_pdf *pdev, pdf_font_resource_t *pdsubf,
const gs_const_string *CMapName, pdf_font_resource_t **pdfont)
{
if (pdsubf->u.cidfont.parent != 0 &&
strings_equal(CMapName, &pdsubf->u.cidfont.parent->u.type0.CMapName))
*pdfont = pdsubf->u.cidfont.parent;
else {
if (pdsubf->u.cidfont.parent == NULL ||
pdf_find_type0_font_resource(pdev, pdsubf, CMapName, pdfont) <= 0) {
int code = pdf_font_type0_alloc(pdev, pdfont, gs_no_id, pdsubf, CMapName);
if (code < 0)
return code;
}
pdsubf->u.cidfont.parent = *pdfont;
}
return 0;
}
private int
transform_delta_inverse(const gs_point *pdelta, const gs_matrix *pmat,
gs_point *ppt)
{
int code = gs_distance_transform_inverse(pdelta->x, pdelta->y, pmat, ppt);
gs_point delta;
if (code < 0)
return code;
if (ppt->y == 0)
return 0;
code = gs_distance_transform(ppt->x, 0.0, pmat, &delta);
if (code < 0)
return 0;
if (fabs(delta.x - pdelta->x) < 0.01 && fabs(delta.y - pdelta->y) < 0.01) {
ppt->y = 0;
}
return 0;
}
int
pdf_update_text_state(pdf_text_process_state_t *ppts,
const pdf_text_enum_t *penum,
pdf_font_resource_t *pdfont, const gs_matrix *pfmat)
{
gx_device_pdf *const pdev = (gx_device_pdf *)penum->dev;
gs_font *font = penum->current_font;
gs_fixed_point cpt;
gs_matrix orig_matrix, smat, tmat;
double
sx = pdev->HWResolution[0] / 72.0,
sy = pdev->HWResolution[1] / 72.0;
float size;
float c_s = 0, w_s = 0;
int mask = 0;
int code = gx_path_current_point(penum->path, &cpt);
if (code < 0)
return code;
{
gs_font_base *cfont = pdf_font_resource_font(pdfont, false);
if (pdfont->FontType == ft_user_defined)
orig_matrix = pdfont->u.simple.s.type3.FontMatrix;
else if (cfont != 0) {
orig_matrix = cfont->FontMatrix;
} else {
pdf_font_orig_matrix(font, &orig_matrix);
}
}
gs_matrix_invert(&orig_matrix, &smat);
gs_matrix_multiply(&smat, pfmat, &smat);
tmat = ctm_only(penum->pis);
tmat.tx = tmat.ty = 0;
gs_matrix_multiply(&smat, &tmat, &tmat);
size = hypot(tmat.yx, tmat.yy) / sy;
if (size < 0.01)
size = hypot(tmat.xx, tmat.xy) / sx;
if (size < 0.01)
size = 1;
if (penum->text.operation & TEXT_ADD_TO_ALL_WIDTHS) {
if (penum->current_font->WMode == 0) {
gs_point pt;
code = transform_delta_inverse(&penum->text.delta_all, &smat, &pt);
if (code >= 0 && pt.y == 0)
c_s = pt.x * size;
else
mask |= TEXT_ADD_TO_ALL_WIDTHS;
}
else
mask |= TEXT_ADD_TO_ALL_WIDTHS;
}
if (penum->text.operation & TEXT_ADD_TO_SPACE_WIDTH) {
gs_point pt;
code = transform_delta_inverse(&penum->text.delta_space, &smat, &pt);
if (code >= 0 && pt.y == 0 && penum->text.space.s_char == 32)
w_s = pt.x * size;
else
mask |= TEXT_ADD_TO_SPACE_WIDTH;
}
tmat.xx /= size;
tmat.xy /= size;
tmat.yx /= size;
tmat.yy /= size;
tmat.tx += fixed2float(cpt.x);
tmat.ty += fixed2float(cpt.y);
ppts->values.character_spacing = c_s;
ppts->values.pdfont = pdfont;
ppts->values.size = size;
ppts->values.matrix = tmat;
ppts->values.render_mode = (penum->pis->text_rendering_mode == 3 ? 3 :
font->PaintType == 0 ? 0 : 1);
ppts->values.word_spacing = w_s;
ppts->font = font;
code = pdf_set_text_process_state(pdev, (const gs_text_enum_t *)penum,
ppts);
return (code < 0 ? code : mask);
}
private double
font_matrix_scaling(const gs_font *font)
{
return fabs((font->FontMatrix.yy != 0 ? font->FontMatrix.yy :
font->FontMatrix.yx));
}
int
pdf_set_text_process_state(gx_device_pdf *pdev,
const gs_text_enum_t *pte,
pdf_text_process_state_t *ppts)
{
if (pdf_render_mode_uses_stroke(pdev, &ppts->values)) {
gs_imager_state *pis = pte->pis;
float save_width = pis->line_params.half_width;
const gs_font *font = ppts->font;
double scaled_width = font->StrokeWidth;
int code;
scaled_width *= font_matrix_scaling(font);
scaled_width *= min(hypot(pte->pis->ctm.xx, pte->pis->ctm.yx) /
pdev->HWResolution[0] * pdev->HWResolution[1],
hypot(pte->pis->ctm.xy, pte->pis->ctm.yy));
pis->line_params.half_width = scaled_width / 2;
code = pdf_prepare_stroke(pdev, pis);
if (code >= 0) {
double scale = 72.0 / pdev->HWResolution[1];
code = gdev_vector_prepare_stroke((gx_device_vector *)pdev,
pis, NULL, NULL, scale);
}
pis->line_params.half_width = save_width;
if (code < 0)
return code;
}
return pdf_set_text_state_values(pdev, &ppts->values);
}
private int
store_glyph_width(pdf_glyph_width_t *pwidth, int wmode, double scale,
const gs_glyph_info_t *pinfo)
{
double w, v;
pwidth->xy.x = pinfo->width[wmode].x * scale;
pwidth->xy.y = pinfo->width[wmode].y * scale;
if (wmode)
w = pwidth->xy.y, v = pwidth->xy.x;
else
w = pwidth->xy.x, v = pwidth->xy.y;
if (v != 0)
return 1;
pwidth->w = w;
pwidth->v.x = pinfo->v.x * scale;
pwidth->v.y = pinfo->v.y * scale;
return 0;
}
private int
get_missing_width(gs_font_base *cfont, int wmode, double scale_c,
pdf_glyph_widths_t *pwidths)
{
gs_font_info_t finfo;
int code;
code = cfont->procs.font_info((gs_font *)cfont, NULL,
FONT_INFO_MISSING_WIDTH, &finfo);
if (code < 0)
return code;
if (wmode) {
pwidths->Width.xy.x = pwidths->real_width.xy.x = 0;
pwidths->Width.xy.y = pwidths->real_width.xy.y =
- finfo.MissingWidth * scale_c;
pwidths->Width.w = pwidths->real_width.w =
pwidths->Width.xy.y;
pwidths->Width.v.x = - pwidths->Width.xy.y / 2;
pwidths->Width.v.y = - pwidths->Width.xy.y;
} else {
pwidths->Width.xy.x = pwidths->real_width.xy.x =
finfo.MissingWidth * scale_c;
pwidths->Width.w = pwidths->real_width.w =
pwidths->Width.xy.x;
pwidths->Width.xy.y = pwidths->real_width.xy.y = 0;
pwidths->Width.v.x = pwidths->Width.v.y = 0;
}
return 1;
}
int
pdf_glyph_widths(pdf_font_resource_t *pdfont, int wmode, gs_glyph glyph,
gs_font *orig_font, pdf_glyph_widths_t *pwidths,
const double cdevproc_result[10])
{
gs_font_base *cfont = pdf_font_resource_font(pdfont, false);
gs_font *ofont = orig_font;
gs_glyph_info_t info;
double sxc, sxo;
double scale_c, scale_o;
int code, rcode = 0;
gs_point v;
int allow_cdevproc_callout = (orig_font->FontType == ft_CID_TrueType
|| orig_font->FontType == ft_CID_encrypted
? GLYPH_INFO_CDEVPROC : 0);
if (ofont->FontType == ft_composite)
return_error(gs_error_unregistered);
code = font_orig_scale((const gs_font *)cfont, &sxc);
if (code < 0)
return code;
code = font_orig_scale(ofont, &sxo);
if (code < 0)
return code;
scale_c = sxc * 1000.0;
scale_o = sxo * 1000.0;
pwidths->Width.v.x = pwidths->Width.v.y = 0;
pwidths->real_width.v.x = pwidths->real_width.v.y = 0;
pwidths->replaced_v = false;
if (glyph == GS_NO_GLYPH)
return get_missing_width(cfont, wmode, scale_c, pwidths);
code = cfont->procs.glyph_info((gs_font *)cfont, glyph, NULL,
GLYPH_INFO_WIDTH0 |
(GLYPH_INFO_WIDTH0 << wmode) |
GLYPH_INFO_OUTLINE_WIDTHS |
(GLYPH_INFO_VVECTOR0 << wmode),
&info);
if (code == gs_error_undefined || !(info.members & (GLYPH_INFO_WIDTH0 << wmode))) {
code = get_missing_width(cfont, wmode, scale_c, pwidths);
if (code < 0)
v.y = 0;
else
v.y = pwidths->Width.v.y;
if (wmode && pdf_is_CID_font(ofont)) {
pdf_glyph_widths_t widths1;
if (get_missing_width(cfont, 0, scale_c, &widths1) < 0)
v.x = 0;
else
v.x = widths1.Width.w / 2;
} else
v.x = pwidths->Width.v.x;
} else if (code < 0)
return code;
else {
code = store_glyph_width(&pwidths->Width, wmode, scale_c, &info);
if (code < 0)
return code;
rcode |= code;
if (info.members & (GLYPH_INFO_VVECTOR0 << wmode)) {
v.y = info.v.y * scale_c;
} else
v.y = 0;
if (wmode && pdf_is_CID_font(ofont)) {
if (info.members & (GLYPH_INFO_WIDTH0 << wmode)) {
v.x = info.width[0].x * scale_c / 2;
} else {
pdf_glyph_widths_t widths1;
if (get_missing_width(cfont, 0, scale_c, &widths1) < 0)
v.x = 0;
else
v.x = widths1.Width.w / 2;
}
} else {
if (info.members  & (GLYPH_INFO_VVECTOR0 << wmode)) {
v.x = info.v.x * scale_c;
} else
v.x = 0;
}
}
pwidths->Width.v = v;
#if 0
if (code > 0)
pwidths->Width.xy.x = pwidths->Width.xy.y = pwidths->Width.w = 0;
#else
if (code > 0 && !pdf_is_CID_font(ofont))
pwidths->Width.xy.x = pwidths->Width.xy.y = pwidths->Width.w = 0;
#endif
if (cdevproc_result == NULL) {
code = ofont->procs.glyph_info(ofont, glyph, NULL,
(GLYPH_INFO_WIDTH0 << wmode) |
(GLYPH_INFO_VVECTOR0 << wmode) |
allow_cdevproc_callout,
&info);
if (info.members & GLYPH_INFO_CDEVPROC) {
if (allow_cdevproc_callout)
return TEXT_PROCESS_CDEVPROC;
else
return_error(gs_error_rangecheck);
}
} else {
info.width[0].x = cdevproc_result[0];
info.width[0].y = cdevproc_result[1];
info.width[1].x = cdevproc_result[6];
info.width[1].y = cdevproc_result[7];
info.v.x = (wmode ? cdevproc_result[8] : 0);
info.v.y = (wmode ? cdevproc_result[9] : 0);
info.members = (GLYPH_INFO_WIDTH0 << wmode) |
(wmode ? GLYPH_INFO_VVECTOR1 : 0);
code = 0;
}
if (code == gs_error_undefined || !(info.members & (GLYPH_INFO_WIDTH0 << wmode)))
pwidths->real_width = pwidths->Width;
else if (code < 0)
return code;
else {
if ((info.members & (GLYPH_INFO_VVECTOR0 | GLYPH_INFO_VVECTOR1)) != 0)
pwidths->replaced_v = true;
else
info.v.x = info.v.y = 0;
code = store_glyph_width(&pwidths->real_width, wmode, scale_o, &info);
if (code < 0)
return code;
rcode |= code;
pwidths->real_width.v.x = info.v.x * scale_o;
pwidths->real_width.v.y = info.v.y * scale_o;
}
return rcode;
}
int
pdf_default_text_begin(gs_text_enum_t *pte, const gs_text_params_t *text,
gs_text_enum_t **ppte)
{
gs_text_params_t text1 = *text;
if(pte->current_font->FontType == 3 && (text1.operation & TEXT_DO_NONE)) {
text1.operation &= ~TEXT_DO_NONE;
text1.operation |= TEXT_DO_DRAW;
}
return gx_default_text_begin(pte->dev, pte->pis, &text1, pte->current_font,
pte->path, pte->pdcolor, pte->pcpath,
pte->memory, ppte);
}
int
pdf_text_process(gs_text_enum_t *pte)
{
pdf_text_enum_t *const penum = (pdf_text_enum_t *)pte;
uint operation = pte->text.operation;
uint size = pte->text.size - pte->index;
gs_text_enum_t *pte_default;
PROCESS_TEXT_PROC((*process));
int code;
gx_device_pdf *pdev = (gx_device_pdf *)penum->dev;
#define BUF_SIZE 100
union bu_ {
byte bytes[BUF_SIZE];
gs_char chars[BUF_SIZE / sizeof(gs_char)];
gs_glyph glyphs[BUF_SIZE / sizeof(gs_glyph)];
} buf;
if (!penum->pte_default && !penum->charproc_accum) {
code = pdf_prepare_text_drawing(pdev, pte);
if (code == gs_error_rangecheck) {
goto default_impl;
}
if (penum->outer_CID != GS_NO_GLYPH) {
goto default_impl;
}
if (code < 0)
return code;
}
if (!penum->pte_default) {
pdev->charproc_just_accumulated = false;
if (penum->cdevproc_callout) {
penum->current_font = penum->orig_font;
}
}
code = -1;
top:
pte_default = penum->pte_default;
if (pte_default) {
if (penum->charproc_accum) {
code = pdf_end_charproc_accum(pdev, penum->current_font, penum->cgp);
if (code < 0)
return code;
penum->charproc_accum = false;
code = gx_default_text_restore_state(pte_default);
if (code < 0)
return code;
gs_text_release(pte_default, "pdf_text_process");
penum->pte_default = 0;
goto top;
}
pdev->pte = pte_default;
code = gs_text_process(pte_default);
pdev->pte = NULL;
if (pte->orig_font->FontType != ft_user_defined)
gs_text_enum_copy_dynamic(pte, pte_default, true);
else {
penum->returned.current_char = pte_default->returned.current_char;
penum->returned.current_glyph = pte_default->returned.current_glyph;
}
pdev->charproc_just_accumulated = false;
if (code == TEXT_PROCESS_RENDER) {
pdev->charproc_ctm = penum->pis->ctm;
if (penum->current_font->FontType == ft_user_defined &&
penum->orig_font->FontType != ft_composite &&
penum->outer_CID == GS_NO_GLYPH &&
!(penum->pte_default->text.operation & TEXT_DO_CHARWIDTH)) {
gs_matrix m;
code = pdf_start_charproc_accum(pdev);
if (code < 0)
return code;
pdf_viewer_state_from_imager_state(pdev, pte->pis, pte->pdcolor);
pdev->state.line_params.half_width = -1;
pdev->state.line_params.cap = gs_cap_unknown;
pdev->state.line_params.join = gs_join_unknown;
pdev->state.line_params.miter_limit = -1;
pdev->state.line_params.dash.pattern_size = -1;
gs_make_identity(&m);
gs_matrix_fixed_from_matrix(&penum->pis->ctm, &m);
return TEXT_PROCESS_RENDER;
}
}
if (code)
return code;
gs_text_release(pte_default, "pdf_text_process");
penum->pte_default = 0;
return 0;
}
{
gs_font *font = pte->orig_font;
switch (font->FontType) {
case ft_CID_encrypted:
case ft_CID_TrueType:
process = process_cid_text;
break;
case ft_encrypted:
case ft_encrypted2:
case ft_TrueType:
case ft_user_defined:
process = process_plain_text;
break;
case ft_composite:
process =
(((gs_font_type0 *)font)->data.FMapType == fmap_CMap ?
process_cmap_text :
process_composite_text);
break;
default:
goto skip;
}
}
if (operation & (TEXT_FROM_STRING | TEXT_FROM_BYTES))
DO_NOTHING;
else if (operation & TEXT_FROM_CHARS)
size *= sizeof(gs_char);
else if (operation & TEXT_FROM_SINGLE_CHAR)
size = sizeof(gs_char);
else if (operation & TEXT_FROM_GLYPHS)
size *= sizeof(gs_glyph);
else if (operation & TEXT_FROM_SINGLE_GLYPH)
size = sizeof(gs_glyph);
else
goto skip;
if (size <= sizeof(buf)) {
code = process(pte, buf.bytes, size);
} else {
byte *buf = gs_alloc_string(pte->memory, size, "pdf_text_process");
if (buf == 0)
return_error(gs_error_VMerror);
code = process(pte, buf, size);
gs_free_string(pte->memory, buf, size, "pdf_text_process");
}
skip:
if (code < 0 ||
(pte->current_font->FontType == ft_user_defined &&
code != TEXT_PROCESS_INTERVENE &&
penum->index < penum->text.size)) {
if (code == gs_error_unregistered)
return code;
if (code == gs_error_VMerror)
return code;
default_impl:
code = pdf_default_text_begin(pte, &pte->text, &pte_default);
if (code < 0)
return code;
penum->pte_default = pte_default;
gs_text_enum_copy_dynamic(pte_default, pte, false);
}
if (penum->pte_default && !code)
goto top;
return code;
}