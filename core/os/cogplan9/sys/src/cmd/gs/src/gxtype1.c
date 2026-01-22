#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsccode.h"
#include "gsline.h"
#include "gsstruct.h"
#include "gxarith.h"
#include "gxchrout.h"
#include "gxfixed.h"
#include "gxistate.h"
#include "gxmatrix.h"
#include "gxcoord.h"
#include "gxfont.h"
#include "gxfont1.h"
#include "gxtype1.h"
#include "gzpath.h"
#define FORCE_HINTS_TO_BIG_PIXELS 1
public_st_gs_font_type1();
public_st_gs_type1_state();
private
ENUM_PTRS_WITH(gs_type1_state_enum_ptrs, gs_type1_state *pcis)
{
index -= 4;
if (index < pcis->ips_count * ST_GLYPH_DATA_NUM_PTRS)
return ENUM_USING(st_glyph_data,
&pcis->ipstack[index / ST_GLYPH_DATA_NUM_PTRS].cs_data,
sizeof(pcis->ipstack[0].cs_data),
index % ST_GLYPH_DATA_NUM_PTRS);
return 0;
}
ENUM_PTR3(0, gs_type1_state, pfont, pis, path);
ENUM_PTR(3, gs_type1_state, callback_data);
ENUM_PTRS_END
private RELOC_PTRS_WITH(gs_type1_state_reloc_ptrs, gs_type1_state *pcis)
{
int i;
RELOC_PTR(gs_type1_state, pfont);
RELOC_PTR(gs_type1_state, pis);
RELOC_PTR(gs_type1_state, path);
RELOC_PTR(gs_type1_state, callback_data);
for (i = 0; i < pcis->ips_count; i++) {
ip_state_t *ipsp = &pcis->ipstack[i];
int diff = ipsp->ip - ipsp->cs_data.bits.data;
RELOC_USING(st_glyph_data, &ipsp->cs_data, sizeof(ipsp->cs_data));
ipsp->ip = ipsp->cs_data.bits.data + diff;
}
} RELOC_PTRS_END
const char gx_extendeg_glyph_name_separator[] = "~GS~";
#define s (*ps)
int
gs_type1_interp_init(register gs_type1_state * pcis, gs_imager_state * pis,
gx_path * ppath, const gs_log2_scale_point * pscale,
const gs_log2_scale_point * psubpixels, bool no_grid_fitting,
int paint_type, gs_font_type1 * pfont)
{
static const gs_log2_scale_point no_scale = {0, 0};
const gs_log2_scale_point *plog2_scale =
(FORCE_HINTS_TO_BIG_PIXELS && pscale != NULL ? pscale : &no_scale);
const gs_log2_scale_point *plog2_subpixels =
(FORCE_HINTS_TO_BIG_PIXELS ? (psubpixels != NULL ? psubpixels : plog2_scale) : &no_scale);
if_debug0('1', "[1]gs_type1_interp_init\n");
pcis->pfont = pfont;
pcis->pis = pis;
pcis->path = ppath;
pcis->callback_data = pfont;
pcis->no_grid_fitting = no_grid_fitting;
pcis->paint_type = paint_type;
pcis->os_count = 0;
pcis->ips_count = 1;
pcis->ipstack[0].ip = 0;
gs_glyph_data_from_null(&pcis->ipstack[0].cs_data);
pcis->ignore_pops = 0;
pcis->init_done = -1;
pcis->sb_set = false;
pcis->width_set = false;
pcis->num_hints = 0;
pcis->seac_accent = -1;
pcis->log2_subpixels = *plog2_subpixels;
set_pixel_scale(&pcis->scale.x, plog2_scale->x);
set_pixel_scale(&pcis->scale.y, plog2_scale->y);
return 0;
}
void
gs_type1_set_callback_data(gs_type1_state *pcis, void *callback_data)
{
pcis->callback_data = callback_data;
}
void
gs_type1_set_lsb(gs_type1_state * pcis, const gs_point * psbpt)
{
pcis->lsb.x = float2fixed(psbpt->x);
pcis->lsb.y = float2fixed(psbpt->y);
pcis->sb_set = true;
}
void
gs_type1_set_width(gs_type1_state * pcis, const gs_point * pwpt)
{
pcis->width.x = float2fixed(pwpt->x);
pcis->width.y = float2fixed(pwpt->y);
pcis->width_set = true;
}
void
gs_type1_finish_init(gs_type1_state * pcis)
{
gs_imager_state *pis = pcis->pis;
const int max_coeff_bits = 11;
gx_matrix_to_fixed_coeff(&ctm_only(pis), &pcis->fc, max_coeff_bits);
pcis->origin.x = pcis->path->position.x;
pcis->origin.y = pcis->path->position.y;
pcis->asb_diff = pcis->adxy.x = pcis->adxy.y = 0;
pcis->flex_count = flex_max;
pcis->vs_offset.x = pcis->vs_offset.y = 0;
pcis->flatness = gs_char_flatness(pis, 0.001);
pcis->init_done = 1;
}
#undef s
int
gs_type1_sbw(gs_type1_state * pcis, fixed lsbx, fixed lsby, fixed wx, fixed wy)
{
if (!pcis->sb_set)
pcis->lsb.x = lsbx, pcis->lsb.y = lsby,
pcis->sb_set = true;
if (!pcis->width_set)
pcis->width.x = wx, pcis->width.y = wy,
pcis->width_set = true;
if_debug4('1', "[1]sb=(%g,%g) w=(%g,%g)\n",
fixed2float(pcis->lsb.x), fixed2float(pcis->lsb.y),
fixed2float(pcis->width.x), fixed2float(pcis->width.y));
return 0;
}
int
gs_type1_blend(gs_type1_state *pcis, fixed *csp, int num_results)
{
gs_type1_data *pdata = &pcis->pfont->data;
int num_values = fixed2int_var(csp[-1]);
int k1 = num_values / num_results - 1;
int i, j;
fixed *base;
fixed *deltas;
if (num_values < num_results ||
num_values % num_results != 0
)
return_error(gs_error_invalidfont);
base = csp - 1 - num_values;
deltas = base + num_results - 1;
for (j = 0; j < num_results;
j++, base++, deltas += k1
)
for (i = 1; i <= k1; i++)
*base += (fixed)(deltas[i] *
pdata->WeightVector.values[i]);
pcis->ignore_pops = num_results;
return num_values - num_results + 2;
}
int
gs_type1_seac(gs_type1_state * pcis, const fixed * cstack, fixed asb,
ip_state_t * ipsp)
{
gs_font_type1 *pfont = pcis->pfont;
gs_glyph_data_t bgdata;
gs_const_string gstr;
int code;
pcis->seac_accent = fixed2int_var(cstack[3]);
pcis->save_asb = asb;
pcis->save_lsb = pcis->lsb;
pcis->save_adxy.x = cstack[0];
pcis->save_adxy.y = cstack[1];
pcis->os_count = 0;
code = pfont->data.procs.seac_data
(pfont, fixed2int_var(cstack[2]), NULL, &gstr, &bgdata);
if (code < 0)
return code;
ipsp->cs_data = bgdata;
return 0;
}
int
gs_type1_endchar(gs_type1_state * pcis)
{
gs_imager_state *pis = pcis->pis;
if (pcis->seac_accent >= 0) {
gs_font_type1 *pfont = pcis->pfont;
gs_glyph_data_t agdata;
int achar = pcis->seac_accent;
gs_const_string gstr;
int code;
agdata.memory = pfont->memory;
pcis->seac_accent = -1;
pcis->asb_diff = pcis->save_asb - pcis->save_lsb.x;
pcis->adxy = pcis->save_adxy;
pcis->os_count = 0;
pcis->ips_count = 1;
code = pfont->data.procs.seac_data(pfont, achar, NULL, &gstr, &agdata);
if (code == gs_error_undefined) {
char buf0[gs_font_name_max + 1], buf1[30];
int l0 = min(pcis->pfont->font_name.size, sizeof(buf0) - 1);
int l1 = min(gstr.size, sizeof(buf1) - 1);
memcpy(buf0, pcis->pfont->font_name.chars, l0);
buf0[l0] = 0;
memcpy(buf1, gstr.data, l1);
buf1[l1] = 0;
eprintf2("The font '%s' misses the glyph '%s' . Continue skipping the glyph.", buf0, buf1);
return 0;
}
if (code < 0)
return code;
pcis->ips_count = 1;
pcis->ipstack[0].cs_data = agdata;
return 1;
}
if (pcis->pfont->PaintType == 0)
pis->fill_adjust.x = pis->fill_adjust.y = -1;
if (!pcis->no_grid_fitting)
gs_imager_setflat(pis, pcis->flatness);
return 0;
}
void
type1_cis_get_metrics(const gs_type1_state * pcis, double psbw[4])
{
psbw[0] = fixed2float(pcis->lsb.x);
psbw[1] = fixed2float(pcis->lsb.y);
psbw[2] = fixed2float(pcis->width.x);
psbw[3] = fixed2float(pcis->width.y);
}
int
gs_type1_piece_codes( gs_font_type1 *pfont,
const gs_glyph_data_t *pgd, gs_char *chars)
{
gs_type1_data *const pdata = &pfont->data;
bool encrypted = pdata->lenIV >= 0;
fixed cstack[ostack_size];
fixed *csp;
ip_state_t ipstack[ipstack_size + 1];
ip_state_t *ipsp = &ipstack[0];
const byte *cip;
crypt_state state;
int c;
int code;
CLEAR_CSTACK(cstack, csp);
cip = pgd->bits.data;
call:
state = crypt_charstring_seed;
if (encrypted) {
int skip = pdata->lenIV;
for (; skip > 0; ++cip, --skip)
decrypt_skip_next(*cip, state);
}
top:
for (;;) {
uint c0 = *cip++;
charstring_next(c0, state, c, encrypted);
if (c >= c_num1) {
if (c < c_pos2_0) {
decode_push_num1(csp, cstack, c);
} else if (c < cx_num4) {
decode_push_num2(csp, cstack, c, cip, state, encrypted);
} else if (c == cx_num4) {
long lw;
decode_num4(lw, cip, state, encrypted);
CS_CHECK_PUSH(csp, cstack);
*++csp = int2fixed(lw);
} else
return_error(gs_error_invalidfont);
continue;
}
#define cnext CLEAR_CSTACK(cstack, csp); goto top
switch ((char_command) c) {
default:
goto out;
case c_callsubr:
c = fixed2int_var(*csp) + pdata->subroutineNumberBias;
code = pdata->procs.subr_data
(pfont, c, false, &ipsp[1].cs_data);
if (code < 0)
return_error(code);
--csp;
ipsp->ip = cip, ipsp->dstate = state;
++ipsp;
cip = ipsp->cs_data.bits.data;
goto call;
case c_return:
gs_glyph_data_free(&ipsp->cs_data, "gs_type1_piece_codes");
--ipsp;
cip = ipsp->ip, state = ipsp->dstate;
goto top;
case cx_hstem:
case cx_vstem:
case c1_hsbw:
cnext;
case cx_endchar:
if (csp < cstack + 3)
goto out;
do_seac:
chars[0] = fixed2int(csp[-1]);
chars[1] = fixed2int(csp[0]);
return 1;
case cx_escape:
charstring_next(*cip, state, c, encrypted);
++cip;
switch ((char1_extended_command) c) {
default:
goto out;
case ce1_vstem3:
case ce1_hstem3:
case ce1_sbw:
cnext;
case ce1_pop:
goto top;
case ce1_seac:
goto do_seac;
case ce1_callothersubr:
switch (fixed2int_var(*csp)) {
default:
goto out;
case 3:
csp -= 2;
goto top;
case 12:
case 13:
case 14:
case 15:
case 16:
case 17:
case 18:
cnext;
}
}
}
#undef cnext
}
out:
return 0;
}
private int
gs_type1_glyph_pieces(gs_font_type1 *pfont, const gs_glyph_data_t *pgd,
int members, gs_glyph_info_t *info)
{
gs_char chars[2];
gs_glyph glyphs[2];
int code = gs_type1_piece_codes(pfont, pgd, chars);
gs_type1_data *const pdata = &pfont->data;
gs_glyph *pieces =
(members & GLYPH_INFO_PIECES ? info->pieces : glyphs);
gs_const_string gstr;
int acode, bcode;
info->num_pieces = 0;
if (code <= 0)
return code;
bcode = pdata->procs.seac_data(pfont, chars[0], &pieces[0], &gstr, NULL);
acode = pdata->procs.seac_data(pfont, chars[1], &pieces[1], &gstr, NULL);
code = (bcode < 0 ? bcode : acode);
info->num_pieces = 2;
return code;
}
int
gs_type1_glyph_info(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,
int members, gs_glyph_info_t *info)
{
gs_font_type1 *const pfont = (gs_font_type1 *)font;
gs_type1_data *const pdata = &pfont->data;
int wmode = ((members & GLYPH_INFO_WIDTH1) != 0);
int piece_members = members & (GLYPH_INFO_NUM_PIECES | GLYPH_INFO_PIECES);
int width_members = (members & ((GLYPH_INFO_WIDTH0 << wmode) | (GLYPH_INFO_VVECTOR0 << wmode)));
int default_members = members & ~(piece_members | GLYPH_INFO_WIDTHS |
GLYPH_INFO_VVECTOR0 | GLYPH_INFO_VVECTOR1 |
GLYPH_INFO_OUTLINE_WIDTHS);
int code = 0;
gs_glyph_data_t gdata;
if (default_members) {
code = gs_default_glyph_info(font, glyph, pmat, default_members, info);
if (code < 0)
return code;
} else
info->members = 0;
if (default_members == members)
return code;
gdata.memory = pfont->memory;
if ((code = pdata->procs.glyph_data(pfont, glyph, &gdata)) < 0)
return code;
if (piece_members) {
code = gs_type1_glyph_pieces(pfont, &gdata, members, info);
if (code < 0)
return code;
info->members |= piece_members;
}
if (width_members) {
gs_imager_state gis;
gs_type1_state cis;
int value;
if (pmat)
gs_matrix_fixed_from_matrix(&gis.ctm, pmat);
else {
gs_matrix imat;
gs_make_identity(&imat);
gs_matrix_fixed_from_matrix(&gis.ctm, &imat);
}
gis.flatness = 0;
code = gs_type1_interp_init(&cis, &gis, NULL ,
NULL, NULL, true, 0, pfont);
if (code < 0)
return code;
cis.no_grid_fitting = true;
code = pdata->interpret(&cis, &gdata, &value);
switch (code) {
case 0:
code = gs_note_error(gs_error_invalidfont);
default:
return code;
case type1_result_callothersubr:
return_error(gs_error_rangecheck);
case type1_result_sbw:
info->width[wmode].x = fixed2float(cis.width.x);
info->width[wmode].y = fixed2float(cis.width.y);
info->v.x = fixed2float(cis.lsb.x);
info->v.y = fixed2float(cis.lsb.y);
break;
}
info->members |= width_members | (GLYPH_INFO_VVECTOR0 << wmode);
}
gs_glyph_data_free(&gdata, "gs_type1_glyph_info");
return code;
}
const gs_font_base *
gs_font_parent(const gs_font_base *pbfont)
{
if (pbfont->FontType == ft_encrypted || pbfont->FontType == ft_encrypted2) {
const gs_font_type1 *pfont1 = (const gs_font_type1 *)pbfont;
if (pfont1->data.parent != NULL)
return pfont1->data.parent;
}
return pbfont;
}