#ifndef ichar1_INCLUDED
#  define ichar1_INCLUDED
#ifndef gs_font_type1_DEFINED
#  define gs_font_type1_DEFINED
typedef struct gs_font_type1_s gs_font_type1;
#endif
int charstring_execchar(i_ctx_t *i_ctx_p, int font_type_mask);
font_proc_glyph_outline(zchar1_glyph_outline);
int zcharstring_outline(gs_font_type1 *pfont, int WMode, const ref *pgref,
const gs_glyph_data_t *pgd,
const gs_matrix *pmat, gx_path *ppath, double sbw[4]);
int
z1_glyph_info(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,
int members, gs_glyph_info_t *info);
int z1_glyph_info_generic(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,
int members, gs_glyph_info_t *info, font_proc_glyph_info((*proc)),
int wmode);
int z1_set_cache(i_ctx_t *i_ctx_p, gs_font_base *pbfont, ref *cnref,
gs_glyph glyph, op_proc_t cont, op_proc_t *exec_cont);
#endif