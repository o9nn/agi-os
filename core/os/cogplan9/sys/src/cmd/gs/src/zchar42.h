#ifndef zchar42_INCLUDED
#  define zchar42_INCLUDED
int zchar42_set_cache(i_ctx_t *i_ctx_p, gs_font_base *pbfont, ref *cnref,
uint glyph_index, op_proc_t cont, op_proc_t *exec_cont, bool put_lsb);
#endif