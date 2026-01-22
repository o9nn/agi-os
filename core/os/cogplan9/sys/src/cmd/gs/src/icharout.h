#ifndef icharout_INCLUDED
# define icharout_INCLUDED
int zchar_exec_char_proc(i_ctx_t *);
typedef enum {
metricsNone = 0,
metricsWidthOnly = 1,
metricsSideBearingAndWidth = 2
} metrics_present;
int
zchar_get_metrics(const gs_font_base * pbfont, const ref * pcnref,
double psbw[4]);
int
zchar_get_metrics2(const gs_font_base * pbfont, const ref * pcnref,
double pwv[4]);
bool zchar_get_CDevProc(const gs_font_base * pbfont, ref **ppcdevproc);
int zchar_set_cache(i_ctx_t *i_ctx_p, const gs_font_base * pbfont,
const ref * pcnref, const double psb[2],
const double pwidth[2], const gs_rect * pbbox,
op_proc_t cont, op_proc_t *exec_cont,
const double Metrics2_sbw_default[4]);
int zchar_charstring_data(gs_font *font, const ref *pgref,
gs_glyph_data_t *pgd);
int zchar_enumerate_glyph(const gs_memory_t *mem, const ref *prdict, int *pindex, gs_glyph *pglyph);
#endif