#ifndef ifont1_INCLUDED
# define ifont1_INCLUDED
typedef struct charstring_font_refs_s {
ref *Private;
ref no_subrs;
ref *OtherSubrs;
ref *Subrs;
ref *GlobalSubrs;
} charstring_font_refs_t;
#define DEFAULT_LENIV_1 4
int charstring_font_get_refs(const_os_ptr op, charstring_font_refs_t *pfr);
int charstring_font_params(const gs_memory_t *mem,
const_os_ptr op, charstring_font_refs_t *pfr,
gs_type1_data *pdata1);
int charstring_font_init(gs_font_type1 *pfont,
const charstring_font_refs_t *pfr,
const gs_type1_data *pdata1);
int build_charstring_font(i_ctx_t *i_ctx_p, os_ptr op,
build_proc_refs * pbuild, font_type ftype,
charstring_font_refs_t *pfr,
gs_type1_data *pdata1,
build_font_options_t options);
#endif