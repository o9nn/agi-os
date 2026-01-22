#ifndef ifcid_INCLUDED
# define ifcid_INCLUDED
int cid_font_system_info_param(gs_cid_system_info_t *pcidsi,
const ref *prfont);
int cid_font_data_param(os_ptr op, gs_font_cid_data *pdata,
ref *pGlyphDirectory);
#endif