#ifndef gxfapi_INCLUDED
#  define gxfapi_INCLUDED
void gx_set_UFST_Callbacks(LPUB8 (*p_PCLEO_charptr)(LPUB8 pfont_hdr, UW16  sym_code),
LPUB8 (*p_PCLchId2ptr)(IF_STATE *pIFS, UW16 chId),
LPUB8 (*p_PCLglyphID2Ptr)(IF_STATE *pIFS, UW16 glyphID));
void gx_reset_UFST_Callbacks(void);
#endif