#include "ghost.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gxfcid.h"
#include "bfont.h"
#include "icid.h"
#include "idict.h"
#include "idparam.h"
#include "ifcid.h"
#include "store.h"
int
cid_font_system_info_param(gs_cid_system_info_t *pcidsi, const ref *prfont)
{
ref *prcidsi;
if (dict_find_string(prfont, "CIDSystemInfo", &prcidsi) <= 0)
return_error(e_rangecheck);
return cid_system_info_param(pcidsi, prcidsi);
}
int
cid_font_data_param(os_ptr op, gs_font_cid_data *pdata, ref *pGlyphDirectory)
{
int code;
ref *pgdir;
check_type(*op, t_dictionary);
if ((code = cid_font_system_info_param(&pdata->CIDSystemInfo, op)) < 0 ||
(code = dict_int_param(op, "CIDCount", 0, max_int, -1,
&pdata->CIDCount)) < 0
)
return code;
if (dict_find_string(op, "GlyphDirectory", &pgdir) <= 0) {
make_null(pGlyphDirectory);
return dict_int_param(op, "GDBytes", 1, MAX_GDBytes, 0,
&pdata->GDBytes);
}
if (r_has_type(pgdir, t_dictionary) || r_is_array(pgdir)) {
*pGlyphDirectory = *pgdir;
code = dict_int_param(op, "GDBytes", 0, MAX_GDBytes, 0,
&pdata->GDBytes);
return code;
} else {
return_error(e_typecheck);
}
}