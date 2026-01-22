#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "ierrors.h"
#include "opcheck.h"
#include "gsparam.h"
#include "gsutil.h"
#include "idict.h"
#include "imemory.h"
#include "iutil.h"
#include "iutil2.h"
int
param_read_password(gs_param_list * plist, const char *kstr, password * ppass)
{
gs_param_string ps;
long ipass;
int code;
ps.data = (const byte *)ppass->data, ps.size = ppass->size,
ps.persistent = false;
code = param_read_string(plist, kstr, &ps);
switch (code) {
case 0:
if (ps.size > MAX_PASSWORD)
return_error(e_limitcheck);
memcpy(ppass->data, ps.data, ps.size);
ppass->size = ps.size;
return 0;
case 1:
return 1;
}
if (code != e_typecheck)
return code;
code = param_read_long(plist, kstr, &ipass);
if (code != 0)
return code;
sprintf((char *)ppass->data, "%ld", ipass);
ppass->size = strlen((char *)ppass->data);
return 0;
}
int
param_write_password(gs_param_list * plist, const char *kstr,
const password * ppass)
{
gs_param_string ps;
ps.data = (const byte *)ppass->data, ps.size = ppass->size,
ps.persistent = false;
if (ps.size > MAX_PASSWORD)
return_error(e_limitcheck);
return param_write_string(plist, kstr, &ps);
}
int
param_check_password(gs_param_list * plist, const password * ppass)
{
if (ppass->size != 0) {
password pass;
int code = param_read_password(plist, "Password", &pass);
if (code)
return code;
if (pass.size != ppass->size ||
bytes_compare(&pass.data[0], pass.size,
&ppass->data[0],
ppass->size) != 0
)
return 1;
}
return 0;
}
private int
dict_find_password(ref ** ppvalue, const ref * pdref, const char *kstr)
{
ref *pvalue;
if (dict_find_string(pdref, kstr, &pvalue) <= 0)
return_error(e_undefined);
if (!r_has_type(pvalue, t_string) ||
r_has_attrs(pvalue, a_read) ||
pvalue->value.const_bytes[0] >= r_size(pvalue)
)
return_error(e_rangecheck);
*ppvalue = pvalue;
return 0;
}
int
dict_read_password(password * ppass, const ref * pdref, const char *pkey)
{
ref *pvalue;
int code = dict_find_password(&pvalue, pdref, pkey);
if (code < 0)
return code;
if (pvalue->value.const_bytes[0] > MAX_PASSWORD)
return_error(e_rangecheck);
memcpy(ppass->data, pvalue->value.const_bytes + 1,
(ppass->size = pvalue->value.const_bytes[0]));
return 0;
}
int
dict_write_password(const password * ppass, ref * pdref, const char *pkey,
bool change_allowed)
{
ref *pvalue;
int code = dict_find_password(&pvalue, pdref, pkey);
if (code < 0)
return code;
if (ppass->size >= r_size(pvalue))
return_error(e_rangecheck);
if (!change_allowed &&
bytes_compare(pvalue->value.bytes + 1, pvalue->value.bytes[0],
ppass->data, ppass->size) != 0)
return_error(e_invalidaccess);
memcpy(pvalue->value.bytes + 1, ppass->data,
(pvalue->value.bytes[0] = ppass->size));
return 0;
}