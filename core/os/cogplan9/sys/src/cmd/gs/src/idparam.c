#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "ierrors.h"
#include "gsmatrix.h"
#include "gsuid.h"
#include "idict.h"
#include "idparam.h"
#include "ilevel.h"
#include "imemory.h"
#include "iname.h"
#include "iutil.h"
#include "oper.h"
#include "store.h"
int
dict_bool_param(const ref * pdict, const char *kstr,
bool defaultval, bool * pvalue)
{
ref *pdval;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
*pvalue = defaultval;
return 1;
}
if (!r_has_type(pdval, t_boolean))
return_error(e_typecheck);
*pvalue = pdval->value.boolval;
return 0;
}
int
dict_int_null_param(const ref * pdict, const char *kstr, int minval,
int maxval, int defaultval, int *pvalue)
{
ref *pdval;
int code;
long ival;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
ival = defaultval;
code = 1;
} else {
switch (r_type(pdval)) {
case t_integer:
ival = pdval->value.intval;
break;
case t_real:
if (pdval->value.realval < minval || pdval->value.realval > maxval)
return_error(e_rangecheck);
ival = (long)pdval->value.realval;
if (ival != pdval->value.realval)
return_error(e_rangecheck);
break;
case t_null:
return 2;
default:
return_error(e_typecheck);
}
code = 0;
}
if (ival < minval || ival > maxval)
return_error(e_rangecheck);
*pvalue = (int)ival;
return code;
}
int
dict_int_param(const ref * pdict, const char *kstr, int minval, int maxval,
int defaultval, int *pvalue)
{
int code = dict_int_null_param(pdict, kstr, minval, maxval,
defaultval, pvalue);
return (code == 2 ? gs_note_error(e_typecheck) : code);
}
int
dict_uint_param(const ref * pdict, const char *kstr,
uint minval, uint maxval, uint defaultval, uint * pvalue)
{
ref *pdval;
int code;
uint ival;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
ival = defaultval;
code = 1;
} else {
check_type_only(*pdval, t_integer);
if (pdval->value.intval != (uint) pdval->value.intval)
return_error(e_rangecheck);
ival = (uint) pdval->value.intval;
code = 0;
}
if (ival < minval || ival > maxval)
return_error(e_rangecheck);
*pvalue = ival;
return code;
}
int
dict_float_param(const ref * pdict, const char *kstr,
floatp defaultval, float *pvalue)
{
ref *pdval;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
*pvalue = defaultval;
return 1;
}
switch (r_type(pdval)) {
case t_integer:
*pvalue = (float)pdval->value.intval;
return 0;
case t_real:
*pvalue = pdval->value.realval;
return 0;
}
return_error(e_typecheck);
}
int
dict_int_array_check_param(const ref * pdict, const char *kstr, uint len,
int *ivec, int under_error, int over_error)
{
ref *pdval;
const ref *pa;
int *pi = ivec;
uint size;
int i;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0)
return 0;
if (!r_has_type(pdval, t_array))
return_error(e_typecheck);
size = r_size(pdval);
if (size > len)
return_error(over_error);
pa = pdval->value.const_refs;
for (i = 0; i < size; i++, pa++, pi++) {
switch (r_type(pa)) {
case t_integer:
if (pa->value.intval != (int)pa->value.intval)
return_error(e_rangecheck);
*pi = (int)pa->value.intval;
break;
case t_real:
if (pa->value.realval < min_int ||
pa->value.realval > max_int ||
pa->value.realval != (int)pa->value.realval
)
return_error(e_rangecheck);
*pi = (int)pa->value.realval;
break;
default:
return_error(e_typecheck);
}
}
return (size == len || under_error >= 0 ? size :
gs_note_error(under_error));
}
int
dict_int_array_param(const ref * pdict, const char *kstr,
uint maxlen, int *ivec)
{
return dict_int_array_check_param(pdict, kstr, maxlen, ivec,
0, e_limitcheck);
}
int
dict_ints_param(const ref * pdict, const char *kstr,
uint len, int *ivec)
{
return dict_int_array_check_param(pdict, kstr, len, ivec,
e_rangecheck, e_rangecheck);
}
int
dict_float_array_check_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint len, float *fvec, const float *defaultvec,
int under_error, int over_error)
{
ref *pdval;
uint size;
int code;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
if (defaultvec == NULL)
return 0;
memcpy(fvec, defaultvec, len * sizeof(float));
return len;
}
if (!r_is_array(pdval))
return_error(e_typecheck);
size = r_size(pdval);
if (size > len)
return_error(over_error);
code = process_float_array(mem, pdval, size, fvec);
return (code < 0 ? code :
size == len || under_error >= 0 ? size :
gs_note_error(under_error));
}
int
dict_float_array_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint maxlen, float *fvec, const float *defaultvec)
{
return dict_float_array_check_param(mem ,pdict, kstr, maxlen, fvec,
defaultvec, 0, e_limitcheck);
}
int
dict_floats_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint maxlen, float *fvec, const float *defaultvec)
{
return dict_float_array_check_param(mem, pdict, kstr, maxlen,
fvec, defaultvec,
e_rangecheck, e_rangecheck);
}
int
dict_proc_param(const ref * pdict, const char *kstr, ref * pproc,
bool defaultval)
{
ref *pdval;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0) {
if (defaultval)
make_empty_const_array(pproc, a_readonly + a_executable);
else
make_t(pproc, t__invalid);
return 1;
}
check_proc(*pdval);
*pproc = *pdval;
return 0;
}
int
dict_matrix_param(const gs_memory_t *mem, const ref * pdict, const char *kstr, gs_matrix * pmat)
{
ref *pdval;
if (pdict == 0 || dict_find_string(pdict, kstr, &pdval) <= 0)
return_error(e_typecheck);
return read_matrix(mem, pdval, pmat);
}
int
dict_uid_param(const ref * pdict, gs_uid * puid, int defaultval,
gs_memory_t * mem, const i_ctx_t *i_ctx_p)
{
ref *puniqueid;
if (pdict == 0) {
uid_set_invalid(puid);
return defaultval;
}
if (level2_enabled &&
dict_find_string(pdict, "XUID", &puniqueid) > 0
) {
long *xvalues;
uint size, i;
if (!r_has_type(puniqueid, t_array))
return_error(e_typecheck);
size = r_size(puniqueid);
if (size == 0)
return_error(e_rangecheck);
xvalues = (long *)gs_alloc_byte_array(mem, size, sizeof(long),
"get XUID");
if (xvalues == 0)
return_error(e_VMerror);
for (i = 0; i < size; i++) {
const ref *pvalue = puniqueid->value.const_refs + i;
if (!r_has_type(pvalue, t_integer)) {
gs_free_object(mem, xvalues, "get XUID");
return_error(e_typecheck);
}
xvalues[i] = pvalue->value.intval;
}
uid_set_XUID(puid, xvalues, size);
return 1;
}
if (dict_find_string(pdict, "UniqueID", &puniqueid) <= 0) {
uid_set_invalid(puid);
return defaultval;
} else {
if (!r_has_type(puniqueid, t_integer) ||
puniqueid->value.intval < 0 ||
puniqueid->value.intval > 0xffffffL
)
return_error(e_rangecheck);
if (puniqueid->value.intval == 0) {
uid_set_invalid(puid);
return defaultval;
} else
uid_set_UniqueID(puid, puniqueid->value.intval);
}
return 0;
}
bool
dict_check_uid_param(const ref * pdict, const gs_uid * puid)
{
ref *puniqueid;
if (uid_is_XUID(puid)) {
uint size = uid_XUID_size(puid);
uint i;
if (dict_find_string(pdict, "XUID", &puniqueid) <= 0)
return false;
if (!r_has_type(puniqueid, t_array) ||
r_size(puniqueid) != size
)
return false;
for (i = 0; i < size; i++) {
const ref *pvalue = puniqueid->value.const_refs + i;
if (!r_has_type(pvalue, t_integer))
return false;
if (pvalue->value.intval != uid_XUID_values(puid)[i])
return false;
}
return true;
} else {
if (dict_find_string(pdict, "UniqueID", &puniqueid) <= 0)
return false;
return (r_has_type(puniqueid, t_integer) &&
puniqueid->value.intval == puid->id);
}
}