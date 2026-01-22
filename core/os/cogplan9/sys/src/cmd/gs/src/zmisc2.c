#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "oper.h"
#include "estack.h"
#include "iddict.h"
#include "idparam.h"
#include "iparam.h"
#include "dstack.h"
#include "ilevel.h"
#include "iname.h"
#include "iutil2.h"
#include "ivmspace.h"
#include "store.h"
private int set_language_level(i_ctx_t *, int);
private int
zlanguagelevel(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_int(op, LANGUAGE_LEVEL);
return 0;
}
private int
zsetlanguagelevel(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = 0;
check_type(*op, t_integer);
if (op->value.intval != LANGUAGE_LEVEL) {
code = set_language_level(i_ctx_p, (int)op->value.intval);
if (code < 0)
return code;
}
LANGUAGE_LEVEL = op->value.intval;
pop(1);
return code;
}
const op_def zmisc2_op_defs[] =
{
{"0.languagelevel", zlanguagelevel},
{"1.setlanguagelevel", zsetlanguagelevel},
op_def_end(0)
};
private int swap_level_dict(i_ctx_t *i_ctx_p, const char *dict_name);
private int swap_entry(i_ctx_t *i_ctx_p, ref elt[2], ref * pdict,
ref * pdict2);
private int
set_language_level(i_ctx_t *i_ctx_p, int new_level)
{
int old_level = LANGUAGE_LEVEL;
ref *pgdict =
ref_stack_index(&d_stack, ref_stack_count(&d_stack) - 2);
ref *level2dict;
int code = 0;
if (new_level < 1 ||
new_level >
(dict_find_string(systemdict, "ll3dict", &level2dict) > 0 ? 3 : 2)
)
return_error(e_rangecheck);
if (dict_find_string(systemdict, "level2dict", &level2dict) <= 0)
return_error(e_undefined);
while (new_level != old_level) {
switch (old_level) {
case 1: {
ref *pdict;
code = dict_find_string(level2dict, "globaldict", &pdict);
if (code > 0) {
if (!r_has_type(pdict, t_dictionary))
return_error(e_typecheck);
*pgdict = *pdict;
}
imemory->gs_lib_ctx->dict_auto_expand = true;
}
code = swap_level_dict(i_ctx_p, "level2dict");
if (code < 0)
return code;
++old_level;
continue;
case 3:
code = swap_level_dict(i_ctx_p, "ll3dict");
if (code < 0)
return code;
--old_level;
continue;
default:
break;
}
switch (new_level) {
case 1: {
int index = dict_first(pgdict);
ref elt[2];
while ((index = dict_next(pgdict, index, &elt[0])) >= 0)
if (r_has_type(&elt[0], t_name))
name_invalidate_value_cache(imemory, &elt[0]);
*pgdict = *systemdict;
imemory->gs_lib_ctx->dict_auto_expand = false;
}
code = swap_level_dict(i_ctx_p, "level2dict");
break;
case 3:
code = swap_level_dict(i_ctx_p, "ll3dict");
break;
default:
return_error(e_Fatal);
}
break;
}
dict_set_top();
return code;
}
private int
swap_level_dict(i_ctx_t *i_ctx_p, const char *dict_name)
{
ref *pleveldict;
ref rleveldict;
int index;
ref elt[2];
ref *psubdict;
if (dict_find_string(systemdict, dict_name, &pleveldict) <= 0)
return_error(e_undefined);
rleveldict = *pleveldict;
index = dict_first(&rleveldict);
while ((index = dict_next(&rleveldict, index, &elt[0])) >= 0)
if (r_has_type(&elt[1], t_dictionary) &&
dict_find(&elt[1], &elt[0], &psubdict) > 0 &&
obj_eq(imemory, &elt[1], psubdict)
) {
int isub = dict_first(&elt[1]);
ref subelt[2];
int found = dict_find(systemdict, &elt[0], &psubdict);
ref rsubdict;
if (found <= 0)
continue;
rsubdict = *psubdict;
while ((isub = dict_next(&elt[1], isub, &subelt[0])) >= 0)
if (!obj_eq(imemory, &subelt[0], &elt[0])) {
int code = swap_entry(i_ctx_p, subelt, &rsubdict, &elt[1]);
if (code < 0)
return code;
}
} else {
int code = swap_entry(i_ctx_p, elt, systemdict, &rleveldict);
if (code < 0)
return code;
}
return 0;
}
private int
swap_entry(i_ctx_t *i_ctx_p, ref elt[2], ref * pdict, ref * pdict2)
{
ref *pvalue;
ref old_value;
int found = dict_find(pdict, &elt[0], &pvalue);
switch (found) {
default:
case 0:
make_null(&old_value);
break;
case 1:
old_value = *pvalue;
}
{
uint space2 = r_space(pdict2);
int code;
r_set_space(pdict2, avm_local);
idict_put(pdict2, &elt[0], &old_value);
if (r_has_type(&elt[1], t_null)) {
code = idict_undef(pdict, &elt[0]);
if (code == e_undefined &&
r_has_type(&old_value, t_null)
)
code = 0;
} else {
uint space = r_space(pdict);
r_set_space(pdict, avm_local);
code = idict_put(pdict, &elt[0], &elt[1]);
r_set_space(pdict, space);
}
r_set_space(pdict2, space2);
return code;
}
}