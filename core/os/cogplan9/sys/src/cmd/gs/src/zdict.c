#include "ghost.h"
#include "oper.h"
#include "iddict.h"
#include "dstack.h"
#include "ilevel.h"
#include "iname.h"
#include "ipacked.h"
#include "ivmspace.h"
#include "store.h"
int
zdict(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
#if arch_sizeof_int < arch_sizeof_long
check_int_leu(*op, max_uint);
#else
if (op->value.intval < 0)
return_error(e_rangecheck);
#endif
return dict_create((uint) op->value.intval, op);
}
private int
zmaxlength(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_dictionary);
check_dict_read(*op);
make_int(op, dict_maxlength(op));
return 0;
}
int
zbegin(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_dictionary);
check_dict_read(*op);
if (dsp == dstop)
return_error(e_dictstackoverflow);
++dsp;
ref_assign(dsp, op);
dict_set_top();
pop(1);
return 0;
}
int
zend(i_ctx_t *i_ctx_p)
{
if (ref_stack_count_inline(&d_stack) == min_dstack_size) {
return_error(e_dictstackunderflow);
}
while (dsp == dsbot) {
ref_stack_pop_block(&d_stack);
}
dsp--;
dict_set_top();
return 0;
}
int
zop_def(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
ref *pvslot;
switch (r_type(op1)) {
case t_name: {
uint nidx = name_index(imemory, op1);
uint htemp;
if_dict_find_name_by_index_top(nidx, htemp, pvslot) {
if (dtop_can_store(op))
goto ra;
}
break;
}
case t_null:
return_error(e_typecheck);
case t__invalid:
return_error(e_stackunderflow);
}
if (!dtop_can_store(op)) {
check_dict_write(*dsp);
return_error(e_invalidaccess);
}
if (dict_find(dsp, op1, &pvslot) <= 0)
return idict_put(dsp, op1, op);
ra:
ref_assign_old_inline(&dsp->value.pdict->values, pvslot, op,
"dict_put(value)");
return 0;
}
int
zdef(i_ctx_t *i_ctx_p)
{
int code = zop_def(i_ctx_p);
if (code >= 0) {
pop(2);
}
return code;
}
private int
zload(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *pvalue;
switch (r_type(op)) {
case t_name:
if ((pvalue = dict_find_name(op)) == 0)
return_error(e_undefined);
ref_assign(op, pvalue);
return 0;
case t_null:
return_error(e_typecheck);
case t__invalid:
return_error(e_stackunderflow);
default: {
uint size = ref_stack_count(&d_stack);
uint i;
for (i = 0; i < size; i++) {
ref *dp = ref_stack_index(&d_stack, i);
check_dict_read(*dp);
if (dict_find(dp, op, &pvalue) > 0) {
ref_assign(op, pvalue);
return 0;
}
}
return_error(e_undefined);
}
}
}
private int
zundef(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(op[-1], t_dictionary);
check_dict_write(op[-1]);
idict_undef(op - 1, op);
pop(2);
return 0;
}
private int
zknown(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
register os_ptr op1 = op - 1;
ref *pvalue;
check_type(*op1, t_dictionary);
check_dict_read(*op1);
make_bool(op1, (dict_find(op1, op, &pvalue) > 0 ? 1 : 0));
pop(1);
return 0;
}
int
zwhere(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref_stack_enum_t rsenum;
check_op(1);
ref_stack_enum_begin(&rsenum, &d_stack);
do {
const ref *const bot = rsenum.ptr;
const ref *pdref = bot + rsenum.size;
ref *pvalue;
while (pdref-- > bot) {
check_dict_read(*pdref);
if (dict_find(pdref, op, &pvalue) > 0) {
push(1);
ref_assign(op - 1, pdref);
make_true(op);
return 0;
}
}
} while (ref_stack_enum_next(&rsenum));
make_false(op);
return 0;
}
int
zcopy_dict(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
int code;
check_type(*op1, t_dictionary);
check_dict_read(*op1);
check_dict_write(*op);
if (!imemory->gs_lib_ctx->dict_auto_expand &&
(dict_length(op) != 0 || dict_maxlength(op) < dict_length(op1))
)
return_error(e_rangecheck);
code = idict_copy(op1, op);
if (code < 0)
return code;
if (!level2_enabled)
r_copy_attrs(dict_access_ref(op), a_write, dict_access_ref(op1));
ref_assign(op1, op);
pop(1);
return 0;
}
private int
zcurrentdict(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
ref_assign(op, dsp);
return 0;
}
private int
zcountdictstack(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint count = ref_stack_count(&d_stack);
push(1);
if (!level2_enabled)
count--;
make_int(op, count);
return 0;
}
private int
zdictstack(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint count = ref_stack_count(&d_stack);
check_write_type(*op, t_array);
if (!level2_enabled)
count--;
return ref_stack_store(&d_stack, op, count, 0, 0, true, idmemory,
"dictstack");
}
private int
zcleardictstack(i_ctx_t *i_ctx_p)
{
while (zend(i_ctx_p) >= 0)
DO_NOTHING;
return 0;
}
private int
zdictcopynew(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
int code;
check_type(*op1, t_dictionary);
check_dict_read(*op1);
check_type(*op, t_dictionary);
check_dict_write(*op);
if (!imemory->gs_lib_ctx->dict_auto_expand)
return_error(e_undefined);
code = idict_copy_new(op1, op);
if (code < 0)
return code;
ref_assign(op1, op);
pop(1);
return 0;
}
private int
zdicttomark(i_ctx_t *i_ctx_p)
{
uint count2 = ref_stack_counttomark(&o_stack);
ref rdict;
int code;
uint idx;
if (count2 == 0)
return_error(e_unmatchedmark);
count2--;
if ((count2 & 1) != 0)
return_error(e_rangecheck);
code = dict_create(count2 >> 1, &rdict);
if (code < 0)
return code;
for (idx = 0; idx < count2; idx += 2) {
code = idict_put(&rdict,
ref_stack_index(&o_stack, idx + 1),
ref_stack_index(&o_stack, idx));
if (code < 0) {
return code;
}
}
ref_stack_pop(&o_stack, count2);
ref_assign(osp, &rdict);
return code;
}
private int
zforceundef(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(op[-1], t_dictionary);
idict_undef(op - 1, op);
pop(2);
return 0;
}
private int
zknownget(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
register os_ptr op1 = op - 1;
ref *pvalue;
check_type(*op1, t_dictionary);
check_dict_read(*op1);
if (dict_find(op1, op, &pvalue) <= 0) {
make_false(op1);
pop(1);
} else {
ref_assign(op1, pvalue);
make_true(op);
}
return 0;
}
private int
zknownundef(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
int code;
check_type(*op1, t_dictionary);
check_dict_write(*op1);
code = idict_undef(op1, op);
make_bool(op1, code == 0);
pop(1);
return 0;
}
private int
zsetmaxlength(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
uint new_size;
int code;
check_type(*op1, t_dictionary);
check_dict_write(*op1);
check_type(*op, t_integer);
#if arch_sizeof_int < arch_sizeof_long
check_int_leu(*op, max_uint);
#else
if (op->value.intval < 0)
return_error(e_rangecheck);
#endif
new_size = (uint) op->value.intval;
if (dict_length(op - 1) > new_size)
return_error(e_dictfull);
code = idict_resize(op - 1, new_size);
if (code >= 0)
pop(2);
return code;
}
const op_def zdict1_op_defs[] = {
{"0cleardictstack", zcleardictstack},
{"1begin", zbegin},
{"0countdictstack", zcountdictstack},
{"0currentdict", zcurrentdict},
{"2def", zdef},
{"1dict", zdict},
{"0dictstack", zdictstack},
{"0end", zend},
{"2known", zknown},
{"1load", zload},
{"1maxlength", zmaxlength},
{"2.undef", zundef},
{"1where", zwhere},
op_def_end(0)
};
const op_def zdict2_op_defs[] = {
{"2.dictcopynew", zdictcopynew},
{"1.dicttomark", zdicttomark},
{"2.forceundef", zforceundef},
{"2.knownget", zknownget},
{"1.knownundef", zknownundef},
{"2.setmaxlength", zsetmaxlength},
op_def_end(0)
};