#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "gsexit.h"
#include "ghost.h"
#include "oper.h"
#include "imemory.h"
#include "idict.h"
#include "iname.h"
#include "stream.h"
#include "strimpl.h"
#include "sfilter.h"
#include "iscan.h"
#include "iutil.h"
#include "dstack.h"
#include "store.h"
private int access_check(i_ctx_t *, int, bool);
private int convert_to_string(const gs_memory_t *mem, os_ptr, os_ptr);
#define ALT_MIN_LONG (-1L << (arch_sizeof_long * 8 - 1))
#define ALT_MAX_LONG (~(ALT_MIN_LONG))
private const double min_int_real = (ALT_MIN_LONG * 1.0 - 1);
private const double max_int_real = (ALT_MAX_LONG * 1.0 + 1);
#define REAL_CAN_BE_INT(v)\
((v) > min_int_real && (v) < max_int_real)
#define ACCESS_REF(opp)\
(r_has_type(opp, t_dictionary) ? dict_access_ref(opp) : opp)
private int
ztype(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref tnref;
int code = array_get(imemory, op, (long)r_btype(op - 1), &tnref);
if (code < 0)
return code;
if (!r_has_type(&tnref, t_name)) {
check_op(2);
{
const char *sname =
gs_struct_type_name_string(gs_object_type(imemory,
op[-1].value.pstruct));
int code = name_ref(imemory, (const byte *)sname, strlen(sname),
(ref *) (op - 1), 0);
if (code < 0)
return code;
}
r_set_attrs(op - 1, a_executable);
} else {
ref_assign(op - 1, &tnref);
}
pop(1);
return 0;
}
private int
ztypenames(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
static const char *const tnames[] = { REF_TYPE_NAME_STRINGS };
int i;
check_ostack(t_next_index);
for (i = 0; i < t_next_index; i++) {
ref *const rtnp = op + 1 + i;
if (i >= countof(tnames) || tnames[i] == 0)
make_null(rtnp);
else {
int code = name_enter_string(imemory, tnames[i], rtnp);
if (code < 0)
return code;
r_set_attrs(rtnp, a_executable);
}
}
osp += t_next_index;
return 0;
}
private int
zcvlit(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *aop;
check_op(1);
aop = ACCESS_REF(op);
r_clear_attrs(aop, a_executable);
return 0;
}
int
zcvx(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *aop;
uint opidx;
check_op(1);
if (r_has_type(op, t_operator) &&
((opidx = op_index(op)) == 0 ||
op_def_is_internal(op_index_def(opidx)))
)
return_error(e_rangecheck);
aop = ACCESS_REF(op);
r_set_attrs(aop, a_executable);
return 0;
}
private int
zxcheck(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(1);
make_bool(op, (r_has_attr(ACCESS_REF(op), a_executable) ? 1 : 0));
return 0;
}
private int
zexecuteonly(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(1);
if (r_has_type(op, t_dictionary))
return_error(e_typecheck);
return access_check(i_ctx_p, a_execute, true);
}
private int
znoaccess(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(1);
if (r_has_type(op, t_dictionary)) {
if (dict_is_permanent_on_dstack(op) ||
!r_has_attr(dict_access_ref(op), a_write)
)
return_error(e_invalidaccess);
}
return access_check(i_ctx_p, 0, true);
}
int
zreadonly(i_ctx_t *i_ctx_p)
{
return access_check(i_ctx_p, a_readonly, true);
}
private int
zrcheck(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = access_check(i_ctx_p, a_read, false);
if (code >= 0)
make_bool(op, code), code = 0;
return code;
}
private int
zwcheck(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = access_check(i_ctx_p, a_write, false);
if (code >= 0)
make_bool(op, code), code = 0;
return code;
}
int
zcvi(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
float fval;
switch (r_type(op)) {
case t_integer:
return 0;
case t_real:
fval = op->value.realval;
break;
default:
return_op_typecheck(op);
case t_string:
{
ref str, token;
int code;
ref_assign(&str, op);
code = scan_string_token(i_ctx_p, &str, &token);
if (code > 0)
code = gs_note_error(e_syntaxerror);
if (code < 0)
return code;
switch (r_type(&token)) {
case t_integer:
*op = token;
return 0;
case t_real:
fval = token.value.realval;
break;
default:
return_error(e_typecheck);
}
}
}
if (!REAL_CAN_BE_INT(fval))
return_error(e_rangecheck);
make_int(op, (long)fval);
return 0;
}
private int
zcvn(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_read_type(*op, t_string);
return name_from_string(imemory, op, op);
}
int
zcvr(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
case t_integer:
make_real(op, (float)op->value.intval);
case t_real:
return 0;
default:
return_op_typecheck(op);
case t_string:
{
ref str, token;
int code;
ref_assign(&str, op);
code = scan_string_token(i_ctx_p, &str, &token);
if (code > 0)
code = gs_note_error(e_syntaxerror);
if (code < 0)
return code;
switch (r_type(&token)) {
case t_integer:
make_real(op, (float)token.value.intval);
return 0;
case t_real:
*op = token;
return 0;
default:
return_error(e_typecheck);
}
}
}
}
private int
zcvrs(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int radix;
check_type(op[-1], t_integer);
if (op[-1].value.intval < 2 || op[-1].value.intval > 36)
return_error(e_rangecheck);
radix = op[-1].value.intval;
check_write_type(*op, t_string);
if (radix == 10) {
switch (r_type(op - 2)) {
case t_integer:
case t_real:
{
int code = convert_to_string(imemory, op - 2, op);
if (code < 0)
return code;
pop(2);
return 0;
}
default:
return_op_typecheck(op - 2);
}
} else {
ulong ival;
byte digits[sizeof(ulong) * 8];
byte *endp = &digits[countof(digits)];
byte *dp = endp;
switch (r_type(op - 2)) {
case t_integer:
ival = (ulong) op[-2].value.intval;
break;
case t_real:
{
float fval = op[-2].value.realval;
if (!REAL_CAN_BE_INT(fval))
return_error(e_rangecheck);
ival = (ulong) (long)fval;
} break;
default:
return_op_typecheck(op - 2);
}
do {
int dit = ival % radix;
*--dp = dit + (dit < 10 ? '0' : ('A' - 10));
ival /= radix;
}
while (ival);
if (endp - dp > r_size(op))
return_error(e_rangecheck);
memcpy(op->value.bytes, dp, (uint) (endp - dp));
r_set_size(op, endp - dp);
}
op[-2] = *op;
pop(2);
return 0;
}
private int
zcvs(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
check_op(2);
check_write_type(*op, t_string);
code = convert_to_string(imemory, op - 1, op);
if (code >= 0)
pop(1);
return code;
}
const op_def ztype_op_defs[] =
{
{"1cvi", zcvi},
{"1cvlit", zcvlit},
{"1cvn", zcvn},
{"1cvr", zcvr},
{"3cvrs", zcvrs},
{"2cvs", zcvs},
{"1cvx", zcvx},
{"1executeonly", zexecuteonly},
{"1noaccess", znoaccess},
{"1rcheck", zrcheck},
{"1readonly", zreadonly},
{"2.type", ztype},
{"0.typenames", ztypenames},
{"1wcheck", zwcheck},
{"1xcheck", zxcheck},
op_def_end(0)
};
private int
access_check(i_ctx_t *i_ctx_p,
int access,
bool modify)
{
os_ptr op = osp;
ref *aop;
switch (r_type(op)) {
case t_dictionary:
aop = dict_access_ref(op);
if (modify) {
if (!r_has_attrs(aop, access))
return_error(e_invalidaccess);
ref_save(op, aop, "access_check(modify)");
r_clear_attrs(aop, a_all);
r_set_attrs(aop, access);
dict_set_top();
return 0;
}
break;
case t_array:
case t_file:
case t_string:
case t_mixedarray:
case t_shortarray:
case t_astruct:
case t_device:;
if (modify) {
if (!r_has_attrs(op, access))
return_error(e_invalidaccess);
r_clear_attrs(op, a_all);
r_set_attrs(op, access);
return 0;
}
aop = op;
break;
default:
return_op_typecheck(op);
}
return (r_has_attrs(aop, access) ? 1 : 0);
}
private int
convert_to_string(const gs_memory_t *mem, os_ptr op1, os_ptr op)
{
uint len;
const byte *pstr = 0;
int code = obj_cvs(mem, op1, op->value.bytes, r_size(op), &len, &pstr);
if (code < 0) {
if (code == e_rangecheck)
switch (r_btype(op1)) {
case t_oparray:
case t_operator:
if (pstr != 0)
switch (*pstr) {
case '%':
case '.':
case '@':
len = r_size(op);
memcpy(op->value.bytes, pstr, len);
goto ok;
}
}
return code;
}
ok:
*op1 = *op;
r_set_size(op1, len);
return 0;
}