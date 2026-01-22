#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gscdefs.h"
#include "gsfunc.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "idict.h"
#include "idparam.h"
#include "ifunc.h"
#include "store.h"
#define MAX_SUB_FUNCTION_DEPTH 3
private int
make_function_proc(i_ctx_t *i_ctx_p, ref *op, gs_function_t *pfn)
{
ref cref;
int code;
code = ialloc_ref_array(&cref, a_executable | a_execute, 2,
".buildfunction");
if (code < 0)
return code;
make_istruct_new(cref.value.refs, a_executable | a_execute, pfn);
make_oper_new(cref.value.refs + 1, 0, zexecfunction);
ref_assign(op, &cref);
return 0;
}
private int
zbuildfunction(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_function_t *pfn;
int code = fn_build_function(i_ctx_p, op, &pfn, imemory);
if (code < 0)
return code;
code = make_function_proc(i_ctx_p, op, pfn);
if (code < 0)
gs_function_free(pfn, true, imemory);
return 0;
}
#ifdef TEST
private int
zscalefunction(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_function_t *pfn;
gs_function_t *psfn;
gs_range_t *ranges;
int code;
uint i;
check_proc(op[-1]);
pfn = ref_function(op - 1);
if (pfn == 0 || !r_is_array(op))
return_error(e_typecheck);
if (r_size(op) != 2 * pfn->params.n)
return_error(e_rangecheck);
ranges = (gs_range_t *)
gs_alloc_byte_array(imemory, pfn->params.n, sizeof(gs_range_t),
"zscalefunction");
if (ranges == 0)
return_error(e_VMerror);
for (i = 0; i < pfn->params.n; ++i) {
ref rval[2];
float val[2];
if ((code = array_get(op, 2 * i, &rval[0])) < 0 ||
(code = array_get(op, 2 * i + 1, &rval[1])) < 0 ||
(code = float_params(rval + 1, 2, val)) < 0)
return code;
ranges[i].rmin = val[0];
ranges[i].rmax = val[1];
}
code = gs_function_make_scaled(pfn, &psfn, ranges, imemory);
gs_free_object(imemory, ranges, "zscalefunction");
if (code < 0 ||
(code = make_function_proc(i_ctx_p, op - 1, psfn)) < 0) {
gs_function_free(psfn, true, imemory);
return code;
}
pop(1);
return 0;
}
#endif
int
zexecfunction(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
if (!r_is_struct(op) ||
!r_has_masked_attrs(op, a_executable | a_execute, a_executable | a_all)
)
return_error(e_typecheck);
{
gs_function_t *pfn = (gs_function_t *) op->value.pstruct;
int m = pfn->params.m, n = pfn->params.n;
int diff = n - (m + 1);
if (diff > 0)
check_ostack(diff);
{
float params[20];
float *in;
float *out;
int code = 0;
if (m + n <= countof(params)) {
in = params;
} else {
in = (float *)ialloc_byte_array(m + n, sizeof(float),
"%execfunction(in/out)");
if (in == 0)
code = gs_note_error(e_VMerror);
}
out = in + m;
if (code < 0 ||
(code = float_params(op - 1, m, in)) < 0 ||
(code = gs_function_evaluate(pfn, in, out)) < 0
)
DO_NOTHING;
else {
if (diff > 0)
push(diff);
else if (diff < 0) {
pop(-diff);
op = osp;
}
code = make_floats(op + 1 - n, out, n);
}
if (in != params)
ifree_object(in, "%execfunction(in)");
return code;
}
}
}
private int
zisencapfunction(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_function_t *pfn;
check_proc(*op);
pfn = ref_function(op);
make_bool(op, pfn != NULL);
return 0;
}
int
fn_build_function(i_ctx_t *i_ctx_p, const ref * op, gs_function_t ** ppfn, gs_memory_t *mem)
{
return fn_build_sub_function(i_ctx_p, op, ppfn, 0, mem);
}
int
fn_build_sub_function(i_ctx_t *i_ctx_p, const ref * op, gs_function_t ** ppfn,
int depth, gs_memory_t *mem)
{
int code, type, i;
gs_function_params_t params;
if (depth > MAX_SUB_FUNCTION_DEPTH)
return_error(e_limitcheck);
check_type(*op, t_dictionary);
code = dict_int_param(op, "FunctionType", 0, max_int, -1, &type);
if (code < 0)
return code;
for (i = 0; i < build_function_type_table_count; ++i)
if (build_function_type_table[i].type == type)
break;
if (i == build_function_type_table_count)
return_error(e_rangecheck);
params.Domain = 0;
params.Range = 0;
code = fn_build_float_array(op, "Domain", true, true, &params.Domain, mem);
if (code < 0)
goto fail;
params.m = code >> 1;
code = fn_build_float_array(op, "Range", false, true, &params.Range, mem);
if (code < 0)
goto fail;
params.n = code >> 1;
return (*build_function_type_table[i].proc)
(i_ctx_p, op, &params, depth + 1, ppfn, mem);
fail:
gs_free_const_object(mem, params.Range, "Range");
gs_free_const_object(mem, params.Domain, "Domain");
return code;
}
int
fn_build_float_array(const ref * op, const char *kstr, bool required,
bool even, const float **pparray, gs_memory_t *mem)
{
ref *par;
int code;
*pparray = 0;
if (dict_find_string(op, kstr, &par) <= 0)
return (required ? gs_note_error(e_rangecheck) : 0);
if (!r_is_array(par))
return_error(e_typecheck);
{
uint size = r_size(par);
float *ptr = (float *)
gs_alloc_byte_array(mem, size, sizeof(float), kstr);
if (ptr == 0)
return_error(e_VMerror);
code = dict_float_array_check_param(mem, op, kstr, size,
ptr, NULL,
0, e_rangecheck);
if (code < 0 || (even && (code & 1) != 0)) {
gs_free_object(mem, ptr, kstr);
return(code < 0 ? code : gs_note_error(e_rangecheck));
}
*pparray = ptr;
}
return code;
}
int
fn_build_float_array_forced(const ref * op, const char *kstr, bool required,
const float **pparray, gs_memory_t *mem)
{
ref *par;
int code;
uint size;
float *ptr;
*pparray = 0;
if (dict_find_string(op, kstr, &par) <= 0)
return (required ? gs_note_error(e_rangecheck) : 0);
if( r_is_array(par) )
size = r_size(par);
else if(r_type(par) == t_integer || r_type(par) == t_real)
size = 1;
else
return_error(e_typecheck);
ptr = (float *)gs_alloc_byte_array(mem, size, sizeof(float), kstr);
if (ptr == 0)
return_error(e_VMerror);
if(r_is_array(par) )
code = dict_float_array_check_param(mem, op, kstr,
size, ptr, NULL,
0, e_rangecheck);
else {
code = dict_float_param(op, kstr, 0., ptr);
if( code == 0 )
code = 1;
}
if (code < 0 ) {
gs_free_object(mem, ptr, kstr);
return code;
}
*pparray = ptr;
return code;
}
gs_function_t *
ref_function(const ref *op)
{
if (r_has_type(op, t_array) &&
r_has_masked_attrs(op, a_executable | a_execute,
a_executable | a_all) &&
r_size(op) == 2 &&
r_has_type_attrs(op->value.refs + 1, t_operator, a_executable) &&
op->value.refs[1].value.opproc == zexecfunction &&
r_is_struct(op->value.refs) &&
r_has_masked_attrs(op->value.refs, a_executable | a_execute,
a_executable | a_all)
)
return (gs_function_t *)op->value.refs->value.pstruct;
return 0;
}
const op_def zfunc_op_defs[] =
{
{"1.buildfunction", zbuildfunction},
#ifdef TEST
{"2.scalefunction", zscalefunction},
#endif
{"1%execfunction", zexecfunction},
{"1.isencapfunction", zisencapfunction},
op_def_end(0)
};