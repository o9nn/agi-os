#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gsstruct.h"
#include "stream.h"
#include "ierrors.h"
#include "estack.h"
#include "ialloc.h"
#include "iastruct.h"
#include "icontext.h"
#include "icremap.h"
#include "idebug.h"
#include "igstate.h"
#include "inamedef.h"
#include "iname.h"
#include "interp.h"
#include "ipacked.h"
#include "ostack.h"
#include "strimpl.h"
#include "sfilter.h"
#include "iscan.h"
#include "iddict.h"
#include "isave.h"
#include "istack.h"
#include "itoken.h"
#include "iutil.h"
#include "ivmspace.h"
#include "dstack.h"
#include "files.h"
#include "oper.h"
#include "store.h"
#include "gpcheck.h"
#define PACKED_SPECIAL_OPS 1
extern_st(st_ref_stack);
public_st_dict_stack();
public_st_exec_stack();
public_st_op_stack();
private int
no_reschedule(i_ctx_t **pi_ctx_p)
{
return_error(e_invalidcontext);
}
int (*gs_interp_reschedule_proc)(i_ctx_t **) = no_reschedule;
int (*gs_interp_time_slice_proc)(i_ctx_t **) = 0;
int gs_interp_time_slice_ticks = 0x7fff;
#ifdef DEBUG
private int
call_operator(op_proc_t op_proc, i_ctx_t *i_ctx_p)
{
int code = op_proc(i_ctx_p);
return code;
}
#else
#  define call_operator(proc, p) ((*(proc))(p))
#endif
#ifdef DEBUG
struct stats_interp_s {
long top;
long lit, lit_array, exec_array, exec_operator, exec_name;
long x_add, x_def, x_dup, x_exch, x_if, x_ifelse,
x_index, x_pop, x_roll, x_sub;
long find_name, name_lit, name_proc, name_oparray, name_operator;
long p_full, p_exec_operator, p_exec_oparray, p_exec_non_x_operator,
p_integer, p_lit_name, p_exec_name;
long p_find_name, p_name_lit, p_name_proc;
} stats_interp;
# define INCR(v) (++(stats_interp.v))
#else
# define INCR(v) DO_NOTHING
#endif
private int estack_underflow(i_ctx_t *);
private int interp(i_ctx_t **, const ref *, ref *);
private int interp_exit(i_ctx_t *);
private void set_gc_signal(i_ctx_t *, int *, int);
private int copy_stack(i_ctx_t *, const ref_stack_t *, ref *);
private int oparray_pop(i_ctx_t *);
private int oparray_cleanup(i_ctx_t *);
private int zsetstackprotect(i_ctx_t *);
private int zcurrentstackprotect(i_ctx_t *);
#ifndef MAX_OSTACK
#  define MAX_OSTACK 800
#endif
#define MIN_BLOCK_OSTACK 16
const int gs_interp_max_op_num_args = MIN_BLOCK_OSTACK;
#ifndef MAX_ESTACK
#  define MAX_ESTACK 5000
#endif
#define MIN_BLOCK_ESTACK 8
#define ES_HEADROOM 20
#ifndef MAX_DSTACK
#  define MAX_DSTACK 20
#endif
#define MIN_BLOCK_DSTACK 3
extern_st(st_ref_stack);
#define OS_GUARD_UNDER 10
#define OS_GUARD_OVER 10
#define OS_REFS_SIZE(body_size)\
(stack_block_refs + OS_GUARD_UNDER + (body_size) + OS_GUARD_OVER)
#define ES_GUARD_UNDER 1
#define ES_GUARD_OVER 10
#define ES_REFS_SIZE(body_size)\
(stack_block_refs + ES_GUARD_UNDER + (body_size) + ES_GUARD_OVER)
#define DS_REFS_SIZE(body_size)\
(stack_block_refs + (body_size))
#define tx_op t_next_index
typedef enum {
tx_op_add = tx_op,
tx_op_def,
tx_op_dup,
tx_op_exch,
tx_op_if,
tx_op_ifelse,
tx_op_index,
tx_op_pop,
tx_op_roll,
tx_op_sub,
tx_next_op
} special_op_types;
#define num_special_ops ((int)tx_next_op - tx_op)
const int gs_interp_num_special_ops = num_special_ops;
const int tx_next_index = tx_next_op;
const op_def interp_op_defs[] = {
op_def_begin_dict("systemdict"),
{"2add", zadd},
{"2def", zdef},
{"1dup", zdup},
{"2exch", zexch},
{"2if", zif},
{"3ifelse", zifelse},
{"1index", zindex},
{"1pop", zpop},
{"2roll", zroll},
{"2sub", zsub},
{"0.currentstackprotect", zcurrentstackprotect},
{"1.setstackprotect", zsetstackprotect},
{"0%interp_exit", interp_exit},
{"0%oparray_pop", oparray_pop},
op_def_end(0)
};
#define make_null_proc(pref)\
make_empty_const_array(pref, a_executable + a_readonly)
int
gs_interp_init(i_ctx_t **pi_ctx_p, const ref *psystem_dict,
gs_dual_memory_t *dmem)
{
gs_context_state_t *pcst = 0;
int code = context_state_alloc(&pcst, psystem_dict, dmem);
if (code >= 0)
code = context_state_load(pcst);
if (code < 0)
lprintf1("Fatal error %d in gs_interp_init!", code);
*pi_ctx_p = pcst;
return code;
}
int
gs_interp_alloc_stacks(gs_ref_memory_t *mem, gs_context_state_t * pcst)
{
gs_ref_memory_t *smem =
(gs_ref_memory_t *)gs_memory_stable((gs_memory_t *)mem);
ref stk;
#define REFS_SIZE_OSTACK OS_REFS_SIZE(MAX_OSTACK)
#define REFS_SIZE_ESTACK ES_REFS_SIZE(MAX_ESTACK)
#define REFS_SIZE_DSTACK DS_REFS_SIZE(MAX_DSTACK)
gs_alloc_ref_array(smem, &stk, 0,
REFS_SIZE_OSTACK + REFS_SIZE_ESTACK +
REFS_SIZE_DSTACK, "gs_interp_alloc_stacks");
{
ref_stack_t *pos = &pcst->op_stack.stack;
r_set_size(&stk, REFS_SIZE_OSTACK);
ref_stack_init(pos, &stk, OS_GUARD_UNDER, OS_GUARD_OVER, NULL,
smem, NULL);
ref_stack_set_error_codes(pos, e_stackunderflow, e_stackoverflow);
ref_stack_set_max_count(pos, MAX_OSTACK);
stk.value.refs += REFS_SIZE_OSTACK;
}
{
ref_stack_t *pes = &pcst->exec_stack.stack;
ref euop;
r_set_size(&stk, REFS_SIZE_ESTACK);
make_oper(&euop, 0, estack_underflow);
ref_stack_init(pes, &stk, ES_GUARD_UNDER, ES_GUARD_OVER, &euop,
smem, NULL);
ref_stack_set_error_codes(pes, e_ExecStackUnderflow,
e_execstackoverflow);
ref_stack_allow_expansion(pes, false);
ref_stack_set_max_count(pes, MAX_ESTACK);
stk.value.refs += REFS_SIZE_ESTACK;
}
{
ref_stack_t *pds = &pcst->dict_stack.stack;
r_set_size(&stk, REFS_SIZE_DSTACK);
ref_stack_init(pds, &stk, 0, 0, NULL, smem, NULL);
ref_stack_set_error_codes(pds, e_dictstackunderflow,
e_dictstackoverflow);
ref_stack_set_max_count(pds, MAX_DSTACK);
}
#undef REFS_SIZE_OSTACK
#undef REFS_SIZE_ESTACK
#undef REFS_SIZE_DSTACK
return 0;
}
void
gs_interp_free_stacks(gs_ref_memory_t * smem, gs_context_state_t * pcst)
{
ref_stack_release(&pcst->dict_stack.stack);
ref_stack_release(&pcst->exec_stack.stack);
ref_stack_release(&pcst->op_stack.stack);
}
void
gs_interp_reset(i_ctx_t *i_ctx_p)
{
ref_stack_clear(&o_stack);
ref_stack_clear(&e_stack);
esp++;
make_oper(esp, 0, interp_exit);
ref_stack_pop_to(&d_stack, min_dstack_size);
dict_set_top();
}
private int
estack_underflow(i_ctx_t *i_ctx_p)
{
return e_ExecStackUnderflow;
}
void
gs_interp_make_oper(ref * opref, op_proc_t proc, int idx)
{
int i;
for (i = num_special_ops; i > 0 && proc != interp_op_defs[i].proc; --i)
DO_NOTHING;
if (i > 0)
make_tasv(opref, tx_op + (i - 1), a_executable, i, opproc, proc);
else
make_tasv(opref, t_operator, a_executable, idx, opproc, proc);
}
int
interp_reclaim(i_ctx_t **pi_ctx_p, int space)
{
i_ctx_t *i_ctx_p = *pi_ctx_p;
gs_gc_root_t ctx_root;
int code;
gs_register_struct_root(imemory_system, &ctx_root,
(void **)pi_ctx_p, "interp_reclaim(pi_ctx_p)");
code = (*idmemory->reclaim)(idmemory, space);
i_ctx_p = *pi_ctx_p;
gs_unregister_root(imemory_system, &ctx_root, "interp_reclaim(pi_ctx_p)");
return code;
}
private int gs_call_interp(i_ctx_t **, ref *, int, int *, ref *);
int
gs_interpret(i_ctx_t **pi_ctx_p, ref * pref, int user_errors, int *pexit_code,
ref * perror_object)
{
i_ctx_t *i_ctx_p = *pi_ctx_p;
gs_gc_root_t error_root;
int code;
gs_register_ref_root(imemory_system, &error_root,
(void **)&perror_object, "gs_interpret");
code = gs_call_interp(pi_ctx_p, pref, user_errors, pexit_code,
perror_object);
i_ctx_p = *pi_ctx_p;
gs_unregister_root(imemory_system, &error_root, "gs_interpret");
set_gc_signal(i_ctx_p, NULL, 0);
return code;
}
private int
gs_call_interp(i_ctx_t **pi_ctx_p, ref * pref, int user_errors,
int *pexit_code, ref * perror_object)
{
ref *epref = pref;
ref doref;
ref *perrordict;
ref error_name;
int code, ccode;
ref saref;
int gc_signal = 0;
i_ctx_t *i_ctx_p = *pi_ctx_p;
*pexit_code = 0;
ialloc_reset_requested(idmemory);
again:
make_null(perror_object);
o_stack.requested = e_stack.requested = d_stack.requested = 0;
while (gc_signal) {
gs_gc_root_t epref_root;
gc_signal = 0;
gs_register_ref_root(imemory_system, &epref_root,
(void **)&epref, "gs_call_interp(epref)");
code = interp_reclaim(pi_ctx_p, -1);
i_ctx_p = *pi_ctx_p;
gs_unregister_root(imemory_system, &epref_root,
"gs_call_interp(epref)");
if (code < 0)
return code;
}
code = interp(pi_ctx_p, epref, perror_object);
i_ctx_p = *pi_ctx_p;
set_gc_signal(i_ctx_p, &gc_signal, 1);
if (esp < esbot)
esp = esbot;
switch (code) {
case e_Fatal:
*pexit_code = 255;
return code;
case e_Quit:
*perror_object = osp[-1];
*pexit_code = code = osp->value.intval;
osp -= 2;
return
(code == 0 ? e_Quit :
code < 0 && code > -100 ? code : e_Fatal);
case e_InterpreterExit:
return 0;
case e_ExecStackUnderflow:
ref_stack_pop_block(&e_stack);
doref = *perror_object;
epref = &doref;
goto again;
case e_VMreclaim:
code = interp_reclaim(pi_ctx_p,
(osp->value.intval == 2 ?
avm_global : avm_local));
i_ctx_p = *pi_ctx_p;
make_oper(&doref, 0, zpop);
epref = &doref;
goto again;
case e_NeedInput:
case e_NeedStdin:
case e_NeedStdout:
case e_NeedStderr:
return code;
}
if (osp < osbot - 1)
osp = osbot - 1;
switch (code) {
case e_dictstackoverflow:
if (ref_stack_extend(&d_stack, d_stack.requested) >= 0) {
dict_set_top();
doref = *perror_object;
epref = &doref;
goto again;
}
if (osp >= ostop) {
if ((ccode = ref_stack_extend(&o_stack, 1)) < 0)
return ccode;
}
ccode = copy_stack(i_ctx_p, &d_stack, &saref);
if (ccode < 0)
return ccode;
ref_stack_pop_to(&d_stack, min_dstack_size);
dict_set_top();
*++osp = saref;
break;
case e_dictstackunderflow:
if (ref_stack_pop_block(&d_stack) >= 0) {
dict_set_top();
doref = *perror_object;
epref = &doref;
goto again;
}
break;
case e_execstackoverflow:
if (osp >= ostop) {
if ((ccode = ref_stack_extend(&o_stack, 1)) < 0)
return ccode;
}
ccode = copy_stack(i_ctx_p, &e_stack, &saref);
if (ccode < 0)
return ccode;
{
uint count = ref_stack_count(&e_stack);
uint limit = ref_stack_max_count(&e_stack) - ES_HEADROOM;
if (count > limit) {
int skip = count - limit;
int i;
for (i = skip; i < skip + MIN_BLOCK_ESTACK; ++i) {
const ref *ep = ref_stack_index(&e_stack, i);
if (r_has_type_attrs(ep, t_null, a_executable)) {
skip = i + 1;
break;
}
}
pop_estack(i_ctx_p, skip);
}
}
*++osp = saref;
break;
case e_stackoverflow:
if (ref_stack_extend(&o_stack, o_stack.requested) >= 0) {
doref = *perror_object;
if (r_is_proc(&doref)) {
*++osp = doref;
make_null_proc(&doref);
}
epref = &doref;
goto again;
}
ccode = copy_stack(i_ctx_p, &o_stack, &saref);
if (ccode < 0)
return ccode;
ref_stack_clear(&o_stack);
*++osp = saref;
break;
case e_stackunderflow:
if (ref_stack_pop_block(&o_stack) >= 0) {
doref = *perror_object;
epref = &doref;
goto again;
}
break;
}
if (user_errors < 0)
return code;
if (gs_errorname(i_ctx_p, code, &error_name) < 0)
return code;
if (dict_find_string(systemdict, "errordict", &perrordict) <= 0 ||
dict_find(perrordict, &error_name, &epref) <= 0
)
return code;
doref = *epref;
epref = &doref;
if (!ERROR_IS_INTERRUPT(code))
*++osp = *perror_object;
goto again;
}
private int
interp_exit(i_ctx_t *i_ctx_p)
{
return e_InterpreterExit;
}
private void
set_gc_signal(i_ctx_t *i_ctx_p, int *psignal, int value)
{
gs_memory_gc_status_t stat;
int i;
for (i = 0; i < countof(idmemory->spaces_indexed); i++) {
gs_ref_memory_t *mem = idmemory->spaces_indexed[i];
gs_ref_memory_t *mem_stable;
if (mem == 0)
continue;
for (;; mem = mem_stable) {
mem_stable = (gs_ref_memory_t *)
gs_memory_stable((gs_memory_t *)mem);
gs_memory_gc_status(mem, &stat);
stat.psignal = psignal;
stat.signal_value = value;
gs_memory_set_gc_status(mem, &stat);
if (mem_stable == mem)
break;
}
}
}
private int
copy_stack(i_ctx_t *i_ctx_p, const ref_stack_t * pstack, ref * arr)
{
uint size = ref_stack_count(pstack);
uint save_space = ialloc_space(idmemory);
int code;
ialloc_set_space(idmemory, avm_local);
code = ialloc_ref_array(arr, a_all, size, "copy_stack");
if (code >= 0)
code = ref_stack_store(pstack, arr, size, 0, 1, true, idmemory,
"copy_stack");
ialloc_set_space(idmemory, save_space);
return code;
}
int
gs_errorname(i_ctx_t *i_ctx_p, int code, ref * perror_name)
{
ref *perrordict, *pErrorNames;
if (dict_find_string(systemdict, "errordict", &perrordict) <= 0 ||
dict_find_string(systemdict, "ErrorNames", &pErrorNames) <= 0
)
return_error(e_undefined);
return array_get(imemory, pErrorNames, (long)(-code - 1), perror_name);
}
int
gs_errorinfo_put_string(i_ctx_t *i_ctx_p, const char *str)
{
ref rstr;
ref *pderror;
int code = string_to_ref(str, &rstr, iimemory, "gs_errorinfo_put_string");
if (code < 0)
return code;
if (dict_find_string(systemdict, "$error", &pderror) <= 0 ||
!r_has_type(pderror, t_dictionary) ||
idict_put_string(pderror, "errorinfo", &rstr) < 0
)
return_error(e_Fatal);
return 0;
}
private int
interp(i_ctx_t **pi_ctx_p ,
const ref * pref ,
ref * perror_object)
{
i_ctx_t *i_ctx_p = *pi_ctx_p;
register const ref_packed *iref_packed = (const ref_packed *)pref;
#ifdef ALIGNMENT_ALIASING_BUG
const ref *iref_temp;
#  define IREF (iref_temp = (const ref *)iref_packed, iref_temp)
#else
#  define IREF ((const ref *)iref_packed)
#endif
#define SET_IREF(rp) (iref_packed = (const ref_packed *)(rp))
register int icount = 0;
register os_ptr iosp = osp;
register es_ptr iesp = esp;
int code;
ref token;
register const ref *pvalue;
os_ptr whichp;
struct interp_error_s {
int code;
int line;
const ref *obj;
ref full;
} ierror;
const name_table *const int_nt = imemory->gs_lib_ctx->gs_name_table;
#define set_error(ecode)\
{ ierror.code = ecode; ierror.line = __LINE__; }
#define return_with_error(ecode, objp)\
{ set_error(ecode); ierror.obj = objp; goto rwe; }
#define return_with_error_iref(ecode)\
{ set_error(ecode); goto rwei; }
#define return_with_code_iref()\
{ ierror.line = __LINE__; goto rweci; }
#define return_with_error_code_op(nargs)\
return_with_code_iref()
#define return_with_stackoverflow(objp)\
{ o_stack.requested = 1; return_with_error(e_stackoverflow, objp); }
#define return_with_stackoverflow_iref()\
{ o_stack.requested = 1; return_with_error_iref(e_stackoverflow); }
int ticks_left = gs_interp_time_slice_ticks;
set_gc_signal(i_ctx_p, &ticks_left, -100);
esfile_clear_cache();
#define IREF_NEXT(ip)\
((const ref_packed *)((const ref *)(ip) + 1))
#define IREF_NEXT_EITHER(ip)\
( r_is_packed(ip) ? (ip) + 1 : IREF_NEXT(ip) )
#define store_state(ep)\
( icount > 0 ? (ep->value.const_refs = IREF + 1, r_set_size(ep, icount)) : 0 )
#define store_state_short(ep)\
( icount > 0 ? (ep->value.packed = iref_packed + 1, r_set_size(ep, icount)) : 0 )
#define store_state_either(ep)\
( icount > 0 ? (ep->value.packed = IREF_NEXT_EITHER(iref_packed), r_set_size(ep, icount)) : 0 )
#define next()\
if ( --icount > 0 ) { iref_packed = IREF_NEXT(iref_packed); goto top; } else goto out
#define next_short()\
if ( --icount <= 0 ) { if ( icount < 0 ) goto up; iesp--; }\
++iref_packed; goto top
#define next_either()\
if ( --icount <= 0 ) { if ( icount < 0 ) goto up; iesp--; }\
iref_packed = IREF_NEXT_EITHER(iref_packed); goto top
#if !PACKED_SPECIAL_OPS
#  undef next_either
#  define next_either() next()
#  undef store_state_either
#  define store_state_either(ep) store_state(ep)
#endif
if (iesp >= estop)
return_with_error(e_execstackoverflow, pref);
++iesp;
ref_assign_inline(iesp, pref);
goto bot;
top:
INCR(top);
#ifdef DEBUG
if (iosp >= osbot &&
(r_type(iosp) == t__invalid || r_type(iosp) >= tx_next_op)
) {
lprintf("Invalid value on o-stack!\n");
return_with_error_iref(e_Fatal);
}
if (gs_debug['I'] ||
(gs_debug['i'] &&
(r_is_packed(iref_packed) ?
r_packed_is_name(iref_packed) :
r_has_type(IREF, t_name)))
) {
os_ptr save_osp = osp;
es_ptr save_esp = esp;
osp = iosp;
esp = iesp;
dlprintf5("d%u,e%u<%u>0x%lx(%d): ",
ref_stack_count(&d_stack), ref_stack_count(&e_stack),
ref_stack_count(&o_stack), (ulong)IREF, icount);
debug_print_ref(imemory, IREF);
if (iosp >= osbot) {
dputs("
debug_print_ref(imemory, iosp);
}
dputc('\n');
osp = save_osp;
esp = save_esp;
fflush(dstderr);
}
#endif
#define lit(t) type_xe_value(t, a_execute)
#define exec(t) type_xe_value(t, a_execute + a_executable)
#define nox(t) type_xe_value(t, 0)
#define nox_exec(t) type_xe_value(t, a_executable)
#define plain(t) type_xe_value(t, 0)
#define plain_exec(t) type_xe_value(t, a_executable)
switch (r_type_xe(iref_packed)) {
#define cases_invalid()\
case plain(t__invalid): case plain_exec(t__invalid)
cases_invalid():
return_with_error_iref(e_Fatal);
#define cases_nox()\
case nox_exec(t_array): case nox_exec(t_dictionary):\
case nox_exec(t_file): case nox_exec(t_string):\
case nox_exec(t_mixedarray): case nox_exec(t_shortarray)
cases_nox():
return_with_error_iref(e_invalidaccess);
#define cases_lit_1()\
case lit(t_array): case nox(t_array):\
case plain(t_boolean): case plain_exec(t_boolean):\
case lit(t_dictionary): case nox(t_dictionary)
#define cases_lit_2()\
case lit(t_file): case nox(t_file):\
case plain(t_fontID): case plain_exec(t_fontID):\
case plain(t_integer): case plain_exec(t_integer):\
case plain(t_mark): case plain_exec(t_mark)
#define cases_lit_3()\
case plain(t_name):\
case plain(t_null):\
case plain(t_oparray):\
case plain(t_operator)
#define cases_lit_4()\
case plain(t_real): case plain_exec(t_real):\
case plain(t_save): case plain_exec(t_save):\
case lit(t_string): case nox(t_string)
#define cases_lit_5()\
case lit(t_mixedarray): case nox(t_mixedarray):\
case lit(t_shortarray): case nox(t_shortarray):\
case plain(t_device): case plain_exec(t_device):\
case plain(t_struct): case plain_exec(t_struct):\
case plain(t_astruct): case plain_exec(t_astruct)
#define cases_lit_array()\
case exec(t_array): case exec(t_mixedarray): case exec(t_shortarray)
cases_lit_1():
cases_lit_2():
cases_lit_3():
cases_lit_4():
cases_lit_5():
INCR(lit);
break;
cases_lit_array():
INCR(lit_array);
break;
case plain_exec(tx_op_add):
x_add:	    INCR(x_add);
if ((code = zop_add(iosp)) < 0)
return_with_error_code_op(2);
iosp--;
next_either();
case plain_exec(tx_op_def):
x_def:	    INCR(x_def);
osp = iosp;
if ((code = zop_def(i_ctx_p)) < 0)
return_with_error_code_op(2);
iosp -= 2;
next_either();
case plain_exec(tx_op_dup):
x_dup:	    INCR(x_dup);
if (iosp < osbot)
return_with_error_iref(e_stackunderflow);
if (iosp >= ostop)
return_with_stackoverflow_iref();
iosp++;
ref_assign_inline(iosp, iosp - 1);
next_either();
case plain_exec(tx_op_exch):
x_exch:	    INCR(x_exch);
if (iosp <= osbot)
return_with_error_iref(e_stackunderflow);
ref_assign_inline(&token, iosp);
ref_assign_inline(iosp, iosp - 1);
ref_assign_inline(iosp - 1, &token);
next_either();
case plain_exec(tx_op_if):
x_if:	    INCR(x_if);
if (!r_has_type(iosp - 1, t_boolean))
return_with_error_iref((iosp <= osbot ?
e_stackunderflow : e_typecheck));
if (!r_is_proc(iosp))
return_with_error_iref(check_proc_failed(iosp));
if (!iosp[-1].value.boolval) {
iosp -= 2;
next_either();
}
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
store_state_either(iesp);
whichp = iosp;
iosp -= 2;
goto ifup;
case plain_exec(tx_op_ifelse):
x_ifelse:   INCR(x_ifelse);
if (!r_has_type(iosp - 2, t_boolean))
return_with_error_iref((iosp < osbot + 2 ?
e_stackunderflow : e_typecheck));
if (!r_is_proc(iosp - 1))
return_with_error_iref(check_proc_failed(iosp - 1));
if (!r_is_proc(iosp))
return_with_error_iref(check_proc_failed(iosp));
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
store_state_either(iesp);
whichp = (iosp[-2].value.boolval ? iosp - 1 : iosp);
iosp -= 3;
ifup:if ((icount = r_size(whichp) - 1) <= 0) {
if (icount < 0)
goto up;
SET_IREF(whichp->value.refs);
if (--ticks_left > 0)
goto top;
}
++iesp;
iesp->tas = whichp->tas;
SET_IREF(iesp->value.refs = whichp->value.refs);
if (--ticks_left > 0)
goto top;
goto slice;
case plain_exec(tx_op_index):
x_index:    INCR(x_index);
osp = iosp;
if ((code = zindex(i_ctx_p)) < 0)
return_with_error_code_op(1);
next_either();
case plain_exec(tx_op_pop):
x_pop:	    INCR(x_pop);
if (iosp < osbot)
return_with_error_iref(e_stackunderflow);
iosp--;
next_either();
case plain_exec(tx_op_roll):
x_roll:	    INCR(x_roll);
osp = iosp;
if ((code = zroll(i_ctx_p)) < 0)
return_with_error_code_op(2);
iosp -= 2;
next_either();
case plain_exec(tx_op_sub):
x_sub:	    INCR(x_sub);
if ((code = zop_sub(iosp)) < 0)
return_with_error_code_op(2);
iosp--;
next_either();
case plain_exec(t_null):
goto bot;
case plain_exec(t_oparray):
INCR(exec_array);
pvalue = IREF->value.const_refs;
opst:
store_state(iesp);
oppr:
if (iesp >= estop - 3)
return_with_error_iref(e_execstackoverflow);
iesp += 4;
osp = iosp;
make_mark_estack(iesp - 3, es_other, oparray_cleanup);
make_int(iesp - 2, ref_stack_count_inline(&o_stack));
make_int(iesp - 1, ref_stack_count_inline(&d_stack));
make_op_estack(iesp, oparray_pop);
goto pr;
prst:
store_state(iesp);
pr:
if ((icount = r_size(pvalue) - 1) <= 0) {
if (icount < 0)
goto up;
SET_IREF(pvalue->value.refs);
if (--ticks_left > 0)
goto top;
}
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
++iesp;
iesp->tas = pvalue->tas;
SET_IREF(iesp->value.refs = pvalue->value.refs);
if (--ticks_left > 0)
goto top;
goto slice;
case plain_exec(t_operator):
INCR(exec_operator);
if (--ticks_left <= 0) {
}
esp = iesp;
osp = iosp;
switch (code = call_operator(real_opproc(IREF), i_ctx_p)) {
case 0:
case 1:
iosp = osp;
next();
case o_push_estack:
store_state(iesp);
opush:iosp = osp;
iesp = esp;
if (--ticks_left > 0)
goto up;
goto slice;
case o_pop_estack:
opop:iosp = osp;
if (esp == iesp)
goto bot;
iesp = esp;
goto up;
case o_reschedule:
store_state(iesp);
goto res;
case e_RemapColor:
oe_remap:	    store_state(iesp);
remap:		    if (iesp + 2 >= estop) {
esp = iesp;
code = ref_stack_extend(&e_stack, 2);
if (code < 0)
return_with_error_iref(code);
iesp = esp;
}
packed_get(imemory, iref_packed, iesp + 1);
make_oper(iesp + 2, 0,
r_ptr(&istate->remap_color_info,
int_remap_color_info_t)->proc);
iesp += 2;
goto up;
}
iosp = osp;
iesp = esp;
return_with_code_iref();
case plain_exec(t_name):
INCR(exec_name);
pvalue = IREF->value.pname->pvalue;
if (!pv_valid(pvalue)) {
uint nidx = names_index(int_nt, IREF);
uint htemp;
INCR(find_name);
if ((pvalue = dict_find_name_by_index_inline(nidx, htemp)) == 0)
return_with_error_iref(e_undefined);
}
switch (r_type_xe(pvalue)) {
cases_invalid():
return_with_error_iref(e_Fatal);
cases_nox():
return_with_error_iref(e_invalidaccess);
cases_lit_1():
cases_lit_2():
cases_lit_3():
cases_lit_4():
cases_lit_5():
INCR(name_lit);
if (iosp >= ostop)
return_with_stackoverflow(pvalue);
++iosp;
ref_assign_inline(iosp, pvalue);
next();
case exec(t_array):
case exec(t_mixedarray):
case exec(t_shortarray):
INCR(name_proc);
goto prst;
case plain_exec(tx_op_add):
goto x_add;
case plain_exec(tx_op_def):
goto x_def;
case plain_exec(tx_op_dup):
goto x_dup;
case plain_exec(tx_op_exch):
goto x_exch;
case plain_exec(tx_op_if):
goto x_if;
case plain_exec(tx_op_ifelse):
goto x_ifelse;
case plain_exec(tx_op_index):
goto x_index;
case plain_exec(tx_op_pop):
goto x_pop;
case plain_exec(tx_op_roll):
goto x_roll;
case plain_exec(tx_op_sub):
goto x_sub;
case plain_exec(t_null):
goto bot;
case plain_exec(t_oparray):
INCR(name_oparray);
pvalue = (const ref *)pvalue->value.const_refs;
goto opst;
case plain_exec(t_operator):
INCR(name_operator);
{
if (--ticks_left <= 0) {
}
esp = iesp;
osp = iosp;
switch (code = call_operator(real_opproc(pvalue),
i_ctx_p)
) {
case 0:
case 1:
iosp = osp;
next();
case o_push_estack:
store_state(iesp);
goto opush;
case o_pop_estack:
goto opop;
case o_reschedule:
store_state(iesp);
goto res;
case e_RemapColor:
goto oe_remap;
}
iosp = osp;
iesp = esp;
return_with_error(code, pvalue);
}
case plain_exec(t_name):
case exec(t_file):
case exec(t_string):
default:
store_state(iesp);
icount = 0;
SET_IREF(pvalue);
goto top;
}
case exec(t_file):
{
stream *s;
scanner_state sstate;
check_read_known_file(s, IREF, return_with_error_iref);
rt:
if (iosp >= ostop)
return_with_stackoverflow_iref();
osp = iosp;
scanner_state_init_options(&sstate, i_ctx_p->scanner_options);
again:
code = scan_token(i_ctx_p, s, &token, &sstate);
iosp = osp;
switch (code) {
case 0:
if (!r_has_attr(&token, a_executable) ||
r_is_array(&token)
) {
iosp++;
ref_assign_inline(iosp, &token);
goto rt;
}
store_state(iesp);
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
esfile_set_cache(++iesp);
ref_assign_inline(iesp, IREF);
SET_IREF(&token);
icount = 0;
goto top;
case e_undefined:
return_with_error(code, &token);
case scan_EOF:
esfile_clear_cache();
goto bot;
case scan_BOS:
store_state(iesp);
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
esfile_set_cache(++iesp);
ref_assign_inline(iesp, IREF);
pvalue = &token;
goto pr;
case scan_Refill:
store_state(iesp);
ref_assign_inline(&token, IREF);
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
++iesp;
ref_assign_inline(iesp, &token);
esp = iesp;
osp = iosp;
code = scan_handle_refill(i_ctx_p, &token, &sstate,
true, true,
ztokenexec_continue);
scan_cont:
iosp = osp;
iesp = esp;
switch (code) {
case 0:
iesp--;
goto again;
case o_push_estack:
esfile_clear_cache();
if (--ticks_left > 0)
goto up;
goto slice;
}
iesp--;
return_with_code_iref();
case scan_Comment:
case scan_DSC_Comment: {
ref file_token;
store_state(iesp);
ref_assign_inline(&file_token, IREF);
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
++iesp;
ref_assign_inline(iesp, &file_token);
esp = iesp;
osp = iosp;
code = ztoken_handle_comment(i_ctx_p, &file_token,
&sstate, &token,
code, true, true,
ztokenexec_continue);
}
goto scan_cont;
default:
return_with_code_iref();
}
}
case exec(t_string):
{
stream ss;
scanner_state sstate;
scanner_state_init_options(&sstate, SCAN_FROM_STRING);
s_init(&ss, NULL);
sread_string(&ss, IREF->value.bytes, r_size(IREF));
osp = iosp;
code = scan_token(i_ctx_p, &ss, &token, &sstate);
iosp = osp;
switch (code) {
case 0:
case scan_BOS:
store_state(iesp);
{
uint size = sbufavailable(&ss);
if (size) {
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
++iesp;
iesp->tas.type_attrs = IREF->tas.type_attrs;
iesp->value.const_bytes = sbufptr(&ss);
r_set_size(iesp, size);
}
}
if (code == 0) {
SET_IREF(&token);
icount = 0;
goto top;
}
pvalue = &token;
goto pr;
case scan_EOF:
goto bot;
case scan_Refill:
code = gs_note_error(e_syntaxerror);
default:
return_with_code_iref();
}
}
default:
{
uint index;
switch (*iref_packed >> r_packed_type_shift) {
case pt_full_ref:
case pt_full_ref + 1:
INCR(p_full);
if (iosp >= ostop)
return_with_stackoverflow_iref();
++iosp;
ref_assign_inline(iosp, IREF);
next();
case pt_executable_operator:
index = *iref_packed & packed_value_mask;
if (--ticks_left <= 0) {
}
if (!op_index_is_operator(index)) {
INCR(p_exec_oparray);
store_state_short(iesp);
index -= op_def_count;
pvalue = (const ref *)
(index < r_size(&op_array_table_global.table) ?
op_array_table_global.table.value.const_refs +
index :
op_array_table_local.table.value.const_refs +
(index - r_size(&op_array_table_global.table)));
goto oppr;
}
INCR(p_exec_operator);
#if PACKED_SPECIAL_OPS
#  define case_xop(xop) case xop - (int)tx_op + 1
switch (index) {
case_xop(tx_op_add):goto x_add;
case_xop(tx_op_def):goto x_def;
case_xop(tx_op_dup):goto x_dup;
case_xop(tx_op_exch):goto x_exch;
case_xop(tx_op_if):goto x_if;
case_xop(tx_op_ifelse):goto x_ifelse;
case_xop(tx_op_index):goto x_index;
case_xop(tx_op_pop):goto x_pop;
case_xop(tx_op_roll):goto x_roll;
case_xop(tx_op_sub):goto x_sub;
case 0:
default:
;
}
#  undef case_xop
#endif
INCR(p_exec_non_x_operator);
esp = iesp;
osp = iosp;
switch (code = call_operator(op_index_proc(index), i_ctx_p)) {
case 0:
case 1:
iosp = osp;
next_short();
case o_push_estack:
store_state_short(iesp);
goto opush;
case o_pop_estack:
iosp = osp;
if (esp == iesp) {
next_short();
}
iesp = esp;
goto up;
case o_reschedule:
store_state_short(iesp);
goto res;
case e_RemapColor:
store_state_short(iesp);
goto remap;
}
iosp = osp;
iesp = esp;
return_with_code_iref();
case pt_integer:
INCR(p_integer);
if (iosp >= ostop)
return_with_stackoverflow_iref();
++iosp;
make_int(iosp,
((int)*iref_packed & packed_int_mask) +
packed_min_intval);
next_short();
case pt_literal_name:
INCR(p_lit_name);
{
uint nidx = *iref_packed & packed_value_mask;
if (iosp >= ostop)
return_with_stackoverflow_iref();
++iosp;
name_index_ref_inline(int_nt, nidx, iosp);
next_short();
}
case pt_executable_name:
INCR(p_exec_name);
{
uint nidx = *iref_packed & packed_value_mask;
pvalue = name_index_ptr_inline(int_nt, nidx)->pvalue;
if (!pv_valid(pvalue)) {
uint htemp;
INCR(p_find_name);
if ((pvalue = dict_find_name_by_index_inline(nidx, htemp)) == 0) {
names_index_ref(int_nt, nidx, &token);
return_with_error(e_undefined, &token);
}
}
if (r_has_masked_attrs(pvalue, a_execute, a_execute + a_executable)) {
INCR(p_name_lit);
if (iosp >= ostop)
return_with_stackoverflow_iref();
++iosp;
ref_assign_inline(iosp, pvalue);
next_short();
}
if (r_is_proc(pvalue)) {
INCR(p_name_proc);
store_state_short(iesp);
goto pr;
}
store_state_short(iesp);
icount = 0;
SET_IREF(pvalue);
goto top;
}
}
}
}
if (iosp >= ostop)
return_with_stackoverflow_iref();
++iosp;
ref_assign_inline(iosp, IREF);
bot:next();
out:
if (!icount) {
iesp--;
iref_packed = IREF_NEXT(iref_packed);
goto top;
}
up:if (--ticks_left < 0)
goto slice;
if (!r_is_proc(iesp)) {
SET_IREF(iesp--);
icount = 0;
goto top;
}
SET_IREF(iesp->value.refs);
icount = r_size(iesp) - 1;
if (icount <= 0) {
iesp--;
if (icount < 0)
goto up;
}
goto top;
res:
*pi_ctx_p = i_ctx_p;
code = (*gs_interp_reschedule_proc)(pi_ctx_p);
i_ctx_p = *pi_ctx_p;
sched:
if (code < 0) {
set_error(code);
make_null_proc(&ierror.full);
SET_IREF(ierror.obj = &ierror.full);
goto error_exit;
}
iosp = osp;
iesp = esp;
goto up;
#if 0
sst:
store_state(iesp);
if (iesp >= estop)
return_with_error_iref(e_execstackoverflow);
iesp++;
ref_assign_inline(iesp, iref);
#endif
slice:
osp = iosp;
esp = iesp;
if (ticks_left <= -100) {
*pi_ctx_p = i_ctx_p;
code = interp_reclaim(pi_ctx_p, -1);
i_ctx_p = *pi_ctx_p;
} else if (gs_interp_time_slice_proc) {
*pi_ctx_p = i_ctx_p;
code = (*gs_interp_time_slice_proc)(pi_ctx_p);
i_ctx_p = *pi_ctx_p;
} else
code = 0;
ticks_left = gs_interp_time_slice_ticks;
set_code_on_interrupt(imemory, &code);
goto sched;
rweci:
ierror.code = code;
rwei:
ierror.obj = IREF;
rwe:
if (!r_is_packed(iref_packed))
store_state(iesp);
else {
packed_get(imemory, (const ref_packed *)ierror.obj, &ierror.full);
store_state_short(iesp);
if (IREF == ierror.obj)
SET_IREF(&ierror.full);
ierror.obj = &ierror.full;
}
error_exit:
if (ERROR_IS_INTERRUPT(ierror.code)) {
if (iesp >= estop)
code = e_execstackoverflow;
else {
iesp++;
ref_assign_inline(iesp, IREF);
}
}
esp = iesp;
osp = iosp;
ref_assign_inline(perror_object, ierror.obj);
return gs_log_error(ierror.code, __FILE__, ierror.line);
}
private int
oparray_pop(i_ctx_t *i_ctx_p)
{
esp -= 3;
return o_pop_estack;
}
private int
oparray_cleanup(i_ctx_t *i_ctx_p)
{
es_ptr ep = esp;
uint ocount_old = (uint) ep[2].value.intval;
uint dcount_old = (uint) ep[3].value.intval;
uint ocount = ref_stack_count(&o_stack);
uint dcount = ref_stack_count(&d_stack);
if (ocount > ocount_old)
ref_stack_pop(&o_stack, ocount - ocount_old);
if (dcount > dcount_old) {
ref_stack_pop(&d_stack, dcount - dcount_old);
dict_set_top();
}
return 0;
}
private int
oparray_no_cleanup(i_ctx_t *i_ctx_p)
{
return 0;
}
private ref *
oparray_find(i_ctx_t *i_ctx_p)
{
long i;
ref *ep;
for (i = 0; (ep = ref_stack_index(&e_stack, i)) != 0; ++i) {
if (r_is_estack_mark(ep) &&
(ep->value.opproc == oparray_cleanup ||
ep->value.opproc == oparray_no_cleanup)
)
return ep;
}
return 0;
}
private int
zsetstackprotect(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *ep = oparray_find(i_ctx_p);
check_type(*op, t_boolean);
if (ep == 0)
return_error(e_rangecheck);
ep->value.opproc =
(op->value.boolval ? oparray_cleanup : oparray_no_cleanup);
pop(1);
return 0;
}
private int
zcurrentstackprotect(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *ep = oparray_find(i_ctx_p);
if (ep == 0)
return_error(e_rangecheck);
push(1);
make_bool(op, ep->value.opproc == oparray_cleanup);
return 0;
}