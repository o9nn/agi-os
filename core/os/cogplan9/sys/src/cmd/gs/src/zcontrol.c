#include "string_.h"
#include "ghost.h"
#include "stream.h"
#include "oper.h"
#include "estack.h"
#include "files.h"
#include "ipacked.h"
#include "iutil.h"
#include "store.h"
private int no_cleanup(i_ctx_t *);
private uint count_exec_stack(i_ctx_t *, bool);
private uint count_to_stopped(i_ctx_t *, long);
private int unmatched_exit(os_ptr, op_proc_t);
private int cond_continue(i_ctx_t *);
private int
zcond(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep = esp;
if (!r_is_array(op))
return_op_typecheck(op);
check_execute(*op);
if ((r_size(op) & 1) != 0)
return_error(e_rangecheck);
if (r_size(op) == 0)
return zpop(i_ctx_p);
check_estack(3);
esp = ep += 3;
ref_assign(ep - 2, op);
make_op_estack(ep - 1, cond_continue);
array_get(imemory, op, 0L, ep);
esfile_check_cache();
pop(1);
return o_push_estack;
}
private int
cond_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep = esp;
int code;
check_type(*op, t_boolean);
if (op->value.boolval) {
array_get(imemory, ep, 1L, ep);
esfile_check_cache();
code = o_pop_estack;
} else if (r_size(ep) > 2) {
const ref_packed *elts = ep->value.packed;
check_estack(2);
r_dec_size(ep, 2);
elts = packed_next(elts);
elts = packed_next(elts);
ep->value.packed = elts;
array_get(imemory, ep, 0L, ep + 2);
make_op_estack(ep + 1, cond_continue);
esp = ep + 2;
esfile_check_cache();
code = o_push_estack;
} else {
esp = ep - 1;
code = o_pop_estack;
}
pop(1);
return code;
}
int
zexec(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(1);
if (!r_has_attr(op, a_executable))
return 0;
check_estack(1);
++esp;
ref_assign(esp, op);
esfile_check_cache();
pop(1);
return o_push_estack;
}
private int
zexecn(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint n, i;
es_ptr esp_orig;
check_int_leu(*op, max_uint - 1);
n = (uint) op->value.intval;
check_op(n + 1);
check_estack(n);
esp_orig = esp;
for (i = 0; i < n; ++i) {
const ref *rp = ref_stack_index(&o_stack, (long)(i + 1));
if (ref_type_uses_access(r_type(rp))) {
if (!r_has_attr(rp, a_execute) &&
r_has_attr(rp, a_executable)
) {
esp = esp_orig;
return_error(e_invalidaccess);
}
}
if (!r_has_type_attrs(rp, t_null, a_executable)) {
++esp;
ref_assign(esp, rp);
}
}
esfile_check_cache();
pop(n + 1);
return o_push_estack;
}
private int end_superexec(i_ctx_t *);
private int
zsuperexec(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep;
check_op(1);
if (!r_has_attr(op, a_executable))
return 0;
check_estack(2);
ep = esp += 3;
make_mark_estack(ep - 2, es_other, end_superexec);
make_op_estack(ep - 1,  end_superexec);
ref_assign(ep, op);
esfile_check_cache();
pop(1);
i_ctx_p->in_superexec++;
return o_push_estack;
}
private int
end_superexec(i_ctx_t *i_ctx_p)
{
i_ctx_p->in_superexec--;
return 0;
}
private int end_runandhide(i_ctx_t *);
private int err_end_runandhide(i_ctx_t *);
private int
zrunandhide(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep;
check_op(2);
if (!r_is_array(op - 1))
return_op_typecheck(op);
if (!r_has_attr(op, a_executable))
return 0;
check_estack(5);
ep = esp += 5;
make_mark_estack(ep - 4, es_other, err_end_runandhide);
make_op_estack(ep - 1,  end_runandhide);
ref_assign(ep, op);
make_int(ep - 3, (int)op[-1].tas.type_attrs);
ref_assign(ep - 2, op - 1);
r_clear_attrs(ep - 2, a_all);
esfile_check_cache();
pop(2);
return o_push_estack;
}
private int
runandhide_restore_hidden(i_ctx_t *i_ctx_p, ref *obj, ref *attrs)
{
os_ptr op = osp;
push(1);
ref_assign(op, obj);
r_clear_attrs(op, a_all);
r_set_attrs(op, attrs->value.intval);
return 0;
}
private int
end_runandhide(i_ctx_t *i_ctx_p)
{
int code;
if ((code = runandhide_restore_hidden(i_ctx_p, esp, esp - 1)) < 0)
return code;
esp -= 2;
return o_pop_estack;
}
private int
err_end_runandhide(i_ctx_t *i_ctx_p)
{
int code;
if ((code = runandhide_restore_hidden(i_ctx_p, esp + 3, esp + 2)) < 0)
return code;
return 0;
}
int
zif(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(op[-1], t_boolean);
check_proc(*op);
if (op[-1].value.boolval) {
check_estack(1);
++esp;
ref_assign(esp, op);
esfile_check_cache();
}
pop(2);
return o_push_estack;
}
int
zifelse(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(op[-2], t_boolean);
check_proc(op[-1]);
check_proc(*op);
check_estack(1);
++esp;
if (op[-2].value.boolval) {
ref_assign(esp, op - 1);
} else {
ref_assign(esp, op);
}
esfile_check_cache();
pop(3);
return o_push_estack;
}
private int
for_pos_int_continue(i_ctx_t *),
for_neg_int_continue(i_ctx_t *),
for_real_continue(i_ctx_t *);
int
zfor(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
register es_ptr ep;
check_estack(7);
ep = esp + 6;
check_proc(*op);
if (r_has_type(op - 3, t_integer) &&
r_has_type(op - 2, t_integer)
) {
make_int(ep - 4, op[-3].value.intval);
make_int(ep - 3, op[-2].value.intval);
switch (r_type(op - 1)) {
case t_integer:
make_int(ep - 2, op[-1].value.intval);
break;
case t_real:
make_int(ep - 2, (long)op[-1].value.realval);
break;
default:
return_op_typecheck(op - 1);
}
if (ep[-3].value.intval >= 0)
make_op_estack(ep, for_pos_int_continue);
else
make_op_estack(ep, for_neg_int_continue);
} else {
float params[3];
int code;
if ((code = float_params(op - 1, 3, params)) < 0)
return code;
make_real(ep - 4, params[0]);
make_real(ep - 3, params[1]);
make_real(ep - 2, params[2]);
make_op_estack(ep, for_real_continue);
}
make_mark_estack(ep - 5, es_for, no_cleanup);
ref_assign(ep - 1, op);
esp = ep;
pop(4);
return o_push_estack;
}
private int
for_pos_int_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
register es_ptr ep = esp;
long var = ep[-3].value.intval;
if (var > ep[-1].value.intval) {
esp -= 5;
return o_pop_estack;
}
push(1);
make_int(op, var);
ep[-3].value.intval = var + ep[-2].value.intval;
ref_assign_inline(ep + 2, ep);
esp = ep + 2;
return o_push_estack;
}
private int
for_neg_int_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
register es_ptr ep = esp;
long var = ep[-3].value.intval;
if (var < ep[-1].value.intval) {
esp -= 5;
return o_pop_estack;
}
push(1);
make_int(op, var);
ep[-3].value.intval = var + ep[-2].value.intval;
ref_assign(ep + 2, ep);
esp = ep + 2;
return o_push_estack;
}
private int
for_real_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep = esp;
float var = ep[-3].value.realval;
float incr = ep[-2].value.realval;
if (incr >= 0 ? (var > ep[-1].value.realval) :
(var < ep[-1].value.realval)
) {
esp -= 5;
return o_pop_estack;
}
push(1);
ref_assign(op, ep - 3);
ep[-3].value.realval = var + incr;
esp = ep + 2;
ref_assign(ep + 2, ep);
return o_push_estack;
}
private int for_samples_continue(i_ctx_t *);
int
zfor_samples(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep;
check_type(op[-3], t_real);
check_type(op[-2], t_integer);
check_type(op[-1], t_real);
check_proc(*op);
check_estack(8);
ep = esp + 7;
make_mark_estack(ep - 6, es_for, no_cleanup);
make_int(ep - 5, 0);
memcpy(ep - 4, op - 3, 3 * sizeof(ref));
ref_assign(ep - 1, op);
make_op_estack(ep, for_samples_continue);
esp = ep;
pop(4);
return o_push_estack;
}
private int
for_samples_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep = esp;
long var = ep[-4].value.intval;
float a = ep[-3].value.realval;
long n = ep[-2].value.intval;
float b = ep[-1].value.realval;
if (var > n) {
esp -= 6;
return o_pop_estack;
}
push(1);
make_real(op, ((n - var) * a + var * b) / n);
ep[-4].value.intval = var + 1;
ref_assign_inline(ep + 2, ep);
esp = ep + 2;
return o_push_estack;
}
private int repeat_continue(i_ctx_t *);
private int
zrepeat(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(op[-1], t_integer);
check_proc(*op);
if (op[-1].value.intval < 0)
return_error(e_rangecheck);
check_estack(5);
push_mark_estack(es_for, no_cleanup);
*++esp = op[-1];
*++esp = *op;
make_op_estack(esp + 1, repeat_continue);
pop(2);
return repeat_continue(i_ctx_p);
}
private int
repeat_continue(i_ctx_t *i_ctx_p)
{
es_ptr ep = esp;
if (--(ep[-1].value.intval) >= 0) {
esp += 2;
ref_assign(esp, ep);
return o_push_estack;
} else {
esp -= 3;
return o_pop_estack;
}
}
private int loop_continue(i_ctx_t *);
private int
zloop(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_proc(*op);
check_estack(4);
push_mark_estack(es_for, no_cleanup);
*++esp = *op;
make_op_estack(esp + 1, loop_continue);
pop(1);
return loop_continue(i_ctx_p);
}
private int
loop_continue(i_ctx_t *i_ctx_p)
{
register es_ptr ep = esp;
ref_assign(ep + 2, ep);
esp = ep + 2;
return o_push_estack;
}
private int
zexit(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref_stack_enum_t rsenum;
uint scanned = 0;
ref_stack_enum_begin(&rsenum, &e_stack);
do {
uint used = rsenum.size;
es_ptr ep = rsenum.ptr + used - 1;
uint count = used;
for (; count; count--, ep--)
if (r_is_estack_mark(ep))
switch (estack_mark_index(ep)) {
case es_for:
pop_estack(i_ctx_p, scanned + (used - count + 1));
return o_pop_estack;
case es_stopped:
return_error(e_invalidexit);
}
scanned += used;
} while (ref_stack_enum_next(&rsenum));
push(2);
return unmatched_exit(op, zexit);
}
private int
stopped_push(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
*op = esp[-1];
esp -= 3;
return o_pop_estack;
}
private int
zstop(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint count = count_to_stopped(i_ctx_p, 1L);
if (count) {
check_ostack(2);
pop_estack(i_ctx_p, count);
op = osp;
push(1);
make_true(op);
return o_pop_estack;
}
push(2);
return unmatched_exit(op, zstop);
}
private int
zzstop(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint count;
check_type(*op, t_integer);
count = count_to_stopped(i_ctx_p, op->value.intval);
if (count) {
ref save_result;
check_op(2);
save_result = op[-1];
pop(2);
pop_estack(i_ctx_p, count);
op = osp;
push(1);
*op = save_result;
return o_pop_estack;
}
return unmatched_exit(op, zzstop);
}
private int
zstopped(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(1);
check_estack(5);
push_mark_estack(es_stopped, no_cleanup);
++esp;
make_false(esp);
++esp;
make_int(esp, 1);
push_op_estack(stopped_push);
*++esp = *op;
esfile_check_cache();
pop(1);
return o_push_estack;
}
private int
zzstopped(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_op(3);
check_estack(5);
push_mark_estack(es_stopped, no_cleanup);
*++esp = op[-1];
*++esp = *op;
push_op_estack(stopped_push);
*++esp = op[-2];
esfile_check_cache();
pop(3);
return o_push_estack;
}
private int
zinstopped(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint count;
check_type(*op, t_integer);
count = count_to_stopped(i_ctx_p, op->value.intval);
if (count) {
push(1);
op[-1] = *ref_stack_index(&e_stack, count - 2);
make_true(op);
} else
make_false(op);
return 0;
}
private int
zcountexecstack(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_int(op, count_exec_stack(i_ctx_p, false));
return 0;
}
private int
zcountexecstack1(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_boolean);
make_int(op, count_exec_stack(i_ctx_p, op->value.boolval));
return 0;
}
private int execstack_continue(i_ctx_t *);
private int execstack2_continue(i_ctx_t *);
private int
push_execstack(i_ctx_t *i_ctx_p, os_ptr op1, bool include_marks,
op_proc_t cont)
{
uint size;
uint depth;
check_write_type(*op1, t_array);
size = r_size(op1);
depth = count_exec_stack(i_ctx_p, include_marks);
if (depth > size)
return_error(e_rangecheck);
{
int code = ref_stack_store_check(&e_stack, op1, size, 0);
if (code < 0)
return code;
}
check_estack(1);
r_set_size(op1, depth);
push_op_estack(cont);
return o_push_estack;
}
private int
zexecstack(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return push_execstack(i_ctx_p, op, false, execstack_continue);
}
private int
zexecstack2(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_boolean);
return push_execstack(i_ctx_p, op - 1, op->value.boolval, execstack2_continue);
}
private int
do_execstack(i_ctx_t *i_ctx_p, bool include_marks, os_ptr op1)
{
os_ptr op = osp;
ref *arefs = op1->value.refs;
uint asize = r_size(op1);
uint i;
ref *rq;
for (i = 0, rq = arefs + asize; rq != arefs; ++i) {
const ref *rp = ref_stack_index(&e_stack, (long)i);
if (r_has_type_attrs(rp, t_null, a_executable) && !include_marks)
continue;
--rq;
ref_assign_old(op1, rq, rp, "execstack");
switch (r_type(rq)) {
case t_operator: {
uint opidx = op_index(rq);
if (opidx == 0 || op_def_is_internal(op_index_def(opidx)))
r_clear_attrs(rq, a_executable);
break;
}
case t_struct:
case t_astruct: {
const char *tname =
gs_struct_type_name_string(
gs_object_type(imemory, rq->value.pstruct));
make_const_string(rq, a_readonly | avm_foreign,
strlen(tname), (const byte *)tname);
break;
}
default:
;
}
}
pop(op - op1);
return 0;
}
private int
execstack_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return do_execstack(i_ctx_p, false, op);
}
private int
execstack2_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return do_execstack(i_ctx_p, op->value.boolval, op - 1);
}
private int
zneedinput(i_ctx_t *i_ctx_p)
{
return e_NeedInput;
}
private int
zquit(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_op(2);
check_type(*op, t_integer);
return_error(e_Quit);
}
private ref *zget_current_file(i_ctx_t *);
private int
zcurrentfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *fp;
push(1);
if (esfile != 0) {
#ifdef DEBUG
ref *efp = zget_current_file(i_ctx_p);
if (esfile != efp) {
lprintf2("currentfile: esfile=0x%lx, efp=0x%lx\n",
(ulong) esfile, (ulong) efp);
ref_assign(op, efp);
} else
#endif
ref_assign(op, esfile);
} else if ((fp = zget_current_file(i_ctx_p)) == 0) {
make_invalid_file(op);
} else {
ref_assign(op, fp);
esfile_set_cache(fp);
}
r_clear_attrs(op, a_executable);
return 0;
}
private ref *
zget_current_file(i_ctx_t *i_ctx_p)
{
ref_stack_enum_t rsenum;
ref_stack_enum_begin(&rsenum, &e_stack);
do {
uint count = rsenum.size;
es_ptr ep = rsenum.ptr + count - 1;
for (; count; count--, ep--)
if (r_has_type_attrs(ep, t_file, a_executable))
return ep;
} while (ref_stack_enum_next(&rsenum));
return 0;
}
const op_def zcontrol1_op_defs[] = {
{"1.cond", zcond},
{"0countexecstack", zcountexecstack},
{"1.countexecstack", zcountexecstack1},
{"0currentfile", zcurrentfile},
{"1exec", zexec},
{"1.execn", zexecn},
{"1execstack", zexecstack},
{"2.execstack", zexecstack2},
{"0exit", zexit},
{"2if", zif},
{"3ifelse", zifelse},
{"0.instopped", zinstopped},
{"0.needinput", zneedinput},
op_def_end(0)
};
const op_def zcontrol2_op_defs[] = {
{"4for", zfor},
{"1loop", zloop},
{"2.quit", zquit},
{"2repeat", zrepeat},
{"0stop", zstop},
{"1.stop", zzstop},
{"1stopped", zstopped},
{"2.stopped", zzstopped},
op_def_end(0)
};
const op_def zcontrol3_op_defs[] = {
{"1%cond_continue", cond_continue},
{"1%execstack_continue", execstack_continue},
{"2%execstack2_continue", execstack2_continue},
{"0%for_pos_int_continue", for_pos_int_continue},
{"0%for_neg_int_continue", for_neg_int_continue},
{"0%for_real_continue", for_real_continue},
{"4%for_samples", zfor_samples},
{"0%for_samples_continue", for_samples_continue},
{"0%loop_continue", loop_continue},
{"0%repeat_continue", repeat_continue},
{"0%stopped_push", stopped_push},
{"1superexec", zsuperexec},
{"0%end_superexec", end_superexec},
{"2.runandhide", zrunandhide},
{"0%end_runandhide", end_runandhide},
op_def_end(0)
};
private int
no_cleanup(i_ctx_t *i_ctx_p)
{
return 0;
}
private uint
count_exec_stack(i_ctx_t *i_ctx_p, bool include_marks)
{
uint count = ref_stack_count(&e_stack);
if (!include_marks) {
uint i;
for (i = count; i--;)
if (r_has_type_attrs(ref_stack_index(&e_stack, (long)i),
t_null, a_executable))
--count;
}
return count;
}
private uint
count_to_stopped(i_ctx_t *i_ctx_p, long mask)
{
ref_stack_enum_t rsenum;
uint scanned = 0;
ref_stack_enum_begin(&rsenum, &e_stack);
do {
uint used = rsenum.size;
es_ptr ep = rsenum.ptr + used - 1;
uint count = used;
for (; count; count--, ep--)
if (r_is_estack_mark(ep) &&
estack_mark_index(ep) == es_stopped &&
(ep[2].value.intval & mask) != 0
)
return scanned + (used - count + 1);
scanned += used;
} while (ref_stack_enum_next(&rsenum));
return 0;
}
void
pop_estack(i_ctx_t *i_ctx_p, uint count)
{
uint idx = 0;
uint popped = 0;
esfile_clear_cache();
for (; idx < count; idx++) {
ref *ep = ref_stack_index(&e_stack, idx - popped);
if (r_is_estack_mark(ep)) {
ref_stack_pop(&e_stack, idx + 1 - popped);
popped = idx + 1;
(*real_opproc(ep)) (i_ctx_p);
}
}
ref_stack_pop(&e_stack, count - popped);
}
private int
unmatched_exit(os_ptr op, op_proc_t opproc)
{
make_oper(op - 1, 0, opproc);
make_int(op, e_invalidexit);
return_error(e_Quit);
}