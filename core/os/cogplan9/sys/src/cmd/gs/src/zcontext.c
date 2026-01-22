#include "memory_.h"
#include "ghost.h"
#include "gp.h"
#include "oper.h"
#include "gsexit.h"
#include "gsgc.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gxalloc.h"
#include "gxstate.h"
#include "stream.h"
#include "files.h"
#include "idict.h"
#include "igstate.h"
#include "icontext.h"
#include "interp.h"
#include "isave.h"
#include "istruct.h"
#include "dstack.h"
#include "estack.h"
#include "ostack.h"
#include "store.h"
private int reschedule_interval = 100;
extern int (*gs_interp_reschedule_proc)(i_ctx_t **);
extern int (*gs_interp_time_slice_proc)(i_ctx_t **);
extern int gs_interp_time_slice_ticks;
typedef enum {
cs_active,
cs_done
} ctx_status_t;
typedef long ctx_index_t;
typedef struct gs_context_s gs_context_t;
typedef struct gs_scheduler_s gs_scheduler_t;
struct gs_context_s {
gs_context_state_t state;
gs_scheduler_t *scheduler;
ctx_status_t status;
ctx_index_t index;
bool detach;
bool saved_local_vm;
bool visible;
ctx_index_t next_index;
ctx_index_t joiner_index;
gs_context_t *table_next;
};
inline private bool
context_is_visible(const gs_context_t *pctx)
{
return (pctx && pctx->visible);
}
inline private gs_context_t *
visible_context(gs_context_t *pctx)
{
return (pctx && pctx->visible ? pctx : (gs_context_t *)0);
}
private
CLEAR_MARKS_PROC(context_clear_marks)
{
gs_context_t *const pctx = vptr;
(*st_context_state.clear_marks)
(cmem, &pctx->state, sizeof(pctx->state), &st_context_state);
}
private
ENUM_PTRS_WITH(context_enum_ptrs, gs_context_t *pctx)
ENUM_PREFIX(st_context_state, 2);
case 0: return ENUM_OBJ(pctx->scheduler);
case 1: {
const gs_context_t *next = pctx->table_next;
while (next && !next->visible)
next = next->table_next;
return ENUM_OBJ(next);
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(context_reloc_ptrs, gs_context_t *pctx)
RELOC_PREFIX(st_context_state);
RELOC_VAR(pctx->scheduler);
RELOC_PTRS_END
gs_private_st_complex_only(st_context, gs_context_t, "gs_context_t",
context_clear_marks, context_enum_ptrs, context_reloc_ptrs, 0);
typedef struct ctx_list_s {
ctx_index_t head_index;
ctx_index_t tail_index;
} ctx_list_t;
typedef struct gs_condition_s {
ctx_list_t waiting;
} gs_condition_t;
gs_private_st_simple(st_condition, gs_condition_t, "conditiontype");
typedef struct gs_lock_s {
ctx_list_t waiting;
ctx_index_t holder_index;
gs_scheduler_t *scheduler;
} gs_lock_t;
gs_private_st_ptrs1(st_lock, gs_lock_t, "locktype",
lock_enum_ptrs, lock_reloc_ptrs, scheduler);
struct gs_scheduler_s {
gs_context_t *current;
long usertime_initial;
ctx_list_t active;
vm_reclaim_proc((*save_vm_reclaim));
ctx_index_t dead_index;
#define CTX_TABLE_SIZE 19
gs_context_t *table[CTX_TABLE_SIZE];
};
private gs_context_t *
index_context(const gs_scheduler_t *psched, long index)
{
gs_context_t *pctx;
if (index == 0)
return 0;
pctx = psched->table[index % CTX_TABLE_SIZE];
while (pctx != 0 && pctx->index != index)
pctx = pctx->table_next;
return pctx;
}
gs_private_st_composite(st_scheduler, gs_scheduler_t, "gs_scheduler",
scheduler_enum_ptrs, scheduler_reloc_ptrs);
private ENUM_PTRS_WITH(scheduler_enum_ptrs, gs_scheduler_t *psched)
{
index -= 1;
if (index < CTX_TABLE_SIZE) {
gs_context_t *pctx = psched->table[index];
while (pctx && !pctx->visible)
pctx = pctx->table_next;
return ENUM_OBJ(pctx);
}
return 0;
}
case 0: return ENUM_OBJ(visible_context(psched->current));
ENUM_PTRS_END
private RELOC_PTRS_WITH(scheduler_reloc_ptrs, gs_scheduler_t *psched)
{
if (psched->current->visible)
RELOC_VAR(psched->current);
{
int i;
for (i = 0; i < CTX_TABLE_SIZE; ++i) {
gs_context_t **ppctx = &psched->table[i];
gs_context_t **pnext;
for (; *ppctx; ppctx = pnext) {
pnext = &(*ppctx)->table_next;
if ((*ppctx)->visible)
RELOC_VAR(*ppctx);
}
}
}
}
RELOC_PTRS_END
private void
context_reclaim(vm_spaces * pspaces, bool global)
{
int i;
gs_context_t *pctx = 0;
gs_scheduler_t *psched = 0;
gs_ref_memory_t *lmem = 0;
chunk_locator_t loc;
for (i = countof(pspaces->memories.indexed) - 1; psched == 0 && i > 0; --i) {
gs_ref_memory_t *mem = pspaces->memories.indexed[i];
const gs_gc_root_t *root = mem->roots;
for (; root; root = root->next) {
if (gs_object_type((gs_memory_t *)mem, *root->p) == &st_context) {
pctx = *root->p;
psched = pctx->scheduler;
lmem = mem;
break;
}
}
}
loc.memory = (gs_ref_memory_t *)gs_memory_stable((gs_memory_t *)lmem);
loc.cp = 0;
for (i = 0; i < CTX_TABLE_SIZE; ++i)
for (pctx = psched->table[i]; pctx; pctx = pctx->table_next)
pctx->visible = chunk_locate_ptr(pctx, &loc);
#ifdef DEBUG
if (!psched->current->visible) {
lprintf("Current context is invisible!\n");
gs_abort((gs_memory_t *)lmem);
}
#endif
psched->save_vm_reclaim(pspaces, global);
for (i = 0; i < CTX_TABLE_SIZE; ++i)
for (pctx = psched->table[i]; pctx; pctx = pctx->table_next)
pctx->visible = true;
}
private int context_create(gs_scheduler_t *, gs_context_t **,
const gs_dual_memory_t *,
const gs_context_state_t *, bool);
private long context_usertime(void);
private int context_param(const gs_scheduler_t *, os_ptr, gs_context_t **);
private void context_destroy(gs_context_t *);
private void stack_copy(ref_stack_t *, const ref_stack_t *, uint, uint);
private int lock_acquire(os_ptr, gs_context_t *);
private int lock_release(ref *);
private void
context_load(gs_scheduler_t *psched, gs_context_t *pctx)
{
if_debug1('"', "[\"]loading %ld\n", pctx->index);
if ( pctx->state.keep_usertime )
psched->usertime_initial = context_usertime();
context_state_load(&pctx->state);
}
private void
context_store(gs_scheduler_t *psched, gs_context_t *pctx)
{
if_debug1('"', "[\"]storing %ld\n", pctx->index);
context_state_store(&pctx->state);
if ( pctx->state.keep_usertime )
pctx->state.usertime_total +=
context_usertime() - psched->usertime_initial;
}
private void
add_last(const gs_scheduler_t *psched, ctx_list_t *pl, gs_context_t *pc)
{
pc->next_index = 0;
if (pl->head_index == 0)
pl->head_index = pc->index;
else
index_context(psched, pl->tail_index)->next_index = pc->index;
pl->tail_index = pc->index;
}
private int ctx_initialize(i_ctx_t **);
private int ctx_reschedule(i_ctx_t **);
private int ctx_time_slice(i_ctx_t **);
private int
zcontext_init(i_ctx_t *i_ctx_p)
{
gs_interp_reschedule_proc = ctx_initialize;
gs_interp_time_slice_proc = ctx_initialize;
gs_interp_time_slice_ticks = 0;
return 0;
}
private int
ctx_initialize(i_ctx_t **pi_ctx_p)
{
i_ctx_t *i_ctx_p = *pi_ctx_p;
gs_ref_memory_t *imem = iimemory_system;
gs_scheduler_t *psched =
gs_alloc_struct_immovable((gs_memory_t *) imem, gs_scheduler_t,
&st_scheduler, "gs_scheduler");
psched->current = 0;
psched->active.head_index = psched->active.tail_index = 0;
psched->save_vm_reclaim = i_ctx_p->memory.spaces.vm_reclaim;
i_ctx_p->memory.spaces.vm_reclaim = context_reclaim;
psched->dead_index = 0;
memset(psched->table, 0, sizeof(psched->table));
if (context_create(psched, &psched->current, &gs_imemory, *pi_ctx_p, true) < 0) {
lprintf("Can't create initial context!");
gs_abort(imemory);
}
psched->current->scheduler = psched;
*pi_ctx_p = &psched->current->state;
gs_interp_reschedule_proc = ctx_reschedule;
gs_interp_time_slice_proc = ctx_time_slice;
gs_interp_time_slice_ticks = reschedule_interval;
return 0;
}
private int
ctx_reschedule(i_ctx_t **pi_ctx_p)
{
gs_context_t *current = (gs_context_t *)*pi_ctx_p;
gs_scheduler_t *psched = current->scheduler;
#ifdef DEBUG
if (*pi_ctx_p != &current->state) {
lprintf2("current->state = 0x%lx, != i_ctx_p = 0x%lx!\n",
(ulong)&current->state, (ulong)*pi_ctx_p);
}
#endif
while (psched->dead_index != 0) {
gs_context_t *dead = index_context(psched, psched->dead_index);
long next_index = dead->next_index;
if (current == dead) {
if_debug1('"', "[\"]storing dead %ld\n", current->index);
context_state_store(&current->state);
current = 0;
}
context_destroy(dead);
psched->dead_index = next_index;
}
if (current != 0)
current->saved_local_vm =
current->state.memory.space_local->saved != 0;
{
gs_context_t *prev = 0;
gs_context_t *ready;
for (ready = index_context(psched, psched->active.head_index);;
prev = ready, ready = index_context(psched, ready->next_index)
) {
if (ready == 0) {
if (current != 0)
context_store(psched, current);
lprintf("No context to run!");
return_error(e_Fatal);
}
if (ready->state.memory.space_local->saved != 0 &&
!ready->saved_local_vm
)
continue;
{
ctx_index_t next_index = ready->next_index;
if (prev)
prev->next_index = next_index;
else
psched->active.head_index = next_index;
if (!next_index)
psched->active.tail_index = (prev ? prev->index : 0);
}
break;
}
if (ready == current)
return 0;
if (current != 0)
context_store(psched, current);
psched->current = ready;
context_load(psched, ready);
*pi_ctx_p = &ready->state;
}
return 0;
}
private int
ctx_time_slice(i_ctx_t **pi_ctx_p)
{
gs_scheduler_t *psched = ((gs_context_t *)*pi_ctx_p)->scheduler;
if (psched->active.head_index == 0)
return 0;
if_debug0('"', "[\"]time-slice\n");
add_last(psched, &psched->active, psched->current);
return ctx_reschedule(pi_ctx_p);
}
private int
zcurrentcontext(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const gs_context_t *current = (const gs_context_t *)i_ctx_p;
push(1);
make_int(op, current->index);
return 0;
}
private int
zdetach(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const gs_scheduler_t *psched = ((gs_context_t *)i_ctx_p)->scheduler;
gs_context_t *pctx;
int code;
if ((code = context_param(psched, op, &pctx)) < 0)
return code;
if_debug2('\'', "[']detach %ld, status = %d\n",
pctx->index, pctx->status);
if (pctx->joiner_index != 0 || pctx->detach)
return_error(e_invalidcontext);
switch (pctx->status) {
case cs_active:
pctx->detach = true;
break;
case cs_done:
context_destroy(pctx);
}
pop(1);
return 0;
}
private int
do_fork(i_ctx_t *i_ctx_p, os_ptr op, const ref * pstdin,
const ref * pstdout, uint mcount, bool local),
values_older_than(const ref_stack_t * pstack, uint first, uint last,
int max_space);
private int
fork_done(i_ctx_t *),
fork_done_with_error(i_ctx_t *),
finish_join(i_ctx_t *),
reschedule_now(i_ctx_t *);
private int
zfork(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint mcount = ref_stack_counttomark(&o_stack);
ref rnull;
if (mcount == 0)
return_error(e_unmatchedmark);
make_null(&rnull);
return do_fork(i_ctx_p, op, &rnull, &rnull, mcount, false);
}
private int
zlocalfork(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint mcount = ref_stack_counttomark(&o_stack);
int code;
if (mcount == 0)
return_error(e_unmatchedmark);
code = values_older_than(&o_stack, 1, mcount - 1, avm_local);
if (code < 0)
return code;
code = do_fork(i_ctx_p, op - 2, op - 1, op, mcount - 2, true);
if (code < 0)
return code;
op = osp;
op[-2] = *op;
pop(2);
return code;
}
private int
do_fork(i_ctx_t *i_ctx_p, os_ptr op, const ref * pstdin, const ref * pstdout,
uint mcount, bool local)
{
gs_context_t *pcur = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = pcur->scheduler;
stream *s;
gs_dual_memory_t dmem;
gs_context_t *pctx;
ref old_userdict, new_userdict;
int code;
check_proc(*op);
if (iimemory_local->save_level)
return_error(e_invalidcontext);
if (r_has_type(pstdout, t_null)) {
code = zget_stdout(i_ctx_p, &s);
if (code < 0)
return code;
pstdout = &ref_stdio[1];
} else
check_read_file(s, pstdout);
if (r_has_type(pstdin, t_null)) {
code = zget_stdin(i_ctx_p, &s);
if (code < 0)
return code;
pstdin = &ref_stdio[0];
} else
check_read_file(s, pstdin);
dmem = gs_imemory;
if (local) {
ref *puserdict;
uint userdict_size;
gs_memory_t *parent = iimemory_local->non_gc_memory;
gs_ref_memory_t *lmem;
gs_ref_memory_t *lmem_stable;
if (dict_find_string(systemdict, "userdict", &puserdict) <= 0 ||
!r_has_type(puserdict, t_dictionary)
)
return_error(e_Fatal);
old_userdict = *puserdict;
userdict_size = dict_maxlength(&old_userdict);
lmem = ialloc_alloc_state(parent, iimemory_local->chunk_size);
lmem_stable = ialloc_alloc_state(parent, iimemory_local->chunk_size);
if (lmem == 0 || lmem_stable == 0) {
gs_free_object(parent, lmem_stable, "do_fork");
gs_free_object(parent, lmem, "do_fork");
return_error(e_VMerror);
}
lmem->space = avm_local;
lmem_stable->space = avm_local;
lmem->stable_memory = (gs_memory_t *)lmem_stable;
dmem.space_local = lmem;
code = context_create(psched, &pctx, &dmem, &pcur->state, false);
if (code < 0) {
return code;
}
code = dict_alloc(lmem, userdict_size, &new_userdict);
if (code < 0) {
context_destroy(pctx);
return code;
}
} else {
code = context_create(psched, &pctx, &dmem, &pcur->state, false);
if (code < 0) {
return code;
}
{
int n;
const gs_state *old;
gs_state *new;
for (n = 0, old = igs; old != 0; old = gs_state_saved(old))
++n;
for (old = pctx->state.pgs; old != 0; old = gs_state_saved(old))
--n;
for (; n > 0 && code >= 0; --n)
code = gs_gsave(pctx->state.pgs);
if (code < 0) {
return code;
}
for (old = igs, new = pctx->state.pgs;
old != 0 && code >= 0;
old = gs_state_saved(old), new = gs_state_saved(new)
)
code = gs_setgstate(new, old);
if (code < 0) {
return code;
}
}
}
pctx->state.language_level = i_ctx_p->language_level;
pctx->state.dict_stack.min_size = idict_stack.min_size;
pctx->state.dict_stack.userdict_index = idict_stack.userdict_index;
pctx->state.stdio[0] = *pstdin;
pctx->state.stdio[1] = *pstdout;
pctx->state.stdio[2] = pcur->state.stdio[2];
{
ref_stack_t *dstack = (ref_stack_t *)&pctx->state.dict_stack;
uint count = ref_stack_count(&d_stack);
uint copy = (local ? min_dstack_size : count);
ref_stack_push(dstack, copy);
stack_copy(dstack, &d_stack, copy, count - copy);
if (local) {
long i;
for (i = 0; i < copy; ++i) {
ref *pdref = ref_stack_index(dstack, i);
if (obj_eq(imemory, pdref, &old_userdict))
*pdref = new_userdict;
}
}
}
{
ref_stack_t *estack = (ref_stack_t *)&pctx->state.exec_stack;
ref_stack_push(estack, 3);
make_mark_estack(estack->p - 2, es_other, fork_done_with_error);
make_oper(estack->p - 1, 0, fork_done);
*estack->p = *op;
}
{
ref_stack_t *ostack = (ref_stack_t *)&pctx->state.op_stack;
uint count = mcount - 2;
ref_stack_push(ostack, count);
stack_copy(ostack, &o_stack, count, osp - op + 1);
}
pctx->state.binary_object_format = pcur->state.binary_object_format;
add_last(psched, &psched->active, pctx);
pop(mcount - 1);
op = osp;
make_int(op, pctx->index);
return 0;
}
private int
values_older_than(const ref_stack_t * pstack, uint first, uint last,
int next_space)
{
uint i;
for (i = first; i <= last; ++i)
if (r_space(ref_stack_index(pstack, (long)i)) >= next_space)
return_error(e_invalidaccess);
return 0;
}
private int
fork_done(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_context_t *pcur = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = pcur->scheduler;
if_debug2('\'', "[']done %ld%s\n", pcur->index,
(pcur->detach ? ", detached" : ""));
ref_stack_pop_to(&d_stack, min_dstack_size);
pop_estack(&pcur->state, ref_stack_count(&e_stack) - 1);
gs_grestoreall(igs);
if (iimemory_local->save_level) {
ref *prestore;
if (dict_find_string(systemdict, "restore", &prestore) <= 0) {
lprintf("restore not found in systemdict!");
return_error(e_Fatal);
}
if (pcur->detach) {
ref_stack_clear(&o_stack);
op = osp;
}
push(1);
make_tv(op, t_save, saveid, alloc_save_current_id(&gs_imemory));
push_op_estack(fork_done);
++esp;
ref_assign(esp, prestore);
return o_push_estack;
}
if (pcur->detach) {
ref_stack_clear(&o_stack);
context_store(psched, pcur);
pcur->next_index = psched->dead_index;
psched->dead_index = pcur->index;
psched->current = 0;
} else {
gs_context_t *pctx = index_context(psched, pcur->joiner_index);
pcur->status = cs_done;
if (pctx != 0)
add_last(psched, &psched->active, pctx);
}
return o_reschedule;
}
private int
fork_done_with_error(i_ctx_t *i_ctx_p)
{
return fork_done(i_ctx_p);
}
private int
zjoin(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = current->scheduler;
gs_context_t *pctx;
int code;
if ((code = context_param(psched, op, &pctx)) < 0)
return code;
if_debug2('\'', "[']join %ld, status = %d\n",
pctx->index, pctx->status);
if (pctx->joiner_index != 0 || pctx->detach || pctx == current ||
pctx->state.memory.space_global !=
current->state.memory.space_global ||
pctx->state.memory.space_local !=
current->state.memory.space_local ||
iimemory_local->save_level != 0
)
return_error(e_invalidcontext);
switch (pctx->status) {
case cs_active:
check_estack(2);
push_op_estack(finish_join);
push_op_estack(reschedule_now);
pctx->joiner_index = current->index;
return o_push_estack;
case cs_done:
{
const ref_stack_t *ostack =
(ref_stack_t *)&pctx->state.op_stack;
uint count = ref_stack_count(ostack);
push(count);
{
ref *rp = ref_stack_index(&o_stack, count);
make_mark(rp);
}
stack_copy(&o_stack, ostack, count, 0);
context_destroy(pctx);
}
}
return 0;
}
private int
finish_join(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = current->scheduler;
gs_context_t *pctx;
int code;
if ((code = context_param(psched, op, &pctx)) < 0)
return code;
if_debug2('\'', "[']finish_join %ld, status = %d\n",
pctx->index, pctx->status);
if (pctx->joiner_index != current->index)
return_error(e_invalidcontext);
pctx->joiner_index = 0;
return zjoin(i_ctx_p);
}
private int
reschedule_now(i_ctx_t *i_ctx_p)
{
return o_reschedule;
}
private int
zyield(i_ctx_t *i_ctx_p)
{
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = current->scheduler;
if (psched->active.head_index == 0)
return 0;
if_debug0('"', "[\"]yield\n");
add_last(psched, &psched->active, current);
return o_reschedule;
}
private int
monitor_cleanup(i_ctx_t *),
monitor_release(i_ctx_t *),
await_lock(i_ctx_t *);
private void
activate_waiting(gs_scheduler_t *, ctx_list_t * pcl);
private int
zcondition(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_condition_t *pcond =
ialloc_struct(gs_condition_t, &st_condition, "zcondition");
if (pcond == 0)
return_error(e_VMerror);
pcond->waiting.head_index = pcond->waiting.tail_index = 0;
push(1);
make_istruct(op, a_all, pcond);
return 0;
}
private int
zlock(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_lock_t *plock = ialloc_struct(gs_lock_t, &st_lock, "zlock");
if (plock == 0)
return_error(e_VMerror);
plock->holder_index = 0;
plock->waiting.head_index = plock->waiting.tail_index = 0;
push(1);
make_istruct(op, a_all, plock);
return 0;
}
private int
zmonitor(i_ctx_t *i_ctx_p)
{
gs_context_t *current = (gs_context_t *)i_ctx_p;
os_ptr op = osp;
gs_lock_t *plock;
gs_context_t *pctx;
int code;
check_stype(op[-1], st_lock);
check_proc(*op);
plock = r_ptr(op - 1, gs_lock_t);
pctx = index_context(current->scheduler, plock->holder_index);
if_debug1('\'', "[']monitor 0x%lx\n", (ulong) plock);
if (pctx != 0) {
if (pctx == current ||
(iimemory_local->save_level != 0 &&
pctx->state.memory.space_local ==
current->state.memory.space_local)
)
return_error(e_invalidcontext);
}
check_estack(4);
code = lock_acquire(op - 1, current);
if (code != 0) {
push_op_estack(zmonitor);
return code;
}
*++esp = op[-1];
push_mark_estack(es_other, monitor_cleanup);
push_op_estack(monitor_release);
*++esp = *op;
pop(2);
return o_push_estack;
}
private int
monitor_cleanup(i_ctx_t *i_ctx_p)
{
int code = lock_release(esp);
if (code < 0)
return code;
--esp;
return o_pop_estack;
}
private int
monitor_release(i_ctx_t *i_ctx_p)
{
int code = lock_release(esp - 1);
if (code < 0)
return code;
esp -= 2;
return o_pop_estack;
}
private int
znotify(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_condition_t *pcond;
check_stype(*op, st_condition);
pcond = r_ptr(op, gs_condition_t);
if_debug1('"', "[\"]notify 0x%lx\n", (ulong) pcond);
pop(1);
op--;
if (pcond->waiting.head_index == 0)
return 0;
activate_waiting(current->scheduler, &pcond->waiting);
return zyield(i_ctx_p);
}
private int
zwait(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = current->scheduler;
gs_lock_t *plock;
gs_context_t *pctx;
gs_condition_t *pcond;
check_stype(op[-1], st_lock);
plock = r_ptr(op - 1, gs_lock_t);
check_stype(*op, st_condition);
pcond = r_ptr(op, gs_condition_t);
if_debug2('"', "[\"]wait lock 0x%lx, condition 0x%lx\n",
(ulong) plock, (ulong) pcond);
pctx = index_context(psched, plock->holder_index);
if (pctx == 0 || pctx != psched->current ||
(iimemory_local->save_level != 0 &&
(r_space(op - 1) == avm_local || r_space(op) == avm_local))
)
return_error(e_invalidcontext);
check_estack(1);
lock_release(op - 1);
add_last(psched, &pcond->waiting, pctx);
push_op_estack(await_lock);
return o_reschedule;
}
private int
await_lock(i_ctx_t *i_ctx_p)
{
gs_context_t *current = (gs_context_t *)i_ctx_p;
os_ptr op = osp;
int code = lock_acquire(op - 1, current);
if (code == 0) {
pop(2);
return 0;
}
push_op_estack(await_lock);
return code;
}
private void
activate_waiting(gs_scheduler_t *psched, ctx_list_t * pcl)
{
gs_context_t *pctx = index_context(psched, pcl->head_index);
gs_context_t *next;
for (; pctx != 0; pctx = next) {
next = index_context(psched, pctx->next_index);
add_last(psched, &psched->active, pctx);
}
pcl->head_index = pcl->tail_index = 0;
}
private int
zusertime_context(i_ctx_t *i_ctx_p)
{
gs_context_t *current = (gs_context_t *)i_ctx_p;
gs_scheduler_t *psched = current->scheduler;
os_ptr op = osp;
long utime = context_usertime();
push(1);
if (!current->state.keep_usertime) {
psched->usertime_initial = utime;
current->state.keep_usertime = true;
}
make_int(op, current->state.usertime_total + utime -
psched->usertime_initial);
return 0;
}
private int
context_create(gs_scheduler_t * psched, gs_context_t ** ppctx,
const gs_dual_memory_t * dmem,
const gs_context_state_t *i_ctx_p, bool copy_state)
{
gs_memory_t *mem = gs_memory_stable((gs_memory_t *)dmem->space_local);
gs_context_t *pctx;
int code;
long ctx_index;
gs_context_t **pte;
pctx = gs_alloc_struct(mem, gs_context_t, &st_context, "context_create");
if (pctx == 0)
return_error(e_VMerror);
if (copy_state) {
pctx->state = *i_ctx_p;
} else {
gs_context_state_t *pctx_st = &pctx->state;
code = context_state_alloc(&pctx_st, systemdict, dmem);
if (code < 0) {
gs_free_object(mem, pctx, "context_create");
return code;
}
}
ctx_index = gs_next_ids(mem, 1);
pctx->scheduler = psched;
pctx->status = cs_active;
pctx->index = ctx_index;
pctx->detach = false;
pctx->saved_local_vm = false;
pctx->visible = true;
pctx->next_index = 0;
pctx->joiner_index = 0;
pte = &psched->table[ctx_index % CTX_TABLE_SIZE];
pctx->table_next = *pte;
*pte = pctx;
*ppctx = pctx;
if (gs_debug_c('\'') | gs_debug_c('"'))
dlprintf2("[']create %ld at 0x%lx\n", ctx_index, (ulong) pctx);
return 0;
}
private int
context_param(const gs_scheduler_t * psched, os_ptr op, gs_context_t ** ppctx)
{
gs_context_t *pctx;
check_type(*op, t_integer);
pctx = index_context(psched, op->value.intval);
if (pctx == 0)
return_error(e_invalidcontext);
*ppctx = pctx;
return 0;
}
private long
context_usertime(void)
{
long secs_ns[2];
gp_get_usertime(secs_ns);
return secs_ns[0] * 1000 + secs_ns[1] / 1000000;
}
private void
context_destroy(gs_context_t * pctx)
{
gs_ref_memory_t *mem = pctx->state.memory.space_local;
gs_scheduler_t *psched = pctx->scheduler;
gs_context_t **ppctx = &psched->table[pctx->index % CTX_TABLE_SIZE];
while (*ppctx != pctx)
ppctx = &(*ppctx)->table_next;
*ppctx = (*ppctx)->table_next;
if (gs_debug_c('\'') | gs_debug_c('"'))
dlprintf3("[']destroy %ld at 0x%lx, status = %d\n",
pctx->index, (ulong) pctx, pctx->status);
if (!context_state_free(&pctx->state))
gs_free_object((gs_memory_t *) mem, pctx, "context_destroy");
}
private void
stack_copy(ref_stack_t * to, const ref_stack_t * from, uint count,
uint from_index)
{
long i;
for (i = (long)count - 1; i >= 0; --i)
*ref_stack_index(to, i) = *ref_stack_index(from, i + from_index);
}
private int
lock_acquire(os_ptr op, gs_context_t * pctx)
{
gs_lock_t *plock = r_ptr(op, gs_lock_t);
if (plock->holder_index == 0) {
plock->holder_index = pctx->index;
plock->scheduler = pctx->scheduler;
return 0;
}
add_last(pctx->scheduler, &plock->waiting, pctx);
return o_reschedule;
}
private int
lock_release(ref * op)
{
gs_lock_t *plock = r_ptr(op, gs_lock_t);
gs_scheduler_t *psched = plock->scheduler;
gs_context_t *pctx = index_context(psched, plock->holder_index);
if (pctx != 0 && pctx == psched->current) {
plock->holder_index = 0;
activate_waiting(psched, &plock->waiting);
return 0;
}
return_error(e_invalidcontext);
}
const op_def zcontext1_op_defs[] = {
{"0condition", zcondition},
{"0currentcontext", zcurrentcontext},
{"1detach", zdetach},
{"2.fork", zfork},
{"1join", zjoin},
{"4.localfork", zlocalfork},
{"0lock", zlock},
{"2monitor", zmonitor},
{"1notify", znotify},
{"2wait", zwait},
{"0yield", zyield},
{"0usertime", zusertime_context},
op_def_end(0)
};
const op_def zcontext2_op_defs[] = {
{"0%fork_done", fork_done},
{"1%finish_join", finish_join},
{"0%monitor_cleanup", monitor_cleanup},
{"0%monitor_release", monitor_release},
{"2%await_lock", await_lock},
op_def_end(zcontext_init)
};