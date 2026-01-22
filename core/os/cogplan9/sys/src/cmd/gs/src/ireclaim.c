#include "ghost.h"
#include "ierrors.h"
#include "gsstruct.h"
#include "iastate.h"
#include "icontext.h"
#include "interp.h"
#include "isave.h"
#include "isstate.h"
#include "dstack.h"
#include "estack.h"
#include "ostack.h"
#include "opdef.h"
#include "store.h"
extern void ialloc_gc_prepare(gs_ref_memory_t *);
private void gs_vmreclaim(gs_dual_memory_t *, bool);
private int ireclaim(gs_dual_memory_t *, int);
private int
ireclaim_init(i_ctx_t *i_ctx_p)
{
gs_imemory.reclaim = ireclaim;
return 0;
}
private int
ireclaim(gs_dual_memory_t * dmem, int space)
{
bool global;
gs_ref_memory_t *mem;
if (space < 0) {
int i;
mem = dmem->space_global;
for (i = 0; i < countof(dmem->spaces_indexed); ++i) {
mem = dmem->spaces_indexed[i];
if (mem == 0)
continue;
if (mem->gc_status.requested > 0 ||
((gs_ref_memory_t *)mem->stable_memory)->gc_status.requested > 0
)
break;
}
} else {
mem = dmem->spaces_indexed[space >> r_space_shift];
}
if_debug3('0', "[0]GC called, space=%d, requestor=%d, requested=%ld\n",
space, mem->space, (long)mem->gc_status.requested);
global = mem->space != avm_local;
ialloc_reset_requested(dmem);
gs_vmreclaim(dmem, global);
ialloc_set_limit(mem);
if (space < 0) {
gs_memory_status_t stats;
ulong allocated;
gs_memory_status((gs_memory_t *) mem, &stats);
allocated = stats.allocated;
if (mem->stable_memory != (gs_memory_t *)mem) {
gs_memory_status(mem->stable_memory, &stats);
allocated += stats.allocated;
}
if (allocated >= mem->gc_status.max_vm) {
return_error(e_VMerror);
}
}
return 0;
}
private void
gs_vmreclaim(gs_dual_memory_t *dmem, bool global)
{
i_ctx_t *i_ctx_p =
(i_ctx_t *)((char *)dmem - offset_of(i_ctx_t, memory));
gs_ref_memory_t *lmem = dmem->space_local;
int code = context_state_store(i_ctx_p);
gs_ref_memory_t *memories[5];
gs_ref_memory_t *mem;
int nmem, i;
memories[0] = dmem->space_system;
memories[1] = mem = dmem->space_global;
nmem = 2;
if (lmem != dmem->space_global)
memories[nmem++] = lmem;
for (i = nmem; --i >= 0;) {
mem = memories[i];
if (mem->stable_memory != (gs_memory_t *)mem)
memories[nmem++] = (gs_ref_memory_t *)mem->stable_memory;
}
for (i = nmem; --i >= 0; )
alloc_close_chunk(memories[i]);
for (i = (global ? i_vm_system : i_vm_local);
i < countof(dmem->spaces_indexed);
++i
) {
gs_ref_memory_t *mem = dmem->spaces_indexed[i];
if (mem == 0 || (i > 0 && mem == dmem->spaces_indexed[i - 1]))
continue;
if (mem->stable_memory != (gs_memory_t *)mem)
ialloc_gc_prepare((gs_ref_memory_t *)mem->stable_memory);
for (;; mem = &mem->saved->state) {
ialloc_gc_prepare(mem);
if (mem->saved == 0)
break;
}
}
{
void *ctxp = i_ctx_p;
gs_gc_root_t context_root;
gs_register_struct_root((gs_memory_t *)lmem, &context_root,
&ctxp, "i_ctx_p root");
GS_RECLAIM(&dmem->spaces, global);
gs_unregister_root((gs_memory_t *)lmem, &context_root, "i_ctx_p root");
i_ctx_p = ctxp;
dmem = &i_ctx_p->memory;
}
*systemdict = *ref_stack_index(&d_stack, ref_stack_count(&d_stack) - 1);
code = context_state_load(i_ctx_p);
dicts_gc_cleanup();
for (i = 0; i < nmem; ++i)
alloc_open_chunk(memories[i]);
}
const op_def ireclaim_l2_op_defs[] =
{
op_def_end(ireclaim_init)
};